# ============================================================
# 0. FUNCIONES PURAS: HELPERS, LECTORES Y VALIDACIONES
# ============================================================
# Todo lo que no depende del estado del pipeline vive aquí. Es el único
# módulo que se puede cargar solo, y por eso es el que cubren los tests
# de tests/testthat.
#
# Requiere que dplyr esté cargado con library(dplyr) (lo hace main.R).
# El resto de paquetes se llama con paquete::funcion.

# --- 0.1 Helper Año-Semana ISO -> lunes de esa semana ---------------
# Implementado en R base para no depender del paquete ISOweek.
# Regla ISO 8601: el 4 de enero siempre cae en la semana 1.
# Vectorizado: acepta columnas completas.
fecha_semana_iso <- function(anio, semana) {
  anio   <- as.integer(anio)
  semana <- as.integer(semana)

  cuatro_enero <- as.Date(sprintf("%04d-01-04", anio))
  # format "%u" devuelve el día ISO de la semana: lunes = 1 ... domingo = 7
  lunes_semana_1 <- cuatro_enero - (as.integer(format(cuatro_enero, "%u")) - 1)

  lunes_semana_1 + (semana - 1) * 7
}

# --- 0.2 Carga de variables de entorno desde un archivo .env --------
# Parser mínimo para no depender del paquete dotenv.
# Formato aceptado: CLAVE=valor, una por línea. Ignora vacías y comentarios.
cargar_env <- function(ruta) {
  if (!file.exists(ruta)) {
    stop("No se encontró el archivo .env en:\n - ", ruta)
  }

  lineas <- trimws(readLines(ruta, warn = FALSE))
  lineas <- lineas[nzchar(lineas) & !startsWith(lineas, "#")]

  for (linea in lineas) {
    partes <- strsplit(linea, "=", fixed = TRUE)[[1]]
    if (length(partes) < 2) next

    clave <- trimws(partes[1])
    # Reconstruye el valor por si contenía "=" (típico en contraseñas)
    valor <- trimws(paste(partes[-1], collapse = "="))
    valor <- gsub('^"|"$|^\'|\'$', "", valor)

    do.call(Sys.setenv, setNames(list(valor), clave))
  }

  message("[OK] .env cargado desde: ", ruta)
}

# --- 0.3 Lectura de los CSV de la fuente demo -----------------------
# Reemplazan la extracción SQL en modo demo. R base alcanza para leer dos
# archivos planos y evita sumar una dependencia.
leer_csv_demo <- function(ruta) {
  tibble::as_tibble(utils::read.csv(ruta, stringsAsFactors = FALSE, encoding = "UTF-8"))
}

# --- 0.4 Curva de maduración normalizada ---------------------------
# Reparte el 100 % de las flores de una semana de evaluación entre los días
# 1..periodo, con forma normal truncada y normalizada a suma 1.
# Es el cálculo central del modelo: de aquí sale la forma de la curva.
curva_maduracion_normalizada <- function(periodo, media, desviacion, etiqueta = "") {
  dias  <- seq_len(periodo)
  pesos <- dnorm(dias, mean = media, sd = desviacion)

  # Evita normalizar una curva no finita o con masa total nula
  if (!all(is.finite(pesos)) || sum(pesos) <= 0) {
    stop("Curva de maduración inválida",
         if (nzchar(etiqueta)) paste0(" para ", etiqueta) else "", ".")
  }

  tibble::tibble(
    Dias         = dias,
    DISTRIBUCION = pesos / sum(pesos)
  )
}

# --- 0.5 Agrupamiento de registros de cosecha en pasadas ------------
# Recibe un vector de fechas YA ORDENADO de forma ascendente y devuelve el
# id de pasada de cada fecha. Una separación >= dias_nueva_pasada inicia
# una pasada nueva.
# OJO: con dias_nueva_pasada = 2, cosecha en sábado y luego lunes
# (diferencia de 2 días) cuenta como dos pasadas distintas.
agrupar_pasadas_cosecha <- function(fechas, dias_nueva_pasada) {
  if (is.unsorted(fechas, na.rm = TRUE)) {
    stop("agrupar_pasadas_cosecha requiere las fechas ordenadas de forma ascendente.")
  }

  diff_dias <- as.numeric(fechas - lag(fechas))
  cumsum(is.na(diff_dias) | diff_dias >= dias_nueva_pasada)
}

# --- 0.6 Lector del archivo de pesos proyectados --------------------
# El archivo del área de Proyecciones tiene layout POSICIONAL, una hoja por
# SubParcela (nombre de hoja = código de SubParcela):
#   fila 1 = años (desde la columna 3) | fila 2 = semanas | fila 6 = pesos
# Si el archivo cambia de estructura, este es el único punto a ajustar.
leer_subparcela <- function(hoja, archivo) {
  # suppressMessages silencia el aviso por columnas sin encabezado, que aquí
  # es esperado: el layout es posicional, no tiene cabecera.
  raw <- suppressMessages(
    readxl::read_excel(path = archivo, sheet = hoja, col_names = FALSE)
  )

  if (nrow(raw) < 6 || ncol(raw) < 3) {
    stop("La hoja ", hoja, " no tiene la estructura mínima esperada: 6 filas y 3 columnas.")
  }

  anios <- raw %>%
    slice(1) %>% select(-(1:2)) %>%
    unlist(use.names = FALSE) %>% as.integer()

  semanas <- raw %>%
    slice(2) %>% select(-(1:2)) %>%
    unlist(use.names = FALSE) %>% as.integer()

  if (anyNA(anios) || anyNA(semanas) || any(semanas < 1 | semanas > 53)) {
    stop("La hoja ", hoja, " contiene Año/Semana vacíos o inválidos en las filas 1-2.")
  }

  datos <- raw %>% slice(6)

  if (nrow(datos) == 0) {
    stop("No se pudo extraer la fila 6 en la hoja ", hoja, ".")
  }

  names(datos)[1:2] <- c("serie", "etiqueta")
  cols_valor <- names(datos)[3:ncol(datos)]

  datos %>%
    mutate(across(all_of(cols_valor), as.numeric)) %>%
    tidyr::pivot_longer(
      cols = all_of(cols_valor), names_to = "columna_excel", values_to = "Peso"
    ) %>%
    mutate(
      indice     = match(columna_excel, cols_valor),
      Anio       = anios[indice],
      Semana     = semanas[indice],
      SubParcela = hoja
    ) %>%
    select(SubParcela, Anio, Semana, Peso)
}

# --- 0.7 Imputación de una semana de evaluación faltante ------------
# Busca la semana real anterior y la posterior más cercanas de la misma
# SubParcela e interpola punto por punto (misma Muestra, Planta y Brote).
# Con una sola vecina disponible, copia ese valor.
#   objetivo       : una fila con SubParcela, Anio_Eval, Semana_Eval, Inicio_Semana
#   datos          : evaluaciones reales, con columna Inicio_Semana
#   semanas_reales : combinaciones SubParcela-semana realmente evaluadas
imputar_una_semana <- function(objetivo, datos, semanas_reales) {
  subparcela <- objetivo$SubParcela[[1]]
  fecha_obj  <- objetivo$Inicio_Semana[[1]]
  anio_obj   <- objetivo$Anio_Eval[[1]]
  semana_obj <- objetivo$Semana_Eval[[1]]

  fechas_reales <- semanas_reales %>%
    filter(SubParcela == subparcela) %>%
    pull(Inicio_Semana)

  anteriores  <- fechas_reales[fechas_reales < fecha_obj]
  posteriores <- fechas_reales[fechas_reales > fecha_obj]

  fecha_ant <- if (length(anteriores)  > 0) max(anteriores)  else as.Date(NA)
  fecha_pos <- if (length(posteriores) > 0) min(posteriores) else as.Date(NA)

  if (is.na(fecha_ant) && is.na(fecha_pos)) {
    stop("No existen semanas reales de referencia para ",
         subparcela, " ", anio_obj, "-S", sprintf("%02d", semana_obj))
  }

  # Distancias en semanas: son los pesos de la interpolación lineal
  dist_ant <- if (!is.na(fecha_ant)) as.numeric(fecha_obj - fecha_ant) / 7 else NA_real_
  dist_pos <- if (!is.na(fecha_pos)) as.numeric(fecha_pos - fecha_obj) / 7 else NA_real_

  claves <- c("Fundo", "SubParcela", "Lote", "idLote",
              "Campania", "Muestra", "Planta", "Brote")

  datos_ant <- datos %>%
    filter(SubParcela == subparcela, Inicio_Semana == fecha_ant) %>%
    select(all_of(claves), Fecha_Anterior = Fecha_Evaluacion, Flores_Anterior = Flores)

  datos_pos <- datos %>%
    filter(SubParcela == subparcela, Inicio_Semana == fecha_pos) %>%
    select(all_of(claves), Fecha_Posterior = Fecha_Evaluacion, Flores_Posterior = Flores)

  full_join(datos_ant, datos_pos, by = claves, relationship = "one-to-one") %>%
    mutate(
      Metodo_Imputacion = case_when(
        !is.na(Flores_Anterior) & !is.na(Flores_Posterior) ~ "INTERPOLACION_ANTERIOR_POSTERIOR",
        !is.na(Flores_Anterior)                            ~ "REFERENCIA_ANTERIOR",
        !is.na(Flores_Posterior)                           ~ "REFERENCIA_POSTERIOR",
        TRUE                                               ~ "SIN_REFERENCIA"
      ),

      Flores = case_when(
        !is.na(Flores_Anterior) & !is.na(Flores_Posterior) ~
          Flores_Anterior +
          (Flores_Posterior - Flores_Anterior) * dist_ant / (dist_ant + dist_pos),
        !is.na(Flores_Anterior)  ~ Flores_Anterior,
        !is.na(Flores_Posterior) ~ Flores_Posterior,
        TRUE                     ~ NA_real_
      ),

      # Mantiene el mismo día de la semana que la evaluación de referencia,
      # para que la semana imputada tenga una sola fecha (ver módulo 08.0).
      Fecha_Evaluacion = case_when(
        !is.na(Fecha_Anterior)  ~ Fecha_Anterior  + dist_ant * 7,
        !is.na(Fecha_Posterior) ~ Fecha_Posterior - dist_pos * 7
      ),

      Anio_Eval   = anio_obj,
      Semana_Eval = semana_obj,

      Semana_Referencia_Anterior  = fecha_ant,
      Semana_Referencia_Posterior = fecha_pos
    )
}

# ============================================================
# 0.8 VALIDACIONES FAIL-FAST
# ============================================================
# Todas detienen la ejecución. La idea es que un problema de datos se
# detecte antes de generar una proyección incorrecta, no después.

# Detiene si existen filas duplicadas para la combinación de claves indicada.
validar_unicidad <- function(df, claves, nombre_df = deparse(substitute(df))) {
  dup <- df %>%
    count(across(all_of(claves)), name = "n") %>%
    filter(n > 1)

  if (nrow(dup) > 0) {
    print(dup)
    stop(glue::glue("Hay duplicados en {nombre_df} para las claves: {paste(claves, collapse = ', ')}"))
  }
}

# Detiene si alguna columna indicada tiene valores NA, no finitos o <= 0.
validar_mayor_a_cero <- function(df, columnas, nombre_df = deparse(substitute(df))) {
  for (col in columnas) {
    if (!is.numeric(df[[col]])) {
      stop(glue::glue("{col} debe ser numérica en {nombre_df}."))
    }

    invalidos <- df %>%
      filter(is.na(.data[[col]]) | !is.finite(.data[[col]]) | .data[[col]] <= 0)

    if (nrow(invalidos) > 0) {
      print(invalidos)
      stop(glue::glue("Hay registros con {col} NA, no finito o <= 0 en {nombre_df}."))
    }
  }
}

# Detiene si el data frame está vacío.
validar_no_vacio <- function(df, nombre_df = deparse(substitute(df))) {
  if (nrow(df) == 0) {
    stop(glue::glue("{nombre_df} no contiene registros."))
  }
}

# Detiene si faltan columnas requeridas.
validar_columnas <- function(df, columnas, nombre_df = deparse(substitute(df))) {
  faltantes <- setdiff(columnas, names(df))
  if (length(faltantes) > 0) {
    stop(glue::glue(
      "Faltan columnas requeridas en {nombre_df}: {paste(faltantes, collapse = ', ')}"
    ))
  }
}

# Detiene si un Excel no tiene las hojas requeridas.
validar_hojas_excel <- function(archivo, hojas) {
  faltantes <- setdiff(hojas, readxl::excel_sheets(archivo))
  if (length(faltantes) > 0) {
    stop("El archivo:\n", archivo,
         "\nno contiene las hojas requeridas: ", paste(faltantes, collapse = ", "))
  }
}

# Detiene si una columna Año/Semana no corresponde a semanas ISO válidas.
# SUPUESTO EN EL BORDE DE DATOS: los insumos Excel entregan la semana en
# convención ISO. No es verificable desde el archivo (no trae la fecha), así
# que solo se valida el rango y que la combinación Año-Semana exista
# realmente: la semana 53 no existe en todos los años.
validar_semana_iso <- function(df, col_anio, col_semana, nombre_df = deparse(substitute(df))) {
  invalidos <- df %>%
    filter(
      is.na(.data[[col_anio]]) | is.na(.data[[col_semana]]) |
      .data[[col_semana]] < 1 | .data[[col_semana]] > 53
    )

  if (nrow(invalidos) > 0) {
    print(invalidos)
    stop(glue::glue("Hay valores {col_anio}/{col_semana} inválidos en {nombre_df}."))
  }

  inexistentes <- df %>%
    distinct(anio = .data[[col_anio]], semana = .data[[col_semana]]) %>%
    mutate(
      fecha  = fecha_semana_iso(anio, semana),
      valida = lubridate::isoyear(fecha) == anio & lubridate::isoweek(fecha) == semana
    ) %>%
    filter(!valida)

  if (nrow(inexistentes) > 0) {
    print(inexistentes)
    stop(glue::glue("{nombre_df} contiene combinaciones Año-Semana que no son semanas ISO válidas."))
  }
}
