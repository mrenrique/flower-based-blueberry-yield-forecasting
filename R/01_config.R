# ============================================================
# 1. CONFIGURACIÓN, PERIODO Y HORIZONTE
# ============================================================
# Lee config/config.yml, resuelve la semana base y la semana de proyección,
# arma las rutas de los archivos y deriva el horizonte efectivo desde el
# programa de cosecha.
#
# Nada de rutas, IDs ni umbrales está incrustado en el código: todo viene
# del YAML. Eso es lo que permite publicar el repositorio y, al mismo
# tiempo, correrlo contra la base real cambiando un solo archivo.

# --- 1.1 Lectura del YAML -------------------------------------------
ruta_config <- file.path(RUTA_REPO, "config", "config.yml")

# config.yml está en .gitignore. Si no existe todavía, se usa el ejemplo:
# viene configurado en modo demo, así que un clon recién bajado corre sin
# ningún paso previo. Para trabajar contra la base real hay que copiarlo.
if (!file.exists(ruta_config)) {
  ruta_config <- file.path(RUTA_REPO, "config", "config.example.yml")
  message("[!] No existe config/config.yml. Se usa config.example.yml (modo demo).\n",
          "    Para una corrida real: copia el ejemplo como config/config.yml y ajústalo.")
}

config <- yaml::read_yaml(ruta_config)

if (!config$fuente %in% c("demo", "sqlserver")) {
  stop("config$fuente debe ser 'demo' o 'sqlserver'. Valor recibido: ", config$fuente)
}

# El YAML puede devolver la lista de SubParcelas como list: se normaliza
# a vector de texto porque se usa en filtros y en el IN de la consulta SQL.
config$subparcelas <- as.character(unlist(config$subparcelas))

if (length(config$subparcelas) == 0) {
  stop("config$subparcelas no puede estar vacío.")
}

message("[OK] Configuración cargada | fuente: ", config$fuente)

# --- 1.2 Resolución de rutas ----------------------------------------
# Las rutas del YAML pueden ser relativas al repositorio o absolutas.
es_ruta_absoluta <- function(p) grepl("^([A-Za-z]:|[\\\\/]|~)", p)

resolver_ruta <- function(p) {
  if (is.null(p)) return(NULL)
  if (es_ruta_absoluta(p)) path.expand(p) else file.path(RUTA_REPO, p)
}

ruta_insumos     <- resolver_ruta(config$rutas$insumos)
ruta_fuente_demo <- resolver_ruta(config$rutas$fuente_demo)
ruta_salida      <- resolver_ruta(config$rutas$salida)
# Carpeta alternativa opcional donde buscar un insumo que no esté en la
# ruta oficial (reemplaza el fallback a Descargas que estaba incrustado).
ruta_fallback    <- resolver_ruta(config$rutas$fallback_insumos)

if (!dir.exists(ruta_salida)) {
  dir.create(ruta_salida, recursive = TRUE)
  message("[OK] Carpeta de salida creada: ", ruta_salida)
}

# --- 1.3 Resolución de la semana base -------------------------------
# - Con anio_base + semana_base definidos: modo manual.
# - Con ambos en null: usa la fecha del sistema.
# - Solo uno definido: error.
# Permite reproyectar una semana pasada sin editar config.yml. Sirve para
# backtesting y para comparar la proyección de dos semanas consecutivas:
#   desde la terminal:     SEMANA_BASE=34 Rscript main.R
#   dentro de una sesión:  SEMANA_BASE_OVERRIDE <- 34; source("main.R")
semana_base_override <- if (exists("SEMANA_BASE_OVERRIDE")) {
  SEMANA_BASE_OVERRIDE
} else if (nzchar(Sys.getenv("SEMANA_BASE"))) {
  as.integer(Sys.getenv("SEMANA_BASE"))
} else {
  NULL
}

if (!is.null(semana_base_override)) {
  config$semana_base <- semana_base_override
  message("[!] Semana base forzada a S", semana_base_override, " (override)")
}

anio_base_cfg   <- config$anio_base
semana_base_cfg <- config$semana_base

if (xor(is.null(anio_base_cfg), is.null(semana_base_cfg))) {
  stop("Define ambos (anio_base y semana_base) o deja ambos en null para usar la semana actual.")
}

anio_actual   <- if (!is.null(anio_base_cfg))   anio_base_cfg   else lubridate::isoyear(Sys.Date())
semana_actual <- if (!is.null(semana_base_cfg)) semana_base_cfg else lubridate::isoweek(Sys.Date())

if (length(anio_actual) != 1 || is.na(anio_actual) || !is.finite(anio_actual)) {
  stop("anio_actual debe contener un único año válido.")
}

if (length(semana_actual) != 1 || is.na(semana_actual) || !semana_actual %in% 1:53) {
  stop("semana_actual debe estar entre 1 y 53.")
}

fecha_base_iso <- fecha_semana_iso(anio_actual, semana_actual)

# La semana 53 no existe en todos los años: la ida y vuelta lo detecta.
if (lubridate::isoyear(fecha_base_iso) != anio_actual ||
    lubridate::isoweek(fecha_base_iso) != semana_actual) {
  stop("La combinación anio_base-semana_base no corresponde a una semana ISO válida.")
}

# La proyección arranca en la semana siguiente a la base.
fecha_proyeccion  <- fecha_base_iso + 7
anio_proyeccion   <- lubridate::isoyear(fecha_proyeccion)
semana_proyeccion <- lubridate::isoweek(fecha_proyeccion)

# --- 1.4 Periodos como string AAAASS --------------------------------
periodo_actual     <- paste0(anio_actual,     sprintf("%02d", semana_actual))
periodo_proyeccion <- paste0(anio_proyeccion, sprintf("%02d", semana_proyeccion))

# --- 1.5 Archivos de entrada ----------------------------------------
# Reemplaza los marcadores de periodo en los nombres del YAML.
nombre_archivo <- function(plantilla) {
  plantilla %>%
    stringr::str_replace_all(stringr::fixed("{periodo_actual}"),     periodo_actual) %>%
    stringr::str_replace_all(stringr::fixed("{periodo_proyeccion}"), periodo_proyeccion)
}

# Busca el archivo en la ruta oficial y, si no está, en la ruta alternativa.
resolver_insumo <- function(plantilla) {
  nombre   <- nombre_archivo(plantilla)
  oficial  <- file.path(ruta_insumos, nombre)

  if (file.exists(oficial) || is.null(ruta_fallback)) return(oficial)

  alternativo <- file.path(ruta_fallback, nombre)
  if (file.exists(alternativo)) {
    message("[!] ", nombre, " no está en la ruta oficial. Se usará: ", alternativo)
    return(alternativo)
  }

  oficial
}

archivo_pesos_ejecutados  <- resolver_insumo(config$archivos$pesos_ejecutados)
archivo_pesos_proyectados <- resolver_insumo(config$archivos$pesos_proyectados)
archivo_params_mad_viab   <- resolver_insumo(config$archivos$parametros_mad_viab)
archivo_kilos_ejecutados  <- resolver_insumo(config$archivos$kilos_sap)
archivo_programa_cosecha  <- resolver_insumo(config$archivos$programa_cosecha)

# --- 1.6 Archivos de salida -----------------------------------------
# Ambos outputs llevan el periodo de proyección: cada corrida semanal deja
# su propio archivo y queda histórico comparable.
archivo_salida_flores_distribuidas <- file.path(
  ruta_salida, paste0("flores_proyectadas_por_fecha_teorica_cosecha_", periodo_proyeccion, ".xlsx")
)

archivo_salida_proyeccion_kilos <- file.path(
  ruta_salida, paste0("proyeccion_kilos_flores_", periodo_proyeccion, ".xlsx")
)

# --- 1.7 Existencia y estructura de los insumos ---------------------
archivos_requeridos <- c(
  archivo_pesos_ejecutados,
  archivo_pesos_proyectados,
  archivo_params_mad_viab,
  archivo_kilos_ejecutados,
  archivo_programa_cosecha
)

# Stand-in de la extracción SQL en modo demo (mismo esquema que devuelve
# SQL Server, para que el módulo 02 no distinga el origen).
archivo_eval_demo    <- file.path(ruta_fuente_demo, "eval_flores.csv")
archivo_maestro_demo <- file.path(ruta_fuente_demo, "maestro_lotes.csv")

if (config$fuente == "demo") {
  archivos_requeridos <- c(archivos_requeridos, archivo_eval_demo, archivo_maestro_demo)
}

# En modo demo los insumos se generan solos la primera vez. Para forzar una
# regeneración, borrar la carpeta data/demo y volver a ejecutar.
if (config$fuente == "demo" && !all(file.exists(archivos_requeridos))) {
  message("[!] Faltan insumos demo. Generándolos...")
  source(file.path(RUTA_REPO, "R", "make_demo_data.R"), encoding = "UTF-8", local = FALSE)
}

faltantes <- archivos_requeridos[!file.exists(archivos_requeridos)]

if (length(faltantes) > 0) {
  stop("No se encontraron los siguientes archivos:\n", paste(faltantes, collapse = "\n"))
}

validar_hojas_excel(archivo_params_mad_viab,  c("Viabilidad", "Parametros"))
validar_hojas_excel(archivo_kilos_ejecutados, "data")
validar_hojas_excel(archivo_programa_cosecha, "programacion")

# --- 1.8 Horizonte efectivo según el programa de cosecha ------------
# El horizonte NO es un número fijo de semanas: se toma de la última fecha
# programada para las SubParcelas configuradas. Si el programa se queda
# corto, la corrida se corta ahí.
horizonte_programado <- readxl::read_excel(archivo_programa_cosecha, sheet = "programacion") %>%
  transmute(
    SubParcela = codigo_subparcela,
    Fecha_Cosecha_Programada = as.Date(fecha)
  ) %>%
  filter(
    SubParcela %in% config$subparcelas,
    !is.na(Fecha_Cosecha_Programada)
  )

if (nrow(horizonte_programado) == 0) {
  stop(
    "El programa de cosecha no contiene fechas programadas para las SubParcelas configuradas: ",
    paste(config$subparcelas, collapse = ", ")
  )
}

fecha_max_programada <- max(horizonte_programado$Fecha_Cosecha_Programada)

if (fecha_max_programada < fecha_proyeccion) {
  stop(
    "El programa de cosecha no contiene fechas dentro del horizonte futuro de proyección. ",
    "Última fecha programada: ", fecha_max_programada,
    " | Inicio de proyección: ", fecha_proyeccion
  )
}

# Semanas del horizonte, incluyendo la inicial y la final
horizonte_semanas <- floor(as.numeric(fecha_max_programada - fecha_proyeccion) / 7) + 1

message(sprintf(
  "[OK] Semana base: %d-S%02d | Proyección: %d-S%02d -> %d-S%02d (horizonte: %d semanas)",
  anio_actual, semana_actual,
  anio_proyeccion, semana_proyeccion,
  lubridate::isoyear(fecha_max_programada), lubridate::isoweek(fecha_max_programada),
  horizonte_semanas
))
