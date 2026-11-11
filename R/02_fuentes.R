# ============================================================
# 2. FUENTES BASE: EVALUACIÓN DE FLORES Y MAESTRO DE LOTES
# ============================================================
# Produce dos tablas con el mismo esquema sin importar el origen:
#   eval_flores   : conteo de flores por planta, brote y fecha de evaluación
#   maestro_lotes : atributos de catastro de cada lote de la campaña
#
# Las columnas internas se mantienen en ASCII (Campania, Anio_*) para no
# depender del locale de Windows. Los nombres con tilde se reponen recién
# al exportar, en el módulo 09, para no romper los reportes Excel que ya
# consumen el archivo de salida.

# --- 2.1 Extracción ------------------------------------------------
if (config$fuente == "sqlserver") {

  # --- 2.1.a SQL Server ---
  for (pq in c("DBI", "odbc")) {
    if (!requireNamespace(pq, quietly = TRUE)) {
      stop("El modo 'sqlserver' requiere el paquete ", pq, ". Instálalo o usa fuente: demo.")
    }
  }

  cargar_env(file.path(RUTA_REPO, ".env"))

  # Driver ODBC: usa el configurado o el primero disponible que mencione SQL Server.
  drivers <- unique(odbc::odbcListDrivers()$name)

  driver_sql <- if (!is.null(config$sql$driver_odbc)) {
    if (!config$sql$driver_odbc %in% drivers) {
      stop("El driver configurado no está instalado: ", config$sql$driver_odbc,
           "\nDrivers disponibles:\n", paste(drivers, collapse = "\n"))
    }
    config$sql$driver_odbc
  } else {
    candidatos <- drivers[grepl("SQL Server", drivers, ignore.case = TRUE)]
    if (length(candidatos) == 0) {
      stop("No se encontró ningún driver ODBC de SQL Server. Instalados:\n",
           paste(drivers, collapse = "\n"))
    }
    candidatos[1]
  }

  message("[OK] Driver ODBC: ", driver_sql)

  conexion <- DBI::dbConnect(
    odbc::odbc(),
    Driver                 = driver_sql,
    Server                 = Sys.getenv("SQL_SERVER"),
    Database               = Sys.getenv("SQL_DATABASE"),
    UID                    = Sys.getenv("SQL_USER"),
    PWD                    = Sys.getenv("SQL_PASSWORD"),
    Encrypt                = if (isTRUE(config$sql$encrypt)) "yes" else "no",
    TrustServerCertificate = if (isTRUE(config$sql$confiar_certificado)) "yes" else "no",
    timeout                = config$sql$timeout
  )

  # finally garantiza el cierre de la conexión aunque una consulta falle.
  # Sin esto, cualquier error entre el connect y el disconnect deja la
  # conexión abierta en el servidor.
  # Los nombres de tabla vienen de config$sql$tablas. En glue_sql, {`x`}
  # (con backticks) interpola un IDENTIFICADOR citado, mientras que {x}
  # interpola un literal: los valores del usuario nunca se concatenan crudos.
  tb <- config$sql$tablas

  tablas_requeridas <- c("evaluacion_flores", "lote", "subparcela", "parcela",
                         "fundo", "variedad", "cultivo", "area_cultivable")
  tablas_faltantes <- setdiff(tablas_requeridas, names(tb))

  if (length(tablas_faltantes) > 0) {
    stop("Faltan nombres de tabla en config$sql$tablas: ",
         paste(tablas_faltantes, collapse = ", "))
  }

  tryCatch({

    # Evaluación de flores de la campaña configurada. El flujo no mezcla campañas.
    eval_flores_raw <- DBI::dbGetQuery(
      conexion,
      glue::glue_sql(
        "
        SELECT
          a.Fundo,
          a.SubParcela,
          a.idLote,
          a.Lote,
          tm.Fecha,
          tm.Muestra,
          tm.Planta,
          tm.Brote,
          tm.Flores,
          tm.Campaña AS Campania
        FROM {`tb$evaluacion_flores`} tm
        INNER JOIN (
          SELECT l.idLote, l.Nombre AS Lote, l.Campaña, sp.SubParcela, p.Parcela, f.Fundo
          FROM {`tb$lote`} l
            INNER JOIN {`tb$subparcela`} sp ON sp.idSubParcela = l.idSubParcela
            INNER JOIN {`tb$parcela`}    p  ON p.id            = sp.idParcela
            INNER JOIN {`tb$fundo`}      f  ON f.id            = p.IdFundo
        ) a ON a.idLote = tm.idLote
        WHERE tm.idEvaluacion = {config$sql$id_evaluacion}
          AND tm.Campaña      = {config$campania}
          AND a.SubParcela IN ({config$subparcelas*})
        ", .con = conexion
      )
    )

    # Maestro de lotes. Se restringe a la campaña vía el área cultivable.
    # Parcela y Variedad se conservan para reincorporarlos al exportable final.
    maestro_lotes <- DBI::dbGetQuery(
      conexion,
      glue::glue_sql(
        "
        SELECT
          f.Fundo,
          v.Variedad,
          p.Parcela,
          sp.SubParcela,
          l.idLote,
          ac.Campaña    AS Campania,
          l.Nombre      AS Lote,
          l.Densidad    AS Total_Plantas,
          ac.Superficie AS Area,
          ROUND(l.Densidad / ac.Superficie, 0) AS Plantas_Ha
        FROM {`tb$lote`} l
          INNER JOIN {`tb$area_cultivable`} ac ON ac.idLote       = l.idLote
          INNER JOIN {`tb$variedad`}        v  ON v.idVariedad    = ac.idVariedad
          INNER JOIN {`tb$subparcela`}      sp ON sp.idSubParcela = l.idSubParcela
          INNER JOIN {`tb$parcela`}         p  ON p.id            = sp.idParcela
          INNER JOIN {`tb$fundo`}           f  ON f.id            = p.IdFundo
          INNER JOIN {`tb$cultivo`}         c  ON c.idCultivo     = v.idCultivo
        WHERE c.idCultivo  = {config$sql$id_cultivo}
          AND ac.Campaña   = {config$campania}
          AND ac.Estado    = 1
          AND sp.SubParcela IN ({config$subparcelas*})
        ", .con = conexion
      )
    )

  }, finally = DBI::dbDisconnect(conexion))

} else {

  # --- 2.1.b Insumos demo sintéticos ---
  # Las rutas se resolvieron en el módulo 01, que también se encarga de
  # generar los archivos si no existen.
  eval_flores_raw <- leer_csv_demo(archivo_eval_demo)
  maestro_lotes   <- leer_csv_demo(archivo_maestro_demo)

  message("[OK] Insumos demo cargados desde: ", ruta_fuente_demo)
}

# --- 2.2 Normalización de la evaluación de flores -------------------
eval_flores <- eval_flores_raw %>%
  rename(Fecha_Evaluacion = Fecha) %>%
  mutate(
    Fecha_Evaluacion = as.Date(Fecha_Evaluacion),
    Semana_Eval      = lubridate::isoweek(Fecha_Evaluacion),
    Anio_Eval        = lubridate::isoyear(Fecha_Evaluacion)
  )

# --- 2.3 Validaciones de la evaluación de flores --------------------
validar_no_vacio(eval_flores, "eval_flores")

validar_columnas(
  eval_flores,
  c("Fundo", "SubParcela", "idLote", "Lote", "Fecha_Evaluacion",
    "Muestra", "Planta", "Brote", "Flores", "Campania", "Semana_Eval", "Anio_Eval"),
  "eval_flores"
)

subparcelas_sin_evaluacion <- setdiff(config$subparcelas, unique(eval_flores$SubParcela))

if (length(subparcelas_sin_evaluacion) > 0) {
  stop("No existen evaluaciones para las SubParcelas configuradas: ",
       paste(subparcelas_sin_evaluacion, collapse = ", "))
}

# Claves necesarias para identificar cada punto evaluado
claves_nulas_flores <- eval_flores %>%
  filter(if_any(c(Fundo, SubParcela, idLote, Lote, Fecha_Evaluacion, Muestra, Planta, Brote), is.na))

if (nrow(claves_nulas_flores) > 0) {
  print(claves_nulas_flores)
  stop("Hay registros de evaluación con claves incompletas.")
}

# Flores puede ser 0 (brote sin flores), pero no negativa ni NA
flores_invalidas <- eval_flores %>%
  filter(is.na(Flores) | !is.finite(Flores) | Flores < 0)

if (nrow(flores_invalidas) > 0) {
  print(flores_invalidas)
  stop("Hay valores de Flores NA, no finitos o negativos.")
}

validar_unicidad(
  eval_flores,
  c("Fundo", "SubParcela", "idLote", "Lote", "Campania",
    "Fecha_Evaluacion", "Muestra", "Planta", "Brote"),
  "eval_flores"
)

# Copia intacta de la extracción: permite re-ejecutar desde el módulo 04
# sin arrastrar las imputaciones de una corrida anterior.
eval_flores_original <- eval_flores

# --- 2.4 Validaciones del maestro de lotes --------------------------
validar_no_vacio(maestro_lotes, "maestro_lotes")

maestro_claves_nulas <- maestro_lotes %>%
  filter(if_any(c(idLote, Lote, SubParcela, Campania), is.na))

if (nrow(maestro_claves_nulas) > 0) {
  print(maestro_claves_nulas)
  stop("El maestro de lotes contiene claves incompletas.")
}

validar_unicidad(maestro_lotes, c("Lote", "Campania"), "maestro_lotes")
validar_unicidad(maestro_lotes, c("idLote", "Campania"), "maestro_lotes")
validar_mayor_a_cero(maestro_lotes, c("Total_Plantas", "Area"), "maestro_lotes")

message("[OK] Flores: ", nrow(eval_flores), " filas | Lotes en maestro: ", nrow(maestro_lotes))
