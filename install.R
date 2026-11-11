# ============================================================
# INSTALACIÓN DE DEPENDENCIAS
# ============================================================
# Uso:  Rscript install.R
#
# Probado con R 4.3.3. El modo demo no necesita ODBC ni credenciales.

repositorio <- "https://cloud.r-project.org"

# --- Pipeline (obligatorias) ----------------------------------------
obligatorias <- c(
  "dplyr",      # verbos de transformación (único paquete que se carga con library)
  "tidyr",      # pivot_longer, crossing
  "purrr",      # map_dfr
  "stringr",    # str_extract, str_replace_all
  "lubridate",  # isoweek, isoyear, ymd
  "glue",       # mensajes de error y glue_sql
  "readxl",     # lectura de insumos Excel
  "writexl",    # escritura de los exportables
  "tibble",
  "yaml"        # config/config.yml
)

# --- Opcionales ------------------------------------------------------
# odbc/DBI   -> solo para fuente: sqlserver
# testthat   -> solo para tests/run_tests.R
# ggplot2    -> solo para R/graficar_proyeccion.R
opcionales <- c("DBI", "odbc", "testthat", "ggplot2", "svglite")

instalar <- function(paquetes, obligatorio) {
  faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]

  if (length(faltantes) == 0) {
    message("[OK] Ya instalados: ", paste(paquetes, collapse = ", "))
    return(invisible(NULL))
  }

  message("Instalando: ", paste(faltantes, collapse = ", "))
  install.packages(faltantes, repos = repositorio)

  sigue_faltando <- faltantes[!faltantes %in% rownames(installed.packages())]

  if (length(sigue_faltando) > 0) {
    mensaje <- paste("No se pudieron instalar:", paste(sigue_faltando, collapse = ", "))
    if (obligatorio) stop(mensaje) else message("[!] ", mensaje, " (opcionales, se puede continuar)")
  }
}

instalar(obligatorias, obligatorio = TRUE)
instalar(opcionales,   obligatorio = FALSE)

message("\n[OK] Listo. Ejecuta la demo con: Rscript main.R")
