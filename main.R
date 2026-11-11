# ============================================================
# PROYECCIÓN DE KILOS EXPORTABLES DE ARÁNDANO A PARTIR DEL CONTEO DE FLORES
# ============================================================
# Orquestador del pipeline. Ejecutar desde la raíz del repositorio:
#
#   Rscript main.R
#
# o, dentro de RStudio, con el proyecto abierto en la raíz:
#
#   source("main.R")
#
# Toda la configuración vive en config/config.yml.
#
# CONVENCIÓN DE PAQUETES: solo dplyr se carga con library(); todo lo demás
# se llama con paquete::funcion para que quede explícito de dónde sale cada
# cosa al leer el código.
#
# CONVENCIÓN DE ESTADO: los módulos se ejecutan en orden y comparten el
# entorno global, igual que un notebook. Cada uno consume los objetos del
# anterior. Es deliberado: permite inspeccionar cualquier tabla intermedia
# en la consola y re-ejecutar solo el tramo afectado durante la calibración.

# ============================================================
# 1. ENTORNO
# ============================================================
RUTA_REPO <- getwd()

if (!dir.exists(file.path(RUTA_REPO, "R")) || !dir.exists(file.path(RUTA_REPO, "config"))) {
  stop("main.R debe ejecutarse desde la raíz del repositorio.\n",
       "Directorio actual: ", RUTA_REPO)
}

suppressPackageStartupMessages(library(dplyr))

for (pq in c("tidyr", "purrr", "stringr", "lubridate", "glue", "readxl", "writexl", "tibble", "yaml")) {
  if (!requireNamespace(pq, quietly = TRUE)) {
    stop("Falta el paquete '", pq, "'. Instala las dependencias con: Rscript install.R")
  }
}

# ============================================================
# 2. MÓDULOS DEL PIPELINE
# ============================================================
MODULOS <- c(
  "00_utils.R",        # validaciones y helpers (sin estado)
  "01_config.R",       # config, periodo, rutas, horizonte efectivo
  "02_fuentes.R",      # evaluación de flores y maestro de lotes
  "03_insumos.R",      # pesos y calendario de cosecha
  "04_parametros.R",   # maduración y viabilidad  <-- punto de re-ejecución
  "05_imputacion.R",   # semanas faltantes de evaluación
  "06_catastro.R",     # maestro + pesos + viabilidad
  "07_curvas.R",       # lista autorizada y curvas de maduración
  "08_distribucion.R", # distribución de flores y ajuste al calendario
  "09_kilos.R"         # kilos, toneladas y exportación
)

# Ejecuta el pipeline desde el módulo indicado hasta el final.
# Durante la calibración se modifica el archivo de parámetros y basta con
# ejecutar_desde(4): los módulos 1-3 no dependen de los parámetros, así que
# no se vuelve a consultar SQL ni a releer los pesos.
ejecutar_desde <- function(modulo_inicial = 0) {
  # Se selecciona por el NÚMERO del módulo (el prefijo del archivo), no por
  # su posición en el vector: ejecutar_desde(4) arranca en 04_parametros.R.
  numeros <- as.integer(substr(MODULOS, 1, 2))

  if (!modulo_inicial %in% numeros) {
    stop("Módulo inicial inválido: ", modulo_inicial,
         ". Valores posibles: ", paste(numeros, collapse = ", "))
  }

  # El módulo 00 siempre se recarga: define las funciones.
  indices <- which(numeros == 0 | numeros >= modulo_inicial)

  for (i in indices) {
    cat("\n")
    message("============================================================")
    message(">>> ", MODULOS[i])
    message("============================================================")
    # local = FALSE: cada módulo deja sus objetos en el entorno global,
    # que es lo que permite inspeccionarlos después en la consola.
    source(file.path(RUTA_REPO, "R", MODULOS[i]), encoding = "UTF-8", local = FALSE)

    # Guardia de carga. Si R corre en un locale que no es UTF-8, source()
    # corta la lectura en el primer carácter acentuado de los comentarios
    # y NO lanza error: el módulo queda a medias y el pipeline "termina"
    # sin haber hecho nada. Se verifica contra una función conocida.
    if (numeros[i] == 0 && !exists("fecha_semana_iso", mode = "function")) {
      stop("El módulo de utilidades no se cargó completo.\n",
           "Causa habitual: R no está en un locale UTF-8 y los archivos sí lo están.\n",
           "Locale actual (LC_CTYPE): ", Sys.getlocale("LC_CTYPE"), "\n",
           "En Linux/macOS: exportar LANG=es_PE.UTF-8 (o C.UTF-8) antes de ejecutar.")
    }
  }

  invisible(TRUE)
}

# ============================================================
# 3. EJECUCIÓN
# ============================================================
inicio <- Sys.time()

ejecutar_desde()

message("\n[OK] Pipeline completo en ",
        round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 1), " segundos.")
