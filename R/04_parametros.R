# ============================================================
# 4. PARÁMETROS DE MADURACIÓN Y VIABILIDAD
# ============================================================
# ====> PUNTO DE RE-EJECUCIÓN <====
# Al calibrar se modifica el archivo de parámetros y se vuelve a correr
# DESDE ESTE MÓDULO. Los módulos 1-3 no dependen de los parámetros, así
# que no hace falta volver a consultar SQL ni releer los pesos:
#
#   source("R/04_parametros.R"); source("R/05_imputacion.R"); ...
#
# o directamente:  ejecutar_desde(4)   (definido en main.R)

# --- 4.1 Hoja Parametros --------------------------------------------
# Es, además, la LISTA AUTORIZADA de combinaciones SubParcela-Año-Semana:
# toda evaluación fuera de ella se excluye antes de proyectar (módulo 07).
params_maduracion <- readxl::read_excel(archivo_params_mad_viab, sheet = "Parametros")

validar_columnas(
  params_maduracion,
  c("SUBPARCELA", "ANIO", "SEMANA", "DIA DE ARRANQUE", "PERIODO", "MEDIA", "DESVIACION"),
  "params_maduracion"
)

params_maduracion <- params_maduracion %>%
  filter(SUBPARCELA %in% config$subparcelas)

validar_no_vacio(params_maduracion, "params_maduracion")
validar_semana_iso(params_maduracion, "ANIO", "SEMANA", "hoja Parametros")
validar_unicidad(params_maduracion, c("SUBPARCELA", "ANIO", "SEMANA"), "params_maduracion")
validar_mayor_a_cero(
  params_maduracion,
  c("DIA DE ARRANQUE", "PERIODO", "MEDIA", "DESVIACION"),
  "params_maduracion"
)

# PERIODO define la cantidad de días de la curva: debe ser entero.
if (any(params_maduracion$PERIODO != floor(params_maduracion$PERIODO))) {
  stop("PERIODO debe contener únicamente valores enteros.")
}

# --- 4.2 Hoja Viabilidad --------------------------------------------
# Viabilidad integral flor nueva -> fruto exportable.
# OJO: el parámetro está definido por SubParcela-Año-Semana DE COSECHA,
# no por semana de evaluación de flores.
#   Prob_Exportable = 1 - No_Viabilidad
viabilidad <- readxl::read_excel(archivo_params_mad_viab, sheet = "Viabilidad")

validar_columnas(viabilidad, c("SubParcela", "Año", "Semana", "No_Viabilidad"), "viabilidad")

viabilidad <- viabilidad %>%
  rename(Anio = `Año`) %>%
  filter(SubParcela %in% config$subparcelas)

validar_no_vacio(viabilidad, "viabilidad")
validar_semana_iso(viabilidad, "Anio", "Semana", "hoja Viabilidad")

if (!is.numeric(viabilidad$No_Viabilidad)) {
  stop("No_Viabilidad debe ser numérica en la hoja Viabilidad.")
}

viabilidad <- viabilidad %>%
  mutate(Prob_Exportable = 1 - No_Viabilidad) %>%
  select(SubParcela, Anio, Semana, Prob_Exportable)

validar_unicidad(viabilidad, c("SubParcela", "Anio", "Semana"), "viabilidad")

prob_invalida <- viabilidad %>%
  filter(is.na(Prob_Exportable) | !is.finite(Prob_Exportable) |
         Prob_Exportable < 0 | Prob_Exportable > 1)

if (nrow(prob_invalida) > 0) {
  print(prob_invalida)
  stop("Prob_Exportable contiene NA, valores no finitos o fuera del rango [0, 1].")
}

message("[OK] Parámetros: ", nrow(params_maduracion), " combinaciones autorizadas | ",
        "viabilidad: ", nrow(viabilidad), " filas")
