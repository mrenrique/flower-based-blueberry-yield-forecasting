# ============================================================
# 8. DISTRIBUCIÓN DE FLORES Y AJUSTE AL CALENDARIO
# ============================================================
# Convierte el conteo de flores en una serie de flores por fecha de cosecha:
#   8.1-8.2  flores promedio por planta de cada lote-semana
#   8.4      se reparten según la curva -> fechas TEÓRICAS de cosecha
#   8.5      se reasignan a la primera fecha REAL/PROGRAMADA disponible
#   8.7      se acumulan por fecha de cosecha asignada

# --- 8.0 Validación del grano de evaluación -------------------------
# flores_evaluadas (8.2) agrupa por Fecha_Evaluacion, pero plantas_evaluadas
# (8.1) agrupa solo hasta Semana_Eval. Si un lote tuviera dos fechas de
# evaluación en la misma semana ISO, FloresProm dividiría las flores de
# ambas fechas entre las plantas distintas de la semana: correcto si son
# plantas distintas, inflado si son las mismas plantas evaluadas dos veces.
# El caso no está definido, así que se detiene.
fechas_multiples_por_semana <- eval_flores_autorizadas %>%
  distinct(idLote, Anio_Eval, Semana_Eval, Fecha_Evaluacion) %>%
  count(idLote, Anio_Eval, Semana_Eval, name = "n_fechas") %>%
  filter(n_fechas > 1)

if (nrow(fechas_multiples_por_semana) > 0) {
  print(fechas_multiples_por_semana)
  stop("Hay lotes con más de una fecha de evaluación en la misma semana ISO. ",
       "Definir el criterio (¿plantas distintas o re-evaluación?) antes de continuar.")
}

# --- 8.1 Plantas evaluadas por lote-semana --------------------------
plantas_evaluadas <- eval_flores_autorizadas %>%
  distinct(SubParcela, Lote, idLote, Campania, Anio_Eval, Semana_Eval, Muestra, Planta) %>%
  count(SubParcela, Lote, idLote, Campania, Anio_Eval, Semana_Eval, name = "Plantas")

# --- 8.2 Flores evaluadas por lote-semana ---------------------------
flores_evaluadas <- eval_flores_autorizadas %>%
  group_by(Fundo, SubParcela, Lote, idLote, Campania, Anio_Eval, Semana_Eval, Fecha_Evaluacion) %>%
  summarise(Flores = sum(Flores), .groups = "drop") %>%
  mutate(Nombre_Curva = paste0("CURVA_", SubParcela, "_", Anio_Eval, "_", Semana_Eval))

# --- 8.3 Toda evaluación debe tener curva ---------------------------
# Tras el filtro 7.1 esto debería quedar vacío por construcción; se mantiene
# como salvaguarda ante desalineación entre la hoja Parametros y las curvas.
curvas_faltantes <- flores_evaluadas %>%
  anti_join(curvas_maduracion, by = "Nombre_Curva") %>%
  distinct(SubParcela, Anio_Eval, Semana_Eval, Nombre_Curva)

if (nrow(curvas_faltantes) > 0) {
  print(curvas_faltantes)
  stop("Hay evaluaciones de flores sin curva de distribución.")
}

# --- 8.4 Aplicación de la curva -> fecha teórica de cosecha ---------
# Fecha teórica = fecha de evaluación + DÍA DE ARRANQUE + día de curva - 1
flores_con_curva <- flores_evaluadas %>%
  inner_join(curvas_maduracion, by = "Nombre_Curva", relationship = "many-to-many") %>%
  mutate(
    Fecha_Cosecha_Teorica = Fecha_Evaluacion + Arranque + Dias - 1,
    Semana_Cosecha        = lubridate::isoweek(Fecha_Cosecha_Teorica),
    Anio_Cosecha          = lubridate::isoyear(Fecha_Cosecha_Teorica)
  )

# --- 8.5 Flores promedio distribuidas por fecha teórica -------------
flores_dist_por_semana <- flores_con_curva %>%
  inner_join(
    plantas_evaluadas,
    by = c("SubParcela", "Lote", "idLote", "Campania", "Anio_Eval", "Semana_Eval"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    FloresProm = Flores / Plantas,
    FloresDist = FloresProm * DISTRIBUCION
  ) %>%
  group_by(Fundo, SubParcela, Lote, idLote, Campania, Semana_Eval,
           Anio_Cosecha, Semana_Cosecha, Fecha_Cosecha_Teorica) %>%
  summarise(FloresPromDist = sum(FloresDist), .groups = "drop")

# --- 8.6 Ajuste contra el calendario de cosecha ---------------------
# Cada aporte se asigna a la PRIMERA fecha de cosecha del lote igual o
# posterior a su fecha teórica: las flores que maduran entre dos cosechas
# se acumulan en la siguiente pasada efectiva.
# El calendario está a grano (Lote, Fecha) y el join va solo por Lote: por
# jerarquía de campo cada Lote pertenece a una única SubParcela.
flores_ajustadas_programa <- flores_dist_por_semana %>%
  # 1. Colapsar las distintas semanas de evaluación en cada fecha teórica
  group_by(Fundo, SubParcela, Lote, idLote, Campania,
           Anio_Cosecha, Semana_Cosecha, Fecha_Cosecha_Teorica) %>%
  summarise(FloresPromDist = sum(FloresPromDist), .groups = "drop") %>%
  # 2. Cruzar con el calendario
  left_join(calendario_cosecha, by = "Lote", relationship = "many-to-many") %>%
  # 3. Conservar solo fechas de cosecha iguales o posteriores a la teórica
  filter(Fecha_Cosecha_Teorica <= Fecha_Cosecha_Asignada) %>%
  # 4. Elegir la primera fecha válida. Se agrupa por el grano completo de la
  #    tabla y no solo por el lote, para no depender de que Fundo, Campania
  #    y Anio/Semana_Cosecha estén determinados por las otras claves.
  group_by(Fundo, SubParcela, Lote, idLote, Campania,
           Anio_Cosecha, Semana_Cosecha, Fecha_Cosecha_Teorica) %>%
  slice_min(Fecha_Cosecha_Asignada, with_ties = FALSE) %>%
  ungroup()

validar_no_vacio(flores_ajustadas_programa, "flores_ajustadas_programa")

# --- 8.7 Control de flores que quedan fuera del horizonte -----------
# Las flores cuya fecha teórica cae después de la última fecha del programa
# no se asignan en esta corrida. Es INTENCIONAL: la proyección se corta
# según el programa disponible, y esas flores se recuperan en la corrida
# siguiente cuando el programa incorpore nuevas fechas.
#
# Por eso el umbral no es una tolerancia fina: en operación normal este
# porcentaje ronda 15-25 %, porque las evaluaciones más recientes maduran
# más allá de las 9 semanas del programa. El control existe para detectar
# una pérdida CATASTRÓFICA (programa vacío, arranque mal configurado,
# calendario desalineado), no para vigilar la pérdida estructural.
control_asignacion <- tibble::tibble(
  Flores_Distribuidas = sum(flores_dist_por_semana$FloresPromDist),
  Flores_Asignadas    = sum(flores_ajustadas_programa$FloresPromDist)
) %>%
  mutate(
    Flores_No_Asignadas = Flores_Distribuidas - Flores_Asignadas,
    Pct_No_Asignado     = if_else(
      Flores_Distribuidas > 0, Flores_No_Asignadas / Flores_Distribuidas, 0
    )
  )

print(control_asignacion)

if (control_asignacion$Pct_No_Asignado > config$reglas$maximo_pct_flores_no_asignadas) {
  stop(sprintf(
    paste0("El %.1f%% de las flores distribuidas quedó fuera del calendario de cosecha ",
           "(máximo admitido: %.1f%%). Revisar el programa de cosecha, el DÍA DE ARRANQUE ",
           "y el horizonte antes de usar esta proyección."),
    100 * control_asignacion$Pct_No_Asignado,
    100 * config$reglas$maximo_pct_flores_no_asignadas
  ))
}

if (control_asignacion$Pct_No_Asignado > config$reglas$aviso_pct_flores_no_asignadas) {
  message(sprintf(
    "[!] %.1f%% de las flores distribuidas quedó fuera del horizonte del programa de cosecha.",
    100 * control_asignacion$Pct_No_Asignado
  ))
}

if (anyNA(flores_ajustadas_programa$FloresPromDist)) {
  stop("Existen NA en FloresPromDist antes de acumular por fecha de cosecha.")
}

# --- 8.8 Acumulación por fecha de cosecha asignada ------------------
# Año y semana de cosecha se derivan de Fecha_Cosecha_Asignada porque son
# las claves con las que el módulo 09 busca peso y viabilidad en el catastro.
flores_acumuladas <- flores_ajustadas_programa %>%
  group_by(Fundo, SubParcela, Lote, idLote, Campania, Fecha_Cosecha_Asignada) %>%
  summarise(Frec_Acumulada = sum(FloresPromDist), .groups = "drop") %>%
  mutate(
    Anio_Cosecha   = lubridate::isoyear(Fecha_Cosecha_Asignada),
    Semana_Cosecha = lubridate::isoweek(Fecha_Cosecha_Asignada)
  ) %>%
  select(Fundo, SubParcela, Lote, idLote, Campania,
         Anio_Cosecha, Semana_Cosecha, Fecha_Cosecha_Asignada, Frec_Acumulada)

# La acumulación no puede crear ni perder flores: solo reagrupa.
control_flores <- tibble::tibble(
  antes   = sum(flores_ajustadas_programa$FloresPromDist),
  despues = sum(flores_acumuladas$Frec_Acumulada)
) %>%
  mutate(diferencia = antes - despues)

print(control_flores)

if (!dplyr::near(control_flores$antes, control_flores$despues)) {
  stop("Se perdieron flores durante la acumulación por fecha de cosecha.")
}

message("[OK] Flores acumuladas en ", nrow(flores_acumuladas), " combinaciones lote-fecha")
