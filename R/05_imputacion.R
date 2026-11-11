# ============================================================
# 5. IMPUTACIÓN DE SEMANAS FALTANTES DE EVALUACIÓN
# ============================================================
# Completa semanas sin conteo de flores que están ENTRE semanas reales de
# la misma SubParcela y que la hoja Parametros declara como autorizadas.
# No extrapola fuera de ese rango ni más allá de la semana base.

# --- 5.1 Semanas reales y detección de faltantes --------------------
eval_flores_control <- eval_flores_original %>%
  mutate(Inicio_Semana = fecha_semana_iso(Anio_Eval, Semana_Eval))

semanas_reales <- eval_flores_control %>%
  distinct(SubParcela, Anio_Eval, Semana_Eval, Inicio_Semana)

semanas_faltantes <- params_maduracion %>%
  transmute(
    SubParcela    = SUBPARCELA,
    Anio_Eval     = ANIO,
    Semana_Eval   = SEMANA,
    Inicio_Semana = fecha_semana_iso(ANIO, SEMANA)
  ) %>%
  inner_join(
    semanas_reales %>%
      group_by(SubParcela) %>%
      summarise(
        Primera_Semana_Real = min(Inicio_Semana),
        Ultima_Semana_Real  = max(Inicio_Semana),
        .groups = "drop"
      ),
    by = "SubParcela"
  ) %>%
  filter(
    between(Inicio_Semana, Primera_Semana_Real, Ultima_Semana_Real),
    Inicio_Semana < fecha_base_iso
  ) %>%
  anti_join(
    semanas_reales,
    by = c("SubParcela", "Anio_Eval", "Semana_Eval", "Inicio_Semana")
  ) %>%
  arrange(SubParcela, Inicio_Semana)

if (nrow(semanas_faltantes) == 0) {
  message("[OK] No hay semanas faltantes de evaluación que requieran imputación.")
} else {
  message("[!] Se imputarán ", nrow(semanas_faltantes), " semanas faltantes:")
  print(semanas_faltantes %>% select(SubParcela, Anio_Eval, Semana_Eval))
}

# --- 5.2 Imputación ------------------------------------------------
# La interpolación punto por punto está en imputar_una_semana() (modulo 00).
# Se aplica una vez por semana faltante detectada.
# --- 5.3 Ejecución -------------------------------------------------
if (nrow(semanas_faltantes) > 0) {

  detalle_imputacion <- purrr::map_dfr(
    seq_len(nrow(semanas_faltantes)),
    ~ imputar_una_semana(
      objetivo       = semanas_faltantes[.x, ],
      datos          = eval_flores_control,
      semanas_reales = semanas_reales
    )
  )

  imputaciones_invalidas <- detalle_imputacion %>%
    filter(is.na(Flores) | !is.finite(Flores) | Flores < 0)

  if (nrow(imputaciones_invalidas) > 0) {
    print(imputaciones_invalidas)
    stop("La imputación produjo valores de Flores inválidos.")
  }

  eval_flores <- bind_rows(
    eval_flores_original,
    detalle_imputacion %>% select(all_of(names(eval_flores_original)))
  ) %>%
    arrange(Fundo, SubParcela, Lote, Muestra, Planta, Brote, Fecha_Evaluacion)

} else {
  eval_flores        <- eval_flores_original
  detalle_imputacion <- tibble::tibble()
}

# --- 5.4 Trazabilidad de lo imputado --------------------------------
if (nrow(detalle_imputacion) > 0) {
  control_imputacion <- detalle_imputacion %>%
    count(SubParcela, Anio_Eval, Semana_Eval, Metodo_Imputacion, name = "Puntos_Imputados") %>%
    arrange(SubParcela, Anio_Eval, Semana_Eval, Metodo_Imputacion)

  print(control_imputacion)

  message("[OK] Imputación terminada: ", nrow(detalle_imputacion),
          " puntos generados en ", nrow(semanas_faltantes), " semanas.")
}
