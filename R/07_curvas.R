# ============================================================
# 7. LISTA AUTORIZADA Y CURVAS DE MADURACIÓN
# ============================================================

# --- 7.1 Filtrado por la lista autorizada (hoja Parametros) ---------
# Solo se proyectan las combinaciones SubParcela-Año-Semana declaradas en
# la hoja Parametros. Lo que queda fuera se reporta pero no se proyecta.
evaluaciones_excluidas <- eval_flores %>%
  anti_join(
    params_maduracion,
    by = c("SubParcela" = "SUBPARCELA", "Anio_Eval" = "ANIO", "Semana_Eval" = "SEMANA")
  ) %>%
  count(Campania, Anio_Eval, Semana_Eval, SubParcela, name = "Filas")

if (nrow(evaluaciones_excluidas) > 0) {
  message("[OK] Filtro hoja Parametros: se excluyen ", sum(evaluaciones_excluidas$Filas),
          " filas en ", nrow(evaluaciones_excluidas), " combinaciones no autorizadas:")
  print(evaluaciones_excluidas)
}

eval_flores_autorizadas <- eval_flores %>%
  semi_join(
    params_maduracion,
    by = c("SubParcela" = "SUBPARCELA", "Anio_Eval" = "ANIO", "Semana_Eval" = "SEMANA")
  )

validar_no_vacio(eval_flores_autorizadas, "eval_flores_autorizadas")

subparcelas_sin_autorizacion <- setdiff(
  config$subparcelas, unique(eval_flores_autorizadas$SubParcela)
)

if (length(subparcelas_sin_autorizacion) > 0) {
  stop("La hoja Parametros dejó sin evaluaciones autorizadas a: ",
       paste(subparcelas_sin_autorizacion, collapse = ", "))
}

# Todo lote con evaluaciones autorizadas necesita al menos una fecha de
# cosecha, o sus flores no tendrían dónde caer.
lotes_sin_calendario <- eval_flores_autorizadas %>%
  distinct(Lote) %>%
  anti_join(calendario_cosecha %>% distinct(Lote), by = "Lote")

if (nrow(lotes_sin_calendario) > 0) {
  stop("Los siguientes lotes con evaluaciones autorizadas no tienen fechas de cosecha: ",
       paste(lotes_sin_calendario$Lote, collapse = ", "))
}

# --- 7.2 Generación de curvas por SubParcela-Año-Semana -------------
curvas_maduracion <- params_maduracion %>%
  rename(
    SubParcela = SUBPARCELA,
    Anio       = ANIO,
    Semana     = SEMANA,
    Arranque   = `DIA DE ARRANQUE`,
    Periodo    = PERIODO,
    Media      = MEDIA,
    Desv       = DESVIACION
  ) %>%
  group_by(SubParcela, Anio, Semana, Arranque, Periodo, Media, Desv) %>%
  group_modify(~ curva_maduracion_normalizada(
    periodo    = .y$Periodo,
    media      = .y$Media,
    desviacion = .y$Desv,
    etiqueta   = paste0(.y$SubParcela, " ", .y$Anio, "-S", .y$Semana)
  )) %>%
  ungroup() %>%
  transmute(
    Dias,
    DISTRIBUCION,
    Nombre_Curva = paste0("CURVA_", SubParcela, "_", Anio, "_", Semana),
    Arranque
  )

# --- 7.3 Validación de las curvas generadas -------------------------
# Cada curva debe repartir exactamente el 100 % de las flores de su semana.
control_curvas <- curvas_maduracion %>%
  group_by(Nombre_Curva) %>%
  summarise(
    suma   = sum(DISTRIBUCION),
    minimo = min(DISTRIBUCION),
    maximo = max(DISTRIBUCION),
    .groups = "drop"
  )

curvas_invalidas <- control_curvas %>%
  filter(
    abs(suma - 1) > config$reglas$tolerancia_curva | minimo < 0 | maximo > 1
  )

if (nrow(curvas_invalidas) > 0) {
  print(curvas_invalidas)
  stop("Existen curvas inválidas: la distribución no suma 1 o tiene valores fuera de [0, 1].")
}

message("[OK] Curvas de maduración generadas: ", nrow(control_curvas))
