# ============================================================
# 6. CATASTRO TÉCNICO PARA LA ESTIMACIÓN
# ============================================================
# Une maestro de lotes + pesos + viabilidad al grano (idLote, Anio, Semana).
# Es la tabla contra la que el módulo 09 convierte flores en kilos.

# --- 6.1 Pesos consolidados y control de rango ----------------------
pesos_completo <- bind_rows(pesos_ejecutados, pesos_proyectados)

# Un peso fuera de rango casi siempre significa error de unidad (kg vs g)
# en el insumo de Calidad, no una baya excepcional.
pesos_fuera_rango <- pesos_completo %>%
  filter(Peso < config$reglas$peso_min_g | Peso > config$reglas$peso_max_g)

if (nrow(pesos_fuera_rango) > 0) {
  print(pesos_fuera_rango)
  stop("Hay valores de Peso fuera del rango esperado [",
       config$reglas$peso_min_g, ", ", config$reglas$peso_max_g,
       "] g. Revisar unidad o calidad de datos.")
}

# --- 6.2 Cobertura del horizonte ------------------------------------
# El chequeo de NAs de 6.3 solo ve filas que EXISTEN en el catastro. Una
# semana de cosecha sin fila de peso no está en el catastro, es invisible,
# y solo aparecería como Kilos NA en el módulo 09 sin diagnóstico útil.
# Este bloque falla temprano y dice exactamente qué falta.
semanas_horizonte <- tibble::tibble(
  Fecha = seq(fecha_proyeccion, fecha_max_programada, by = "week")
) %>%
  transmute(
    Anio   = lubridate::isoyear(Fecha),
    Semana = lubridate::isoweek(Fecha)
  ) %>%
  distinct() %>%
  tidyr::crossing(SubParcela = config$subparcelas)

cobertura_pesos <- semanas_horizonte %>%
  anti_join(pesos_completo, by = c("SubParcela", "Anio", "Semana"))

cobertura_viab <- semanas_horizonte %>%
  anti_join(viabilidad, by = c("SubParcela", "Anio", "Semana"))

if (nrow(cobertura_pesos) > 0) {
  print(cobertura_pesos)
  stop("Faltan pesos para ", nrow(cobertura_pesos),
       " combinaciones SubParcela-Año-Semana del horizonte de proyección.")
}

if (nrow(cobertura_viab) > 0) {
  print(cobertura_viab)
  stop("Faltan parámetros de viabilidad para ", nrow(cobertura_viab),
       " combinaciones SubParcela-Año-Semana del horizonte de proyección.")
}

# --- 6.3 Armado del catastro ----------------------------------------
# El cruce maestro x pesos es intencionalmente many-to-many: expande cada
# lote a todas las semanas con peso disponible de su SubParcela.
catastro <- maestro_lotes %>%
  left_join(
    pesos_completo,
    by = c("Campania", "SubParcela"),
    relationship = "many-to-many"
  ) %>%
  left_join(
    viabilidad,
    by = c("SubParcela", "Anio", "Semana"),
    relationship = "many-to-one"
  ) %>%
  select(idLote, Anio, Semana, Total_Plantas, Plantas_Ha, Area, Peso, Prob_Exportable)

validar_unicidad(catastro, c("idLote", "Anio", "Semana"), "catastro")

# Peso, Total_Plantas, Area y Prob_Exportable son críticos para toda fila
# que pueda entrar en la estimación.
na_criticos_catastro <- catastro %>%
  filter(if_any(c(Peso, Total_Plantas, Area, Prob_Exportable), is.na))

if (nrow(na_criticos_catastro) > 0) {
  print(na_criticos_catastro)
  stop("Existen ", nrow(na_criticos_catastro),
       " registros con NA en columnas críticas del catastro.")
}

message("[OK] Catastro: ", nrow(catastro), " filas (lote x semana)")
