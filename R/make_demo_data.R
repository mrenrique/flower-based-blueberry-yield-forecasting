# ============================================================
# GENERADOR DE INSUMOS DEMO SINTÉTICOS
# ============================================================
# Produce los seis insumos del pipeline con la MISMA estructura que los
# archivos reales, pero con datos inventados. Permite ejecutar el proyecto
# completo sin SQL Server, sin credenciales y sin ningún dato productivo.
#
# Se ejecuta automáticamente desde el módulo 01 cuando falta algún insumo.
# Para regenerarlos, borrar la carpeta data/demo y volver a correr.
#
# ANCLAJE DE CAMPAÑA: todas las ventanas se fijan a semanas ISO de la
# campaña, NO a la semana base. Eso hace que dos corridas con semanas base
# distintas (34 y 35, por ejemplo) proyecten la MISMA campaña con distinto
# corte de información, que es lo que permite comparar "proyección anterior"
# contra "proyección actual" como en el reporte real.
#
# IMPORTANTE: los valores son estructuralmente válidos (pasan todas las
# validaciones y respetan los rangos de configuración) y la forma estacional
# imita una campaña real de arándano (dos picos de cosecha). Las MAGNITUDES
# no están calibradas a rendimientos reales: sirven para demostrar el flujo,
# no para sacar conclusiones agronómicas.

set.seed(42)

dir.create(ruta_insumos,     recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_fuente_demo, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 1. CALENDARIO FIJO DE LA CAMPAÑA DEMO
# ============================================================
SEM_INICIO_EVAL    <- 2    # primera semana con evaluación de flores
SEM_HUECO_EVAL     <- 29   # semana omitida en una SubParcela (dispara imputación)
SEM_INICIO_COSECHA <- 12   # primera semana con cosecha
SEM_FIN_PROGRAMA   <- 44   # última semana del programa de cosecha
REZAGO_SAP         <- 2    # semanas de atraso de la carga de kilos SAP

# Ritmo de cosecha: dos pasadas por semana en días fijos. Los lotes alternan
# entre dos pares de días para que no coincidan todos el mismo día.
# Un ritmo regular evita que el reparto semanal dependa de cuántas fechas
# cayeron en cada semana, que es lo que ensuciaba la curva.
DIAS_COSECHA <- list(c(1, 4), c(2, 5))   # lunes/jueves y martes/viernes

# Las evaluaciones llegan hasta la semana base: no se puede haber evaluado
# el futuro. La hoja Parametros (lista autorizada) hace el mismo corte.
semanas_eval <- SEM_INICIO_EVAL:semana_actual
lunes_eval   <- fecha_semana_iso(anio_actual, semanas_eval)
fechas_eval  <- lunes_eval + 2                      # se evalúa los miércoles

# Última semana con cosecha ejecutada cargada en SAP
sem_fin_ejecutado <- semana_actual - REZAGO_SAP

# Pesos y viabilidad: deben cubrir TODA semana que pueda recibir cosecha.
# Si falta viabilidad para una semana con peso, el catastro queda con
# Prob_Exportable NA y el pipeline se detiene en el módulo 06.
semanas_con_peso <- (SEM_INICIO_COSECHA - 2):(SEM_FIN_PROGRAMA + 4)
lunes_con_peso   <- fecha_semana_iso(anio_actual, semanas_con_peso)

# ============================================================
# 2. INTENSIDAD ESTACIONAL DE FLORACIÓN
# ============================================================
# Dos picos, como una campaña real de arándano. La cosecha aparece unas
# 10.5 semanas después de la evaluación (DÍA DE ARRANQUE 63 + media de la
# curva 11 días), así que los picos de floración en las semanas 10.5 y 28.5
# se traducen en picos de cosecha alrededor de las semanas 21 y 39.
intensidad_floracion <- function(semana) {
  3 +
  18 * exp(-0.5 * ((semana - 10.5) / 3.2)^2) +   # pico temprano, el mayor
  12 * exp(-0.5 * ((semana - 28.5) / 3.8)^2)     # pico tardío
}

# ============================================================
# 3. MAESTRO DE LOTES
# ============================================================
# Dos lotes por cada SubParcela configurada.
lotes_demo <- purrr::map_dfr(seq_along(config$subparcelas), function(i) {
  sp <- config$subparcelas[i]
  tibble::tibble(
    SubParcela = sp,
    Lote       = sprintf("L-%s-%02d", sp, 1:2),
    idLote     = as.integer(1000 + i * 10 + 1:2)
  )
})

maestro_demo <- lotes_demo %>%
  mutate(
    Fundo         = "FUNDO_DEMO",
    Parcela       = paste0("PARCELA_", SubParcela),
    Variedad      = rep(c("VENTURA", "BILOXI"), length.out = dplyr::n()),
    Campania      = config$campania,
    Area          = round(runif(dplyr::n(), 3.0, 4.0), 1),
    Total_Plantas = as.integer(round(runif(dplyr::n(), 10800, 14000)))
  ) %>%
  mutate(Plantas_Ha = round(Total_Plantas / Area, 0)) %>%
  select(Fundo, Variedad, Parcela, SubParcela, idLote, Campania,
         Lote, Total_Plantas, Area, Plantas_Ha)

write.csv(maestro_demo, archivo_maestro_demo, row.names = FALSE, fileEncoding = "UTF-8")

# ============================================================
# 4. EVALUACIÓN DE FLORES
# ============================================================
# Grano: lote x fecha x muestra x planta x brote.
grilla_muestreo <- tidyr::crossing(Muestra = 1:2, Planta = 1:3, Brote = 1:5)

# El hueco de evaluación se abre en una sola SubParcela, para que el módulo
# 05 tenga algo real que imputar.
subparcela_con_hueco <- config$subparcelas[min(9, length(config$subparcelas))]

eval_demo <- purrr::map_dfr(seq_along(semanas_eval), function(i) {
  lotes_demo %>%
    tidyr::crossing(grilla_muestreo) %>%
    mutate(
      Fecha  = fechas_eval[i],
      Flores = rpois(dplyr::n(), lambda = intensidad_floracion(semanas_eval[i]))
    )
}) %>%
  filter(!(SubParcela == subparcela_con_hueco &
           Fecha == fechas_eval[semanas_eval == SEM_HUECO_EVAL])) %>%
  left_join(maestro_demo %>% select(idLote, Fundo, Campania), by = "idLote") %>%
  select(Fundo, SubParcela, idLote, Lote, Fecha, Muestra, Planta, Brote, Flores, Campania) %>%
  arrange(SubParcela, Lote, Fecha, Muestra, Planta, Brote)

write.csv(eval_demo, archivo_eval_demo, row.names = FALSE, fileEncoding = "UTF-8")

# ============================================================
# 5. PARÁMETROS DE MADURACIÓN Y VIABILIDAD
# ============================================================
# Hoja Parametros: incluye TODAS las semanas de evaluación, también la
# omitida, porque es la lista autorizada y la que dispara la imputación.
parametros_demo <- tidyr::crossing(
  SUBPARCELA = config$subparcelas,
  SEMANA     = semanas_eval
) %>%
  mutate(
    ANIO              = anio_actual,
    # Desfase flor -> inicio de cosecha, ~9 semanas. Varía ligeramente por
    # SubParcela, igual que en la calibración real.
    #
    # ajuste_calibracion simula lo que hace el proyectador cada semana: mueve
    # el DÍA DE ARRANQUE de las evaluaciones RECIENTES al comparar la curva
    # contra lo ejecutado. Las semanas antiguas ya no se tocan. Gracias a esto
    # dos corridas con semanas base distintas no salen idénticas en el tramo
    # proyectado, que es justamente lo que el reporte comparativo muestra.
    ajuste_calibracion = if_else(SEMANA >= semana_actual - 12,
                                 (semana_proyeccion %% 3) - 1, 0),
    `DIA DE ARRANQUE` = 63 + (match(SUBPARCELA, config$subparcelas) %% 3) +
                        ajuste_calibracion,
    PERIODO           = 21,
    MEDIA             = 11,
    DESVIACION        = 4
  ) %>%
  select(SUBPARCELA, ANIO, SEMANA, `DIA DE ARRANQUE`, PERIODO, MEDIA, DESVIACION) %>%
  arrange(SUBPARCELA, ANIO, SEMANA)

# Hoja Viabilidad: por SubParcela-Año-Semana DE COSECHA.
viabilidad_demo <- tidyr::crossing(
  SubParcela = config$subparcelas,
  idx        = seq_along(lunes_con_peso)
) %>%
  mutate(
    `Año`         = lubridate::isoyear(lunes_con_peso[idx]),
    Semana        = lubridate::isoweek(lunes_con_peso[idx]),
    No_Viabilidad = round(runif(dplyr::n(), 0.18, 0.30), 3)
  ) %>%
  select(SubParcela, `Año`, Semana, No_Viabilidad) %>%
  arrange(SubParcela, `Año`, Semana)

writexl::write_xlsx(
  list(Parametros = parametros_demo, Viabilidad = viabilidad_demo),
  archivo_params_mad_viab
)

# ============================================================
# 6. PESOS EJECUTADOS (FORMATO DEL ARCHIVO DE CALIDAD)
# ============================================================
# El archivo real trae la SubParcela bajo el encabezado "Parcela" y el año
# ISO bajo "Año Semana". Se reproduce tal cual, con varias filas por
# SubParcela-semana (el pipeline las promedia).
idx_peso_cerrado <- which(semanas_con_peso < semana_actual)

pesos_ejecutados_demo <- tidyr::crossing(
  Parcela = config$subparcelas,
  idx     = idx_peso_cerrado,
  replica = 1:3
) %>%
  mutate(
    `Año Semana`   = lubridate::isoyear(lunes_con_peso[idx]),
    Semana         = lubridate::isoweek(lunes_con_peso[idx]),
    Peso           = round(1.75 + 0.012 * idx + rnorm(dplyr::n(), 0, 0.08), 2),
    nombre_campaña = paste0(config$campania, "-", config$campania + 1)
  ) %>%
  select(Parcela, `Año Semana`, Semana, Peso, nombre_campaña)

writexl::write_xlsx(pesos_ejecutados_demo, archivo_pesos_ejecutados)

# ============================================================
# 7. PESOS PROYECTADOS (LAYOUT POSICIONAL, UNA HOJA POR SUBPARCELA)
# ============================================================
# Layout que espera leer_subparcela():
#   fila 1 = años (desde la columna 3) | fila 2 = semanas | fila 6 = pesos
# Todo se escribe como texto porque una misma columna mezcla años, semanas
# y pesos; el lector convierte los tipos.
idx_peso_abierto <- which(semanas_con_peso >= semana_actual)

armar_hoja_pesos <- function(subparcela) {
  anios   <- lubridate::isoyear(lunes_con_peso[idx_peso_abierto])
  semanas <- lubridate::isoweek(lunes_con_peso[idx_peso_abierto])
  pesos   <- round(1.75 + 0.012 * idx_peso_abierto +
                     rnorm(length(idx_peso_abierto), 0, 0.05), 2)

  n_col <- length(anios) + 2
  hoja  <- as.data.frame(matrix("", nrow = 6, ncol = n_col), stringsAsFactors = FALSE)
  names(hoja) <- paste0("V", seq_len(n_col))

  hoja[1, 3:n_col] <- as.character(anios)
  hoja[2, 3:n_col] <- as.character(semanas)
  hoja[3, 1] <- "SUBPARCELA"; hoja[3, 2] <- subparcela
  hoja[4, 1] <- "SERIE";      hoja[4, 2] <- "Peso proyectado (g)"
  hoja[6, 1] <- "1";          hoja[6, 2] <- "PESO_PROYECTADO"
  hoja[6, 3:n_col] <- as.character(pesos)

  hoja
}

hojas_pesos <- setNames(lapply(config$subparcelas, armar_hoja_pesos), config$subparcelas)

writexl::write_xlsx(hojas_pesos, archivo_pesos_proyectados, col_names = FALSE)

# ============================================================
# 8. CALENDARIO DE COSECHA POR LOTE
# ============================================================
# Fechas de cosecha de toda la campaña, cada DIAS_ENTRE_PASADAS días y sin
# domingos. Se parten en dos:
#   - ejecutadas  -> hasta sem_fin_ejecutado, van al archivo de kilos SAP
#   - programadas -> desde ahí hasta SEM_FIN_PROGRAMA, al programa de cosecha
fecha_inicio_cosecha <- fecha_semana_iso(anio_actual, SEM_INICIO_COSECHA)
fecha_fin_programa   <- fecha_semana_iso(anio_actual, SEM_FIN_PROGRAMA) + 4
fecha_corte_sap      <- fecha_semana_iso(anio_actual, sem_fin_ejecutado) + 6

todos_los_dias <- seq(fecha_inicio_cosecha, fecha_fin_programa, by = 1)

calendario_demo <- purrr::map_dfr(seq_len(nrow(lotes_demo)), function(i) {
  dias_del_lote <- DIAS_COSECHA[[(i - 1) %% length(DIAS_COSECHA) + 1]]
  fechas <- todos_los_dias[as.integer(format(todos_los_dias, "%u")) %in% dias_del_lote]

  tibble::tibble(
    SubParcela = lotes_demo$SubParcela[i],
    Lote       = lotes_demo$Lote[i],
    Fecha      = fechas
  )
}) %>%
  mutate(Ejecutada = Fecha <= fecha_corte_sap)

# --- 8.1 Kilos ejecutados SAP ---------------------------------------
# El pipeline solo usa la EXISTENCIA del registro para derivar fechas de
# cosecha; la columna Kilos se genera para que el gráfico comparativo tenga
# una serie de "ejecutado" independiente del modelo.
#
# ESCALA_SAP está calibrada para que los kilos ejecutados queden en el mismo
# orden de magnitud que la salida del modelo. Si se cambian las plantas por
# lote, el peso o la intensidad de floración, hay que recalibrarla.
ESCALA_SAP <- 92

# Cada tercera fecha se duplica al día siguiente: genera pasadas de dos días
# y ejercita el agrupamiento de agrupar_pasadas_cosecha().
ejecutadas <- calendario_demo %>%
  filter(Ejecutada) %>%
  group_by(Lote) %>%
  mutate(pasada_de_dos_dias = row_number() %% 3 == 0) %>%
  ungroup()

kilos_sap_demo <- bind_rows(
  ejecutadas,
  ejecutadas %>% filter(pasada_de_dos_dias) %>% mutate(Fecha = Fecha + 1)
) %>%
  filter(Fecha <= fecha_corte_sap, format(Fecha, "%u") != "7") %>%
  distinct(Lote, Fecha) %>%
  mutate(semana_cosecha = lubridate::isoweek(Fecha)) %>%
  # El volumen se define por SEMANA y recién después se reparte entre las
  # fechas de esa semana. Si se asignara por fecha, una semana con dos
  # pasadas duplicaría los kilos de una con una sola, y la serie de
  # ejecutado quedaría con un serrucho que no existe en la realidad.
  group_by(Lote, semana_cosecha) %>%
  mutate(
    # La cosecha sigue la misma forma estacional que la floración, desplazada
    # las ~10.5 semanas que tarda la maduración.
    kilos_semana = ESCALA_SAP * intensidad_floracion(semana_cosecha - 10.5) *
                   runif(1, 0.90, 1.10),
    Kilos        = round(kilos_semana / dplyr::n(), 1)
  ) %>%
  ungroup() %>%
  mutate(
    `Tipo Cosecha`        = if_else(runif(dplyr::n()) < 0.85, "Exportable", "Industrial"),
    `Tipo Cultivo`        = "Arandano",
    `Descripción de Lote` = Lote,
    `F.Produc`            = format(Fecha, "%Y-%m-%d")
  ) %>%
  select(`Tipo Cosecha`, `Tipo Cultivo`, `Descripción de Lote`, `F.Produc`, Kilos)

writexl::write_xlsx(list(data = kilos_sap_demo), archivo_kilos_ejecutados)

# --- 8.2 Programa de cosecha ----------------------------------------
programa_demo <- calendario_demo %>%
  filter(!Ejecutada) %>%
  transmute(
    codigo_subparcela = SubParcela,
    lote              = Lote,
    fecha             = format(Fecha, "%Y-%m-%d")
  )

writexl::write_xlsx(list(programacion = programa_demo), archivo_programa_cosecha)

# ============================================================
# 9. RESUMEN
# ============================================================
message("[OK] Insumos demo generados (campaña anclada a semanas ISO fijas):")
message("     SubParcelas: ", length(config$subparcelas),
        " | lotes: ", nrow(lotes_demo),
        " | evaluaciones: S", min(semanas_eval), "-S", max(semanas_eval))
message("     Cosecha ejecutada hasta S", sem_fin_ejecutado,
        " | programa hasta S", SEM_FIN_PROGRAMA)
message("     - ", basename(archivo_eval_demo),         " (", nrow(eval_demo), " puntos)")
message("     - ", basename(archivo_params_mad_viab),   " (Parametros + Viabilidad)")
message("     - ", basename(archivo_pesos_ejecutados),  " (", nrow(pesos_ejecutados_demo), " filas)")
message("     - ", basename(archivo_pesos_proyectados), " (", length(hojas_pesos), " hojas)")
message("     - ", basename(archivo_kilos_ejecutados),  " (", nrow(kilos_sap_demo), " registros SAP)")
message("     - ", basename(archivo_programa_cosecha),  " (", nrow(programa_demo), " fechas)")
