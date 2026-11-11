# ============================================================
# 3. INSUMOS EXCEL: PESOS Y CALENDARIO DE COSECHA
# ============================================================
# Cuatro fuentes:
#   3.1 pesos ejecutados   (Calidad)  -> semanas cerradas
#   3.2 pesos proyectados  (Proyección) -> semanas abiertas
#   3.4 fechas de cosecha ejecutadas (SAP)
#   3.5 fechas de cosecha programadas (programa de cosecha)
#
# El calendario final (3.7) combina ejecutado y programado al grano
# (Lote, Fecha). Es la base del ajuste temporal del módulo 08.

# --- 3.1 Pesos ejecutados de Calidad --------------------------------
# Conserva solo semanas CERRADAS (anteriores a la semana base) y promedia
# el peso por SubParcela-Año-Semana. Las semanas abiertas se cubren en 3.2.
# Nota: el archivo de Calidad trae la SubParcela bajo el encabezado
# "Parcela"; se renombra al leer porque el archivo es externo y no se
# modifica su cabecera.
pesos_ejecutados <- readxl::read_excel(archivo_pesos_ejecutados) %>%
  rename(SubParcela = Parcela, Anio = `Año Semana`) %>%
  mutate(Campania = as.integer(stringr::str_extract(nombre_campaña, "^\\d{4}"))) %>%
  filter(
    SubParcela %in% config$subparcelas,
    Campania == config$campania,
    (Anio < anio_actual) | (Anio == anio_actual & Semana < semana_actual)
  ) %>%
  group_by(Campania, Anio, Semana, SubParcela) %>%
  # Sin na.rm deliberadamente: un Peso NA debe propagarse y morir en
  # validar_mayor_a_cero, no desaparecer del promedio.
  summarise(Peso = mean(Peso), .groups = "drop")

validar_no_vacio(pesos_ejecutados, "pesos_ejecutados")
validar_semana_iso(pesos_ejecutados, "Anio", "Semana", "pesos_ejecutados")
validar_unicidad(pesos_ejecutados, c("Campania", "SubParcela", "Anio", "Semana"), "pesos_ejecutados")
validar_mayor_a_cero(pesos_ejecutados, "Peso", "pesos_ejecutados")

# --- 3.2 Pesos proyectados ------------------------------------------
# Cada hoja del archivo corresponde a una SubParcela y tiene layout
# posicional. La lectura está en leer_subparcela() (modulo 00), que es el
# único punto a ajustar si el área de Proyecciones cambia la plantilla.
hojas_disponibles    <- readxl::excel_sheets(archivo_pesos_proyectados)
subparcelas_sin_hoja <- setdiff(config$subparcelas, hojas_disponibles)

if (length(subparcelas_sin_hoja) > 0) {
  stop("Las siguientes SubParcelas no tienen hoja en el archivo de pesos proyectados: ",
       paste(subparcelas_sin_hoja, collapse = ", "))
}

pesos_proyectados <- purrr::map_dfr(
  config$subparcelas,
  ~ leer_subparcela(hoja = .x, archivo = archivo_pesos_proyectados)
) %>%
  mutate(Campania = config$campania) %>%
  filter(
    (Anio > anio_actual) | (Anio == anio_actual & Semana >= semana_actual),
    !is.na(Peso)
  ) %>%
  arrange(SubParcela, Anio, Semana)

validar_no_vacio(pesos_proyectados, "pesos_proyectados")
validar_semana_iso(pesos_proyectados, "Anio", "Semana", "pesos_proyectados")
validar_unicidad(pesos_proyectados, c("Campania", "Anio", "Semana", "SubParcela"), "pesos_proyectados")
validar_mayor_a_cero(pesos_proyectados, "Peso", "pesos_proyectados")

# --- 3.3 Ejecutado y proyectado no deben solaparse ------------------
# Una SubParcela-Año-Semana no puede tener peso real y peso proyectado a
# la vez: indicaría que el corte de semana base quedó mal definido.
solapes_pesos <- pesos_ejecutados %>%
  distinct(Campania, Anio, Semana, SubParcela) %>%
  inner_join(
    pesos_proyectados %>% distinct(Campania, Anio, Semana, SubParcela),
    by = c("Campania", "Anio", "Semana", "SubParcela")
  )

if (nrow(solapes_pesos) > 0) {
  print(solapes_pesos)
  stop("Hay solape entre pesos ejecutados y proyectados. Revisar SubParcela-Año-Semana.")
}

message(sprintf(
  "[OK] Pesos | última semana ejecutada: %d-S%02d | primera proyectada: %d-S%02d",
  max(pesos_ejecutados$Anio),
  max(pesos_ejecutados$Semana[pesos_ejecutados$Anio == max(pesos_ejecutados$Anio)]),
  min(pesos_proyectados$Anio),
  min(pesos_proyectados$Semana[pesos_proyectados$Anio == min(pesos_proyectados$Anio)])
))

# --- 3.4 Fechas de cosecha EJECUTADAS por lote (kilos SAP) ----------
# Cada registro SAP aporta una fecha en la que hubo cosecha; no se usa el
# valor de kilos, solo la existencia del registro.
# Las fechas de un lote se ordenan y se agrupan en pasadas: una separación
# >= reglas$dias_nueva_pasada inicia una pasada nueva. De cada pasada se
# toma la fecha máxima como fecha de cosecha asignada.
fechas_cosecha_ejecutadas <- readxl::read_excel(archivo_kilos_ejecutados, sheet = "data") %>%
  filter(
    `Tipo Cosecha` %in% c("Exportable", "Industrial"),
    `Tipo Cultivo` == "Arandano"
  ) %>%
  select(Lote = `Descripción de Lote`, Fecha_Cosecha_Real = `F.Produc`) %>%
  mutate(Fecha_Cosecha_Real = lubridate::ymd(Fecha_Cosecha_Real)) %>%
  filter(
    Lote %in% unique(maestro_lotes$Lote),
    (lubridate::isoyear(Fecha_Cosecha_Real) < anio_actual) |
    (lubridate::isoyear(Fecha_Cosecha_Real) == anio_actual &
     lubridate::isoweek(Fecha_Cosecha_Real) < semana_actual)
  ) %>%
  arrange(Lote, Fecha_Cosecha_Real) %>%
  group_by(Lote) %>%
  mutate(grupo_pasada = agrupar_pasadas_cosecha(
    Fecha_Cosecha_Real, config$reglas$dias_nueva_pasada
  )) %>%
  group_by(Lote, grupo_pasada) %>%
  summarise(Fecha_Cosecha_Asignada = max(Fecha_Cosecha_Real), .groups = "drop") %>%
  select(Lote, Fecha_Cosecha_Asignada)

# No se exige que exista cosecha ejecutada: al inicio de campaña la tabla
# puede estar vacía legítimamente. Lo que no puede pasar es que quede vacía
# y el código asuma que tiene filas (max() sobre vacío devuelve -Inf).
hay_ejecutado <- nrow(fechas_cosecha_ejecutadas) > 0

if (!hay_ejecutado) {
  message("[!] No hay fechas de cosecha ejecutadas antes de la semana base. ",
          "Se proyectará solo contra el programa de cosecha.")
}

# --- 3.5 Fechas de cosecha PROGRAMADAS por lote ---------------------
# El generador del programa ya exporta codigo_subparcela; la SubParcela
# solo se usa para filtrar y el resultado queda en (Lote, Fecha).
fechas_cosecha_programadas <- readxl::read_excel(archivo_programa_cosecha, sheet = "programacion") %>%
  rename(
    SubParcela = codigo_subparcela,
    Lote = lote,
    Fecha_Cosecha_Programada = fecha
  ) %>%
  mutate(Fecha_Cosecha_Programada = as.Date(Fecha_Cosecha_Programada)) %>%
  filter(SubParcela %in% config$subparcelas) %>%
  select(Lote, Fecha_Cosecha_Programada)

validar_no_vacio(fechas_cosecha_programadas, "fechas_cosecha_programadas")

programacion_invalida <- fechas_cosecha_programadas %>%
  filter(is.na(Lote) | is.na(Fecha_Cosecha_Programada))

if (nrow(programacion_invalida) > 0) {
  print(programacion_invalida)
  stop("El programa de cosecha contiene Lote o Fecha_Cosecha_Programada NA.")
}

validar_unicidad(
  fechas_cosecha_programadas, c("Lote", "Fecha_Cosecha_Programada"),
  "fechas_cosecha_programadas"
)

lotes_programados_no_maestro <- fechas_cosecha_programadas %>%
  distinct(Lote) %>%
  anti_join(maestro_lotes %>% distinct(Lote), by = "Lote")

if (nrow(lotes_programados_no_maestro) > 0) {
  print(lotes_programados_no_maestro)
  stop("El programa de cosecha contiene lotes que no existen en maestro_lotes.")
}

# --- 3.6 Ejecutado y programado no deben solaparse ------------------
# Por lote, la última fecha ejecutada debe ser anterior a la primera programada.
if (hay_ejecutado) {
  message(
    "[OK] Cosecha | última ejecutada: ", max(fechas_cosecha_ejecutadas$Fecha_Cosecha_Asignada),
    " | primera programada: ", min(fechas_cosecha_programadas$Fecha_Cosecha_Programada)
  )

  solapes_periodo <- inner_join(
    fechas_cosecha_ejecutadas %>%
      group_by(Lote) %>%
      summarise(Ultima_Ejecutada = max(Fecha_Cosecha_Asignada), .groups = "drop"),
    fechas_cosecha_programadas %>%
      group_by(Lote) %>%
      summarise(Primera_Programada = min(Fecha_Cosecha_Programada), .groups = "drop"),
    by = "Lote"
  ) %>%
    filter(Ultima_Ejecutada >= Primera_Programada)

  if (nrow(solapes_periodo) > 0) {
    print(solapes_periodo)
    stop("Hay lotes cuya programación comienza antes de terminar la cosecha ejecutada.")
  }
}

# --- 3.7 Calendario completo: ejecutado + programado ----------------
# Grano (Lote, Fecha). El join del módulo 08 va solo por Lote: por jerarquía
# de campo cada Lote pertenece a una única SubParcela.
calendario_cosecha <- bind_rows(
  fechas_cosecha_ejecutadas,
  fechas_cosecha_programadas %>% rename(Fecha_Cosecha_Asignada = Fecha_Cosecha_Programada)
)

validar_unicidad(calendario_cosecha, c("Lote", "Fecha_Cosecha_Asignada"), "calendario_cosecha")

message("[OK] Calendario de cosecha: ", nrow(calendario_cosecha), " fechas en ",
        n_distinct(calendario_cosecha$Lote), " lotes")
