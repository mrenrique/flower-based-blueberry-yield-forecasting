# ============================================================
# 9. ESTIMACIÓN DE KILOS Y EXPORTACIÓN
# ============================================================
#   Kilos     = Frec_Acumulada x Total_Plantas x (Peso / 1000) x Prob_Exportable
#   Toneladas = Kilos / 1000

# --- 9.1 Cálculo de kilos y toneladas -------------------------------
# Parcela y Variedad se reincorporan desde el maestro por idLote solo para
# el exportable (idLote -> Lote-Campaña -> una sola Parcela).
proyeccion_kilos <- flores_acumuladas %>%
  left_join(
    catastro,
    by = c("idLote", "Anio_Cosecha" = "Anio", "Semana_Cosecha" = "Semana"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    maestro_lotes %>% distinct(idLote, Parcela, Variedad),
    by = "idLote",
    relationship = "many-to-one"
  ) %>%
  mutate(
    Kilos     = Frec_Acumulada * Total_Plantas * (Peso / 1000) * Prob_Exportable,
    Toneladas = Kilos / 1000,
    # Tramo separa las fechas anteriores al inicio formal de la proyección
    # de las futuras.
    # OJO: la semana base queda marcada como HISTORICO aunque use peso
    # proyectado y pueda usar fecha de cosecha programada. No interpretar
    # HISTORICO como ejecutado puro.
    Tramo = if_else(Fecha_Cosecha_Asignada < fecha_proyeccion, "HISTORICO", "PROYECTADO")
  ) %>%
  relocate(Parcela,  .after = Fundo) %>%
  relocate(Variedad, .after = Parcela) %>%
  relocate(idLote, Campania, .after = Lote)

# --- 9.2 Controles de calidad ---------------------------------------
# ¿El join final infló filas?
if (nrow(proyeccion_kilos) != nrow(flores_acumuladas)) {
  stop("El join final cambió el número de filas. ",
       "Antes: ", nrow(flores_acumuladas), " | Después: ", nrow(proyeccion_kilos))
}

na_proyeccion_kilos <- proyeccion_kilos %>% filter(is.na(Kilos))

if (nrow(na_proyeccion_kilos) > 0) {
  print(na_proyeccion_kilos %>%
          select(SubParcela, Lote, Variedad, Anio_Cosecha, Semana_Cosecha))
  stop("La proyección contiene ", nrow(na_proyeccion_kilos), " filas con Kilos NA.")
}

# --- 9.3 Resumen de la corrida --------------------------------------
resumen_proyeccion <- proyeccion_kilos %>%
  group_by(Tramo, Anio_Cosecha, Semana_Cosecha) %>%
  summarise(Toneladas = sum(Toneladas), .groups = "drop") %>%
  arrange(Anio_Cosecha, Semana_Cosecha)

print(resumen_proyeccion, n = Inf)

message(sprintf(
  "[OK] Proyección %s | %.1f t proyectadas en el horizonte | %.1f t en tramo histórico",
  periodo_proyeccion,
  sum(proyeccion_kilos$Toneladas[proyeccion_kilos$Tramo == "PROYECTADO"]),
  sum(proyeccion_kilos$Toneladas[proyeccion_kilos$Tramo == "HISTORICO"])
))

# --- 9.4 Exportación ------------------------------------------------
# Las columnas internas vuelven a su nombre original con tilde para no
# romper las consultas de los reportes Excel que consumen estos archivos.
exportar_con_nombres_originales <- function(df) {
  rename(df, `Campaña` = Campania)
}

for (archivo in c(archivo_salida_flores_distribuidas, archivo_salida_proyeccion_kilos)) {
  if (file.exists(archivo)) {
    message("[!] Se sobrescribe el archivo existente: ", archivo)
  }
}

# Output A: detalle de flores por fecha TEÓRICA, antes de forzar el
# calendario y antes de convertir a kilos. Alimenta el simulador de curvas.
writexl::write_xlsx(
  exportar_con_nombres_originales(flores_dist_por_semana),
  archivo_salida_flores_distribuidas
)

# Output B: resultado final en kilos, ya ajustado al calendario.
writexl::write_xlsx(
  exportar_con_nombres_originales(proyeccion_kilos),
  archivo_salida_proyeccion_kilos
)

message("[OK] Output A: ", archivo_salida_flores_distribuidas)
message("[OK] Output B: ", archivo_salida_proyeccion_kilos)
