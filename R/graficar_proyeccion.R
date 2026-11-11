# ============================================================
# FIGURA COMPARATIVA: PROYECCIÓN ACTUAL vs ANTERIOR vs EJECUTADO
# ============================================================
# Reproduce el gráfico del reporte semanal de revisión: la curva de la
# semana actual contra la de la semana anterior y contra lo realmente
# cosechado. Es la vista con la que el proyectador decide si calibra.
#
# Uso, desde la raíz del repositorio:
#
#   SEMANA_BASE=34 Rscript main.R     # proyección "anterior"
#   Rscript main.R                    # proyección "actual"
#   Rscript R/graficar_proyeccion.R
#
# Con un solo exportable en output/ dibuja dos series en vez de tres.
# Genera docs/img/curva_proyeccion.svg (el que se muestra en el README).
# ggplot2 es dependencia SOLO de este script, no del pipeline.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

for (pq in c("ggplot2", "svglite", "readxl", "yaml")) {
  if (!requireNamespace(pq, quietly = TRUE)) {
    stop("Este script requiere el paquete '", pq, "'.")
  }
}

source(file.path("R", "00_utils.R"), encoding = "UTF-8")

# --- 1. Configuración mínima ----------------------------------------
ruta_config <- file.path("config", "config.yml")
if (!file.exists(ruta_config)) ruta_config <- file.path("config", "config.example.yml")
config <- yaml::read_yaml(ruta_config)

subparcelas <- as.character(unlist(config$subparcelas))

# SubParcela a graficar: con 18 series el gráfico es ilegible, así que se
# muestra una. Cambiar aquí para revisar otra.
SUBPARCELA_GRAFICO <- subparcelas[1]

# --- 2. Exportables: el más reciente y el anterior -------------------
archivos <- sort(list.files("output", pattern = "^proyeccion_kilos_flores_.*\\.xlsx$",
                            full.names = TRUE))

if (length(archivos) == 0) {
  stop("No hay exportables en output/. Ejecuta primero: Rscript main.R")
}

archivo_actual   <- archivos[length(archivos)]
archivo_anterior <- if (length(archivos) >= 2) archivos[length(archivos) - 1] else NULL

toneladas_por_semana <- function(archivo, etiqueta) {
  readxl::read_excel(archivo) %>%
    filter(SubParcela == SUBPARCELA_GRAFICO) %>%
    group_by(Semana = Semana_Cosecha) %>%
    summarise(Toneladas = sum(Toneladas), .groups = "drop") %>%
    mutate(Serie = etiqueta)
}

proyecciones <- toneladas_por_semana(archivo_actual, "Proy. actual")

if (!is.null(archivo_anterior)) {
  proyecciones <- bind_rows(
    toneladas_por_semana(archivo_anterior, "Proy. anterior"),
    proyecciones
  )
}

# --- 3. Ejecutado real desde los kilos SAP ---------------------------
# El pipeline usa este archivo solo para derivar FECHAS de cosecha; aquí se
# aprovecha la columna de kilos para tener una serie de ejecutado que no
# depende del modelo.
archivo_sap <- file.path(config$rutas$insumos, config$archivos$kilos_sap)

ejecutado <- readxl::read_excel(archivo_sap, sheet = "data") %>%
  filter(
    `Tipo Cosecha` %in% c("Exportable", "Industrial"),
    `Tipo Cultivo` == "Arandano",
    grepl(SUBPARCELA_GRAFICO, `Descripción de Lote`, fixed = TRUE)
  ) %>%
  mutate(Semana = lubridate::isoweek(lubridate::ymd(`F.Produc`))) %>%
  group_by(Semana) %>%
  summarise(Toneladas = sum(Kilos) / 1000, .groups = "drop") %>%
  mutate(Serie = "Ejecutado SAP")

serie <- bind_rows(ejecutado, proyecciones) %>%
  mutate(Serie = factor(Serie, levels = c("Ejecutado SAP", "Proy. actual", "Proy. anterior"))) %>%
  arrange(Serie, Semana)

# --- 4. Corte entre ejecutado y proyectado ---------------------------
ultima_ejecutada <- max(ejecutado$Semana)
corte            <- ultima_ejecutada + 0.5
techo            <- max(serie$Toneladas)

# Etiquetas directas al final de cada serie: la identidad no queda solo en
# el color. La de ejecutado se ancla a la izquierda porque termina sobre la
# línea de corte.
etiquetas <- serie %>%
  group_by(Serie) %>%
  filter(Semana == max(Semana)) %>%
  ungroup() %>%
  # Las dos proyecciones terminan en valores parecidos y sus etiquetas se
  # pisan: se separan verticalmente según cuál queda más arriba.
  group_by(Semana) %>%
  mutate(
    desplazamiento = if (dplyr::n() > 1) {
      if_else(Toneladas >= mean(Toneladas), 1, -1) * techo * 0.055
    } else {
      0
    }
  ) %>%
  ungroup()

# Paleta categórica validada para daltonismo. Verde = ejecutado y azul =
# proyección actual mantienen la convención del reporte Excel del área.
# La proyección anterior va en gris punteado: es contexto de comparación,
# no una serie protagonista.
colores <- c("Ejecutado SAP" = "#1baf7a", "Proy. actual" = "#2a78d6", "Proy. anterior" = "#a8a7a0")
grosor  <- c("Ejecutado SAP" = 1.0,       "Proy. actual" = 1.0,       "Proy. anterior" = 0.55)
tipo    <- c("Ejecutado SAP" = "solid",   "Proy. actual" = "solid",   "Proy. anterior" = "22")

grafico <- ggplot(serie, aes(Semana, Toneladas, color = Serie)) +
  annotate("rect", xmin = corte, xmax = max(serie$Semana) + 0.6,
           ymin = 0, ymax = techo * 1.12, fill = "#0b0b0b", alpha = 0.035) +
  annotate("text", x = corte - 0.6, y = techo * 1.07, label = "ejecutado",
           hjust = 1, size = 3, color = "#8a897f") +
  annotate("text", x = corte + 0.6, y = techo * 1.07, label = "proyectado",
           hjust = 0, size = 3, color = "#8a897f") +
  geom_vline(xintercept = corte, linetype = "dashed", linewidth = 0.4, color = "#a8a7a0") +
  geom_line(aes(linewidth = Serie, linetype = Serie)) +
  geom_point(data = filter(serie, Serie != "Proy. anterior"), size = 1.4) +
  geom_text(data = filter(etiquetas, Serie != "Ejecutado SAP"),
            aes(y = Toneladas + desplazamiento, label = Serie),
            hjust = -0.12, size = 3.1, fontface = "bold", show.legend = FALSE) +
  geom_text(data = filter(etiquetas, Serie == "Ejecutado SAP"), aes(label = Serie),
            hjust = 1.08, vjust = -1.4, size = 3.1, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = colores) +
  scale_linewidth_manual(values = grosor, guide = "none") +
  scale_linetype_manual(values = tipo, guide = "none") +
  scale_x_continuous(breaks = seq(12, max(serie$Semana), by = 3), labels = function(x) paste0("S", x),
                     expand = expansion(mult = c(0.02, 0.15))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.14))) +
  labs(
    title    = paste0("Proyección vs. ejecutado — SubParcela ", SUBPARCELA_GRAFICO),
    subtitle = "Toneladas exportables por semana ISO de cosecha · datos demo sintéticos",
    x = NULL, y = "Toneladas"
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 1, linetype = "solid"))) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "top",
    legend.title       = element_blank(),
    legend.margin      = margin(b = -4),
    panel.background   = element_rect(fill = "#fcfcfb", color = NA),
    plot.background    = element_rect(fill = "#fcfcfb", color = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#e6e5e0", linewidth = 0.3),
    plot.title         = element_text(face = "bold", color = "#0b0b0b", size = 12.5),
    plot.subtitle      = element_text(color = "#52514e", size = 8.8),
    axis.title         = element_text(color = "#52514e", size = 9),
    axis.text          = element_text(color = "#52514e", size = 8.5)
  )

dir.create(file.path("docs", "img"), recursive = TRUE, showWarnings = FALSE)
ruta_svg <- file.path("docs", "img", "curva_proyeccion.svg")

ggsave(ruta_svg, grafico, width = 8.6, height = 4.3, device = svglite::svglite)

message("[OK] SubParcela graficada: ", SUBPARCELA_GRAFICO)
message("[OK] Actual: ",   basename(archivo_actual))
message("[OK] Anterior: ", if (is.null(archivo_anterior)) "(no disponible)" else basename(archivo_anterior))
message("[OK] Figura: ", ruta_svg)
