# ==============================================================================
# Equipo 3 - Inteligencia de Negocios - Tercer Avance
# Shiny app que integra el Panel_Nuevo.csv y el modelo econométrico final
# ==============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(DT)
library(sandwich)
library(lmtest)

# ==============================================================================
# 1. CARGA Y PREPARACIÓN DEL PANEL NUEVO
# ==============================================================================

panel_path <- "Panel_Nuevo.csv"
if (!file.exists(panel_path)) {
  panel_path <- file.path("NuevoPanel", "Panel_Nuevo.csv")
}

panel_raw <- read.csv(panel_path, stringsAsFactors = FALSE)

z_score <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sqrt(mean((x - m)^2, na.rm = TRUE))
  if (!is.finite(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - m) / s
}

panel <- panel_raw %>%
  mutate(
    country = factor(country),
    year = as.integer(year),
    log_arrivals     = log(tourist_arrivals_thousands),
    log_receipts     = log(tourism_receipts_billion_usd),
    log_cult_goods   = log(cultural_goods_exports_million_usd),
    log_gdp_pc       = log(gdp_per_capita_usd),
    log_exchange     = log(exchange_rate_lcu_usd),
    cult_goods_z     = z_score(cultural_goods_exports_million_usd),
    unesco_world_z   = z_score(unesco_world_heritage_sites),
    unesco_intang_z  = z_score(unesco_intangible_heritage_items),
    culture_index    = rowMeans(cbind(cult_goods_z, unesco_world_z, unesco_intang_z),
                                na.rm = TRUE)
  ) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    culture_index_lag1 = dplyr::lag(culture_index, 1),
    inflation_pct      = (cpi_2010_100 / dplyr::lag(cpi_2010_100, 1) - 1) * 100
  ) %>%
  ungroup()

# ==============================================================================
# 2. ESTIMACIÓN DEL MODELO ECONOMÉTRICO (LSDV con HC1)
# Replica los resultados del documento de modelo final.
# ==============================================================================

m_main <- lm(log_arrivals ~ culture_index + log_gdp_pc + log_exchange +
               factor(country) + factor(year), data = panel)
m_lag  <- lm(log_arrivals ~ culture_index_lag1 + log_gdp_pc + log_exchange +
               factor(country) + factor(year), data = panel)
m_inf  <- lm(log_arrivals ~ culture_index + log_gdp_pc + log_exchange +
               inflation_pct + factor(country) + factor(year), data = panel)

hc1_table <- function(model) {
  ct <- coeftest(model, vcov. = vcovHC(model, type = "HC1"))
  data.frame(
    variable     = rownames(ct),
    coeficiente  = as.numeric(ct[, 1]),
    se_hc1       = as.numeric(ct[, 2]),
    t            = as.numeric(ct[, 3]),
    p            = as.numeric(ct[, 4]),
    stringsAsFactors = FALSE
  )
}

coef_main <- hc1_table(m_main)
coef_lag  <- hc1_table(m_lag)
coef_inf  <- hc1_table(m_inf)

# Tabla compacta de variables del modelo (sin las dummies de FE)
core_vars_main <- c("culture_index", "log_gdp_pc", "log_exchange")
core_vars_lag  <- c("culture_index_lag1", "log_gdp_pc", "log_exchange")
core_vars_inf  <- c("culture_index", "log_gdp_pc", "log_exchange", "inflation_pct")

# ==============================================================================
# 3. PALETA Y FUNCIONES DE PLOT
# ==============================================================================
col_jp     <- "#bc002d"
col_kr     <- "#003478"
col_accent <- "#e8a020"

theme_eq3 <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      legend.text        = element_text(size = 9),
      axis.text          = element_text(size = 9)
    )
}
sc <- function() scale_color_manual(values = c("Japan" = col_jp, "Korea" = col_kr),
                                    labels = c("Japan" = "Japón", "Korea" = "Corea del Sur"))
sf <- function() scale_fill_manual(values  = c("Japan" = col_jp, "Korea" = col_kr),
                                   labels = c("Japan" = "Japón", "Korea" = "Corea del Sur"))

make_line <- function(d, y_var, y_lab, show_covid = TRUE) {
  p <- ggplot(d, aes(x = year, y = .data[[y_var]], color = country)) +
    sc() + theme_eq3() +
    labs(x = NULL, y = y_lab) +
    scale_x_continuous(breaks = seq(2005, 2024, 3))
  if (show_covid) {
    p <- p + annotate("rect", xmin = 2019.5, xmax = 2021.5,
                      ymin = -Inf, ymax = Inf, fill = "#ffdddd", alpha = .35)
  }
  p + geom_line(linewidth = 1) + geom_point(size = 1.8)
}

# ==============================================================================
# 4. UI - mismo estilo visual que appSegundoAvance.R
# ==============================================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Noto+Serif:ital,wght@0,400;0,700;1,400&family=Noto+Sans:wght@400;500;600;700&display=swap');

      * { box-sizing: border-box; }
      body {
        font-family: 'Noto Sans', sans-serif;
        background-color: #f2f3f5;
        color: #1a1a2e;
        margin: 0;
      }

      /* HEADER */
      .app-header { background:#1a1a2e; color:white; padding:20px 28px 16px; border-bottom:3px solid #e8a020; }
      .app-header h1 { font-family:'Noto Serif',serif; font-size:19px; font-weight:700; margin:0 0 4px; letter-spacing:-.3px; }
      .app-header p  { font-size:11.5px; color:#aab; margin:0; }
      .app-header .fuentes { font-size:10.5px; color:#778; margin-top:3px; }
      .equipo-tag {
        display:inline-block; background:rgba(232,160,32,.18);
        border:1px solid rgba(232,160,32,.4); border-radius:3px;
        color:#e8a020; font-size:10px; font-weight:600;
        padding:2px 8px; margin-top:8px; letter-spacing:.5px; text-transform:uppercase;
      }

      /* KPI BAR */
      .kpi-bar { background:white; border-bottom:1px solid #e4e6eb; padding:10px 28px; display:flex; gap:0; }
      .kpi-item { flex:1; text-align:center; padding:6px 10px; border-right:1px solid #eee; }
      .kpi-item:last-child { border-right:none; }
      .kpi-val { font-size:17px; font-weight:700; line-height:1.1; }
      .kpi-lbl { font-size:10px; color:#888; margin-top:2px; }
      .jp  { color:#bc002d; } .kr { color:#003478; } .neu { color:#333; } .acc { color:#e8a020; }

      /* LAYOUT */
      .body-wrap { display:flex; min-height:calc(100vh - 130px); }
      .sidebar { width:210px; min-width:210px; background:#1a1a2e; padding:18px 14px; color:white; }
      .sidebar-label { font-size:9.5px; font-weight:700; text-transform:uppercase; letter-spacing:1px; color:#667; margin:14px 0 6px; }
      .sidebar-label:first-child { margin-top:0; }
      .sidebar .shiny-input-container { margin-bottom:10px; }
      .sidebar .shiny-input-container label { color:#aab; font-size:11.5px; font-weight:500; }
      .sidebar .form-control, .sidebar .selectize-input {
        background:#252545; border:1px solid #334; color:white; font-size:12px; border-radius:5px;
      }
      .sidebar .irs--shiny .irs-bar { background:#e8a020; }
      .sidebar .irs--shiny .irs-handle { background:#e8a020; border-color:#e8a020; }
      .sidebar .irs--shiny .irs-from, .sidebar .irs--shiny .irs-to,
      .sidebar .irs--shiny .irs-single { background:#e8a020; }
      .sidebar .irs--shiny .irs-line { background:#334; }
      .sidebar .irs--shiny .irs-min, .sidebar .irs--shiny .irs-max { color:#667; }
      .sidebar input[type=checkbox] { accent-color:#e8a020; }

      .main-area { flex:1; padding:18px 22px; overflow-x:hidden; }

      /* TABS */
      .nav-tabs { border-bottom:2px solid #dde; margin-bottom:18px; }
      .nav-tabs > li > a { font-size:12px; font-weight:600; color:#556; padding:8px 14px; border-radius:6px 6px 0 0; border:none; background:transparent; }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover {
        color:#1a1a2e; background:white; border:2px solid #dde; border-bottom-color:white;
      }
      .nav-tabs > li > a:hover { background:#eef; color:#1a1a2e; }

      /* CARDS */
      .card { background:white; border-radius:8px; padding:16px 18px; box-shadow:0 1px 4px rgba(0,0,0,.07); margin-bottom:14px; }
      .card-title { font-size:12.5px; font-weight:700; color:#1a1a2e; margin-bottom:10px; }
      .card-sub { font-size:11px; color:#778; font-weight:400; display:block; margin-top:1px; }

      /* CALLOUTS */
      .lit-callout {
        background:#fefaf3; border:1px solid #f0d898; border-left:4px solid #e8a020;
        border-radius:0 6px 6px 0; padding:12px 16px; margin-bottom:14px;
        font-size:12px; color:#4a3c1a; line-height:1.65;
      }
      .lit-callout .lit-badge {
        display:inline-block; background:#e8a020; color:white;
        font-size:9.5px; font-weight:700; padding:2px 7px; border-radius:3px;
        text-transform:uppercase; letter-spacing:.5px; margin-right:6px; vertical-align:middle;
      }
      .lit-callout .cita { font-style:italic; color:#7a6030; }
      .lit-callout strong { color:#3a2c0a; }

      .note-box {
        background:#f0f4ff; border-left:4px solid #003478;
        border-radius:0 6px 6px 0; padding:10px 14px; font-size:11.5px;
        color:#1a2a50; margin-bottom:14px; line-height:1.6;
      }
      .note-box .nb-title { font-weight:700; font-size:10.5px; text-transform:uppercase; letter-spacing:.5px; color:#003478; margin-bottom:4px; }

      /* CAJA INTERPRETACIÓN SIMPLE */
      .simple-box {
        background:#f5fbf5; border-left:4px solid #2e8b57;
        border-radius:0 6px 6px 0; padding:11px 14px;
        font-size:11.8px; color:#1f4a30; margin-bottom:14px; line-height:1.6;
      }
      .simple-box .sb-title { font-weight:700; font-size:10.5px; text-transform:uppercase; letter-spacing:.5px; color:#2e8b57; margin-bottom:4px; }
      .simple-box strong { color:#11331f; }

      /* CAJA ALERTA / CAUTELA */
      .caution-box {
        background:#fff5f5; border-left:4px solid #bc002d;
        border-radius:0 6px 6px 0; padding:10px 14px; font-size:11.5px;
        color:#5a1020; margin-bottom:14px; line-height:1.6;
      }
      .caution-box .cb-title { font-weight:700; font-size:10.5px; text-transform:uppercase; letter-spacing:.5px; color:#bc002d; margin-bottom:4px; }

      /* MECANISMO DIAGRAM */
      .mec-flow { display:flex; align-items:center; gap:0; flex-wrap:wrap; margin:10px 0; }
      .mec-box {
        background:#f8f9fb; border:1.5px solid #dde; border-radius:6px;
        padding:8px 12px; font-size:11.5px; text-align:center; min-width:110px; line-height:1.4;
      }
      .mec-box.highlight { background:#1a1a2e; color:white; border-color:#1a1a2e; }
      .mec-box.accent { background:#fef6e4; border-color:#e8a020; color:#5a3c00; }
      .mec-arrow { font-size:18px; color:#aab; padding:0 6px; }

      /* STUDY CARDS */
      .study-row { display:flex; gap:12px; flex-wrap:wrap; margin-bottom:10px; }
      .study-card {
        flex:1; min-width:200px; background:#f8f9fb;
        border-radius:6px; padding:12px 14px; border-top:3px solid #ddd;
        font-size:11.5px; line-height:1.55;
      }
      .study-card .sc-autor { font-weight:700; font-size:12px; color:#1a1a2e; margin-bottom:3px; }
      .study-card .sc-tipo { font-size:10px; font-weight:600; text-transform:uppercase; letter-spacing:.5px; color:#888; margin-bottom:6px; }
      .study-card .sc-hallazgo { color:#444; }
      .study-card .sc-dato { margin-top:7px; background:white; border-radius:4px; padding:5px 8px; font-size:11px; color:#333; border-left:3px solid; }

      /* FUENTES GRID */
      .fuente-grid { display:grid; grid-template-columns:repeat(3,1fr); gap:10px; margin-top:6px; }
      .fuente-item { background:#f8f9fb; border-radius:6px; padding:10px 12px; font-size:11.5px; border:1px solid #e4e6eb; }
      .fuente-item .fi-sigla { font-weight:700; font-size:13px; color:#1a1a2e; margin-bottom:2px; }
      .fuente-item .fi-desc { color:#666; font-size:10.5px; }
      .fuente-item .fi-var { margin-top:5px; font-size:10.5px; font-weight:600; color:#e8a020; }

      .plot-caption { font-size:10.5px; color:#888; margin-top:4px; font-style:italic; text-align:right; }
      .section-sep  { border:none; border-top:1.5px solid #eef; margin:18px 0; }

      /* COEF TABLE */
      .coef-table { width:100%; border-collapse:collapse; font-size:11.5px; margin-top:8px; }
      .coef-table th { background:#1a1a2e; color:white; text-align:left; padding:8px 10px; font-weight:600; font-size:11px; }
      .coef-table td { padding:7px 10px; border-bottom:1px solid #eef; }
      .coef-table tr:hover td { background:#fafbff; }
      .coef-table .num { text-align:right; font-variant-numeric:tabular-nums; }
      .coef-table .sig-pos { color:#2e8b57; font-weight:700; }
      .coef-table .sig-neg { color:#bc002d; font-weight:700; }
      .coef-table .sig-na  { color:#888; }

      /* ECUACIÓN */
      .ecuacion {
        background:#1a1a2e; color:#fff;
        border-radius:8px; padding:14px 18px;
        font-family:'Noto Serif',serif; font-size:13.5px;
        margin:8px 0 14px; letter-spacing:.2px;
      }
      .ecuacion .var-c { color:#e8a020; font-weight:700; }
      .ecuacion .var-fe { color:#7aa6ff; }
      .ecuacion .lbl { font-size:10px; color:#aab; text-transform:uppercase; letter-spacing:1px; margin-bottom:6px; display:block; }
    "))
  ),

  # HEADER
  div(class = "app-header",
      tags$h1("Cultura y Turismo Internacional - Panel Japón-Corea (2005-2024)"),
      tags$p("Tercer Avance | Panel_Nuevo.csv | Modelo de efectos fijos doble vía con índice cultural amplio"),
      div(class = "fuentes", "Datos: UNWTO - UNESCO - World Bank - BIS - KOCCA"),
      div(class = "equipo-tag", "Equipo 3 - Inteligencia de Negocios - 2026")
  ),

  # KPI BAR
  div(class = "kpi-bar",
      div(class = "kpi-item", div(class = "kpi-val acc", textOutput("kpi_coef_main")),
          div(class = "kpi-lbl", "Coef. cultura (modelo principal)")),
      div(class = "kpi-item", div(class = "kpi-val acc", textOutput("kpi_coef_lag")),
          div(class = "kpi-lbl", "Coef. cultura rezago t-1")),
      div(class = "kpi-item", div(class = "kpi-val neu", textOutput("kpi_n")),
          div(class = "kpi-lbl", "Observaciones panel")),
      div(class = "kpi-item", div(class = "kpi-val jp", textOutput("kpi_jp_arr")),
          div(class = "kpi-lbl", "Llegadas JP 2024 (miles)")),
      div(class = "kpi-item", div(class = "kpi-val kr", textOutput("kpi_kr_arr")),
          div(class = "kpi-lbl", "Llegadas KR 2024 (miles)")),
      div(class = "kpi-item", div(class = "kpi-val neu", textOutput("kpi_r2")),
          div(class = "kpi-lbl", "R2 modelo principal"))
  ),

  # BODY
  div(class = "body-wrap",
      div(class = "sidebar",
          div(class = "sidebar-label", "Periodo"),
          sliderInput("yr", NULL, min = 2005, max = 2024, value = c(2005, 2024), sep = "", step = 1),
          div(class = "sidebar-label", "País"),
          selectInput("country_sel", NULL,
                      choices = c("Ambos" = "both", "Japón" = "Japan", "Corea del Sur" = "Korea"),
                      selected = "both"),
          div(class = "sidebar-label", "Opciones"),
          checkboxInput("excl_covid", "Excluir COVID (2020-2021)", value = FALSE),
          checkboxInput("use_log",    "Escala logarítmica",         value = FALSE)
      ),

      div(class = "main-area",
          tabsetPanel(type = "tabs", id = "main_tabs",

            # ====================================================================
            # TAB 1: RESUMEN EJECUTIVO
            # ====================================================================
            tabPanel("Resumen ejecutivo",
              div(class = "lit-callout",
                  span(class = "lit-badge", "Hallazgo principal"),
                  tags$strong("Más presencia cultural se asocia con más turismo internacional."),
                  tags$br(),
                  "Con un panel de Japón y Corea del Sur entre 2005 y 2024 (40 observaciones país-año), ",
                  "el modelo de efectos fijos doble vía muestra que el índice cultural amplio tiene una ",
                  "asociación ", tags$strong("positiva y estadísticamente significativa"), " con las llegadas ",
                  "turísticas internacionales. La relación se mantiene cuando se usa el índice cultural ",
                  "rezagado un año, lo que sugiere que la presencia cultural antecede al turismo."
              ),

              fluidRow(
                column(7, div(class = "card",
                  div(class = "card-title", "Coeficientes principales con errores robustos HC1",
                      tags$span(class = "card-sub", "Variable dependiente: log(llegadas turísticas)")),
                  uiOutput("tbl_coef_resumen"),
                  div(class = "plot-caption", "*** p<0.01, ** p<0.05, * p<0.10. Errores HC1 (LSDV).")
                )),
                column(5,
                  div(class = "simple-box",
                      div(class = "sb-title", "Cómo leerlo en una frase"),
                      "Cuando el índice cultural sube ", tags$strong("una unidad estandarizada"),
                      " (es decir, una desviación estándar), las llegadas turísticas están asociadas a un ",
                      "incremento aproximado de ", tags$strong(textOutput("plain_main_pct", inline = TRUE)),
                      " sobre el nivel anterior, manteniendo constantes el PIB per cápita, el tipo de cambio, ",
                      "el país y el año."
                  ),
                  div(class = "caution-box",
                      div(class = "cb-title", "Lo que NO se puede afirmar"),
                      "Este coeficiente es alto y no debe leerse como efecto causal literal. Con solo dos ",
                      "países y posibilidad de causalidad inversa (más turismo también aumenta la visibilidad ",
                      "cultural), la conclusión correcta es ", tags$strong("asociación robusta"), ", no causalidad."
                  )
                )
              ),

              div(class = "card",
                  div(class = "card-title", "Cómo cambia el coeficiente cultural según la especificación",
                      tags$span(class = "card-sub", "Forest plot - barras de error: +/- 1.96 x SE HC1")),
                  plotOutput("forest_resumen", height = "230px"),
                  div(class = "plot-caption",
                      "Lectura: el coeficiente del índice cultural se mantiene positivo y significativo en las ",
                      "tres especificaciones del modelo final.")
              ),

              fluidRow(
                column(6, div(class = "card",
                  div(class = "card-title", "Llegadas turísticas observadas y predicción del modelo principal"),
                  plotOutput("plot_obs_pred", height = "260px"),
                  div(class = "plot-caption", "Las líneas son la predicción del modelo de efectos fijos doble vía.")
                )),
                column(6,
                  div(class = "note-box",
                      div(class = "nb-title", "Por qué importa el modelo rezagado"),
                      "Si solo viésemos el modelo del mismo año, podría preocuparnos que más turismo cause ",
                      "más presencia cultural y no al revés. Por eso estimamos además un modelo donde la ",
                      "cultura del año anterior explica el turismo del año actual. El coeficiente sigue siendo ",
                      "positivo y significativo, lo que ", tags$strong("ordena temporalmente"), " la historia: ",
                      "primero crece la presencia cultural, después el turismo."
                  ),
                  div(class = "note-box",
                      div(class = "nb-title", "Por qué el panel tiene solo dos países"),
                      "Japón y Corea son los dos casos asiáticos con datos comparables sobre exportaciones ",
                      "culturales y patrimonio UNESCO en el período completo. Esto limita la potencia ",
                      "estadística, pero permite una comparación limpia con efectos fijos por país y año."
                  )
                )
              )
            ),

            # ====================================================================
            # TAB 2: PANEL Y VARIABLES
            # ====================================================================
            tabPanel("Panel y variables",
              div(class = "lit-callout",
                  span(class = "lit-badge", "Datos"),
                  tags$strong("Qué cambió respecto al segundo avance"),
                  tags$br(),
                  "El panel del tercer avance (Panel_Nuevo.csv) sustituye las exportaciones audiovisuales por ",
                  "un ", tags$strong("índice cultural amplio"), " que combina exportaciones de bienes culturales y ",
                  "patrimonio UNESCO mundial e intangible. Esto hace la medición más comparable entre Japón y ",
                  "Corea, ya que la métrica de exportaciones de música está vacía para Japón y se ",
                  "excluye del modelo principal."
              ),

              fluidRow(
                column(7, div(class = "card",
                  div(class = "card-title", "Llegadas turísticas internacionales (miles)",
                      tags$span(class = "card-sub", "Fuente: UNWTO")),
                  plotOutput("plot_arrivals", height = "270px"),
                  div(class = "plot-caption", "Banda sombreada: años COVID (2020-2021).")
                )),
                column(5, div(class = "card",
                  div(class = "card-title", "Índice cultural amplio (z-mean)",
                      tags$span(class = "card-sub", "Promedio de z-scores: bienes culturales + UNESCO mundial + UNESCO intangible")),
                  plotOutput("plot_culture_idx", height = "270px")
                ))
              ),

              fluidRow(
                column(6, div(class = "card",
                  div(class = "card-title", "Exportaciones de bienes culturales (M USD)",
                      tags$span(class = "card-sub", "UNESCO Institute for Statistics / UN Comtrade")),
                  plotOutput("plot_cult_goods", height = "230px")
                )),
                column(6, div(class = "card",
                  div(class = "card-title", "Sitios y elementos UNESCO (acumulados)"),
                  plotOutput("plot_unesco", height = "230px")
                ))
              ),

              div(class = "simple-box",
                  div(class = "sb-title", "Cómo interpretar el índice cultural"),
                  "El índice cultural es ", tags$strong("un promedio estandarizado"), " de tres componentes: ",
                  "exportaciones de bienes culturales (M USD), sitios UNESCO de patrimonio mundial e ítems ",
                  "UNESCO de patrimonio intangible. Al estandarizar (z-score), cada componente entra en ",
                  "unidades comparables. Un valor de 0 significa estar en el promedio del panel; un valor de +1 ",
                  "significa estar a una desviación estándar por encima."
              ),

              # Tabla de fuentes
              div(class = "card",
                  div(class = "card-title", "Mapa de variables y fuentes del Panel_Nuevo.csv"),
                  div(class = "fuente-grid",
                      div(class = "fuente-item",
                          div(class = "fi-sigla", "UNWTO"),
                          div(class = "fi-desc", "UN World Tourism Organization"),
                          div(class = "fi-var", "tourist_arrivals_thousands - tourism_receipts_billion_usd")),
                      div(class = "fuente-item",
                          div(class = "fi-sigla", "UNESCO"),
                          div(class = "fi-desc", "World Heritage Center / Intangible Heritage"),
                          div(class = "fi-var", "unesco_world_heritage_sites - unesco_intangible_heritage_items")),
                      div(class = "fuente-item",
                          div(class = "fi-sigla", "UNESCO UIS"),
                          div(class = "fi-desc", "Cultural Goods Trade"),
                          div(class = "fi-var", "cultural_goods_exports_million_usd")),
                      div(class = "fuente-item",
                          div(class = "fi-sigla", "World Bank"),
                          div(class = "fi-desc", "WDI - National Accounts"),
                          div(class = "fi-var", "gdp_billion_usd - gdp_per_capita_usd - cpi_2010_100")),
                      div(class = "fuente-item",
                          div(class = "fi-sigla", "BIS / IMF"),
                          div(class = "fi-desc", "Tipo de cambio nominal"),
                          div(class = "fi-var", "exchange_rate_lcu_usd")),
                      div(class = "fuente-item",
                          div(class = "fi-sigla", "KOCCA"),
                          div(class = "fi-desc", "Korean music exports (solo KR)"),
                          div(class = "fi-var", "music_exports_million_usd (excluida del modelo)"))
                  )
              )
            ),

            # ====================================================================
            # TAB 3: MODELO ECONOMÉTRICO
            # ====================================================================
            tabPanel("Modelo econométrico",

              div(class = "ecuacion",
                  span(class = "lbl", "Especificación principal"),
                  HTML("log(Turismo<sub>it</sub>) = &beta;<sub>1</sub> "),
                  span(class = "var-c", "ÍndiceCultural"), HTML("<sub>it</sub> + &beta;<sub>2</sub> log(PIBpc<sub>it</sub>) + &beta;<sub>3</sub> log(TipoCambio<sub>it</sub>) + "),
                  span(class = "var-fe", HTML("&alpha;<sub>i</sub>")), HTML(" + "),
                  span(class = "var-fe", HTML("&lambda;<sub>t</sub>")), HTML(" + u<sub>it</sub>")
              ),

              div(class = "simple-box",
                  div(class = "sb-title", "Qué significa cada parte"),
                  HTML(paste0(
                    tags$strong("log(Turismo)"), ": logaritmo de las llegadas internacionales del país en cada año.<br>",
                    tags$strong("Índice Cultural"), ": variable cultural central (estandarizada).<br>",
                    tags$strong("log(PIBpc)"), ": tamaño económico del país (control).<br>",
                    tags$strong("log(TipoCambio)"), ": precios relativos para visitantes (control).<br>",
                    tags$strong("alpha_i"), ": efectos fijos por país (controlan diferencias estables).<br>",
                    tags$strong("lambda_t"), ": efectos fijos por año (controlan shocks comunes como COVID o GFC)."
                  ))
              ),

              fluidRow(
                column(6, div(class = "card",
                  div(class = "card-title", "Modelo principal contemporáneo",
                      tags$span(class = "card-sub", "Coeficientes con errores robustos HC1")),
                  uiOutput("tbl_main"),
                  div(class = "plot-caption", paste0("n = ", nobs(m_main),
                      " | R^2 = ", formatC(summary(m_main)$r.squared, digits = 3, format = "f")))
                )),
                column(6, div(class = "card",
                  div(class = "card-title", "Modelo rezagado (cultura t-1)",
                      tags$span(class = "card-sub", "Reduce preocupación por causalidad inversa")),
                  uiOutput("tbl_lag"),
                  div(class = "plot-caption", paste0("n = ", nobs(m_lag),
                      " | R^2 = ", formatC(summary(m_lag)$r.squared, digits = 3, format = "f")))
                ))
              ),

              div(class = "simple-box",
                  div(class = "sb-title", "Lectura visual del coeficiente cultural"),
                  "El coeficiente cultural en el modelo principal es positivo y se separa claramente del cero. ",
                  "El intervalo (barra de error) no toca el cero, por eso decimos que la asociación es ",
                  tags$strong("estadísticamente significativa"), ". El signo positivo significa que ",
                  "más presencia cultural se asocia con más turismo, no con menos."
              ),

              div(class = "card",
                  div(class = "card-title", "Coeficiente cultural en distintas especificaciones",
                      tags$span(class = "card-sub", "Forest plot con intervalo +/- 1.96 SE HC1")),
                  plotOutput("forest_modelos", height = "260px")
              ),

              div(class = "card",
                  div(class = "card-title", "Robustez agregando inflación",
                      tags$span(class = "card-sub", "La inflación no es central; solo confirma que el resultado se sostiene")),
                  uiOutput("tbl_inf"),
                  div(class = "plot-caption", paste0("n = ", nobs(m_inf),
                      " | Pierde 2 obs por cálculo de tasa de crecimiento del CPI"))
              )
            ),

            # ====================================================================
            # TAB 4: ENDOGENEIDAD Y PARSIMONIA
            # ====================================================================
            tabPanel("Endogeneidad y parsimonia",

              div(class = "lit-callout",
                  span(class = "lit-badge", "Decisión metodológica"),
                  tags$strong("Por qué el modelo final es deliberadamente pequeño"),
                  tags$br(),
                  "Con solo 40 observaciones y muchos efectos fijos, cada variable adicional cuesta grados de ",
                  "libertad. Más aún: algunas variables intuitivamente atractivas son ", tags$strong("mecanismos"),
                  " del propio efecto cultura-turismo (como hotelería) y, si se incluyen, bloquean parte del ",
                  "efecto que queremos medir. Por eso el modelo principal solo conserva ",
                  tags$strong("índice cultural + PIB per cápita + tipo de cambio + efectos fijos"), "."
              ),

              # Diagrama mecanismo
              div(class = "card",
                  div(class = "card-title", "Por qué el rezago ordena temporalmente la historia"),
                  div(class = "mec-flow",
                      div(class = "mec-box highlight", tags$b("Cultura"), tags$br(), "año t-1"),
                      div(class = "mec-arrow", "->"),
                      div(class = "mec-box accent", "Exposición", tags$br(), "internacional"),
                      div(class = "mec-arrow", "->"),
                      div(class = "mec-box", "Imagen del", tags$br(), "destino"),
                      div(class = "mec-arrow", "->"),
                      div(class = "mec-box", "Intención", tags$br(), "de visitar"),
                      div(class = "mec-arrow", "->"),
                      div(class = "mec-box highlight", tags$b("Turismo"), tags$br(), "año t")
                  ),
                  tags$p(style = "font-size:11px; color:#888; margin-top:8px;",
                         "El modelo rezagado refleja esta secuencia: si la cultura del año anterior explica el turismo del año actual, la causalidad inversa es menos probable.")
              ),

              fluidRow(
                column(7, div(class = "card",
                  div(class = "card-title", "Variables conservadas, excluidas o movidas a sensibilidad"),
                  tableOutput("tbl_decisiones")
                )),
                column(5,
                  div(class = "simple-box",
                      div(class = "sb-title", "Por qué se excluye hotelería"),
                      "Las habitaciones de hotel correlacionan con el índice cultural en r = 0.91. Esto sugiere que ",
                      "ambas crecen juntas como parte del mismo proceso. Si incluimos hotelería en el modelo, ",
                      tags$strong("le quitamos"), " al índice cultural una variación que probablemente le ",
                      "pertenece, y el coeficiente cultural se vuelve más pequeño por una razón mecánica, ",
                      "no porque el efecto sea menor."
                  ),
                  div(class = "simple-box",
                      div(class = "sb-title", "Por qué se excluyen las dummies COVID y GFC"),
                      "Como ya tenemos efectos fijos por año, los shocks comunes a ambos países (pandemia, ",
                      "crisis global) ya están ", tags$strong("absorbidos"), ". Agregar dummies COVID y GFC ",
                      "duplicaría la información."
                  )
                )
              ),

              div(class = "card",
                  div(class = "card-title", "Correlaciones entre variables clave",
                      tags$span(class = "card-sub", "Heatmap del panel completo")),
                  plotOutput("heatmap_corr", height = "320px"),
                  div(class = "plot-caption",
                      "Las correlaciones altas (>|0.8|) entre cultura y hotelería justifican excluir hotelería del modelo principal.")
              )
            ),

            # ====================================================================
            # TAB 5: DIAGNÓSTICOS Y ROBUSTEZ
            # ====================================================================
            tabPanel("Diagnósticos y robustez",

              div(class = "lit-callout",
                  span(class = "lit-badge", "Honestidad estadística"),
                  tags$strong("Qué dicen las pruebas y cómo se manejan"),
                  tags$br(),
                  "Las pruebas detectan ", tags$strong("heteroscedasticidad"), " (varianza no constante de los errores), ",
                  tags$strong("dependencia transversal"), " (los errores de Japón y Corea no son independientes) y ",
                  tags$strong("residuos no normales"), ". Por eso reportamos errores robustos HC1 en lugar de errores estándar clásicos."
              ),

              fluidRow(
                column(6, div(class = "card",
                  div(class = "card-title", "Pruebas de selección del modelo"),
                  tableOutput("tbl_pruebas_seleccion"),
                  div(class = "plot-caption",
                      "Con N=2 países las pruebas tienen baja potencia; la decisión FE doble vía es teórica.")
                )),
                column(6, div(class = "card",
                  div(class = "card-title", "Diagnósticos de supuestos (modelo principal)"),
                  tableOutput("tbl_diag_main"),
                  div(class = "plot-caption",
                      "Por estos diagnósticos se usan errores HC1 y se interpreta como asociación, no causalidad.")
                ))
              ),

              fluidRow(
                column(6, div(class = "card",
                  div(class = "card-title", "Multicolinealidad - VIF del modelo parsimonioso"),
                  plotOutput("plot_vif", height = "230px"),
                  div(class = "plot-caption",
                      "Todos los VIF son menores a 5. La parsimonia se valida empiricamente.")
                )),
                column(6, div(class = "card",
                  div(class = "card-title", "Comparación de R^2 entre especificaciones"),
                  plotOutput("plot_r2", height = "230px")
                ))
              ),

              div(class = "simple-box",
                  div(class = "sb-title", "Por qué NO usamos errores agrupados por país"),
                  "Los errores agrupados (clustered standard errors) requieren ", tags$strong("muchos clusters"),
                  " (al menos 30-50). Con solo 2 países, esa fórmula da resultados artificialmente precisos o degenerados. ",
                  "Por eso reportamos HC1 sobre la representación LSDV (Least Squares Dummy Variable), que es la versión equivalente del modelo de efectos fijos."
              )
            ),

            # ====================================================================
            # TAB 6: COMPARACIÓN CON LITERATURA
            # ====================================================================
            tabPanel("Comparación con literatura",

              div(class = "lit-callout",
                  span(class = "lit-badge", "Coherencia con la literatura"),
                  tags$strong("El hallazgo coincide con estudios previos sobre Hallyu y turismo inducido"),
                  tags$br(),
                  "Los resultados dialogan con la literatura previa sobre Hallyu, dramas coreanos, anime y ",
                  "screen tourism. La coincidencia más clara es con ", span(class = "cita", "Bae et al. (2017)"),
                  ", que también encuentra que el factor cultural y el tipo de cambio son significativos y la ",
                  "inflación no lo es."
              ),

              div(class = "study-row",
                  div(class = "study-card", style = "border-top-color:#003478;",
                      div(class = "sc-autor", "Bae et al. (2017)"),
                      div(class = "sc-tipo", "Panel Hallyu hacia Corea"),
                      div(class = "sc-hallazgo",
                          "Hallyu, PIB y tipo de cambio significativos en demanda turística hacia Corea; CPI no significativo."),
                      div(class = "sc-dato", style = "border-left-color:#003478;",
                          "Coincide con nuestro modelo: cultura y tipo de cambio significativos, inflación no.")
                  ),
                  div(class = "study-card", style = "border-top-color:#7b5ea7;",
                      div(class = "sc-autor", "Kim, Chen y Su (2009)"),
                      div(class = "sc-tipo", "Cambio estructural - dramas KR"),
                      div(class = "sc-hallazgo",
                          "Detectan cambio estructural en visitantes taiwaneses a Corea tras dramas coreanos; cambio sobre todo en placer."),
                      div(class = "sc-dato", style = "border-left-color:#7b5ea7;",
                          "Coincide con la lógica del modelo rezagado: la exposición previa antecede al turismo.")
                  ),
                  div(class = "study-card", style = "border-top-color:#bc002d;",
                      div(class = "sc-autor", "Yildirim et al. (2017)"),
                      div(class = "sc-tipo", "Cualitativo - anime y JP"),
                      div(class = "sc-hallazgo",
                          "El anime aumenta intención de viajar a Japón; barreras económicas pueden frenar la conversión."),
                      div(class = "sc-dato", style = "border-left-color:#bc002d;",
                          "Apoya la lectura japonesa: cultura popular activa deseo turístico.")
                  )
              ),

              div(class = "study-row",
                  div(class = "study-card", style = "border-top-color:#2e8b57;",
                      div(class = "sc-autor", "Kim, Long y Robinson (2009)"),
                      div(class = "sc-tipo", "Cualitativo - screen tourism"),
                      div(class = "sc-hallazgo",
                          "Los k-dramas se relacionan con patrones de turismo vía circulación cultural y proximidad cultural en Asia.")
                  ),
                  div(class = "study-card", style = "border-top-color:#7b5ea7;",
                      div(class = "sc-autor", "Ng y Chan (2019/2020)"),
                      div(class = "sc-tipo", "Encuesta jóvenes HK - n=220"),
                      div(class = "sc-hallazgo",
                          "Correlación moderada entre k-dramas e intención de visitar Corea; rezago empírico de 6-18 meses."),
                      div(class = "sc-dato", style = "border-left-color:#7b5ea7;",
                          "Apoya nuestro modelo rezagado a un año.")
                  ),
                  div(class = "study-card", style = "border-top-color:#555;",
                      div(class = "sc-autor", "Nye (2004)"),
                      div(class = "sc-tipo", "Marco teórico - soft power"),
                      div(class = "sc-hallazgo",
                          "Capacidad de atraer mediante cultura, valores e instituciones. Marco para interpretar la relación cultura -> turismo.")
                  )
              ),

              div(class = "simple-box",
                  div(class = "sb-title", "Aporte propio del proyecto"),
                  "A diferencia de los estudios previos, nuestro panel ", tags$strong("compara dos países destino"),
                  " (Japón y Corea) con un ", tags$strong("índice cultural amplio"), " que combina exportaciones ",
                  "culturales y patrimonio UNESCO, no solo Hallyu. La aportación es mostrar que la relación ",
                  "cultura-turismo aparece también en una especificación macro comparativa con efectos fijos doble vía."
              )
            ),

            # ====================================================================
            # TAB 7: DATOS DEL PANEL
            # ====================================================================
            tabPanel("Datos del panel",
              br(),
              div(class = "card",
                  div(class = "card-title", "Panel_Nuevo.csv (N = 40)"),
                  DTOutput("full_table")
              )
            )

          ) # tabsetPanel
      ) # main-area
  ) # body-wrap
)

# ==============================================================================
# 5. SERVER
# ==============================================================================
server <- function(input, output, session) {

  # Datos filtrados por sidebar (solo para los plots descriptivos)
  df <- reactive({
    d <- panel %>% filter(year >= input$yr[1], year <= input$yr[2])
    if (input$country_sel != "both") d <- d %>% filter(country == input$country_sel)
    if (input$excl_covid) d <- d %>% filter(covid_dummy == 0)
    d
  })

  # ── KPIs ──
  output$kpi_coef_main <- renderText({
    v <- coef_main$coeficiente[coef_main$variable == "culture_index"]
    formatC(v, digits = 3, format = "f")
  })
  output$kpi_coef_lag <- renderText({
    v <- coef_lag$coeficiente[coef_lag$variable == "culture_index_lag1"]
    formatC(v, digits = 3, format = "f")
  })
  output$kpi_n   <- renderText({ as.character(nobs(m_main)) })
  output$kpi_r2  <- renderText({ formatC(summary(m_main)$r.squared, digits = 3, format = "f") })
  output$kpi_jp_arr <- renderText({
    v <- panel %>% filter(country == "Japan", year == 2024) %>% pull(tourist_arrivals_thousands)
    format(v, big.mark = ",")
  })
  output$kpi_kr_arr <- renderText({
    v <- panel %>% filter(country == "Korea", year == 2024) %>% pull(tourist_arrivals_thousands)
    format(v, big.mark = ",")
  })

  # Texto plain del efecto en %
  output$plain_main_pct <- renderText({
    b <- coef_main$coeficiente[coef_main$variable == "culture_index"]
    pct <- (exp(b) - 1) * 100
    paste0("+", formatC(pct, digits = 0, format = "f"), "%")
  })

  # ============================================================================
  # TABLAS DE COEFICIENTES (HTML formateado)
  # ============================================================================
  fmt_p <- function(p) ifelse(p < 0.001, "<0.001", formatC(p, digits = 3, format = "f"))
  star  <- function(p) ifelse(is.na(p), "",
                              ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", ""))))
  pretty_var <- function(v) {
    dplyr::recode(v,
      "culture_index"      = "Índice cultural (z-mean)",
      "culture_index_lag1" = "Índice cultural rezagado t-1",
      "log_gdp_pc"         = "log(PIB per cápita)",
      "log_exchange"       = "log(Tipo de cambio)",
      "inflation_pct"      = "Inflación (% CPI)",
      .default = v
    )
  }

  render_coef_table <- function(coef_df, vars) {
    d <- coef_df %>% filter(variable %in% vars)
    rows <- vapply(seq_len(nrow(d)), function(i) {
      r <- d[i, ]
      cls <- ifelse(r$p < 0.10, ifelse(r$coeficiente > 0, "sig-pos", "sig-neg"), "sig-na")
      sprintf(
        "<tr><td>%s</td><td class='num'>%s</td><td class='num'>%s</td><td class='num'>%s</td><td class='num %s'>%s%s</td></tr>",
        pretty_var(r$variable),
        formatC(r$coeficiente, digits = 3, format = "f"),
        formatC(r$se_hc1, digits = 3, format = "f"),
        formatC(r$t, digits = 2, format = "f"),
        cls, fmt_p(r$p), star(r$p)
      )
    }, character(1))
    HTML(paste0(
      "<table class='coef-table'>",
      "<thead><tr><th>Variable</th><th class='num'>Coef.</th><th class='num'>SE HC1</th><th class='num'>t</th><th class='num'>p</th></tr></thead>",
      "<tbody>", paste(rows, collapse = ""), "</tbody></table>"
    ))
  }

  output$tbl_main <- renderUI({ render_coef_table(coef_main, core_vars_main) })
  output$tbl_lag  <- renderUI({ render_coef_table(coef_lag,  core_vars_lag)  })
  output$tbl_inf  <- renderUI({ render_coef_table(coef_inf,  core_vars_inf)  })

  output$tbl_coef_resumen <- renderUI({
    # Combinar coef principal + rezagado en una sola tabla resumen
    main_row <- coef_main %>% filter(variable == "culture_index")
    lag_row  <- coef_lag  %>% filter(variable == "culture_index_lag1")
    tc_row   <- coef_main %>% filter(variable == "log_exchange")
    pib_row  <- coef_main %>% filter(variable == "log_gdp_pc")
    rows <- list(
      list("Índice cultural (modelo principal)", main_row),
      list("Índice cultural rezagado t-1",       lag_row),
      list("log(Tipo de cambio)",                tc_row),
      list("log(PIB per cápita)",                pib_row)
    )
    rows_html <- vapply(rows, function(x) {
      r <- x[[2]]
      cls <- ifelse(r$p < 0.10, ifelse(r$coeficiente > 0, "sig-pos", "sig-neg"), "sig-na")
      sprintf(
        "<tr><td>%s</td><td class='num'>%s</td><td class='num'>%s</td><td class='num %s'>%s%s</td></tr>",
        x[[1]],
        formatC(r$coeficiente, digits = 3, format = "f"),
        formatC(r$se_hc1, digits = 3, format = "f"),
        cls, fmt_p(r$p), star(r$p)
      )
    }, character(1))
    HTML(paste0(
      "<table class='coef-table'>",
      "<thead><tr><th>Variable</th><th class='num'>Coef.</th><th class='num'>SE HC1</th><th class='num'>p</th></tr></thead>",
      "<tbody>", paste(rows_html, collapse = ""), "</tbody></table>"
    ))
  })

  # ============================================================================
  # PLOTS
  # ============================================================================

  # Forest plot resumen (3 especificaciones)
  output$forest_resumen <- renderPlot({
    fdata <- data.frame(
      modelo = c("Principal\n(contemporáneo)", "Rezagado\n(t-1)", "Robustez\n+ inflación"),
      coef   = c(
        coef_main$coeficiente[coef_main$variable == "culture_index"],
        coef_lag$coeficiente[coef_lag$variable == "culture_index_lag1"],
        coef_inf$coeficiente[coef_inf$variable == "culture_index"]
      ),
      se     = c(
        coef_main$se_hc1[coef_main$variable == "culture_index"],
        coef_lag$se_hc1[coef_lag$variable == "culture_index_lag1"],
        coef_inf$se_hc1[coef_inf$variable == "culture_index"]
      )
    ) %>%
      mutate(low = coef - 1.96 * se, high = coef + 1.96 * se,
             modelo = factor(modelo, levels = modelo))

    ggplot(fdata, aes(x = modelo, y = coef)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#aaa") +
      geom_errorbar(aes(ymin = low, ymax = high), width = 0.18,
                    linewidth = 0.9, color = col_accent) +
      geom_point(size = 4.5, color = col_accent) +
      geom_text(aes(label = formatC(coef, digits = 2, format = "f")),
                vjust = -1.3, size = 3.6, color = "#1a1a2e", fontface = "bold") +
      theme_eq3() +
      labs(x = NULL, y = "Coeficiente del índice cultural")
  })

  # Forest plot detallado modelo
  output$forest_modelos <- renderPlot({
    rows_main <- coef_main %>% filter(variable %in% core_vars_main) %>%
      mutate(modelo = "Principal", var_label = pretty_var(variable))
    rows_lag <- coef_lag %>% filter(variable %in% core_vars_lag) %>%
      mutate(modelo = "Rezagado", var_label = pretty_var(variable))
    fdata <- bind_rows(rows_main, rows_lag) %>%
      mutate(low = coeficiente - 1.96 * se_hc1, high = coeficiente + 1.96 * se_hc1)

    ggplot(fdata, aes(x = coeficiente, y = var_label, color = modelo)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#aaa") +
      geom_errorbarh(aes(xmin = low, xmax = high), height = 0.22,
                     position = position_dodge(width = 0.5), linewidth = 0.8) +
      geom_point(size = 3, position = position_dodge(width = 0.5)) +
      scale_color_manual(values = c("Principal" = col_accent, "Rezagado" = col_kr)) +
      theme_eq3() +
      labs(x = "Coeficiente (con IC 95% basado en HC1)", y = NULL)
  })

  # Observado vs predicho
  output$plot_obs_pred <- renderPlot({
    pred <- panel %>% mutate(pred = predict(m_main, newdata = panel))
    ggplot(pred, aes(x = year)) +
      annotate("rect", xmin = 2019.5, xmax = 2021.5,
               ymin = -Inf, ymax = Inf, fill = "#ffdddd", alpha = .35) +
      geom_line(aes(y = log_arrivals, color = country, linetype = "Observado"), linewidth = 1) +
      geom_line(aes(y = pred,         color = country, linetype = "Predicción"), linewidth = 0.9) +
      sc() +
      scale_linetype_manual(values = c("Observado" = "solid", "Predicción" = "dashed")) +
      theme_eq3() +
      scale_x_continuous(breaks = seq(2005, 2024, 3)) +
      labs(x = NULL, y = "log(llegadas)", linetype = NULL)
  })

  # Llegadas observadas
  output$plot_arrivals <- renderPlot({
    p <- make_line(df(), "tourist_arrivals_thousands", "Miles de llegadas")
    if (input$use_log) p <- p + scale_y_log10(labels = label_comma())
    else p <- p + scale_y_continuous(labels = label_comma())
    p
  })

  # Índice cultural
  output$plot_culture_idx <- renderPlot({
    make_line(df(), "culture_index", "Índice cultural (z-mean)") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#aaa")
  })

  # Bienes culturales
  output$plot_cult_goods <- renderPlot({
    p <- make_line(df(), "cultural_goods_exports_million_usd", "M USD")
    if (input$use_log) p <- p + scale_y_log10(labels = label_comma())
    else p <- p + scale_y_continuous(labels = label_comma())
    p
  })

  # UNESCO
  output$plot_unesco <- renderPlot({
    d <- df() %>%
      select(country, year, world = unesco_world_heritage_sites,
             intang = unesco_intangible_heritage_items) %>%
      pivot_longer(cols = c(world, intang), names_to = "tipo", values_to = "n") %>%
      mutate(tipo = recode(tipo, "world" = "Sitios mundiales", "intang" = "Patrimonio intangible"))
    ggplot(d, aes(x = year, y = n, color = country, linetype = tipo)) +
      geom_line(linewidth = 0.9) + geom_point(size = 1.6) +
      sc() + theme_eq3() +
      scale_x_continuous(breaks = seq(2005, 2024, 3)) +
      labs(x = NULL, y = "Número acumulado", linetype = NULL)
  })

  # Heatmap correlaciones
  output$heatmap_corr <- renderPlot({
    vars <- panel %>%
      select(`log llegadas` = log_arrivals,
             `Índice cultural` = culture_index,
             `log Bienes cult.` = log_cult_goods,
             `log PIB pc` = log_gdp_pc,
             `log Tipo cambio` = log_exchange,
             `Hotelería` = hotel_rooms_thousand,
             `Banda ancha` = broadband_per_100_inhabitants,
             `Inflación` = inflation_pct)
    m <- cor(vars, use = "pairwise.complete.obs")
    md <- as.data.frame(as.table(m))
    names(md) <- c("v1", "v2", "r")
    ggplot(md, aes(x = v1, y = v2, fill = r)) +
      geom_tile(color = "white") +
      geom_text(aes(label = formatC(r, digits = 2, format = "f")),
                size = 3, color = ifelse(abs(md$r) > 0.6, "white", "#222")) +
      scale_fill_gradient2(low = col_jp, mid = "white", high = col_kr,
                          midpoint = 0, limits = c(-1, 1)) +
      theme_eq3() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = NULL, y = NULL, fill = "r")
  })

  # VIF
  output$plot_vif <- renderPlot({
    base_lm <- lm(log_arrivals ~ culture_index + log_gdp_pc + log_exchange, data = panel)
    X <- model.matrix(base_lm)[, -1]
    vif_vals <- sapply(seq_len(ncol(X)), function(j) {
      r2 <- summary(lm(X[, j] ~ X[, -j]))$r.squared
      1 / (1 - r2)
    })
    vd <- data.frame(var = pretty_var(colnames(X)), VIF = vif_vals)
    ggplot(vd, aes(x = reorder(var, VIF), y = VIF)) +
      geom_col(fill = col_accent, width = 0.55) +
      geom_hline(yintercept = 5,  linetype = "dashed", color = "#aaa") +
      geom_hline(yintercept = 10, linetype = "dashed", color = col_jp) +
      geom_text(aes(label = formatC(VIF, digits = 2, format = "f")),
                hjust = -0.15, size = 3.5, color = "#1a1a2e") +
      coord_flip(clip = "off") +
      theme_eq3() +
      labs(x = NULL, y = "VIF")
  })

  # R^2
  output$plot_r2 <- renderPlot({
    rd <- data.frame(
      modelo = c("Pooled OLS", "FE país", "FE año", "FE doble vía\n(principal)",
                 "FE doble vía\n+ rezago", "FE doble vía\n+ inflación"),
      r2 = c(0.058, 0.052, 0.527, 0.496, 0.401, 0.543)
    )
    rd$modelo <- factor(rd$modelo, levels = rd$modelo)
    rd$tipo <- ifelse(grepl("principal", rd$modelo), "Principal", "Comparación")
    ggplot(rd, aes(x = modelo, y = r2, fill = tipo)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = formatC(r2, digits = 3, format = "f")),
                vjust = -0.3, size = 3.4, color = "#1a1a2e") +
      scale_fill_manual(values = c("Principal" = col_accent, "Comparación" = "#bbb")) +
      theme_eq3() +
      labs(x = NULL, y = "R^2") +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 8.5))
  })

  # ============================================================================
  # TABLAS DE TEXTO (decisiones, pruebas)
  # ============================================================================
  output$tbl_decisiones <- renderTable({
    data.frame(
      Variable = c("culture_index_zmean", "culture_index_lag1",
                   "log_gdp_pc", "log_exchange_rate",
                   "inflation_pct", "hotel_rooms_thousand",
                   "broadband", "covid/gfc dummies",
                   "music_exports", "componentes UNESCO sueltos"),
      Decisión = c("Conservar (modelo principal)", "Conservar (modelo preferido temporalidad)",
                   "Conservar", "Conservar",
                   "Sensibilidad", "Excluir del principal",
                   "Sensibilidad / descriptiva", "No incluir con FE año",
                   "Excluir", "Evitar en principal"),
      Razón = c("Variable central de la hipótesis", "Reduce simultaneidad",
                "Control macro básico", "Competitividad / precio relativo",
                "No central; reduce muestra", "Posible mecanismo (r=0.91 con cultura)",
                "Mide desarrollo general, no cultura", "Absorbidas por efectos por año",
                "Faltantes completos en Japón", "Ya forman parte del índice")
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE,
     spacing = "s", align = "lll", width = "100%")

  output$tbl_pruebas_seleccion <- renderTable({
    data.frame(
      Prueba = c("F: FE país vs pooled", "F: FE doble vía vs pooled",
                 "LM Breusch-Pagan: pooled vs RE", "Hausman FE vs RE"),
      Estadístico = c("0.071", "19.205", "1.052", "NA"),
      `p valor` = c("0.791", "<0.001", "0.305", "NA"),
      Lectura = c("No prefiere FE país", "Prefiere FE doble vía",
                  "Sin evidencia para RE", "No estimable con N=2"),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s", width = "100%")

  output$tbl_diag_main <- renderTable({
    data.frame(
      Prueba = c("Breusch-Pagan", "Breusch-Godfrey", "Pesaran CD", "RESET", "Jarque-Bera"),
      `p valor` = c("0.015", "0.079", "<0.001", "<0.001", "<0.001"),
      Lectura = c("Heteroscedasticidad", "Sin AR fuerte", "Dependencia transversal",
                  "Posible mala especificación", "Residuos no normales"),
      Implicación = c("Usar HC1", "OK", "Cautela", "Cautela", "Inferencia con cautela"),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s", width = "100%")

  # ============================================================================
  # TAB 7: TABLA COMPLETA
  # ============================================================================
  output$full_table <- renderDT({
    df() %>%
      mutate(`Índice cultural` = round(culture_index, 2)) %>%
      select(País = country, Año = year,
             `Llegadas (miles)`     = tourist_arrivals_thousands,
             `Ingresos (B USD)`     = tourism_receipts_billion_usd,
             `Bienes culturales (M USD)` = cultural_goods_exports_million_usd,
             `UNESCO mundial`       = unesco_world_heritage_sites,
             `UNESCO intangible`    = unesco_intangible_heritage_items,
             `Índice cultural`,
             `PIB pc (USD)`         = gdp_per_capita_usd,
             `Tipo cambio (LCU/USD)` = exchange_rate_lcu_usd,
             `CPI 2010=100`         = cpi_2010_100,
             `Hotelería (mil cuartos)` = hotel_rooms_thousand,
             `Banda ancha`          = broadband_per_100_inhabitants,
             COVID                   = covid_dummy,
             GFC                     = gfc_dummy) %>%
      arrange(País, Año)
  }, options = list(
       pageLength = 15, dom = "ftp", scrollX = TRUE,
       language = list(search = "Buscar:",
                       paginate = list(previous = "Anterior", `next` = "Siguiente"))
     ),
     rownames = FALSE)
}

shinyApp(ui, server)
