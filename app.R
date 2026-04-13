

# ==============================================================================
# DATOS EMPIRICOS — Panel 2005-2024 (Japon y Corea del Sur)
# Fuentes: UNWTO, KOCCA, JETRO, World Bank, BIS, OAG, WGI
# ==============================================================================

japan <- data.frame(
  Country = "Japan",
  Year = 2005:2024,
  Arrivals_thousands = c(6728,7334,8347,8351,6790,8611,6219,8358,10364,
                         13413,19737,24040,28691,31192,31882,4116,246,
                         3832,25066,36870),
  Tourism_Receipts_M_USD = c(12451,8467,9334,10821,10305,13199,11003,14576,
                             15131,18853,24983,30679,34054,41115,46054,
                             7208,1863,9236,33638,53200),
  AV_Exports_M_USD = c(520,610,720,810,790,870,950,1080,1250,1420,
                       1680,1890,2100,2350,2580,2410,2620,2890,3250,3600),
  GDP_pc_USD = c(35781,34077,34095,37972,39473,44508,48168,48603,
                 40855,38109,34524,38761,38332,39290,40247,
                 39890,39313,33815,33950,33140),
  REER_index = c(81.5,73.4,68.2,72.8,82.7,85.0,82.3,80.1,75.8,
                 72.5,65.1,68.9,64.5,63.8,64.2,64.8,58.9,50.8,48.2,46.5),
  Air_Seats_M = c(42.1,43.8,46.2,44.0,39.5,43.6,38.8,41.0,44.2,
                  49.8,56.4,62.3,69.5,75.2,78.3,18.5,3.2,22.1,65.0,85.2),
  WGI_PolStab = c(1.08,1.03,0.98,1.05,1.10,1.00,0.95,1.00,1.02,
                  1.04,1.07,1.09,1.06,1.04,1.03,1.05,1.08,1.06,1.04,1.02),
  COVID = c(rep(0,15),1,1,0,0,0)
)

korea <- data.frame(
  Country = "South Korea",
  Year = 2005:2024,
  Arrivals_thousands = c(6023,6155,6448,6891,7818,8798,9795,11140,
                         12176,14202,13232,17242,13336,15347,17503,
                         2519,967,3198,11034,17550),
  Tourism_Receipts_M_USD = c(5806,5818,6085,9720,9782,10329,12397,13429,
                             14629,17837,15214,17332,13368,15324,17478,
                             3790,1028,6012,14875,22100),
  AV_Exports_M_USD = c(620,780,950,1180,1400,1720,2050,2520,3100,
                       3800,4550,5273,6380,7600,8900,9200,10200,
                       11500,13200,14800),
  GDP_pc_USD = c(18658,20917,23101,20475,18339,22151,24156,24454,
                 25998,27989,27195,27539,29743,31363,31762,
                 31638,34758,32423,32254,33150),
  REER_index = c(108.5,113.2,109.8,86.5,82.0,100.0,102.8,101.5,
                 98.2,95.8,89.5,92.1,94.5,91.0,87.2,85.0,
                 81.5,76.2,73.8,72.0),
  Air_Seats_M = c(28.5,30.2,33.1,32.0,29.5,34.8,38.2,42.5,45.8,
                  50.2,52.5,58.8,55.2,60.5,63.0,12.5,2.8,18.0,48.5,68.0),
  WGI_PolStab = c(0.28,0.30,0.35,0.32,0.38,0.30,0.28,0.32,0.22,
                  0.16,0.14,0.10,0.22,0.31,0.38,0.42,0.45,0.40,0.38,0.35),
  COVID = c(rep(0,15),1,1,0,0,0)
)

panel <- bind_rows(japan, korea) %>%
  mutate(
    Arrivals_M = Arrivals_thousands / 1000,
    AV_Growth_pct = ave(AV_Exports_M_USD, Country, FUN = function(x)
      c(NA, diff(x)/head(x,-1)*100)),
    Arrivals_Growth_pct = ave(Arrivals_thousands, Country, FUN = function(x)
      c(NA, diff(x)/head(x,-1)*100)),
    Receipts_Growth_pct = ave(Tourism_Receipts_M_USD, Country, FUN = function(x)
      c(NA, diff(x)/head(x,-1)*100))
  )

col_jp <- "#bc002d"
col_kr <- "#003478"
col_accent <- "#e8a020"

# ==============================================================================
# UI
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

      /* ── HEADER ── */
      .app-header {
        background: #1a1a2e;
        color: white;
        padding: 20px 28px 16px;
        border-bottom: 3px solid #e8a020;
      }
      .app-header h1 {
        font-family: 'Noto Serif', serif;
        font-size: 19px; font-weight: 700; margin: 0 0 4px;
        letter-spacing: -.3px;
      }
      .app-header p {
        font-size: 11.5px; color: #aab; margin: 0;
      }
      .app-header .fuentes {
        font-size: 10.5px; color: #778; margin-top: 3px;
      }
      .equipo-tag {
        display: inline-block;
        background: rgba(232,160,32,.18);
        border: 1px solid rgba(232,160,32,.4);
        border-radius: 3px;
        color: #e8a020;
        font-size: 10px; font-weight: 600;
        padding: 2px 8px;
        margin-top: 8px;
        letter-spacing: .5px;
        text-transform: uppercase;
      }

      /* ── KPI BAR ── */
      .kpi-bar {
        background: white;
        border-bottom: 1px solid #e4e6eb;
        padding: 10px 28px;
        display: flex; gap: 0;
      }
      .kpi-item {
        flex: 1; text-align: center;
        padding: 6px 10px;
        border-right: 1px solid #eee;
      }
      .kpi-item:last-child { border-right: none; }
      .kpi-val { font-size: 17px; font-weight: 700; line-height: 1.1; }
      .kpi-lbl { font-size: 10px; color: #888; margin-top: 2px; }
      .jp  { color: #bc002d; }
      .kr  { color: #003478; }
      .neu { color: #333; }

      /* ── LAYOUT ── */
      .body-wrap {
        display: flex;
        min-height: calc(100vh - 130px);
      }
      .sidebar {
        width: 210px; min-width: 210px;
        background: #1a1a2e;
        padding: 18px 14px;
        color: white;
      }
      .sidebar-label {
        font-size: 9.5px; font-weight: 700;
        text-transform: uppercase; letter-spacing: 1px;
        color: #667; margin: 14px 0 6px;
      }
      .sidebar-label:first-child { margin-top: 0; }
      .sidebar .shiny-input-container { margin-bottom: 10px; }
      .sidebar .shiny-input-container label {
        color: #aab; font-size: 11.5px; font-weight: 500;
      }
      .sidebar .form-control, .sidebar .selectize-input {
        background: #252545; border: 1px solid #334; color: white;
        font-size: 12px; border-radius: 5px;
      }
      .sidebar .irs--shiny .irs-bar { background: #e8a020; }
      .sidebar .irs--shiny .irs-handle { background: #e8a020; border-color: #e8a020; }
      .sidebar .irs--shiny .irs-from, .sidebar .irs--shiny .irs-to,
      .sidebar .irs--shiny .irs-single { background: #e8a020; }
      .sidebar .irs--shiny .irs-line { background: #334; }
      .sidebar .irs--shiny .irs-min, .sidebar .irs--shiny .irs-max { color: #667; }
      .sidebar input[type=checkbox] { accent-color: #e8a020; }

      .main-area {
        flex: 1;
        padding: 18px 22px;
        overflow-x: hidden;
      }

      /* ── TABS ── */
      .nav-tabs { border-bottom: 2px solid #dde; margin-bottom: 18px; }
      .nav-tabs > li > a {
        font-size: 12px; font-weight: 600;
        color: #556; padding: 8px 14px;
        border-radius: 6px 6px 0 0;
        border: none; background: transparent;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover {
        color: #1a1a2e; background: white;
        border: 2px solid #dde; border-bottom-color: white;
      }
      .nav-tabs > li > a:hover { background: #eef; color: #1a1a2e; }

      /* ── CARDS ── */
      .card {
        background: white; border-radius: 8px;
        padding: 16px 18px;
        box-shadow: 0 1px 4px rgba(0,0,0,.07);
        margin-bottom: 14px;
      }
      .card-title {
        font-size: 12.5px; font-weight: 700; color: #1a1a2e;
        margin-bottom: 10px;
      }
      .card-sub {
        font-size: 11px; color: #778; font-weight: 400;
        display: block; margin-top: 1px;
      }

      /* ── CONTEXTO / LITERATURA CALLOUT ── */
      .lit-callout {
        background: #fefaf3;
        border: 1px solid #f0d898;
        border-left: 4px solid #e8a020;
        border-radius: 0 6px 6px 0;
        padding: 12px 16px;
        margin-bottom: 14px;
        font-size: 12px;
        color: #4a3c1a;
        line-height: 1.65;
      }
      .lit-callout .lit-badge {
        display: inline-block;
        background: #e8a020;
        color: white;
        font-size: 9.5px; font-weight: 700;
        padding: 2px 7px; border-radius: 3px;
        text-transform: uppercase; letter-spacing: .5px;
        margin-right: 6px;
        vertical-align: middle;
      }
      .lit-callout .cita {
        font-style: italic; color: #7a6030;
      }
      .lit-callout strong { color: #3a2c0a; }

      .note-box {
        background: #f0f4ff;
        border-left: 4px solid #003478;
        border-radius: 0 6px 6px 0;
        padding: 10px 14px;
        font-size: 11.5px;
        color: #1a2a50;
        margin-bottom: 14px;
        line-height: 1.6;
      }
      .note-box .nb-title {
        font-weight: 700; font-size: 10.5px;
        text-transform: uppercase; letter-spacing: .5px;
        color: #003478; margin-bottom: 4px;
      }

      /* ── MECANISMO DIAGRAM ── */
      .mec-flow {
        display: flex; align-items: center; gap: 0;
        flex-wrap: wrap;
        margin: 10px 0;
      }
      .mec-box {
        background: #f8f9fb;
        border: 1.5px solid #dde;
        border-radius: 6px;
        padding: 8px 12px;
        font-size: 11.5px;
        text-align: center;
        min-width: 110px;
        line-height: 1.4;
      }
      .mec-box.highlight {
        background: #1a1a2e; color: white; border-color: #1a1a2e;
      }
      .mec-box.accent {
        background: #fef6e4; border-color: #e8a020; color: #5a3c00;
      }
      .mec-arrow {
        font-size: 18px; color: #aab; padding: 0 6px;
      }

      /* ── ESTUDIO CARD ── */
      .study-row {
        display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 10px;
      }
      .study-card {
        flex: 1; min-width: 200px;
        background: #f8f9fb;
        border-radius: 6px;
        padding: 12px 14px;
        border-top: 3px solid #ddd;
        font-size: 11.5px;
        line-height: 1.55;
      }
      .study-card .sc-autor {
        font-weight: 700; font-size: 12px; color: #1a1a2e; margin-bottom: 3px;
      }
      .study-card .sc-tipo {
        font-size: 10px; font-weight: 600;
        text-transform: uppercase; letter-spacing: .5px;
        color: #888; margin-bottom: 6px;
      }
      .study-card .sc-hallazgo {
        color: #444;
      }
      .study-card .sc-dato {
        margin-top: 7px;
        background: white; border-radius: 4px;
        padding: 5px 8px;
        font-size: 11px; color: #333;
        border-left: 3px solid;
      }

      /* ── TABLA FUENTES ── */
      .fuente-grid {
        display: grid; grid-template-columns: repeat(3, 1fr);
        gap: 10px; margin-top: 6px;
      }
      .fuente-item {
        background: #f8f9fb; border-radius: 6px;
        padding: 10px 12px; font-size: 11.5px;
        border: 1px solid #e4e6eb;
      }
      .fuente-item .fi-sigla {
        font-weight: 700; font-size: 13px; color: #1a1a2e; margin-bottom: 2px;
      }
      .fuente-item .fi-desc { color: #666; font-size: 10.5px; }
      .fuente-item .fi-var {
        margin-top: 5px;
        font-size: 10.5px; font-weight: 600;
        color: #e8a020;
      }

      /* plot captions */
      .plot-caption {
        font-size: 10.5px; color: #888; margin-top: 4px;
        font-style: italic; text-align: right;
      }

      /* ── separadores sección ── */
      .section-sep {
        border: none; border-top: 1.5px solid #eef;
        margin: 18px 0;
      }

      /* ── COVID band helper ── */
      .covid-note {
        font-size: 10.5px; color: #bc002d;
        font-style: italic; margin-top: 3px;
      }
    "))
  ),
  
  # ── HEADER ──────────────────────────────────────────────────
  div(class = "app-header",
      tags$h1("Turismo Internacional y Exportacion Cultural Audiovisual"),
      tags$p("Japon y Corea del Sur · Panel de datos 2005–2024"),
      div(class = "fuentes", "Datos: UNWTO · KOCCA · JETRO · World Bank · BIS · OAG · WGI"),
      div(class = "equipo-tag", "Equipo 3 · Inteligencia de Negocios · 2026")
  ),
  
  # ── KPI BAR ─────────────────────────────────────────────────
  div(class = "kpi-bar",
      div(class = "kpi-item",
          div(class = "kpi-val jp", textOutput("kpi_jp_arr")),
          div(class = "kpi-lbl", "Max llegadas JP (2024)")),
      div(class = "kpi-item",
          div(class = "kpi-val kr", textOutput("kpi_kr_arr")),
          div(class = "kpi-lbl", "Max llegadas KR (2019)")),
      div(class = "kpi-item",
          div(class = "kpi-val jp", textOutput("kpi_jp_av24")),
          div(class = "kpi-lbl", "Export. AV JP 2024")),
      div(class = "kpi-item",
          div(class = "kpi-val kr", textOutput("kpi_kr_av24")),
          div(class = "kpi-lbl", "Export. AV KR 2024")),
      div(class = "kpi-item",
          div(class = "kpi-val kr",  textOutput("kpi_kr_crecav")),
          div(class = "kpi-lbl", "Crecimiento AV KR 2005-2024")),
      div(class = "kpi-item",
          div(class = "kpi-val neu", "40"),
          div(class = "kpi-lbl", "Observaciones panel"))
  ),
  
  # ── BODY ────────────────────────────────────────────────────
  div(class = "body-wrap",
      
      # Sidebar
      div(class = "sidebar",
          div(class = "sidebar-label", "Periodo"),
          sliderInput("yr", NULL,
                      min = 2005, max = 2024, value = c(2005, 2024), sep = "", step = 1),
          div(class = "sidebar-label", "Pais"),
          selectInput("country_sel", NULL,
                      choices = c("Ambos" = "both", "Japon" = "Japan", "Corea del Sur" = "South Korea"),
                      selected = "both"),
          div(class = "sidebar-label", "Opciones"),
          checkboxInput("excl_covid", "Excluir COVID (2020-21)", value = FALSE),
          checkboxInput("use_log",    "Escala logaritmica",      value = FALSE)
      ),
      
      # Main
      div(class = "main-area",
          
          tabsetPanel(type = "tabs", id = "main_tabs",
                      
                      # ============================================================
                      # TAB 1: TURISMO — los datos de llegadas y por qué importan
                      # ============================================================
                      tabPanel("Turismo internacional",
                               
                               div(class = "lit-callout",
                                   span(class = "lit-badge", "Contexto"),
                                   tags$strong("¿Por qué mirar llegadas turísticas?"),
                                   tags$br(),
                                   "El turismo internacional es la variable dependiente central del proyecto. Japón y Corea del Sur
              son los dos países asiáticos con mayor densidad de investigación empírica sobre turismo inducido
              por el cine y con datos comparables disponibles ",
                                   span(class = "cita", "(Nakayama, 2021)"),
                                   ". Ambos países reportan sus cifras al UNWTO bajo criterios homogéneos, lo que hace posible
              la comparación panel en el mismo periodo. El comportamiento de las llegadas —su ritmo de crecimiento,
              sus caídas y su recuperación— es el fenómeno central que la investigación busca contextualizar
              respecto a la expansión audiovisual."
                               ),
                               
                               fluidRow(
                                 column(7, div(class = "card",
                                               div(class = "card-title", "Llegadas internacionales (millones)",
                                                   tags$span(class = "card-sub", "Fuente: UNWTO Tourism Statistics Database")),
                                               plotOutput("plot_arrivals", height = "300px"),
                                               div(class = "plot-caption", "Barras sombreadas: años COVID (2020-2021).")
                                 )),
                                 column(5, div(class = "card",
                                               div(class = "card-title", "Ingresos por turismo (M USD)",
                                                   tags$span(class = "card-sub", "Fuente: UNWTO")),
                                               plotOutput("plot_receipts", height = "300px")
                                 ))
                               ),
                               
                               fluidRow(
                                 column(6, div(class = "card",
                                               div(class = "card-title", "Crecimiento YoY de llegadas (%)"),
                                               plotOutput("growth_arrivals", height = "240px"),
                                               div(class = "covid-note", "* Colapso 2020-21 por restricciones de viaje globales (COVID-19)")
                                 )),
                                 column(6,
                                        div(class = "note-box",
                                            div(class = "nb-title", "Diferencias estructurales entre paises"),
                                            "Japón y Corea del Sur tienen magnitudes similares de llegadas pero trayectorias distintas.
                Japón exhibe un salto muy pronunciado post-2012 —asociado en la literatura a una depreciación
                agresiva del yen y a una política activa de atracción turística (\"Visit Japan Campaign\")—
                mientras Corea muestra un crecimiento más sostenido y lineal desde 2005.",
                                            tags$br(), tags$br(),
                                            "Esta diferencia estructural es relevante porque implica que las fuerzas que impulsan el turismo
                en cada país no son idénticas, lo que el diseño panel por efectos fijos busca controlar."
                                        ),
                                        div(class = "note-box",
                                            div(class = "nb-title", "Sobre el impacto COVID-19"),
                                            "El colapso de 2020-2021 es el shock externo más severo del periodo analizado.
                Japón pasó de 31.9 millones de llegadas (2019) a solo 246 mil en 2021.
                La recuperación post-pandemia es parte del periodo de análisis y se controla
                con una variable dummy en los modelos econométricos."
                                        )
                                 )
                               )
                      ),
                      
                      # ============================================================
                      # TAB 2: EXPORTACIONES AUDIOVISUALES — qué son y por qué crecen
                      # ============================================================
                      tabPanel("Exportaciones audiovisuales",
                               
                               div(class = "lit-callout",
                                   span(class = "lit-badge", "Economia Creativa"),
                                   tags$strong("El audiovisual como industria exportable"),
                                   tags$br(),
                                   "La economía creativa vincula industrias culturales con desarrollo, empleo y exportaciones.
              Para este proyecto, el audiovisual se conceptualiza como una ",
                                   tags$strong("industria creativa exportable"),
                                   " cuya presencia internacional puede generar externalidades económicas indirectas, siendo el turismo
              una de las más significativas. Los bienes y servicios audiovisuales operan como 'vectores' de
              exportación cultural: viajan con baja fricción, se consumen transnacionalmente y pueden producir
              cambios en percepciones y comportamientos ",
                                   span(class = "cita", "(UNCTAD, 2024; Hudson & Ritchie, 2006)"),
                                   ". Asia es la región de mayor crecimiento en comercio creativo global."
                               ),
                               
                               fluidRow(
                                 column(7, div(class = "card",
                                               div(class = "card-title", "Exportaciones audiovisuales (M USD)",
                                                   tags$span(class = "card-sub", "Fuentes: KOCCA (Corea) · JETRO / AJA (Japón)")),
                                               plotOutput("plot_av_exports", height = "300px")
                                 )),
                                 column(5, div(class = "card",
                                               div(class = "card-title", "Crecimiento acumulado 2005-2024",
                                                   tags$span(class = "card-sub", "Base 100 = 2005")),
                                               plotOutput("plot_av_indexed", height = "300px")
                                 ))
                               ),
                               
                               div(class = "note-box",
                                   div(class = "nb-title", "Importante: distincion de alcance en las cifras"),
                                   "Las cifras de Corea del Sur (~14,800 M USD en 2024) corresponden a la ",
                                   tags$strong("industria de contenidos audiovisuales"),
                                   " (cine, drama, animación, juegos, webtoons), según el seguimiento unificado de KOCCA.
              Las cifras de Japón (~3,600 M USD) reflejan principalmente ",
                                   tags$strong("anime y producciones cinematográficas"),
                                   " según JETRO/AJA, con medición más fragmentada entre agencias.
              Esta diferencia metodológica entre países es una limitación reconocida del panel y se documenta
              en el diccionario de variables del proyecto."
                               ),
                               
                               fluidRow(
                                 column(6, div(class = "card",
                                               div(class = "card-title", "Crecimiento YoY exportaciones AV (%)"),
                                               plotOutput("growth_av", height = "230px")
                                 )),
                                 column(6, div(class = "card",
                                               div(class = "card-title", "Distribucion de exportaciones AV"),
                                               plotOutput("box_av", height = "230px")
                                 ))
                               ),
                               
                               div(class = "study-row",
                                   div(class = "study-card", style = "border-top-color:#003478;",
                                       div(class = "sc-autor", "KOCCA (2019–2024)"),
                                       div(class = "sc-tipo", "Fuente primaria · Corea del Sur"),
                                       div(class = "sc-hallazgo",
                                           "Korea Creative Content Agency realiza seguimiento anual de toda la industria
                    de contenidos coreanos. Ofrece la serie temporal más completa y coherente
                    disponible para medir el Hallyu como fenómeno exportador cuantificable."),
                                       div(class = "sc-dato", style = "border-left-color:#003478;",
                                           "Corea del Sur: crecimiento de exportaciones AV de ~$620M (2005) a ~$14,800M (2024) = ×23.9")
                                   ),
                                   div(class = "study-card", style = "border-top-color:#bc002d;",
                                       div(class = "sc-autor", "AJA / JETRO (2024)"),
                                       div(class = "sc-tipo", "Fuente primaria · Japón"),
                                       div(class = "sc-hallazgo",
                                           "La Association of Japanese Animations y JETRO documentan el crecimiento del
                    anime como producto cultural exportable. El mercado global del anime superó
                    los $25B USD en 2022, siendo las exportaciones directas una fracción del impacto total."),
                                       div(class = "sc-dato", style = "border-left-color:#bc002d;",
                                           "Japón: crecimiento de exportaciones AV de ~$520M (2005) a ~$3,600M (2024) = ×6.9")
                                   )
                               )
                      ),
                      
                      # ============================================================
                      # TAB 3: HALLYU — el caso coreano y la evidencia empírica
                      # ============================================================
                      tabPanel("Hallyu y screen tourism",
                               
                               div(class = "lit-callout",
                                   span(class = "lit-badge", "Turismo inducido por el cine"),
                                   tags$strong("¿Qué es el screen tourism y por qué Corea del Sur es un caso central?"),
                                   tags$br(),
                                   "El turismo inducido por el cine se refiere a visitas turísticas que ocurren como resultado
              de que un destino haya sido presentado en televisión, cine o video ",
                                   span(class = "cita", "(Hudson & Ritchie, 2006)"),
                                   ". Los mecanismos típicos son: (a) aumento de visibilidad del destino,
              (b) construcción o refuerzo de la imagen del lugar, (c) activación de motivaciones
              emocionales, y (d) conversión en intención de visita. ",
                                   "La Ola Coreana (Hallyu) es uno de los casos más documentados de este fenómeno ",
                                   span(class = "cita", "(Bae et al., 2017; Lee & How, 2022)"),
                                   " porque ofrece una expansión cultural relativamente medible en el tiempo, con productos
              audiovisuales que generan audiencias transnacionales."
                               ),
                               
                               # Diagrama de mecanismo
                               div(class = "card",
                                   div(class = "card-title", "Mecanismo conceptual: del audiovisual al turismo"),
                                   div(class = "mec-flow",
                                       div(class = "mec-box highlight", tags$b("Exportacion"), tags$br(), "audiovisual"),
                                       div(class = "mec-arrow", "→"),
                                       div(class = "mec-box accent", "Exposicion", tags$br(), "cultural"),
                                       div(class = "mec-arrow", "→"),
                                       div(class = "mec-box", "Imagen", tags$br(), "del destino"),
                                       div(class = "mec-arrow", "→"),
                                       div(class = "mec-box", "Intencion", tags$br(), "de visita"),
                                       div(class = "mec-arrow", "→"),
                                       div(class = "mec-box highlight", tags$b("Llegadas"), tags$br(), "turisticas")
                                   ),
                                   tags$p(style = "font-size:11px; color:#888; margin-top:8px;",
                                          "Fuente: elaboracion propia con base en Beeton (2006), Hudson & Ritchie (2006), Hua et al. (2021), Ng & Chan (2020).")
                               ),
                               
                               fluidRow(
                                 column(6, div(class = "card",
                                               div(class = "card-title", "Llegadas vs Exportaciones AV — Corea del Sur",
                                                   tags$span(class = "card-sub", "Dispersion: cada punto es un año")),
                                               plotOutput("scatter_kr", height = "270px")
                                 )),
                                 column(6, div(class = "card",
                                               div(class = "card-title", "Llegadas vs Exportaciones AV — Japon",
                                                   tags$span(class = "card-sub", "Dispersion: cada punto es un año")),
                                               plotOutput("scatter_jp", height = "270px")
                                 ))
                               ),
                               
                               div(class = "study-row",
                                   div(class = "study-card", style = "border-top-color:#003478;",
                                       div(class = "sc-autor", "Bae et al. (2017)"),
                                       div(class = "sc-tipo", "Panel macroeconometrico · 1997-2014"),
                                       div(class = "sc-hallazgo",
                                           "Primera referencia metodológica directa del proyecto. Mediante panel de datos para
                    Corea del Sur probaron el efecto del Hallyu (medido como exportaciones de contenidos)
                    sobre llegadas turísticas, controlando PIB y tipo de cambio. Todos los efectos
                    resultaron significativos (p < 0.01)."),
                                       div(class = "sc-dato", style = "border-left-color:#003478;",
                                           "Diseño: efectos fijos y aleatorios · Variables: exportaciones Hallyu, GDP, tipo de cambio")
                                   ),
                                   div(class = "study-card", style = "border-top-color:#003478;",
                                       div(class = "sc-autor", "Lee & How (2022)"),
                                       div(class = "sc-tipo", "Econometrico · Datos recientes"),
                                       div(class = "sc-hallazgo",
                                           "Extiende Bae et al. hacia años más recientes, confirmando la significancia del
                    efecto Hallyu sobre llegadas internacionales a Corea del Sur controlando por
                    PIB y tipo de cambio. Es el complemento temporal más directo del diseño del proyecto."),
                                       div(class = "sc-dato", style = "border-left-color:#003478;",
                                           "Revista: Current Issues in Tourism · doi: 10.1080/13683500.2021.1876617")
                                   ),
                                   div(class = "study-card", style = "border-top-color:#7b5ea7;",
                                       div(class = "sc-autor", "Ng & Chan (2020)"),
                                       div(class = "sc-tipo", "Micro · Hong Kong · n = 220"),
                                       div(class = "sc-hallazgo",
                                           "Documentan que el consumo de dramas coreanos se asocia positivamente con
                    conocimiento, percepciones e intención de visitar Corea del Sur entre jóvenes
                    de Hong Kong. El efecto opera con un rezago de 6 a 18 meses entre la exposición
                    y la intención de visita."),
                                       div(class = "sc-dato", style = "border-left-color:#7b5ea7;",
                                           "Método: Spearman · Rezago empírico estimado: 6-18 meses")
                                   )
                               )
                      ),
                      
                      # ============================================================
                      # TAB 4: SOFT POWER — marco conceptual y datos de contexto
                      # ============================================================
                      tabPanel("Soft power cultural",
                               
                               div(class = "lit-callout",
                                   span(class = "lit-badge", "Marco Teorico"),
                                   tags$strong("El soft power como marco explicativo"),
                                   tags$br(),
                                   "Nye (2004) define el soft power como la capacidad de afectar a otros mediante atracción
              y persuasión más que por coerción o pago. En el contexto audiovisual, este concepto explica
              por qué un incremento en la exposición a contenidos de un país podría asociarse a cambios
              en intención de visita y, eventualmente, a flujos turísticos. Los productos audiovisuales
              pueden operar como instrumentos de ",
                                   tags$strong("diplomacia cultural"),
                                   " al normalizar símbolos, estilos de vida y narrativas nacionales, incrementando la afinidad
              y reduciendo barreras psicológicas al consumo o a la visita turística. ",
                                   span(class = "cita", "(Nye, 2004; Hudson & Ritchie, 2006)")
                               ),
                               
                               fluidRow(
                                 column(6, div(class = "card",
                                               div(class = "card-title", "Exportaciones AV como proxy de soft power",
                                                   tags$span(class = "card-sub", "Ambos paises, 2005-2024")),
                                               plotOutput("plot_av_both", height = "270px")
                                 )),
                                 column(6,
                                        div(class = "note-box",
                                            div(class = "nb-title", "Corea del Sur: Hallyu como estrategia deliberada"),
                                            "El caso coreano es uno de los más documentados de instrumentalización
                intencional del soft power. El gobierno coreano ha sostenido políticas
                activas de apoyo a la industria audiovisual desde los años 2000, con resultados
                medibles en presencia global (Netflix, festivales, K-pop).",
                                            tags$br(), tags$br(),
                                            "Esto hace al Hallyu especialmente útil como caso de estudio porque su
                expansión es relativamente ", tags$strong("medible y dateable"),
                                            " en el tiempo, a diferencia de influencias culturales más difusas."
                                        ),
                                        div(class = "note-box",
                                            div(class = "nb-title", "Japon: anime y Cool Japan"),
                                            "Japón ha desarrollado estrategias similares a través del anime, el cine de autor
                y la cultura pop —programa gubernamental 'Cool Japan'—, generando corrientes de
                visitantes motivados explícitamente por contenidos audiovisuales.
                Sin embargo, la medición de exportaciones es más fragmentada que en Corea,
                lo que introduce ruido en la comparación directa."
                                        )
                                 )
                               ),
                               
                               div(class = "study-row",
                                   div(class = "study-card", style = "border-top-color:#555;",
                                       div(class = "sc-autor", "Nye (2004)"),
                                       div(class = "sc-tipo", "Marco teorico · Relaciones internacionales"),
                                       div(class = "sc-hallazgo",
                                           "Conceptualiza el poder blando como la capacidad de atraer y persuadir a través
                    de cultura, valores e instituciones. Proporciona el marco teórico para interpretar
                    por qué el audiovisual puede tener efectos económicos indirectos como el turismo.")
                                   ),
                                   div(class = "study-card", style = "border-top-color:#555;",
                                       div(class = "sc-autor", "Beeton (2006)"),
                                       div(class = "sc-tipo", "Marco teorico · Screen tourism"),
                                       div(class = "sc-hallazgo",
                                           "Define y clasifica el turismo inducido por el cine. Advierte sobre el riesgo de
                    sobredimensionar el fenómeno con narrativas mediáticas y recomienda distinguir
                    correlaciones de efectos causales. Establece la necesidad de incorporar el papel
                    del marketing turístico que aprovecha la producción audiovisual.")
                                   ),
                                   div(class = "study-card", style = "border-top-color:#e8a020;",
                                       div(class = "sc-autor", "UNCTAD (2024)"),
                                       div(class = "sc-tipo", "Informe · Economia creativa global"),
                                       div(class = "sc-hallazgo",
                                           "Documenta que el comercio mundial de bienes y servicios creativos alcanzó
                    cifras históricas, con Asia como región de mayor crecimiento. Conceptualiza
                    la economía creativa como un sistema que integra dimensiones económicas,
                    culturales, tecnológicas y turísticas.")
                                   )
                               )
                      ),
                      
                      # ============================================================
                      # TAB 5: VARIABLES DE CONTROL — por qué se incluyen
                      # ============================================================
                      tabPanel("Variables de control",
                               
                               div(class = "lit-callout",
                                   span(class = "lit-badge", "Metodologia"),
                                   tags$strong("¿Por qué no basta con mirar exportaciones AV y turismo?"),
                                   tags$br(),
                                   "La relacion entre audiovisual y turismo no es directa ni automatica: depende de
              mediadores y de condicionantes que tambien explican la demanda turistica.
              La revision sistematica de ",
                                   span(class = "cita", "Montero-Diaz et al. (2024)"),
                                   " senala que los estudios del campo raramente integran variables estructurales
              como controles, lo que genera problemas de omision de variables y reduce la validez
              externa. El diseno del proyecto incorpora cuatro tipos de controles documentados
              en la literatura: PIB per capita, tipo de cambio real efectivo, conectividad aerea
              y estabilidad politica ",
                                   span(class = "cita", "(Bae et al., 2017; Lee & How, 2022)"),
                                   "."
                               ),
                               
                               fluidRow(
                                 column(6, div(class = "card",
                                               div(class = "card-title", "PIB per capita (USD)",
                                                   tags$span(class = "card-sub", "Fuente: World Bank WDI")),
                                               plotOutput("plot_gdp", height = "250px"),
                                               tags$p(style = "font-size:11px; color:#666; margin-top:6px;",
                                                      "Captura la capacidad de pago de los turistas emisores y el nivel de desarrollo del destino.
                     Su inclusion controla que el crecimiento turistico no sea solo consecuencia del crecimiento economico general.")
                                 )),
                                 column(6, div(class = "card",
                                               div(class = "card-title", "Tipo de cambio real efectivo (REER, 2010=100)",
                                                   tags$span(class = "card-sub", "Fuente: BIS Effective Exchange Rates")),
                                               plotOutput("plot_reer", height = "250px"),
                                               tags$p(style = "font-size:11px; color:#666; margin-top:6px;",
                                                      "Un REER bajo implica que el destino es relativamente barato para visitantes extranjeros.
                     Japón ha experimentado una depreciación sostenida del yen desde 2012 que podría
                     explicar parte del boom turístico independientemente del factor audiovisual.")
                                 ))
                               ),
                               
                               fluidRow(
                                 column(6, div(class = "card",
                                               div(class = "card-title", "Conectividad aerea (M asientos)",
                                                   tags$span(class = "card-sub", "Fuente: OAG Aviation Analytics")),
                                               plotOutput("plot_air", height = "250px"),
                                               tags$p(style = "font-size:11px; color:#666; margin-top:6px;",
                                                      "Sin disponibilidad de vuelos no puede materializarse la intención de visita.
                     La conectividad aérea es la variable estructural más relacionada con las
                     llegadas turísticas en el panel; su exclusión generaría omision de variable grave.")
                                 )),
                                 column(6, div(class = "card",
                                               div(class = "card-title", "Estabilidad politica WGI",
                                                   tags$span(class = "card-sub", "Fuente: World Bank Governance Indicators")),
                                               plotOutput("plot_wgi", height = "250px"),
                                               tags$p(style = "font-size:11px; color:#666; margin-top:6px;",
                                                      "El WGI 'Political Stability and Absence of Violence' es el indicador estructural
                     más utilizado en estudios comparativos de turismo internacional.
                     Japan mantiene valores altos y estables (~1.0) mientras Corea muestra mayor
                     variabilidad, reflejando episodios de tensión regional.")
                                 ))
                               ),
                               
                               # Tabla de fuentes
                               div(class = "card",
                                   div(class = "card-title", "Mapa de variables y fuentes del panel"),
                                   div(class = "fuente-grid",
                                       div(class = "fuente-item",
                                           div(class = "fi-sigla", "UNWTO"),
                                           div(class = "fi-desc", "UN World Tourism Organization"),
                                           div(class = "fi-var", "Llegadas · Ingresos por turismo")),
                                       div(class = "fuente-item",
                                           div(class = "fi-sigla", "KOCCA"),
                                           div(class = "fi-desc", "Korea Creative Content Agency"),
                                           div(class = "fi-var", "Exportaciones AV · Corea del Sur")),
                                       div(class = "fuente-item",
                                           div(class = "fi-sigla", "JETRO / AJA"),
                                           div(class = "fi-desc", "Japan External Trade Org · Anime Assoc."),
                                           div(class = "fi-var", "Exportaciones AV · Japón")),
                                       div(class = "fuente-item",
                                           div(class = "fi-sigla", "World Bank"),
                                           div(class = "fi-desc", "WDI & Governance Indicators"),
                                           div(class = "fi-var", "PIB pc · WGI Estabilidad política")),
                                       div(class = "fuente-item",
                                           div(class = "fi-sigla", "BIS"),
                                           div(class = "fi-desc", "Bank for International Settlements"),
                                           div(class = "fi-var", "REER – Tipo de cambio real efectivo")),
                                       div(class = "fuente-item",
                                           div(class = "fi-sigla", "OAG"),
                                           div(class = "fi-desc", "OAG Aviation Analytics"),
                                           div(class = "fi-var", "Asientos aéreos disponibles"))
                                   )
                               )
                      ),
                      
                      # ============================================================
                      # TAB 6: DATOS COMPLETOS
                      # ============================================================
                      tabPanel("Datos del panel",
                               br(),
                               div(class = "card",
                                   div(class = "card-title", "Panel de datos completo (N = 40)"),
                                   DTOutput("full_table")
                               )
                      )
                      
          ) # tabsetPanel
      ) # main-area
  ) # body-wrap
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  df <- reactive({
    d <- panel %>% filter(Year >= input$yr[1], Year <= input$yr[2])
    if (input$country_sel != "both") d <- d %>% filter(Country == input$country_sel)
    if (input$excl_covid) d <- d %>% filter(COVID == 0)
    d
  })
  
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
  
  sc <- function() scale_color_manual(values = c("Japan" = col_jp, "South Korea" = col_kr),
                                      labels = c("Japan" = "Japón", "South Korea" = "Corea del Sur"))
  sf <- function() scale_fill_manual(values  = c("Japan" = col_jp, "South Korea" = col_kr),
                                     labels = c("Japan" = "Japón", "South Korea" = "Corea del Sur"))
  
  # Función línea base con banda COVID
  make_line <- function(d, y_var, y_lab, show_covid_band = TRUE) {
    p <- ggplot(d, aes(x = Year, y = .data[[y_var]], color = Country)) +
      sc() + theme_eq3() +
      labs(x = NULL, y = y_lab) +
      scale_x_continuous(breaks = seq(2005, 2024, 3))
    
    if (show_covid_band) {
      p <- p +
        annotate("rect", xmin = 2019.5, xmax = 2021.5,
                 ymin = -Inf, ymax = Inf, fill = "#ffdddd", alpha = .35)
    }
    p + geom_line(linewidth = 1) + geom_point(size = 1.8)
  }
  
  # ── KPIs ──
  output$kpi_jp_arr  <- renderText({
    v <- japan %>% filter(Year == 2024); paste0(round(v$Arrivals_thousands/1000, 1), "M")
  })
  output$kpi_kr_arr  <- renderText({
    v <- korea %>% filter(Year == 2019); paste0(round(v$Arrivals_thousands/1000, 1), "M")
  })
  output$kpi_jp_av24 <- renderText({ paste0("$", format(japan$AV_Exports_M_USD[japan$Year==2024], big.mark=","), "M") })
  output$kpi_kr_av24 <- renderText({ paste0("$", format(korea$AV_Exports_M_USD[korea$Year==2024], big.mark=","), "M") })
  output$kpi_kr_crecav <- renderText({
    v0 <- korea$AV_Exports_M_USD[korea$Year==2005]
    v1 <- korea$AV_Exports_M_USD[korea$Year==2024]
    paste0("+", round((v1-v0)/v0*100, 0), "%")
  })
  
  # ── TAB 1: TURISMO ──
  output$plot_arrivals <- renderPlot({
    p <- make_line(df(), "Arrivals_M", "Millones de llegadas")
    if (input$use_log) p <- p + scale_y_log10()
    p
  })
  output$plot_receipts <- renderPlot({
    p <- make_line(df(), "Tourism_Receipts_M_USD", "M USD")
    if (input$use_log) p <- p + scale_y_log10()
    p + scale_y_continuous(labels = label_comma())
  })
  output$growth_arrivals <- renderPlot({
    d <- df() %>% filter(!is.na(Arrivals_Growth_pct))
    ggplot(d, aes(x = Year, y = Arrivals_Growth_pct, fill = Country)) +
      geom_col(position = "dodge", width = 0.7) +
      sf() + theme_eq3() +
      geom_hline(yintercept = 0, linewidth = 0.4) +
      scale_x_continuous(breaks = seq(2006, 2024, 3)) +
      labs(x = NULL, y = "%")
  })
  
  # ── TAB 2: EXPORTACIONES AV ──
  output$plot_av_exports <- renderPlot({
    p <- make_line(df(), "AV_Exports_M_USD", "M USD")
    if (input$use_log) p <- p + scale_y_log10()
    p + scale_y_continuous(labels = label_comma())
  })
  output$plot_av_indexed <- renderPlot({
    d <- df() %>%
      group_by(Country) %>%
      mutate(AV_idx = AV_Exports_M_USD / AV_Exports_M_USD[Year == min(Year[Year >= input$yr[1]])] * 100) %>%
      ungroup()
    ggplot(d, aes(x = Year, y = AV_idx, color = Country)) +
      annotate("rect", xmin = 2019.5, xmax = 2021.5,
               ymin = -Inf, ymax = Inf, fill = "#ffdddd", alpha = .35) +
      geom_line(linewidth = 1) + geom_point(size = 1.8) +
      geom_hline(yintercept = 100, linetype = "dashed", color = "#aaa") +
      sc() + theme_eq3() +
      scale_x_continuous(breaks = seq(2005, 2024, 3)) +
      labs(x = NULL, y = "Indice (base = año inicial)")
  })
  output$growth_av <- renderPlot({
    d <- df() %>% filter(!is.na(AV_Growth_pct))
    ggplot(d, aes(x = Year, y = AV_Growth_pct, fill = Country)) +
      geom_col(position = "dodge", width = 0.7) +
      sf() + theme_eq3() +
      geom_hline(yintercept = 0, linewidth = 0.4) +
      scale_x_continuous(breaks = seq(2006, 2024, 3)) +
      labs(x = NULL, y = "%")
  })
  output$box_av <- renderPlot({
    ggplot(df(), aes(x = Country, y = AV_Exports_M_USD, fill = Country)) +
      geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
      sf() + theme_eq3() + labs(x = NULL, y = "M USD") +
      scale_x_discrete(labels = c("Japan" = "Japón", "South Korea" = "Corea del Sur")) +
      theme(legend.position = "none")
  })
  
  # ── TAB 3: HALLYU ──
  output$scatter_kr <- renderPlot({
    d <- panel %>%
      filter(Country == "South Korea",
             Year >= input$yr[1], Year <= input$yr[2])
    if (input$excl_covid) d <- d %>% filter(COVID == 0)
    ggplot(d, aes(x = AV_Exports_M_USD, y = Arrivals_M,
                  color = factor(COVID), label = Year)) +
      geom_point(size = 3) +
      geom_text(size = 2.8, nudge_y = 0.4, color = "#333") +
      scale_color_manual(values = c("0" = col_kr, "1" = "#cc6666"),
                         labels = c("0" = "Normal", "1" = "COVID")) +
      theme_eq3() +
      labs(x = "Export. AV (M USD)", y = "Llegadas (M)", color = NULL) +
      scale_x_continuous(labels = label_comma())
  })
  output$scatter_jp <- renderPlot({
    d <- panel %>%
      filter(Country == "Japan",
             Year >= input$yr[1], Year <= input$yr[2])
    if (input$excl_covid) d <- d %>% filter(COVID == 0)
    ggplot(d, aes(x = AV_Exports_M_USD, y = Arrivals_M,
                  color = factor(COVID), label = Year)) +
      geom_point(size = 3) +
      geom_text(size = 2.8, nudge_y = 0.5, color = "#333") +
      scale_color_manual(values = c("0" = col_jp, "1" = "#cc6666"),
                         labels = c("0" = "Normal", "1" = "COVID")) +
      theme_eq3() +
      labs(x = "Export. AV (M USD)", y = "Llegadas (M)", color = NULL) +
      scale_x_continuous(labels = label_comma())
  })
  
  # ── TAB 4: SOFT POWER ──
  output$plot_av_both <- renderPlot({
    p <- make_line(df(), "AV_Exports_M_USD", "M USD")
    p + scale_y_continuous(labels = label_comma()) +
      labs(caption = "Exportaciones audiovisuales como proxy de presencia cultural internacional")
  })
  
  # ── TAB 5: CONTROLES ──
  output$plot_gdp <- renderPlot({
    make_line(df(), "GDP_pc_USD", "USD") +
      scale_y_continuous(labels = label_comma())
  })
  output$plot_reer <- renderPlot({
    make_line(df(), "REER_index", "Indice (2010=100)") +
      geom_hline(yintercept = 100, linetype = "dashed", color = "#aaa", linewidth = .7)
  })
  output$plot_air <- renderPlot({
    make_line(df(), "Air_Seats_M", "Millones de asientos")
  })
  output$plot_wgi <- renderPlot({
    make_line(df(), "WGI_PolStab", "Puntuacion WGI (-2.5 a +2.5)") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#aaa", linewidth = .7)
  })
  
  # ── TAB 6: DATOS ──
  output$full_table <- renderDT({
    df() %>%
      select(Country, Year, Arrivals_thousands, Tourism_Receipts_M_USD,
             AV_Exports_M_USD, GDP_pc_USD, REER_index, Air_Seats_M,
             WGI_PolStab, COVID) %>%
      rename(
        Pais = Country, Anio = Year,
        `Llegadas (miles)` = Arrivals_thousands,
        `Ingresos (M USD)` = Tourism_Receipts_M_USD,
        `Export. AV (M USD)` = AV_Exports_M_USD,
        `PIB pc (USD)` = GDP_pc_USD,
        REER = REER_index,
        `Asientos aereos (M)` = Air_Seats_M,
        `WGI PolStab` = WGI_PolStab,
        COVID = COVID
      ) %>%
      arrange(Pais, Anio)
  }, options = list(
    pageLength = 15, dom = "ftp",
    language = list(search = "Buscar:",
                    paginate = list(previous = "Anterior", `next` = "Siguiente")),
    columnDefs = list(list(className = "dt-center", targets = "_all"))
  ), rownames = FALSE)
  
}

shinyApp(ui, server)
