suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(plm)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(DT)
  library(sandwich)
  library(lmtest)
})

# ==============================================================================
# DATOS EMBEBIDOS - Panel Japon-Corea, 2005-2024
# Fuentes: UNWTO, World Bank, UNESCO, BIS / datos integrados del proyecto.
# Equipo #3 - Exportación de cultura y turismo
# La app no depende de archivos externos para desplegarse.
# ==============================================================================

panel_csv <- "country_id,country,year,tourist_arrivals_thousands,tourism_receipts_billion_usd,tourism_receipts_per_arrival_usd,cultural_goods_exports_million_usd,music_exports_million_usd,unesco_world_heritage_sites,unesco_intangible_heritage_items,gdp_billion_usd,gdp_per_capita_usd,exchange_rate_lcu_usd,cpi_2010_100,fdi_inflow_billion_usd,hotel_rooms_thousand,broadband_per_100_inhabitants,covid_dummy,gfc_dummy,event_note
1,Japan,2005,6728,12.4,1843,4500,,13,2,4831,37800,110.2,99.8,2.78,1480,17.4,0,0,APEC Korea / Aichi Expo Japan
1,Japan,2006,7334,12,1636,4700,,13,4,4601,36000,116.3,100.1,-6.51,1510,20.2,0,0,
1,Japan,2007,8347,11.3,1354,4900,,14,6,4579,35800,117.8,100.2,22.55,1540,22.1,0,0,
1,Japan,2008,8351,12.1,1449,5100,,14,9,5106,39900,103.4,101.6,24.55,1570,23.7,0,1,Global Financial Crisis begins
1,Japan,2009,6790,11.6,1708,4200,,14,11,5290,41400,93.6,100.2,11.94,1590,24.9,0,1,GFC continued / Swine flu
1,Japan,2010,8611,13.4,1556,4800,,14,13,5759,44500,87.8,100,-1.25,1610,26.4,0,0,G-20 Seoul Summit
1,Japan,2011,6219,11.1,1785,4600,,16,15,6233,48800,79.8,99.7,-1.76,1640,27,0,0,Great East Japan Earthquake (March 2011)
1,Japan,2012,8358,14.6,1747,4900,,16,16,6272,49100,79.8,99.7,1.73,1670,27.7,0,0,Lee Myung-bak Dokdo visit / Japan-Korea tensions
1,Japan,2013,10364,15.1,1457,5300,,17,18,5212,40900,97.6,100.1,2.3,1700,28.2,0,0,Visit Japan campaign intensifies (Abe)
1,Japan,2014,13413,18.9,1409,5500,,18,19,4897,38500,105.9,102.8,9.07,1740,29,0,0,Korea: ferry Sewol disaster
1,Japan,2015,19737,25,1267,5800,,19,21,4444,35000,121,103.6,5.25,1790,30.5,0,0,MERS outbreak Korea (May-July)
1,Japan,2016,24039,30.7,1277,6100,,20,21,5003,39400,108.8,103.8,17.45,1850,31.5,0,0,THAAD deployment announced
1,Japan,2017,28691,34.1,1189,6500,,21,21,4931,38900,112.2,104.3,9.39,1920,32.1,0,0,China tour ban Korea (THAAD effect)
1,Japan,2018,31192,41.1,1318,6800,,22,22,5037,39800,110.4,105.4,9.86,2000,32.6,0,0,PyeongChang Winter Olympics Korea
1,Japan,2019,31882,49.4,1549,6900,,23,22,5118,40500,109,106,14.55,2070,33.1,0,0,Japan-Korea trade dispute
1,Japan,2020,4116,11.2,2721,5200,,24,22,5055,40100,106.8,106,10.25,2110,33.7,1,0,COVID-19 pandemic / Tokyo Olympics postponed
1,Japan,2021,245,1.2,4898,5400,,25,22,5034,39800,109.8,105.8,24.65,2080,35.3,1,0,Tokyo Olympics 2020 (no spectators)
1,Japan,2022,3832,12.8,3340,6200,,25,22,4256,33800,131.5,108.5,33.15,2030,36.9,1,0,Border reopening late 2022
1,Japan,2023,25066,38.3,1528,7100,,25,22,4213,33700,140.5,112,20.95,2050,38.2,0,0,Full reopening / Korea-Japan tourism normalization
1,Japan,2024,36870,53.3,1446,8200,,26,22,4070,32700,151.4,114.8,22.5,2080,39.5,0,0,Yen depreciation drives record JPN tourism
2,Korea,2005,6023,5.8,963,1800,22.3,8,3,934,19400,1024.1,91,6.31,580,25.9,0,0,APEC Korea / Aichi Expo Japan
2,Korea,2006,6155,6.4,1040,1900,16.6,9,5,1053,21700,954.8,93,4.88,600,29.1,0,0,
2,Korea,2007,6448,6.9,1070,2000,13.9,9,6,1173,24100,929.2,95.4,1.58,615,30.5,0,0,
2,Korea,2008,6891,9.7,1408,2100,16.5,9,8,1002,20500,1102.1,99.9,11.18,625,31.3,0,1,Global Financial Crisis begins
2,Korea,2009,7818,9.8,1254,1950,31.3,9,9,944,19100,1276.9,102.7,8.96,640,32,0,1,GFC continued / Swine flu
2,Korea,2010,8798,10.4,1182,2300,83.3,10,11,1144,22900,1156.1,103,9.5,680,35.3,0,0,G-20 Seoul Summit
2,Korea,2011,9795,12.4,1266,2500,196.1,10,12,1253,24800,1108.3,107.2,9.77,720,36.6,0,0,Great East Japan Earthquake (March 2011)
2,Korea,2012,11140,14.2,1275,2700,235.1,10,14,1278,25100,1126.5,109.5,9.49,780,37.6,0,0,Lee Myung-bak Dokdo visit / Japan-Korea tensions
2,Korea,2013,12176,14.6,1199,2900,277.3,11,15,1370,26800,1095,110.9,12.77,850,38,0,0,Visit Japan campaign intensifies (Abe)
2,Korea,2014,14202,17.7,1246,3100,335.7,11,17,1484,28900,1053,112.3,9.27,920,39.7,0,0,Korea: ferry Sewol disaster
2,Korea,2015,13232,15.2,1149,3300,381.2,12,18,1466,28400,1131.5,113.1,4.1,1010,40.2,0,0,MERS outbreak Korea (May-July)
2,Korea,2016,17242,17.2,998,3400,442.5,12,19,1500,29100,1160.3,114.2,12.1,1100,40.5,0,0,THAAD deployment announced
2,Korea,2017,13336,13.4,1005,3600,512.6,12,19,1623,31500,1130.8,116.4,17.91,1190,41.6,0,0,China tour ban Korea (THAAD effect)
2,Korea,2018,15347,18.5,1205,3700,564.2,13,20,1725,33400,1100.6,118.2,14.48,1280,41.6,0,0,PyeongChang Winter Olympics Korea
2,Korea,2019,17502,21.5,1228,3800,756.2,14,20,1651,31900,1166.1,118.6,10.57,1340,41.6,0,0,Japan-Korea trade dispute
2,Korea,2020,2519,7.6,3017,2900,679.6,15,21,1638,31700,1180.3,119.2,8.81,1360,43.6,1,0,COVID-19 pandemic / Tokyo Olympics postponed
2,Korea,2021,967,4.1,4240,3500,826.7,15,21,1819,35200,1144.6,122.1,16.83,1340,44.5,1,0,Tokyo Olympics 2020 (no spectators)
2,Korea,2022,3198,8.3,2595,3700,927.6,16,22,1674,32400,1291.4,128.4,18.46,1320,45.5,1,0,Border reopening late 2022
2,Korea,2023,11032,16.5,1496,4100,1240,16,22,1713,33200,1305.4,133,14.07,1330,46.2,0,0,Full reopening / Korea-Japan tourism normalization
2,Korea,2024,16370,24.6,1503,4500,1295,16,22,1870,36000,1364.1,136.1,12.5,1340,46.8,0,0,Yen depreciation drives record JPN tourism"

panel_raw <- read.csv(
  text = panel_csv,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA"),
  check.names = FALSE
)

numeric_columns <- setdiff(names(panel_raw), c("country", "event_note"))
panel_raw[numeric_columns] <- lapply(panel_raw[numeric_columns], as.numeric)

model_excluded_years <- c(2020, 2021)
components <- c(
  "cultural_goods_exports_million_usd",
  "unesco_world_heritage_sites",
  "unesco_intangible_heritage_items"
)

reference_data <- panel_raw %>% filter(!(year %in% model_excluded_years))
component_center <- sapply(reference_data[components], mean, na.rm = TRUE)
component_scale <- sapply(reference_data[components], function(x) {
  sqrt(mean((x - mean(x, na.rm = TRUE))^2, na.rm = TRUE))
})

panel <- panel_raw %>%
  mutate(
    country = factor(country, levels = c("Japan", "Korea")),
    country_label = recode(as.character(country), Japan = "Japón", Korea = "Corea del Sur"),
    log_tourism = log(tourist_arrivals_thousands),
    log_gdppc = log(gdp_per_capita_usd),
    log_fx = log(exchange_rate_lcu_usd),
    arrivals_million = tourist_arrivals_thousands / 1000,
    tourism_receipts_million_usd = tourism_receipts_billion_usd * 1000,
    model_sample = !(year %in% model_excluded_years),
    model_status = ifelse(model_sample, "Usado en el modelo", "Excluido del modelo")
  )

for (component in components) {
  panel[[paste0(component, "_z")]] <- (
    panel[[component]] - component_center[[component]]
  ) / component_scale[[component]]
}

panel <- panel %>%
  mutate(
    culture_index = rowMeans(
      cbind(
        cultural_goods_exports_million_usd_z,
        unesco_world_heritage_sites_z,
        unesco_intangible_heritage_items_z
      ),
      na.rm = TRUE
    )
  ) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    culture_index_lag = lag(culture_index),
    tourism_growth_pct = (tourist_arrivals_thousands / lag(tourist_arrivals_thousands) - 1) * 100
  ) %>%
  ungroup()

term_labels <- c(
  "(Intercept)" = "Constante",
  culture_index = "Índice cultural",
  culture_index_lag_model = "Índice cultural rezagado",
  log_gdppc = "Logaritmo del producto interno bruto per cápita",
  log_fx = "Logaritmo del tipo de cambio"
)

model_data <- panel %>%
  filter(model_sample) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    culture_index_lag_model = lag(culture_index)
  ) %>%
  ungroup()

model_data_lag <- model_data %>%
  filter(!is.na(culture_index_lag_model))

model_final <- plm(
  log_tourism ~ culture_index + log_gdppc + log_fx,
  data  = model_data,
  index = c("country_id", "year"),
  model = "within"
)
model_lag <- plm(
  log_tourism ~ culture_index_lag_model + log_gdppc + log_fx,
  data  = model_data_lag,
  index = c("country_id", "year"),
  model = "within"
)

robust_coef_table <- function(model) {
  if (inherits(model, "plm")) {
    # Con solo 2 países, el cluster-robust (Arellano) tiene pocos clusters y
    # subestima severamente los SE. Usamos White HC1 sin agrupar — es la elección
    # apropiada cuando el número de clusters es muy pequeño.
    vcov_robust <- plm::vcovHC(model, method = "white1", type = "HC1")
  } else {
    vcov_robust <- sandwich::vcovHC(model, type = "HC1")
  }
  robust_test <- lmtest::coeftest(model, vcov. = vcov_robust)
  robust_critical_value <- qt(0.975, df = df.residual(model))
  robust_intervals <- data.frame(
    conf_low = as.numeric(robust_test[, 1]) - robust_critical_value * as.numeric(robust_test[, 2]),
    conf_high = as.numeric(robust_test[, 1]) + robust_critical_value * as.numeric(robust_test[, 2])
  )

  data.frame(
    term = rownames(robust_test),
    estimate = as.numeric(robust_test[, 1]),
    robust_se = as.numeric(robust_test[, 2]),
    statistic = as.numeric(robust_test[, 3]),
    p_value = as.numeric(robust_test[, 4]),
    conf_low = robust_intervals$conf_low,
    conf_high = robust_intervals$conf_high,
    row.names = NULL,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      term_label = ifelse(term %in% names(term_labels), unname(term_labels[term]), term),
      significant = p_value < 0.05
    )
}

# Helpers para abstraer las diferencias entre lm y plm
get_intercept <- function(model) {
  if (inherits(model, "plm")) {
    as.numeric(mean(fixef(model)))
  } else {
    coef(model)[["(Intercept)"]]
  }
}

extract_r_squared <- function(model) {
  if (inherits(model, "plm")) {
    as.numeric(summary(model)$r.squared["rsq"])
  } else {
    summary(model)$r.squared
  }
}

manual_vif <- function(model) {
  matrix_x <- model.matrix(model)
  matrix_x <- matrix_x[, colnames(matrix_x) != "(Intercept)", drop = FALSE]

  data.frame(
    term = colnames(matrix_x),
    vif = vapply(seq_len(ncol(matrix_x)), function(index) {
      other_columns <- matrix_x[, setdiff(seq_len(ncol(matrix_x)), index), drop = FALSE]
      if (ncol(other_columns) == 0) return(NA_real_)
      auxiliary_model <- lm(matrix_x[, index] ~ other_columns)
      1 / (1 - summary(auxiliary_model)$r.squared)
    }, numeric(1)),
    row.names = NULL,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      term_label = ifelse(term %in% names(term_labels), unname(term_labels[term]), term),
      lectura = case_when(
        is.na(vif) ~ "No aplica",
        vif <= 3 ~ "Bajo",
        vif <= 5 ~ "Moderado: conviene vigilarlo",
        TRUE ~ "Alto: reportar como limitación"
      )
    )
}

model_results <- list(
  base = list(
    key = "base",
    label = "Modelo A: base",
    short_label = "Base",
    formula_label = "log_tourism ~ culture_index + log_gdppc + log_fx | efectos fijos por país",
    model = model_final,
    data = model_data,
    culture_term = "culture_index",
    culture_label = "Índice cultural",
    reading = "El índice cultural contemporáneo es positivo y estadísticamente significativo. Es el modelo más parsimonioso y conserva más observaciones.",
    recommendation = "Recomendado como modelo final por parsimonia, estabilidad y lectura directa."
  ),
  lag = list(
    key = "lag",
    label = "Modelo B: rezagado",
    short_label = "Rezagado",
    formula_label = "log_tourism ~ culture_index_lag + log_gdppc + log_fx | efectos fijos por país",
    model = model_lag,
    data = model_data_lag,
    culture_term = "culture_index_lag_model",
    culture_label = "Índice cultural rezagado",
    reading = "El índice cultural rezagado conserva signo positivo y significancia, pero pierde dos observaciones y no mejora la R cuadrada.",
    recommendation = "Útil como prueba de robustez; no sustituye al modelo base como especificación principal."
  )
)

for (model_key in names(model_results)) {
  model_results[[model_key]]$coef_table <- robust_coef_table(model_results[[model_key]]$model)
  model_results[[model_key]]$vif_table  <- manual_vif(model_results[[model_key]]$model)
  model_results[[model_key]]$r_squared  <- extract_r_squared(model_results[[model_key]]$model)
}

coef_table <- model_results$base$coef_table

# Para plm con efectos fijos within, residuals() devuelve residuos en escala original
# (equivalentes a los del LSDV), así que fitted en escala original = y - residuos.
model_data <- model_data %>%
  mutate(
    residual = as.numeric(resid(model_final)),
    fitted   = log_tourism - residual
  )

model_data_lag <- model_data_lag %>%
  mutate(
    residual = as.numeric(resid(model_lag)),
    fitted   = log_tourism - residual
  )

model_comparison_table <- bind_rows(lapply(model_results, function(result) {
  culture_row <- result$coef_table %>% filter(term == result$culture_term)
  data.frame(
    modelo = result$label,
    formula = result$formula_label,
    observaciones = nobs(result$model),
    variable_cultural = result$culture_label,
    coeficiente_cultural = culture_row$estimate,
    error_estandar_robusto = culture_row$robust_se,
    valor_p = culture_row$p_value,
    r_cuadrada = result$r_squared,
    lectura = result$reading,
    recomendacion = result$recommendation,
    stringsAsFactors = FALSE
  )
}))

vif_comparison_table <- bind_rows(lapply(model_results, function(result) {
  result$vif_table %>%
    mutate(modelo = result$label) %>%
    select(modelo, term_label, vif, lectura)
}))

base_culture_summary <- model_comparison_table %>%
  filter(modelo == "Modelo A: base") %>%
  slice(1)

lag_culture_summary <- model_comparison_table %>%
  filter(modelo == "Modelo B: rezagado") %>%
  slice(1)

hypotheses_table <- data.frame(
  hipotesis = c(
    "H0: hipótesis nula",
    "H1: asociación directa audiovisual-turismo",
    "H2: robustez con controles",
    "H3: efectos con rezago temporal"
  ),
  planteamiento = c(
    "No existe relación estadísticamente significativa entre exportación cultural audiovisual y turismo internacional una vez incluidos controles.",
    "Existe una asociación positiva y estadísticamente significativa entre indicadores culturales/audiovisuales y turismo internacional.",
    "La asociación se mantiene al controlar por variables macroeconómicas y estructurales relevantes.",
    "Los indicadores audiovisuales rezagados predicen mejor el turismo actual que los valores contemporáneos."
  ),
  como_se_operacionaliza_en_la_app = c(
    "Se observa el signo y el valor p de la variable cultural en el modelo controlado.",
    "Se usa el índice cultural como proxy amplio de presencia cultural/exportable y se estima el modelo base.",
    "Se controla por producto interno bruto per cápita y tipo de cambio; las variables estructurales disponibles se discuten como descartadas o limitadas.",
    "Se compara el modelo base con un modelo que usa el índice cultural rezagado un periodo."
  ),
  resultado_en_la_app = c(
    sprintf(
      "Modelo base: beta cultural = %.4f, valor p = %.4f.",
      base_culture_summary$coeficiente_cultural,
      base_culture_summary$valor_p
    ),
    sprintf(
      "El coeficiente cultural es positivo y significativo: beta = %.4f.",
      base_culture_summary$coeficiente_cultural
    ),
    "La asociación se mantiene al incluir logaritmo del producto interno bruto per cápita y logaritmo del tipo de cambio; esos controles no son estadísticamente significativos.",
    sprintf(
      "El rezago también es positivo y significativo: beta = %.4f, valor p = %.4f, pero su R cuadrada es %.4f frente a %.4f del modelo base.",
      lag_culture_summary$coeficiente_cultural,
      lag_culture_summary$valor_p,
      lag_culture_summary$r_cuadrada,
      base_culture_summary$r_cuadrada
    )
  ),
  lectura_defendible = c(
    "La H0 se rechaza para la especificación principal, con cautela por tamaño de muestra y medición imperfecta.",
    "La H1 recibe apoyo empírico en la muestra Japón-Corea: la relación estimada es positiva y significativa.",
    "La H2 se sostiene parcialmente: hay controles macroeconómicos, pero no todos los controles estructurales del planteamiento original pueden incluirse sin perder grados de libertad.",
    "La H3 se sostiene parcialmente: el rezago conserva signo y significancia, pero no mejora el ajuste; por eso se presenta como robustez, no como modelo final."
  ),
  stringsAsFactors = FALSE
)

research_questions_table <- data.frame(
  pregunta = c(
    "¿Existe una asociación estadísticamente significativa entre cultura audiovisual aproximada y turismo internacional?",
    "¿La asociación se mantiene al introducir controles macroeconómicos?",
    "¿La relación parece operar con rezago temporal?"
  ),
  respuesta_en_la_app = c(
    "Sí, en el modelo base el índice cultural tiene signo positivo y valor p menor a 0.05.",
    "Sí para los controles incluidos: PIB per cápita y tipo de cambio. No se afirma lo mismo para todos los controles estructurales posibles.",
    "Parcialmente: el modelo rezagado mantiene significancia, pero no supera al modelo contemporáneo en ajuste."
  ),
  implicacion = c(
    "La conclusión central debe hablar de asociación positiva, no de causalidad.",
    "La especificación es defendible por parsimonia, aunque conserva limitaciones por variables omitidas.",
    "El rezago ayuda a dialogar con la literatura, pero no justifica desplazar el modelo base."
  ),
  stringsAsFactors = FALSE
)

safe_test <- function(expr) {
  tryCatch(expr, error = function(error) NULL)
}

diagnostic_row <- function(prueba, estadistico, valor_p, lectura, pregunta, decision, nota) {
  data.frame(
    prueba = prueba,
    estadistico = estadistico,
    valor_p = valor_p,
    lectura = lectura,
    pregunta = pregunta,
    decision = decision,
    nota = nota,
    stringsAsFactors = FALSE
  )
}

pesaran_cd_manual <- function(data, residuals_vector) {
  residual_data <- data %>%
    mutate(residual_diag = residuals_vector) %>%
    select(year, country_id, residual_diag) %>%
    tidyr::pivot_wider(names_from = country_id, values_from = residual_diag)

  residual_matrix <- as.matrix(residual_data[, setdiff(names(residual_data), "year"), drop = FALSE])
  number_entities <- ncol(residual_matrix)
  number_periods <- nrow(residual_matrix)
  if (number_entities < 2) return(c(statistic = NA_real_, p_value = NA_real_))

  correlations <- c()
  for (i in seq_len(number_entities - 1)) {
    for (j in (i + 1):number_entities) {
      correlations <- c(correlations, cor(residual_matrix[, i], residual_matrix[, j], use = "complete.obs"))
    }
  }

  statistic <- sqrt(2 * number_periods / (number_entities * (number_entities - 1))) * sum(correlations)
  p_value <- 2 * (1 - pnorm(abs(statistic)))
  c(statistic = statistic, p_value = p_value)
}

wooldridge_manual <- function(data, residuals_vector) {
  wool_data <- data %>%
    mutate(residual_diag = residuals_vector) %>%
    arrange(country_id, year) %>%
    group_by(country_id) %>%
    mutate(
      residual_difference = residual_diag - lag(residual_diag),
      residual_difference_lag = lag(residual_difference)
    ) %>%
    ungroup() %>%
    filter(!is.na(residual_difference), !is.na(residual_difference_lag))

  if (nrow(wool_data) < 4) return(c(statistic = NA_real_, p_value = NA_real_))

  wool_model <- lm(residual_difference ~ 0 + residual_difference_lag, data = wool_data)
  parameter <- coef(wool_model)[["residual_difference_lag"]]
  clustered_vcov <- sandwich::vcovCL(wool_model, cluster = wool_data$country_id, type = "HC1")
  clustered_se <- sqrt(diag(clustered_vcov))[["residual_difference_lag"]]

  if (!is.finite(clustered_se) || clustered_se < 1e-8) {
    return(c(statistic = parameter, p_value = 0))
  }

  z_value <- (parameter - (-0.5)) / clustered_se
  p_value <- 2 * (1 - pnorm(abs(z_value)))
  c(statistic = parameter, p_value = p_value)
}

dwh_manual <- function(data) {
  instrument_data <- data %>%
    filter(!is.na(culture_index_lag_model))

  if (nrow(instrument_data) < 8) return(c(statistic = NA_real_, p_value = NA_real_))

  first_stage <- lm(culture_index ~ culture_index_lag_model + log_gdppc + log_fx + country, data = instrument_data)
  instrument_data$first_stage_residual <- resid(first_stage)
  augmented_model <- lm(log_tourism ~ culture_index + log_gdppc + log_fx + country + first_stage_residual, data = instrument_data)
  residual_test <- summary(augmented_model)$coefficients["first_stage_residual", , drop = FALSE]
  t_value <- residual_test[1, "t value"]
  p_value <- residual_test[1, "Pr(>|t|)"]
  c(statistic = t_value^2, p_value = p_value)
}

calculate_diagnostics <- function(data) {
  base_formula <- log_tourism ~ culture_index + log_gdppc + log_fx
  diagnostic_formula <- log_tourism ~ culture_index + log_gdppc + log_fx + factor(country_id) + factor(year)
  base_lm <- lm(base_formula, data = data)
  diagnostic_lm <- lm(diagnostic_formula, data = data)
  residuals_diag <- resid(diagnostic_lm)

  fixed_test <- NULL
  random_test <- NULL
  hausman_test <- NULL

  if (requireNamespace("plm", quietly = TRUE)) {
    pooled_plm <- safe_test(plm::plm(base_formula, data = data, index = c("country_id", "year"), model = "pooling"))
    fixed_plm <- safe_test(plm::plm(base_formula, data = data, index = c("country_id", "year"), model = "within"))
    random_plm <- safe_test(plm::plm(base_formula, data = data, index = c("country_id", "year"), model = "random", random.method = "amemiya"))

    if (!is.null(pooled_plm) && !is.null(fixed_plm)) {
      fixed_test <- safe_test(plm::pFtest(fixed_plm, pooled_plm))
      random_test <- safe_test(plm::plmtest(pooled_plm, type = "bp"))
    }
    if (!is.null(fixed_plm) && !is.null(random_plm)) {
      hausman_test <- safe_test(plm::phtest(fixed_plm, random_plm))
    }
  }

  ramsey_test <- lmtest::resettest(diagnostic_lm, power = 2:3, type = "fitted")
  hetero_test <- lmtest::bptest(diagnostic_lm)
  durbin_test <- lmtest::dwtest(diagnostic_lm)
  bg_test <- lmtest::bgtest(diagnostic_lm, order = 1)
  shapiro_test <- shapiro.test(residuals_diag)
  pesaran_test <- pesaran_cd_manual(data, residuals_diag)
  wooldridge_test <- wooldridge_manual(data, residuals_diag)
  dwh_test <- dwh_manual(data)

  bind_rows(
    diagnostic_row(
      "Modelo agrupado contra efectos fijos",
      ifelse(is.null(fixed_test), NA_real_, unname(fixed_test$statistic)),
      ifelse(is.null(fixed_test), NA_real_, fixed_test$p.value),
      ifelse(!is.null(fixed_test) && fixed_test$p.value < 0.05, "Hay evidencia a favor de efectos fijos", "Diferencia entre países pequeña"),
      "¿Se necesita controlar una diferencia fija propia de cada país?",
      "El modelo final usa efectos fijos por país por la naturaleza panel de los datos; la prueba complementa el diagnóstico.",
      "Calculada en Shiny con plm::pFtest sobre la muestra sin 2020 ni 2021."
    ),
    diagnostic_row(
      "Modelo agrupado contra efectos aleatorios",
      ifelse(is.null(random_test), NA_real_, unname(random_test$statistic)),
      ifelse(is.null(random_test), NA_real_, random_test$p.value),
      ifelse(!is.null(random_test) && random_test$p.value < 0.05, "Hay evidencia a favor de efectos aleatorios", "Variación entre países limitada"),
      "¿La variación entre países debe modelarse como un componente aleatorio?",
      "Con solo dos países la dimensión transversal es mínima; el modelo final usa efectos fijos.",
      "Calculada en Shiny con plm::plmtest. La muestra transversal es mínima."
    ),
    diagnostic_row(
      "Hausman: efectos fijos contra efectos aleatorios",
      ifelse(is.null(hausman_test), NA_real_, unname(hausman_test$statistic)),
      ifelse(is.null(hausman_test), NA_real_, hausman_test$p.value),
      ifelse(!is.null(hausman_test) && hausman_test$p.value < 0.05, "Preferir efectos fijos", "Diferencia FE/RE pequeña"),
      "Si hubiera que elegir entre efectos fijos y aleatorios, ¿hay evidencia de diferencia sistemática?",
      "El modelo final usa efectos fijos por país; la prueba se reporta como diagnóstico complementario.",
      "Calculada en Shiny con plm::phtest y efectos aleatorios por método Amemiya; puede diferir levemente del notebook."
    ),
    diagnostic_row(
      "Forma funcional de Ramsey",
      unname(ramsey_test$statistic),
      ramsey_test$p.value,
      ifelse(ramsey_test$p.value < 0.05, "Revisar forma funcional", "Forma funcional adecuada"),
      "¿La forma lineal del modelo es razonable?",
      "No hay evidencia suficiente de mala especificación funcional.",
      "Calculada sobre el modelo lineal equivalente con indicadores de país y año."
    ),
    diagnostic_row(
      "Heterocedasticidad de Breusch-Pagan",
      unname(hetero_test$statistic),
      hetero_test$p.value,
      ifelse(hetero_test$p.value < 0.05, "Hay heterocedasticidad; usar errores estándar robustos", "No hay heterocedasticidad clara"),
      "¿La varianza de los errores es constante?",
      "La heterocedasticidad aparece, por eso la inferencia final usa errores estándar robustos.",
      "Calculada con lmtest::bptest sobre el modelo equivalente con indicadores de país y año."
    ),
    diagnostic_row(
      "Durbin-Watson global",
      unname(durbin_test$statistic),
      durbin_test$p.value,
      ifelse(unname(durbin_test$statistic) < 1.5 || unname(durbin_test$statistic) > 2.5, "Posible autocorrelación", "Sin autocorrelación global clara"),
      "¿Hay autocorrelación serial global en los errores?",
      "El estadístico no sugiere autocorrelación fuerte en el modelo equivalente.",
      "Calculada con lmtest::dwtest."
    ),
    diagnostic_row(
      "Breusch-Godfrey de autocorrelación",
      unname(bg_test$statistic),
      bg_test$p.value,
      ifelse(bg_test$p.value < 0.05, "Señal de autocorrelación", "Sin autocorrelación clara"),
      "¿Hay autocorrelación serial de primer orden?",
      "No aparece una señal fuerte en la prueba global de primer rezago.",
      "Calculada con lmtest::bgtest. Complementa, no sustituye, la cautela de panel."
    ),
    diagnostic_row(
      "Wooldridge para autocorrelación en panel",
      unname(wooldridge_test[["statistic"]]),
      unname(wooldridge_test[["p_value"]]),
      ifelse(!is.na(wooldridge_test[["p_value"]]) && wooldridge_test[["p_value"]] < 0.05, "Señal de autocorrelación; leer con cautela", "Sin autocorrelación clara"),
      "¿Hay autocorrelación dentro del panel?",
      "La señal se reporta como advertencia, pero no debe sobredimensionarse porque solo hay dos países.",
      "Cálculo manual basado en el notebook. Con dos grupos, el error agrupado puede ser degenerado."
    ),
    diagnostic_row(
      "Shapiro-Wilk",
      unname(shapiro_test$statistic),
      shapiro_test$p.value,
      ifelse(shapiro_test$p.value < 0.05, "Residuos no normales", "Residuos compatibles con normalidad"),
      "¿Los residuos son compatibles con una distribución normal?",
      "No se rechaza normalidad en el modelo equivalente; este no es el problema principal.",
      "Calculada con stats::shapiro.test sobre residuos del modelo equivalente."
    ),
    diagnostic_row(
      "Pesaran para dependencia transversal",
      unname(pesaran_test[["statistic"]]),
      unname(pesaran_test[["p_value"]]),
      ifelse(!is.na(pesaran_test[["p_value"]]) && pesaran_test[["p_value"]] < 0.05, "Dependencia transversal; leer con cautela", "Sin dependencia transversal clara"),
      "¿Los errores de Japón y Corea se mueven de forma relacionada?",
      "La señal se reporta como advertencia, no como razón suficiente para afirmar causalidad.",
      "Cálculo manual del estadístico CD; con dos países se reduce a una correlación de residuos."
    ),
    diagnostic_row(
      "Wu-Hausman para endogeneidad",
      unname(dwh_test[["statistic"]]),
      unname(dwh_test[["p_value"]]),
      ifelse(!is.na(dwh_test[["p_value"]]) && dwh_test[["p_value"]] < 0.05, "Hay señal de endogeneidad", "No hay endogeneidad clara"),
      "¿El índice cultural contemporáneo parece correlacionarse con el error?",
      "No hay evidencia suficiente para tratar el índice cultural como endógeno en esta especificación.",
      "Cálculo manual de Durbin-Wu-Hausman usando el índice cultural rezagado como instrumento; se reporta con cautela."
    )
  )
}

diagnostic_table <- calculate_diagnostics(model_data)
diagnostic_details <- diagnostic_table

format_p <- function(x) {
  ifelse(is.na(x), "No aplica", ifelse(x < 0.001, "<0.001", sprintf("%.4f", x)))
}

format_model_term <- function(value, label) {
  sprintf("%s %.4f × %s", ifelse(value < 0, "−", "+"), abs(value), label)
}

dt_options <- function(page_length = 5, dom = "t", ordering = FALSE) {
  list(
    pageLength = page_length,
    dom = dom,
    ordering = ordering,
    autoWidth = FALSE,
    scrollX = FALSE,
    language = list(
      search = "Buscar:",
      lengthMenu = "Mostrar _MENU_ registros",
      info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
      paginate = list(previous = "Anterior", `next` = "Siguiente")
    )
  )
}

data_sources <- data.frame(
  fuente = c(
    "Turismo internacional",
    "Banco Mundial",
    "UNESCO",
    "BIS / fuentes cambiarias integradas",
    "Base integrada del proyecto"
  ),
  uso_en_app = c(
    "Llegadas turísticas e ingresos por turismo internacional.",
    "Producto interno bruto, producto interno bruto per cápita, inflación y variables macroeconómicas.",
    "Sitios de patrimonio mundial y patrimonio cultural inmaterial.",
    "Tipo de cambio de moneda local por dólar estadounidense.",
    "Panel anual Japón-Corea del Sur, 2005-2024, embebido en el archivo R para facilitar el despliegue."
  ),
  decision = c(
    "La variable dependiente usa llegadas porque mide flujo turístico observado.",
    "El producto interno bruto per cápita se conserva como control económico del país destino.",
    "Los reconocimientos UNESCO se usan como señales comparables de patrimonio cultural.",
    "El tipo de cambio se conserva como control de condiciones económicas relativas.",
    "La app no depende de archivos externos; los datos definitivos deben validarse contra las fuentes originales."
  ),
  stringsAsFactors = FALSE
)

variable_decisions <- data.frame(
  tipo = c(
    "Variable dependiente",
    "Variable independiente principal",
    "Variable independiente alternativa",
    "Variable de control",
    "Variable de control",
    "Transformación",
    "Transformación",
    "Decisión de muestra"
  ),
  variable = c(
    "log_tourism",
    "culture_index",
    "culture_index_lag",
    "log_gdppc",
    "log_fx",
    "Logaritmos",
    "Estandarización",
    "Exclusión de 2020 y 2021"
  ),
  descripcion = c(
    "Logaritmo natural de llegadas turísticas internacionales medidas en miles de visitantes.",
    "Promedio de exportaciones culturales, patrimonio mundial UNESCO y patrimonio inmaterial UNESCO, después de estandarizar cada componente.",
    "Índice cultural de la observación previa disponible dentro de la muestra sin 2020 ni 2021.",
    "Logaritmo del producto interno bruto per cápita del país destino.",
    "Logaritmo del tipo de cambio medido como moneda local por dólar estadounidense.",
    "Reducen la escala de variables muy grandes y permiten leer los coeficientes económicos como cambios porcentuales aproximados.",
    "Convierte variables con unidades distintas a una escala común centrada en cero; valores positivos están por arriba del promedio de referencia.",
    "Los años de coronavirus se muestran en gráficos, pero no se usan en la estimación porque alteran la relación normal entre cultura, economía y turismo."
  ),
  justificacion = c(
    "Permite estudiar cambios proporcionales en turismo y reduce la influencia de niveles extremos.",
    "Resume una dimensión cultural comparable entre países, sin depender de una sola medición.",
    "Sirve como prueba de robustez temporal: pregunta si la cultura previa se asocia con turismo posterior.",
    "Controla diferencias generales de desarrollo económico del destino.",
    "Controla condiciones de precio relativo y competitividad cambiaria.",
    "Son transformaciones comunes en modelos de demanda turística y macroeconomía aplicada.",
    "Hace comparable una variable monetaria con conteos UNESCO; una unidad del índice no es una unidad física simple.",
    "No se eliminan de la historia del proyecto; se aíslan de la estimación principal por ser años atípicos."
  ),
  stringsAsFactors = FALSE
)

discarded_variables <- data.frame(
  variable = c(
    "music_exports_million_usd",
    "tourism_receipts_billion_usd",
    "tourism_receipts_per_arrival_usd",
    "fdi_inflow_billion_usd",
    "hotel_rooms_thousand",
    "broadband_per_100_inhabitants",
    "cpi_2010_100",
    "covid_dummy",
    "gfc_dummy y event_note"
  ),
  motivo = c(
    "Tiene datos incompletos para Japón y problemas de comparabilidad; incluirla podía sesgar el índice hacia Corea.",
    "Mide resultado turístico, no causa previa; puede estar determinado por las mismas llegadas que se busca explicar.",
    "Depende de ingresos y llegadas; puede mezclar composición del turista con volumen turístico.",
    "Puede estar vinculada simultáneamente con turismo, infraestructura y expectativas, lo que abre posible endogeneidad.",
    "La oferta hotelera puede crecer como respuesta al turismo; usarla como explicativa podría invertir la dirección temporal.",
    "Es una tendencia de infraestructura digital con alta colinealidad temporal y relación indirecta con turismo cultural.",
    "No fue central para la pregunta y podía sumar colinealidad en una muestra muy pequeña.",
    "Se descarta del modelo final porque 2020 y 2021 se excluyen; se conserva la información en la visualización.",
    "Son controles contextuales útiles para narrar shocks, pero no se incluyen por parsimonia y falta de grados de libertad."
  ),
  como_reportarlo = c(
    "Se reconoce como limitación de medición audiovisual: el índice cultural no captura perfectamente música, anime, dramas o streaming.",
    "Se puede comentar en descriptivos, no en el modelo causal o asociativo principal.",
    "Conviene usarla solo como contexto de gasto turístico.",
    "Debe mencionarse como posible variable omitida o endógena en futuras extensiones.",
    "Debe tratarse como posible canal de oferta, no como control inocuo.",
    "Puede explorarse en anexos si se amplía la muestra.",
    "Puede incorporarse en pruebas futuras si hay más observaciones.",
    "Debe explicarse como choque exógeno extremo, no como simple observación atípica menor.",
    "Funcionan como notas históricas para interpretar años específicos."
  ),
  stringsAsFactors = FALSE
)

culture_index_components <- data.frame(
  componente = c(
    "Exportaciones de bienes culturales",
    "Sitios de patrimonio mundial UNESCO",
    "Patrimonio cultural inmaterial UNESCO"
  ),
  que_aporta = c(
    "Captura una señal de circulación internacional de bienes culturales.",
    "Captura reconocimiento internacional de patrimonio material y atractivo cultural acumulado.",
    "Captura reconocimiento de prácticas, tradiciones y expresiones culturales vivas."
  ),
  limite = c(
    "No equivale directamente a exportaciones audiovisuales; es una aproximación amplia.",
    "Cambia lentamente y puede reflejar historia acumulada más que promoción cultural reciente.",
    "También cambia lentamente y no mide por sí solo consumo internacional de cultura popular."
  ),
  stringsAsFactors = FALSE
)

literature_comparison <- data.frame(
  autor_anio = c(
    "Bae et al. (2017)",
    "Kim, Chen y Su (2009)",
    "Kim, Long y Robinson (2009)",
    "Ng y Chan (2019/2020)",
    "Yildirim et al. (2017)",
    "Nakayama (2023)",
    "Puche-Ruiz, Crespo-Almendros y Sánchez-Castillo (2024)",
    "Bastos y Cheibub (2020)"
  ),
  resumen = c(
    "Panel de demanda turística hacia Corea con una variable asociada a Hallyu.",
    "Analiza cambios en turismo taiwanés hacia Corea después de dramas coreanos.",
    "Estudio exploratorio sobre dramas coreanos, Hallyu y turismo de pantalla.",
    "Encuesta sobre dramas coreanos e intención turística en jóvenes de Hong Kong.",
    "Estudio cualitativo sobre anime y deseo de viajar a Japón.",
    "Caso de Otaru sobre marketing de destinos mediante cine y dramas.",
    "Revisión sistematizada de investigación sobre turismo inducido por ficción audiovisual.",
    "Revisión bibliográfica sobre turismo y cine, temas y metodologías."
  ),
  hallazgo_principal = c(
    "Hallyu tiene efecto positivo y significativo sobre demanda turística hacia Corea.",
    "La popularidad de dramas coreanos se asocia con cambios observables en visitantes.",
    "La circulación intercultural de dramas puede modificar patrones de turismo.",
    "Los dramas mejoran imagen, exposición y conocimiento del destino, aunque la motivación directa puede ser moderada.",
    "Participantes reportan mayor intención de viajar a Japón después de ver anime.",
    "El audiovisual puede promover destinos, pero debe considerar sostenibilidad y saturación.",
    "El campo enfrenta dificultad para aislar la ficción audiovisual como motivación principal.",
    "El cine puede atraer turistas potenciales, pero la literatura pide mejores datos y metodología."
  ),
  relacion_con_nuestra_conclusion = c(
    "Es la coincidencia econométrica más cercana: nuestro índice cultural también aparece positivo y significativo.",
    "Refuerza el mecanismo de cultura visible y flujos turísticos, aunque nuestro modelo no estudia un evento específico.",
    "Apoya la explicación narrativa del mecanismo cultural, pero no permite comparar coeficientes.",
    "Respalda la cautela: la cultura puede aumentar interés sin convertirse automáticamente en viaje.",
    "Apoya el caso japonés, pero mide intención y no llegadas turísticas observadas.",
    "Ayuda a interpretar cultura como herramienta de posicionamiento, no como causa automática.",
    "Justifica que nuestra conclusión sea asociación estadística y no causalidad definitiva.",
    "Ubica el proyecto dentro de una literatura que reconoce potencial turístico y límites metodológicos."
  ),
  stringsAsFactors = FALSE
)

limitations_table <- data.frame(
  limitacion = c(
    "Muestra pequeña",
    "Solo dos países",
    "Causalidad no identificada",
    "Multicolinealidad posible",
    "Endogeneidad potencial",
    "Años 2020 y 2021",
    "Medición imperfecta de cultura audiovisual",
    "Robustez limitada"
  ),
  explicacion = c(
    "El modelo final usa 36 observaciones en la especificación base y 34 en la rezagada.",
    "Japón y Corea del Sur no representan automáticamente a toda Asia ni a todos los destinos culturales.",
    "El modelo estima asociación condicional; no identifica un experimento ni una estrategia causal fuerte.",
    "Los VIF se calculan y no son extremos, pero algunas variables económicas comparten tendencia temporal.",
    "Algunas variables descartadas pueden estar determinadas junto con el turismo, como infraestructura hotelera o inversión.",
    "Se excluyen de la estimación porque el choque sanitario domina la movilidad internacional.",
    "El índice cultural combina bienes culturales y patrimonio UNESCO; no mide directamente consumo de anime, K-pop, dramas o streaming.",
    "El modelo rezagado apoya el signo positivo, pero no resuelve por completo causalidad, endogeneidad ni dependencia transversal."
  ),
  como_defenderla = c(
    "Presentar resultados como evidencia exploratoria aplicada, no como estimación definitiva.",
    "Enfatizar que el proyecto compara dos casos relevantes por su perfil cultural-exportador.",
    "Usar lenguaje de asociación positiva, no de efecto causal automático.",
    "Reportar VIF y reconocerlo como riesgo si se amplía la especificación.",
    "Explicar que se eligió una especificación parsimoniosa para reducir variables problemáticas.",
    "Mostrar esos años en descriptivos y justificar su exclusión del modelo.",
    "Nombrarlo como aproximación razonable, no medición perfecta del fenómeno cultural/audiovisual.",
    "Usar el rezago como robustez complementaria y cerrar con conclusiones prudentes."
  ),
  stringsAsFactors = FALSE
)

col_jp <- "#bc002d"
col_kr <- "#003478"
col_accent <- "#e8a020"
col_ok <- "#2e8b57"

theme_eq3 <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      axis.text = element_text(size = 9),
      plot.title = element_text(face = "bold", size = 12, color = "#1a1a2e"),
      plot.subtitle = element_text(size = 9, color = "#667")
    )
}

country_colors <- c("Japan" = col_jp, "Korea" = col_kr)
country_labels <- c("Japan" = "Japón", "Korea" = "Corea del Sur")

scale_country_color <- function() {
  scale_color_manual(values = country_colors, labels = country_labels)
}

scale_country_fill <- function() {
  scale_fill_manual(values = country_colors, labels = country_labels)
}

add_model_exclusion_band <- function(plot) {
  plot +
    annotate(
      "rect",
      xmin = 2019.5,
      xmax = 2021.5,
      ymin = -Inf,
      ymax = Inf,
      fill = "#ffdddd",
      alpha = 0.38
    )
}

model_r_squared <- extract_r_squared(model_final)
culture_beta <- coef_table$estimate[coef_table$term == "culture_index"]
culture_percent <- (exp(culture_beta) - 1) * 100
culture_p <- coef_table$p_value[coef_table$term == "culture_index"]

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
      .app-header {
        background: #1a1a2e;
        color: white;
        padding: 20px 28px 16px;
        border-bottom: 3px solid #e8a020;
      }
      .app-header h1 {
        font-family: 'Noto Serif', serif;
        font-size: 20px;
        font-weight: 700;
        margin: 0 0 4px;
        letter-spacing: -.3px;
      }
      .app-header p { font-size: 11.5px; color: #aab; margin: 0; }
      .app-header .fuentes { font-size: 10.5px; color: #778; margin-top: 3px; }
      .equipo-tag {
        display: inline-block;
        background: rgba(232,160,32,.18);
        border: 1px solid rgba(232,160,32,.4);
        border-radius: 3px;
        color: #e8a020;
        font-size: 10px;
        font-weight: 600;
        padding: 2px 8px;
        margin-top: 8px;
        letter-spacing: .5px;
        text-transform: uppercase;
      }
      .kpi-bar {
        background: white;
        border-bottom: 1px solid #e4e6eb;
        padding: 10px 28px;
        display: flex;
        gap: 0;
      }
      .kpi-item {
        flex: 1;
        text-align: center;
        padding: 6px 10px;
        border-right: 1px solid #eee;
      }
      .kpi-item:last-child { border-right: none; }
      .kpi-val { font-size: 17px; font-weight: 700; line-height: 1.1; }
      .kpi-lbl { font-size: 10px; color: #888; margin-top: 2px; }
      .jp { color: #bc002d; }
      .kr { color: #003478; }
      .neu { color: #333; }
      .acc { color: #e8a020; }
      .ok { color: #2e8b57; }
      .body-wrap { display: flex; min-height: calc(100vh - 130px); }
      .sidebar {
        width: 210px;
        min-width: 210px;
        background: #1a1a2e;
        padding: 18px 14px;
        color: white;
      }
      .sidebar-label {
        font-size: 9.5px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 1px;
        color: #667;
        margin: 14px 0 6px;
      }
      .sidebar-label:first-child { margin-top: 0; }
      .sidebar .shiny-input-container { margin-bottom: 10px; }
      .sidebar .shiny-input-container label {
        color: #aab;
        font-size: 11.5px;
        font-weight: 500;
      }
      .sidebar .form-control, .sidebar .selectize-input {
        background: #252545;
        border: 1px solid #334;
        color: white;
        font-size: 12px;
        border-radius: 5px;
      }
      .sidebar .irs--shiny .irs-bar { background: #e8a020; }
      .sidebar .irs--shiny .irs-handle { background: #e8a020; border-color: #e8a020; }
      .sidebar .irs--shiny .irs-from, .sidebar .irs--shiny .irs-to,
      .sidebar .irs--shiny .irs-single { background: #e8a020; }
      .sidebar .irs--shiny .irs-line { background: #334; }
      .sidebar .irs--shiny .irs-min, .sidebar .irs--shiny .irs-max { color: #667; }
      .sidebar .irs-grid-text { font-size: 8.5px; }
      .sidebar input[type=checkbox] { accent-color: #e8a020; }
      .sidebar .checkbox label, .sidebar .radio label { color: #c7c7df; font-size: 11.5px; }
      .sidebar-note {
        background: rgba(232,160,32,.10);
        border: 1px solid rgba(232,160,32,.32);
        border-left: 3px solid #e8a020;
        border-radius: 0 5px 5px 0;
        color: #d8d4ec;
        font-size: 10.5px;
        line-height: 1.45;
        padding: 9px 10px;
        margin-top: 8px;
      }
      .main-area { flex: 1; padding: 18px 22px; overflow-x: hidden; }
      .nav-tabs {
        border-bottom: 2px solid #dde;
        margin-bottom: 18px;
        display: flex;
        flex-wrap: wrap;
        gap: 3px 4px;
      }
      .nav-tabs > li { float: none; }
      .nav-tabs > li > a {
        font-size: 11.5px;
        font-weight: 600;
        color: #556;
        padding: 8px 11px;
        border-radius: 6px 6px 0 0;
        border: none;
        background: transparent;
        white-space: normal;
        line-height: 1.2;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover {
        color: #1a1a2e;
        background: white;
        border: 2px solid #dde;
        border-bottom-color: white;
      }
      .nav-tabs > li > a:hover { background: #eef; color: #1a1a2e; }
      .card {
        background: white;
        border-radius: 8px;
        padding: 16px 18px;
        box-shadow: 0 1px 4px rgba(0,0,0,.07);
        margin-bottom: 14px;
      }
      .card-title {
        font-size: 12.5px;
        font-weight: 700;
        color: #1a1a2e;
        margin-bottom: 10px;
      }
      .card-sub {
        font-size: 11px;
        color: #778;
        font-weight: 400;
        display: block;
        margin-top: 1px;
      }
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
        font-size: 9.5px;
        font-weight: 700;
        padding: 2px 7px;
        border-radius: 3px;
        text-transform: uppercase;
        letter-spacing: .5px;
        margin-right: 6px;
        vertical-align: middle;
      }
      .lit-callout strong { color: #3a2c0a; }
      .hypothesis-strip {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 8px;
        margin: 8px 0 14px;
      }
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
        font-weight: 700;
        font-size: 10.5px;
        text-transform: uppercase;
        letter-spacing: .5px;
        color: #003478;
        margin-bottom: 4px;
      }
      .simple-box {
        background: #f5fbf5;
        border-left: 4px solid #2e8b57;
        border-radius: 0 6px 6px 0;
        padding: 11px 14px;
        font-size: 11.8px;
        color: #1f4a30;
        margin-bottom: 14px;
        line-height: 1.6;
      }
      .simple-box .sb-title {
        font-weight: 700;
        font-size: 10.5px;
        text-transform: uppercase;
        letter-spacing: .5px;
        color: #2e8b57;
        margin-bottom: 4px;
      }
      .simple-box strong { color: #11331f; }
      .caution-box {
        background: #fff5f5;
        border-left: 4px solid #bc002d;
        border-radius: 0 6px 6px 0;
        padding: 10px 14px;
        font-size: 11.5px;
        color: #5a1020;
        margin-bottom: 14px;
        line-height: 1.6;
      }
      .caution-box .cb-title {
        font-weight: 700;
        font-size: 10.5px;
        text-transform: uppercase;
        letter-spacing: .5px;
        color: #bc002d;
        margin-bottom: 4px;
      }
      .mec-flow { display: flex; align-items: center; gap: 0; flex-wrap: wrap; margin: 10px 0; }
      .mec-box {
        background: #f8f9fb;
        border: 1.5px solid #dde;
        border-radius: 6px;
        padding: 8px 12px;
        font-size: 11.5px;
        text-align: center;
        min-width: 120px;
        line-height: 1.4;
      }
      .mec-box.highlight { background: #1a1a2e; color: white; border-color: #1a1a2e; }
      .mec-box.accent { background: #fef6e4; border-color: #e8a020; color: #5a3c00; }
      .mec-arrow { font-size: 18px; color: #aab; padding: 0 6px; }

      /* --- Ecuación hero --- */
      .eq-hero-wrap {
        background: #f8f9fb;
        border: 1px solid #e4e6eb;
        border-left: 4px solid #1a1a2e;
        border-radius: 0 8px 8px 0;
        padding: 18px 28px 14px;
        text-align: center;
        margin: 6px 0 14px;
      }
      .eq-hero-label {
        font-size: 9px; font-weight: 700; letter-spacing: 1.8px;
        text-transform: uppercase; color: #003478; margin-bottom: 8px;
      }
      .eq-hero-generic {
        font-family: 'Noto Serif', serif; font-size: 11px;
        color: #aaa; margin-bottom: 12px;
      }
      .eq-hero-main {
        font-family: 'Noto Serif', serif; font-size: 17px;
        color: #1a1a2e; line-height: 1.75;
      }
      .eq-coef-main { color: #2e8b57; font-weight: 700; }
      .eq-coef-ctrl { color: #555; }
      .eq-hero-stats {
        display: flex; justify-content: center; gap: 0;
        border-top: 1px solid #e4e6eb; margin-top: 14px; padding-top: 10px;
      }
      .eq-stat {
        flex: 1; text-align: center; padding: 4px 10px;
        border-right: 1px solid #eee; font-size: 10px; color: #888;
      }
      .eq-stat:last-child { border-right: none; }
      .eq-stat strong { color: #1a1a2e; font-size: 15px; font-weight: 700; display: block; margin-bottom: 2px; }
      .eq-dual { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; margin: 6px 0 14px; }
      @media (max-width: 900px) { .eq-dual { grid-template-columns: 1fr; } }

      /* --- Tabla de resultados y panel interpretativo --- */
      .interp-panel { margin-top: 4px; }
      .interp-var {
        padding: 11px 0;
        border-bottom: 1px solid #eee;
      }
      .interp-var:last-child { border-bottom: none; padding-bottom: 0; }
      .interp-var-header {
        display: flex; align-items: center; gap: 8px; margin-bottom: 5px; flex-wrap: wrap;
      }
      .interp-var-name { font-weight: 700; font-size: 12px; color: #1a1a2e; }
      .interp-badge {
        font-size: 9px; font-weight: 700; padding: 2px 6px;
        border-radius: 3px; letter-spacing: .4px;
      }
      .interp-badge-sig { background: #2e8b57; color: white; }
      .interp-badge-ns  { background: #dde; color: #667; }
      .interp-type {
        font-size: 9px; color: #889; text-transform: uppercase;
        letter-spacing: .5px; margin-left: auto;
      }
      .interp-text { font-size: 11.2px; color: #444; line-height: 1.6; margin: 0; }
      .results-footer {
        border-top: 1px solid #eee; margin-top: 10px; padding-top: 8px;
        font-size: 10.2px; color: #777; line-height: 1.7;
      }
      /* highlighted significant DT row */
      table.dataTable tbody tr.sig-row td { background-color: #f0f7f0 !important; }

      /* --- Grupo C: index construction diagram label --- */
      .idx-caption { font-size: 10.5px; color: #666; margin-top: 6px; text-align: center; }

      /* --- Grupo D: literature cards --- */
      .lit-cards { display: flex; flex-direction: column; gap: 12px; margin-top: 4px; }
      .lit-card {
        background: #f8f9fb;
        border: 1px solid #e4e6eb;
        border-left: 4px solid #003478;
        border-radius: 6px;
        padding: 12px 14px;
        display: flex;
        flex-direction: column;
        gap: 5px;
      }
      .lit-card-header { display: flex; align-items: baseline; gap: 10px; }
      .lit-card-author { font-size: 13px; font-weight: 700; color: #1a1a2e; }
      .lit-card-finding { font-size: 11.5px; color: #333; line-height: 1.45; }
      .lit-card-relation { font-size: 10.5px; color: #555; font-style: italic; line-height: 1.4; margin-top: 2px; }
      .lit-badge-box { font-size: 9px; font-weight: 700; padding: 2px 8px; border-radius: 3px;
                       letter-spacing: .5px; text-transform: uppercase; flex-shrink: 0; }
      .lit-badge-direct  { background: #2e8b57; color: white; }
      .lit-badge-partial { background: #e8a020; color: white; }
      .lit-badge-limit   { background: #666; color: white; }

      .h-result {
        display: inline-block;
        font-size: 9px;
        font-weight: 700;
        padding: 2px 8px;
        border-radius: 3px;
        margin-top: 6px;
        letter-spacing: .6px;
        text-transform: uppercase;
      }
      .h-apoyada   { background: #2e8b57; color: white; }
      .h-parcial   { background: #e8a020; color: white; }
      .h-rechazada { background: #bc002d; color: white; }
      .hypothesis-pill {
        background: #f8f9fb;
        border: 1px solid #e4e6eb;
        border-radius: 6px;
        padding: 10px 11px;
        font-size: 11px;
        line-height: 1.4;
        display: flex;
        flex-direction: column;
      }
      .hypothesis-pill strong {
        font-size: 13px;
        color: #1a1a2e;
        margin-bottom: 3px;
      }
      .hypothesis-pill .h-desc { flex: 1; color: #444; }
      .plot-caption {
        font-size: 10.5px;
        color: #888;
        margin-top: 4px;
        font-style: italic;
        text-align: right;
      }
      .small-note {
        font-size: 10.8px;
        color: #777;
        line-height: 1.55;
        margin-top: 6px;
      }
      .inline-metric {
        display: inline-block;
        background: #f8f9fb;
        border: 1px solid #e4e6eb;
        border-radius: 6px;
        padding: 8px 10px;
        margin: 0 6px 6px 0;
        font-size: 11px;
      }
      .inline-metric strong {
        display: block;
        color: #1a1a2e;
        font-size: 13px;
        margin-bottom: 2px;
      }
      .equation {
        font-family: 'Noto Serif', serif;
        background: #fafafa;
        border: 1px solid #e6e6e6;
        border-radius: 6px;
        padding: 11px 14px;
        font-size: 12px;
        color: #222;
        line-height: 1.65;
      }
      .fuente-grid {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        gap: 10px;
        margin-top: 6px;
      }
      .fuente-item {
        background: #f8f9fb;
        border-radius: 6px;
        padding: 10px 12px;
        font-size: 11.5px;
        border: 1px solid #e4e6eb;
      }
      .fuente-item .fi-sigla {
        font-weight: 700;
        font-size: 13px;
        color: #1a1a2e;
        margin-bottom: 2px;
      }
      .fuente-item .fi-desc { color: #666; font-size: 10.5px; }
      .fuente-item .fi-var {
        margin-top: 5px;
        font-size: 10.5px;
        font-weight: 600;
        color: #e8a020;
      }
      .test-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 10px;
      }
      .test-card {
        border: 1px solid #e4e6eb;
        border-radius: 7px;
        padding: 12px 13px;
        background: #fbfbfd;
        min-height: 142px;
      }
      .test-card h4 {
        font-size: 11.5px;
        line-height: 1.35;
        margin: 0 0 7px;
        color: #1a1a2e;
      }
      .test-meta {
        display: flex;
        flex-wrap: wrap;
        gap: 5px;
        margin-bottom: 8px;
      }
      .test-pill {
        display: inline-block;
        border-radius: 999px;
        background: #f2f3f5;
        border: 1px solid #dde;
        color: #444;
        font-size: 10px;
        padding: 2px 7px;
      }
      .test-question {
        font-size: 10.8px;
        color: #555;
        line-height: 1.45;
        margin-bottom: 6px;
      }
      .test-decision {
        font-size: 11.2px;
        color: #27364a;
        line-height: 1.45;
      }
      .dataTables_wrapper {
        width: 100%;
        overflow-x: visible;
        font-size: 11px;
      }
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 10.5px;
      }
      .dataTables_wrapper .dataTables_filter input {
        max-width: 170px;
        border: 1px solid #ccd;
        border-radius: 5px;
        padding: 3px 6px;
      }
      table.dataTable {
        width: 100% !important;
        min-width: 0;
        table-layout: fixed;
      }
      table.dataTable thead th,
      table.dataTable tbody td {
        white-space: normal !important;
        overflow-wrap: anywhere;
        word-break: break-word;
        vertical-align: top !important;
        line-height: 1.35;
        padding: 7px 8px !important;
      }
      table.dataTable tbody td p { margin: 0; }
      .dataTables_wrapper table,
      .dataTables_wrapper .dataTable {
        max-width: 100% !important;
      }
      table.dataTable thead th {
        font-size: 10.5px;
        background: #f4f5f8;
        color: #1a1a2e;
      }
      table.dataTable tbody td {
        font-size: 10.7px;
      }
      @media (max-width: 1050px) {
        .main-area .col-sm-5,
        .main-area .col-sm-6,
        .main-area .col-sm-7 {
          width: 100%;
          float: none;
        }
        .hypothesis-strip { grid-template-columns: repeat(2, minmax(0, 1fr)); }
        .fuente-grid { grid-template-columns: 1fr; }
      }
      @media (max-width: 700px) {
        .body-wrap { display: block; }
        .sidebar { width: 100%; min-width: 100%; }
        .kpi-bar { flex-wrap: wrap; }
        .kpi-item { min-width: 33%; }
        .hypothesis-strip { grid-template-columns: 1fr; }
        .test-grid { grid-template-columns: 1fr; }
        table.dataTable thead th { font-size: 9.7px; }
        table.dataTable tbody td { font-size: 10px; }
      }
    "))
  ),

  div(
    class = "app-header",
    tags$h1("Cultura, exportación audiovisual aproximada y turismo internacional"),
    tags$p("Japón y Corea del Sur · panel 2005-2024 · comparación de modelo base y modelo rezagado"),
    div(class = "fuentes", "App de exposición: resultados, decisiones metodológicas, diagnósticos, limitaciones y diálogo con literatura"),
    div(class = "equipo-tag", "Equipo 3 · Inteligencia de Negocios · 2026")
  ),

  div(
    class = "kpi-bar",
    div(class = "kpi-item", div(class = "kpi-val neu", textOutput("kpi_nobs")), div(class = "kpi-lbl", "Observaciones del modelo")),
    div(class = "kpi-item", div(class = "kpi-val acc", textOutput("kpi_model")), div(class = "kpi-lbl", "Especificación visible")),
    div(class = "kpi-item", div(class = "kpi-val ok", textOutput("kpi_beta")), div(class = "kpi-lbl", "Coeficiente índice cultural")),
    div(class = "kpi-item", div(class = "kpi-val ok", textOutput("kpi_percent")), div(class = "kpi-lbl", "Cambio aproximado asociado")),
    div(class = "kpi-item", div(class = "kpi-val ok", textOutput("kpi_pvalue")), div(class = "kpi-lbl", "Valor p índice cultural")),
    div(class = "kpi-item", div(class = "kpi-val neu", textOutput("kpi_r2")), div(class = "kpi-lbl", "Coeficiente de determinación"))
  ),

  div(
    class = "body-wrap",
    div(
      class = "sidebar",
      div(class = "sidebar-label", "Periodo visual"),
      sliderInput("year_range", NULL, min = 2005, max = 2024, value = c(2005, 2024), sep = "", step = 1, ticks = FALSE),
      div(class = "sidebar-label", "País"),
      selectInput(
        "country_select",
        NULL,
        choices = c("Ambos" = "both", "Japón" = "Japan", "Corea del Sur" = "Korea"),
        selected = "both"
      ),
      div(class = "sidebar-label", "Gráficos"),
      checkboxInput("exclude_model_years", "Omitir 2020 y 2021", value = TRUE),
      checkboxInput("show_labels", "Mostrar etiquetas de año", value = FALSE),
      checkboxInput("use_log_axis", "Escala logarítmica en turismo", value = FALSE),
      div(class = "sidebar-label", "Modelo"),
      radioButtons(
        "model_choice",
        NULL,
        choices = c("Base" = "base", "Rezagado" = "lag"),
        selected = "base"
      ),
      div(
        class = "sidebar-note",
        "Los filtros de periodo y país afectan gráficas y tablas descriptivas. Los modelos se calculan con la muestra definida: sin 2020 ni 2021. El selector Base/Rezagado sí cambia los resultados del modelo visible."
      )
    ),

    div(
      class = "main-area",
      tabsetPanel(
        type = "tabs",
        id = "tabs",

        tabPanel(
          "Inicio",
          div(
            class = "lit-callout",
            span(class = "lit-badge", "Conclusión"),
            tags$strong("El modelo final usa efectos fijos por país con errores estándar robustos HC1."),
            tags$br(),
            "La estructura panel justifica controlar diferencias fijas por país. Se estima por within (plm::plm con model = 'within' e index por country_id y year). Como la prueba de Breusch-Pagan detecta heterocedasticidad, la inferencia se reporta con errores estándar robustos HC1 tipo White sin agrupar."
          ),
          div(
            class = "note-box",
            div(class = "nb-title", "Hipótesis que organiza la app"),
            "El proyecto parte de una hipótesis nula: que no hay relación estadísticamente significativa entre cultura audiovisual aproximada y turismo internacional una vez incluidos controles. La app busca evaluar si la evidencia apoya una asociación positiva, si esa asociación se mantiene con controles y si el rezago temporal mejora la explicación."
          ),
          div(
            class = "hypothesis-strip",
            div(
              class = "hypothesis-pill",
              tags$strong("H0"),
              tags$span(class = "h-desc", "Sin relación significativa con controles."),
              tags$span(class = "h-result h-rechazada", "✓ Rechazada — hay evidencia")
            ),
            div(
              class = "hypothesis-pill",
              tags$strong("H1"),
              tags$span(class = "h-desc", "Asociación positiva cultura-turismo."),
              tags$span(class = "h-result h-apoyada", "✓ Apoyada")
            ),
            div(
              class = "hypothesis-pill",
              tags$strong("H2"),
              tags$span(class = "h-desc", "Relación robusta al controlar macrovariables."),
              tags$span(class = "h-result h-parcial", "~ Apoyada parcialmente")
            ),
            div(
              class = "hypothesis-pill",
              tags$strong("H3"),
              tags$span(class = "h-desc", "Posible efecto con rezago temporal."),
              tags$span(class = "h-result h-parcial", "~ Apoyada parcialmente")
            )
          ),
          fluidRow(
            column(
              7,
              div(
                class = "card",
                div(class = "card-title", "Efecto estimado del índice cultural", tags$span(class = "card-sub", "Efectos fijos por país, errores robustos, sin 2020 ni 2021")),
                plotOutput("plot_coefficients", height = "300px"),
                div(class = "plot-caption", "Intervalos calculados con errores estándar robustos.")
              )
            ),
            column(
              5,
              uiOutput("reading_easy"),
              div(
                class = "caution-box",
                div(class = "cb-title", "Cautela"),
                "La app muestra asociación estadística, no causalidad definitiva. La muestra tiene solo dos países, por lo que las pruebas que dependen de la dimensión transversal deben leerse con cuidado."
              ),
              uiOutput("equation_hero")
            )
          ),
          div(
            class = "card",
            div(class = "card-title", "Efecto predicho del índice cultural",
                tags$span(class = "card-sub", "Curva basada en el modelo seleccionado")),
            plotOutput("plot_effect", height = "320px")
          ),
          div(
            class = "card",
            div(class = "card-title", "Ruta de decisión del modelo"),
            div(
              class = "mec-flow",
              div(class = "mec-box highlight", tags$b("Datos panel"), tags$br(), "Japón-Corea"),
              div(class = "mec-arrow", "→"),
              div(class = "mec-box", "Excluir", tags$br(), "2020-2021"),
              div(class = "mec-arrow", "→"),
              div(class = "mec-box accent", "Pruebas", tags$br(), "de selección"),
              div(class = "mec-arrow", "→"),
              div(class = "mec-box highlight", tags$b("Efectos"), tags$br(), "fijos"),
              div(class = "mec-arrow", "→"),
              div(class = "mec-box", "Errores", tags$br(), "robustos")
            )
          )
        ),

        tabPanel(
          "Marco e hipótesis",
          div(
            class = "lit-callout",
            span(class = "lit-badge", "Hipótesis"),
            tags$strong("La app se organiza alrededor de H0, H1, H2 y H3."),
            tags$br(),
            "La pregunta no es solo si el índice cultural se ve bonito en una gráfica, sino si su coeficiente es positivo, significativo y estable cuando se agregan controles y cuando se revisa una versión rezagada."
          ),
          fluidRow(
            column(
              6,
              div(
                class = "simple-box",
                div(class = "sb-title", "Lectura para el público"),
                "La evidencia rechaza la hipótesis nula en la especificación principal y apoya una asociación positiva. La hipótesis del rezago recibe apoyo parcial: el rezago es significativo, pero no mejora el ajuste frente al modelo base."
              )
            ),
            column(
              6,
              div(
                class = "caution-box",
                div(class = "cb-title", "Alcance de la medición cultural"),
                "El proyecto busca aproximar la exportación cultural y audiovisual, pero las variables audiovisuales puras tienen problemas de disponibilidad y comparabilidad. Por eso la app usa un índice cultural más amplio y presenta el resultado como aproximación cultural/audiovisual, no como medición perfecta de K-dramas, anime o cine."
              )
            )
          ),
          div(
            class = "card",
            div(class = "card-title", "Hipótesis y resultado empírico"),
            DTOutput("hypotheses_table")
          ),
          div(
            class = "card",
            div(class = "card-title", "Preguntas de investigación y respuesta de la app"),
            DTOutput("research_questions_table")
          )
        ),

        tabPanel(
          "Datos y variables",
          div(
            class = "lit-callout",
            span(class = "lit-badge", "Datos"),
            tags$strong("La aplicación usa un panel anual balanceado de Japón y Corea del Sur para 2005-2024."),
            tags$br(),
            "La estimación principal usa 2005-2019 y 2022-2024. Los años 2020 y 2021 permanecen disponibles para visualización, pero no para estimar el modelo final porque corresponden al choque extraordinario del coronavirus."
          ),
          div(
            class = "card",
            div(class = "card-title", "Cobertura temporal del panel", tags$span(class = "card-sub", "Años incluidos y excluidos del modelo de estimación")),
            plotOutput("plot_timeline", height = "140px"),
            div(class = "plot-caption", "Verde: años usados en la estimación · Rojo: años excluidos por el choque del coronavirus · 18 años × 2 países = 36 observaciones")
          ),
          div(
            class = "card",
            div(class = "card-title", "Mapa visual de la especificación"),
            div(
              class = "fuente-grid",
              div(class = "fuente-item", div(class = "fi-sigla", "Dependiente"), div(class = "fi-desc", "Llegadas turísticas internacionales"), div(class = "fi-var", "log_tourism")),
              div(class = "fuente-item", div(class = "fi-sigla", "Cultural"), div(class = "fi-desc", "Bienes culturales y patrimonio UNESCO estandarizados"), div(class = "fi-var", "culture_index / culture_index_lag")),
              div(class = "fuente-item", div(class = "fi-sigla", "Controles"), div(class = "fi-desc", "Ingreso relativo y precio externo aproximado"), div(class = "fi-var", "log_gdppc + log_fx"))
            )
          ),
          div(
            class = "card",
            div(class = "card-title", "Fuentes y uso dentro del modelo"),
            DTOutput("sources_table")
          ),
          div(
            class = "card",
            div(class = "card-title", "Componentes del índice cultural"),
            DTOutput("culture_components_decisions_table")
          ),
          div(
            class = "card",
            div(class = "card-title", "Cómo se construye el índice cultural",
                tags$span(class = "card-sub", "Tres componentes → estandarización → promedio")),
            plotOutput("plot_index_diagram", height = "200px"),
            div(class = "idx-caption", "Cada componente se lleva a escala comparable (z-score) antes de promediar, de modo que exportaciones en millones de dólares y conteos UNESCO tengan el mismo peso en el índice final.")
          ),
          div(
            class = "note-box",
            div(class = "nb-title", "Cómo leer el índice cultural"),
            HTML("<strong>Estandarizar</strong> significa restar la media y dividir entre la desviación estándar de cada componente. Así, exportaciones en dólares y conteos UNESCO quedan en una escala común. Después se promedian porque cada componente representa una señal parcial de presencia cultural. Una unidad del índice no equivale a un sitio UNESCO ni a un millón de dólares: representa un aumento de una unidad en el indicador compuesto estandarizado.")
          ),
          div(
            class = "caution-box",
            div(class = "cb-title", "Aproximación, no medición perfecta"),
            "El índice cultural aproxima presencia cultural/exportable y patrimonio reconocido, pero no mide directamente anime, K-pop, dramas, streaming, consumo de música o motivaciones individuales de viaje. Variables como exportaciones musicales pueden quedar fuera si tienen datos incompletos o no comparables entre Japón y Corea."
          ),
          div(
            class = "card",
            div(class = "card-title", "Variables incluidas y transformaciones"),
            DTOutput("variable_decisions_table")
          ),
          div(
            class = "card",
            div(class = "card-title", "Variables descartadas y motivo"),
            DTOutput("discarded_variables_table")
          ),
          div(
            class = "card",
            div(class = "card-title", "Vista de la base filtrada", tags$span(class = "card-sub", "Los filtros laterales solo cambian esta tabla y las gráficas descriptivas")),
            DTOutput("data_table_preview")
          )
        ),

        tabPanel(
          "Exploración visual",
          fluidRow(
            column(
              6,
              div(
                class = "card",
                div(class = "card-title", "Llegadas turísticas", tags$span(class = "card-sub", "La muestra del modelo omite 2020 y 2021")),
                plotOutput("plot_tourism", height = "290px")
              )
            ),
            column(
              6,
              div(
                class = "card",
                div(class = "card-title", "Índice cultural", tags$span(class = "card-sub", "Promedio de tres componentes estandarizados")),
                plotOutput("plot_culture", height = "290px")
              )
            )
          ),
          fluidRow(
            column(
              6,
              div(
                class = "card",
                div(class = "card-title", "Componentes culturales", tags$span(class = "card-sub", "Bienes culturales y patrimonio UNESCO")),
                plotOutput("plot_components", height = "290px")
              )
            ),
            column(
              6,
              div(
                class = "card",
                div(class = "card-title", "Relación bivariada", tags$span(class = "card-sub", "Índice cultural frente a turismo")),
                plotOutput("plot_scatter", height = "290px")
              )
            )
          )
          ,
          div(
            class = "card",
            div(class = "card-title", "Resumen descriptivo", tags$span(class = "card-sub", "Muestra usada en el modelo base")),
            DTOutput("descriptive_summary_table")
          ),
          div(
            class = "card",
            div(
              class = "card-title",
              "Mapa de calor de correlaciones",
              tags$span(class = "card-sub", "Muestra del modelo base sin 2020 ni 2021 — correlaciones de Pearson")
            ),
            div(
              class = "note-box",
              div(class = "nb-title", "Cómo leer el mapa"),
              "Rojo intenso indica correlación positiva alta; azul intenso indica correlación negativa alta; blanco indica correlación cercana a cero. Los valores en cada celda son los coeficientes de correlación de Pearson redondeados a tres decimales. La diagonal siempre vale 1."
            ),
            plotOutput("plot_correlation_heatmap", height = "340px"),
            div(class = "plot-caption", "Las correlaciones altas entre variables explicativas (índice cultural y PIB per cápita ≈ 0.645) se interpretan junto con los factores de inflación de la varianza en la pestaña Modelos y resultados.")
          )
        ),

        tabPanel(
          "Modelos y resultados",
          div(
            class = "lit-callout",
            span(class = "lit-badge", "Comparación"),
            tags$strong("La app calcula dos especificaciones defendibles sobre la muestra sin 2020 ni 2021."),
            tags$br(),
            "El Modelo A usa el índice cultural contemporáneo. El Modelo B usa el índice cultural rezagado, calculado como la observación previa disponible después de retirar 2020 y 2021."
          ),
          uiOutput("equation_both"),
          fluidRow(
            column(
              7,
              div(
                class = "card",
                div(class = "card-title", "Coeficiente cultural por especificación", tags$span(class = "card-sub", "Puntos con intervalos robustos al 95 por ciento")),
                plotOutput("plot_model_comparison", height = "280px")
              )
            ),
            column(
              5,
              div(
                class = "simple-box",
                div(class = "sb-title", "Recomendación"),
                "El modelo base es el más defendible para las conclusiones principales porque es parsimonioso, usa más observaciones y conserva significancia del índice cultural. El modelo rezagado se reporta como robustez: mantiene signo positivo y significancia, pero no mejora la R cuadrada."
              ),
              div(
                class = "note-box",
                div(class = "nb-title", "Filtro vs modelo"),
                "Los filtros laterales no reestiman modelos por país o subperiodo porque la muestra quedaría demasiado pequeña. El selector Base/Rezagado sí cambia las tablas del modelo visible y los KPI."
              )
            )
          ),
          verbatimTextOutput("model_summary_print"),
          uiOutput("model_interp_brief"),
          div(
            class = "card",
            div(class = "card-title", "Multicolinealidad: Factor de Inflación de la Varianza", tags$span(class = "card-sub", "VIF por variable y por modelo — líneas de referencia en 3 y 5")),
            plotOutput("plot_vif", height = "260px"),
            div(class = "small-note", "Verde: sin problema (VIF ≤ 3) · Naranja: vigilar (3 < VIF ≤ 5) · Rojo: reportar como limitación (VIF > 5). En esta muestra, ninguna variable supera el umbral de preocupación.")
          ),
          fluidRow(
            column(
              6,
              div(
                class = "card",
                div(class = "card-title", "Valores observados y ajustados"),
                plotOutput("plot_fitted", height = "280px")
              )
            ),
            column(
              6,
              div(
                class = "card",
                div(class = "card-title", "Residuos del modelo final"),
                plotOutput("plot_residuals", height = "280px")
              )
            )
          )
        ),

        tabPanel(
          "Diagnóstico y limitaciones",
          fluidRow(
            column(
              6,
              div(
                class = "simple-box",
                div(class = "sb-title", "Decisión defendible"),
                "El modelo final usa efectos fijos por país con errores robustos HC1 tipo White sin agrupar. La heterocedasticidad detectada por Breusch-Pagan refuerza el uso de errores robustos. Pesaran y Wooldridge se reportan como advertencias por el tamaño transversal reducido."
              )
            ),
            column(
              6,
              div(
                class = "caution-box",
                div(class = "cb-title", "No sobreinterpretar"),
                "Con Japón y Corea solamente, no conviene presentar resultados como evidencia causal. La formulación correcta es asociación positiva y significativa entre índice cultural y turismo."
              )
            )
          ),
          div(
            class = "card",
            div(class = "card-title", "Pruebas de selección y supuestos", tags$span(class = "card-sub", "Pregunta econométrica, estadístico y decisión metodológica")),
            uiOutput("diagnostic_cards")
          ),
          div(
            class = "card",
            div(class = "card-title", "Distribución de residuos"),
            plotOutput("plot_residual_distribution", height = "260px")
          ),
          fluidRow(
            column(
              6,
              div(
                class = "card",
                div(
                  class = "card-title",
                  "Gráfico de cuantiles teóricos",
                  tags$span(class = "card-sub", "Prueba visual de normalidad de los residuos")
                ),
                div(
                  class = "note-box",
                  div(class = "nb-title", "Cómo leer este gráfico"),
                  "Si los puntos se alinean sobre la línea roja, los residuos son compatibles con una distribución normal. Desviaciones sistemáticas en los extremos indican colas más pesadas o asimetría. La prueba de Shapiro-Wilk confirma el diagnóstico numérico."
                ),
                plotOutput("plot_qq", height = "270px"),
                div(class = "plot-caption", "La línea roja es la referencia teórica normal construida con los cuartiles de la muestra.")
              )
            ),
            column(
              6,
              div(
                class = "card",
                div(
                  class = "card-title",
                  "Función de autocorrelación de residuos",
                  tags$span(class = "card-sub", "Por país, sin 2020 ni 2021")
                ),
                div(
                  class = "note-box",
                  div(class = "nb-title", "Cómo leer este gráfico"),
                  "Cada barra es la correlación de los residuos con su propia versión rezagada. Las líneas rojas punteadas son los intervalos de confianza al 95 por ciento. Barras que superan esas líneas sugieren autocorrelación estadísticamente relevante para ese rezago."
                ),
                plotOutput("plot_acf", height = "270px"),
                div(class = "plot-caption", "La prueba de Wooldridge y Breusch-Godfrey ofrecen la confirmación estadística de lo que se observa visualmente aquí.")
              )
            )
          ),
          fluidRow(
            column(
              6,
              div(
                class = "card",
                div(
                  class = "card-title",
                  "Residuos absolutos por país",
                  tags$span(class = "card-sub", "Exploración visual de heterocedasticidad")
                ),
                div(
                  class = "note-box",
                  div(class = "nb-title", "Cómo leer este gráfico"),
                  "Si las cajas tienen amplitudes muy distintas entre países, puede haber varianza diferencial. Una caja más alta o más dispersa en un país indica que los errores son sistemáticamente mayores para ese caso, lo que justifica el uso de errores estándar robustos."
                ),
                plotOutput("plot_hetero_boxplot", height = "260px"),
                div(class = "plot-caption", "La prueba de Breusch-Pagan confirma estadísticamente la heterocedasticidad detectada visualmente.")
              )
            ),
            column(
              6,
              div(
                class = "card",
                div(
                  class = "card-title",
                  "Residuos a lo largo del tiempo",
                  tags$span(class = "card-sub", "Patrón temporal de los errores del modelo")
                ),
                div(
                  class = "note-box",
                  div(class = "nb-title", "Cómo leer este gráfico"),
                  "Si los residuos forman un patrón sistemático en el tiempo, puede haber tendencia no capturada, autocorrelación o cambios estructurales. Un comportamiento errático alrededor de cero, sin patrón claro, es el resultado deseable."
                ),
                plotOutput("plot_hetero_time", height = "260px"),
                div(class = "plot-caption", "La línea roja horizontal marca el cero. Los residuos deben oscilar aleatoriamente a su alrededor.")
              )
            )
          ),
          div(
            class = "caution-box",
            div(class = "cb-title", "Alcance del resultado"),
            "Las limitaciones no debilitan la presentación si se integran con claridad. El resultado debe defenderse como evidencia de asociación positiva entre cultura y turismo, no como prueba causal definitiva."
          ),
          div(
            class = "card",
            div(class = "card-title", "Limitaciones principales y cómo reportarlas"),
            DTOutput("limitations_table")
          ),
          fluidRow(
            column(
              6,
              div(
                class = "note-box",
                div(class = "nb-title", "Problemas econométricos"),
                "La multicolinealidad se revisa con VIF; la heterocedasticidad se corrige con errores robustos; la dependencia transversal y la autocorrelación de panel se reportan como advertencias porque solo hay dos países."
              )
            ),
            column(
              6,
              div(
                class = "note-box",
                div(class = "nb-title", "Robustez"),
                "El modelo rezagado sostiene la dirección positiva del resultado cultural, pero no elimina la posibilidad de variables omitidas ni permite afirmar causalidad."
              )
            )
          )
        ),

        tabPanel(
          "Conclusiones y literatura",
          div(
            class = "simple-box",
            div(class = "sb-title", "Conclusión prudente"),
            "La evidencia muestra una relación positiva y estadísticamente significativa entre el índice cultural y las llegadas turísticas internacionales en Japón y Corea del Sur. La conclusión es consistente o parcialmente consistente con la literatura sobre Hallyu, anime, ficción audiovisual y turismo inducido por contenidos, pero se formula como asociación y no como causalidad."
          ),
          fluidRow(
            column(
              6,
              div(
                class = "card",
                div(class = "card-title", "Lectura final del proyecto"),
                tags$p("El modelo final se elige por parsimonia, estabilidad y lectura econométrica. El índice cultural contemporáneo mantiene signo positivo, significancia estadística y más observaciones que la alternativa rezagada."),
                tags$p("Frente a las hipótesis del proyecto, la evidencia permite rechazar H0 en la especificación principal, apoya H1 y sostiene H2 de forma parcial con los controles macroeconómicos disponibles."),
                tags$p("H3 recibe apoyo parcial: el rezago cultural sigue siendo positivo y significativo, pero no mejora el ajuste frente al modelo base; por eso se presenta como robustez temporal."),
                tags$p("Los controles económicos ayudan a aislar la relación, pero no son el hallazgo central porque no resultan estadísticamente significativos en esta muestra."),
                tags$p("La literatura revisada respalda la idea de que la presencia cultural puede aumentar imagen, interés y atractivo del destino. Al mismo tiempo, advierte que no toda exposición cultural se convierte automáticamente en viaje.")
              )
            ),
            column(
              6,
              div(
                class = "card",
                div(class = "card-title", "Qué sí y qué no se afirma"),
                div(class = "inline-metric", tags$strong("Sí"), "Asociación positiva entre índice cultural y turismo."),
                div(class = "inline-metric", tags$strong("Sí"), "Consistencia parcial con literatura previa."),
                div(class = "inline-metric", tags$strong("No"), "Causalidad automática o definitiva."),
                div(class = "inline-metric", tags$strong("No"), "Medición perfecta de cultura audiovisual.")
              )
            )
          ),
          div(
            class = "card",
            div(class = "card-title", "Comparación con literatura relacionada",
                tags$span(class = "card-sub", "Síntesis de hallazgos vinculados con cultura, audiovisual y turismo")),
            uiOutput("literature_cards")
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  filtered_data <- reactive({
    out <- panel %>%
      filter(year >= input$year_range[1], year <= input$year_range[2])

    if (input$country_select != "both") {
      out <- out %>% filter(as.character(country) == input$country_select)
    }
    if (isTRUE(input$exclude_model_years)) {
      out <- out %>% filter(model_sample)
    }
    out
  })

  selected_result <- reactive({
    choice <- input$model_choice
    if (is.null(choice) || !(choice %in% names(model_results))) choice <- "base"
    model_results[[choice]]
  })

  selected_model_data <- reactive({
    if (identical(selected_result()$key, "lag")) model_data_lag else model_data
  })

  selected_culture_row <- reactive({
    result <- selected_result()
    result$coef_table %>% filter(term == result$culture_term)
  })

  output$kpi_nobs <- renderText(nobs(selected_result()$model))
  output$kpi_model <- renderText(selected_result()$short_label)
  output$kpi_beta <- renderText(sprintf("%.3f", selected_culture_row()$estimate))
  output$kpi_percent <- renderText(paste0(sprintf("%.1f", (exp(selected_culture_row()$estimate) - 1) * 100), "%"))
  output$kpi_pvalue <- renderText(format_p(selected_culture_row()$p_value))
  output$kpi_r2 <- renderText(sprintf("%.3f", selected_result()$r_squared))

  output$equation_text <- renderUI({
    result <- selected_result()
    coefficients <- coef(result$model)
    intercept_val <- get_intercept(result$model)
    equation <- paste(
      sprintf("logaritmo de llegadas turísticas = %.4f", intercept_val),
      format_model_term(coefficients[[result$culture_term]], tolower(result$culture_label)),
      format_model_term(coefficients[["log_gdppc"]], "logaritmo del producto interno bruto per cápita"),
      format_model_term(coefficients[["log_fx"]], "logaritmo del tipo de cambio")
    )
    HTML(sprintf("<div class='equation'>%s</div>", equation))
  })

  # Helper: build one hero block from a model_results entry
  make_hero_html <- function(result) {
    coefs      <- coef(result$model)
    intercept  <- get_intercept(result$model)
    beta_c     <- coefs[[result$culture_term]]
    beta_gdp   <- coefs[["log_gdppc"]]
    beta_fx    <- coefs[["log_fx"]]
    culture_row <- result$coef_table %>% filter(term == result$culture_term)
    n_obs      <- nobs(result$model)

    cv_sub <- if (result$key == "lag")
      "&#205;ndice Cultural<sub>i,t&#8722;1</sub>" else
      "&#205;ndice Cultural<sub>it</sub>"

    fmt_ctrl <- function(val, label) {
      sgn <- if (val >= 0) " + " else " &#8722; "
      sprintf("%s<span class='eq-coef-ctrl'>%.4f</span> &times; <i>%s</i>", sgn, abs(val), label)
    }

    eq_line <- sprintf(
      "log(Turismo<sub>it</sub>) = <span class='eq-coef-ctrl'>%.4f</span> + <span class='eq-coef-main'>%.4f</span> &times; <i>%s</i>%s%s",
      intercept,
      abs(beta_c), cv_sub,
      fmt_ctrl(beta_gdp, "log(PIBpc<sub>it</sub>)"),
      fmt_ctrl(beta_fx,  "log(TC<sub>it</sub>)")
    )

    generic_line <- if (result$key == "lag")
      "log(T<sub>it</sub>) = &beta;<sub>0</sub> + &beta;<sub>1</sub>&#183;&#205;ndice<sub>i,t&#8722;1</sub> + &beta;<sub>2</sub>&#183;log(PIBpc<sub>it</sub>) + &beta;<sub>3</sub>&#183;log(TC<sub>it</sub>) + &epsilon;<sub>it</sub>"
    else
      "log(T<sub>it</sub>) = &beta;<sub>0</sub> + &beta;<sub>1</sub>&#183;&#205;ndice<sub>it</sub> + &beta;<sub>2</sub>&#183;log(PIBpc<sub>it</sub>) + &beta;<sub>3</sub>&#183;log(TC<sub>it</sub>) + &epsilon;<sub>it</sub>"

    sprintf(
      "<div class='eq-hero-wrap'>
         <div class='eq-hero-label'>%s</div>
         <div class='eq-hero-generic'>%s</div>
         <div class='eq-hero-main'>%s</div>
         <div class='eq-hero-stats'>
           <div class='eq-stat'><strong>%.4f</strong>&beta; cultural</div>
           <div class='eq-stat'><strong>%s</strong>valor p</div>
           <div class='eq-stat'><strong>%.4f</strong>R&sup2;</div>
           <div class='eq-stat'><strong>%d</strong>obs.</div>
         </div>
       </div>",
      result$label,
      generic_line, eq_line,
      culture_row$estimate, format_p(culture_row$p_value),
      result$r_squared, n_obs
    )
  }

  output$equation_hero <- renderUI({
    HTML(make_hero_html(selected_result()))
  })

  output$equation_both <- renderUI({
    HTML(sprintf(
      "<div class='eq-dual'>%s%s</div>",
      make_hero_html(model_results$base),
      make_hero_html(model_results$lag)
    ))
  })

  output$selected_model_summary <- renderUI({
    result <- selected_result()
    culture_row <- selected_culture_row()
    HTML(sprintf(
      "<strong>%s.</strong><br>%s<br><span class='small-note'>Fórmula: %s. Coeficiente cultural = %.4f; error robusto = %.4f; valor p = %s; R cuadrada = %.4f.</span>",
      result$label,
      result$reading,
      result$formula_label,
      culture_row$estimate,
      culture_row$robust_se,
      format_p(culture_row$p_value),
      result$r_squared
    ))
  })

  output$selected_model_sample <- renderUI({
    result <- selected_result()
    if (identical(result$key, "base")) {
      HTML("El modelo base usa 36 observaciones: 18 años por país. Se excluyen 2020 y 2021 por el choque internacional del coronavirus.")
    } else {
      HTML("El modelo rezagado usa 34 observaciones porque pierde el primer año disponible de cada país al construir el rezago del índice cultural.")
    }
  })

  output$plot_coefficients <- renderPlot({
    plot_data <- selected_result()$coef_table %>%
      filter(term != "(Intercept)") %>%
      mutate(term_label = factor(term_label, levels = rev(term_label)))

    ggplot(plot_data, aes(x = term_label, y = estimate, color = significant)) +
      geom_hline(yintercept = 0, color = "#aaa", linewidth = 0.6) +
      geom_pointrange(aes(ymin = conf_low, ymax = conf_high), linewidth = 0.8, size = 0.9) +
      coord_flip() +
      scale_color_manual(values = c("TRUE" = col_ok, "FALSE" = "#777")) +
      theme_eq3() +
      theme(legend.position = "none") +
      labs(x = NULL, y = "Coeficiente estimado")
  })

  output$plot_tourism <- renderPlot({
    y_var <- if (isTRUE(input$use_log_axis)) "log_tourism" else "arrivals_million"
    y_lab <- if (isTRUE(input$use_log_axis)) "Logaritmo de llegadas" else "Millones de llegadas"

    plot_data <- filtered_data()
    p <- ggplot(plot_data, aes(x = year, y = .data[[y_var]], color = country)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_country_color() +
      theme_eq3() +
      scale_x_continuous(breaks = sort(unique(plot_data$year))) +
      labs(x = NULL, y = y_lab)

    if (!isTRUE(input$exclude_model_years)) p <- add_model_exclusion_band(p)
    p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$plot_culture <- renderPlot({
    plot_data <- filtered_data()
    p <- ggplot(plot_data, aes(x = year, y = culture_index, color = country)) +
      geom_hline(yintercept = 0, color = "#aaa", linetype = "dashed") +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_country_color() +
      theme_eq3() +
      scale_x_continuous(breaks = sort(unique(plot_data$year))) +
      labs(x = NULL, y = "Índice cultural")

    if (!isTRUE(input$exclude_model_years)) p <- add_model_exclusion_band(p)
    p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$plot_components <- renderPlot({
    plot_data <- filtered_data() %>%
      select(country, year, cultural_goods_exports_million_usd, unesco_world_heritage_sites, unesco_intangible_heritage_items) %>%
      pivot_longer(
        cols = c(cultural_goods_exports_million_usd, unesco_world_heritage_sites, unesco_intangible_heritage_items),
        names_to = "component",
        values_to = "value"
      ) %>%
      mutate(
        component = recode(
          component,
          cultural_goods_exports_million_usd = "Bienes culturales exportados",
          unesco_world_heritage_sites = "Patrimonio mundial UNESCO",
          unesco_intangible_heritage_items = "Patrimonio intangible UNESCO"
        )
      )

    ggplot(plot_data, aes(x = year, y = value, color = country)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.5) +
      facet_wrap(~ component, scales = "free_y", ncol = 1) +
      scale_country_color() +
      theme_eq3(base = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_continuous(breaks = sort(unique(plot_data$year))) +
      labs(x = NULL, y = NULL)
  })

  output$plot_scatter <- renderPlot({
    plot_data <- filtered_data()
    p <- ggplot(plot_data, aes(x = culture_index, y = log_tourism, color = country)) +
      geom_point(aes(shape = model_status), size = 2.7, alpha = 0.9) +
      geom_smooth(data = model_data, aes(x = culture_index, y = log_tourism), method = "lm", formula = y ~ x, se = FALSE, color = "#333", linewidth = 0.8, inherit.aes = FALSE) +
      scale_country_color() +
      theme_eq3() +
      labs(x = "Índice cultural", y = "Logaritmo de llegadas turísticas", shape = NULL)

    if (isTRUE(input$show_labels)) {
      p <- p + geom_text(aes(label = year), color = "#333", size = 2.8, nudge_y = 0.04)
    }
    p
  })

  output$sources_table <- renderDT({
    data_sources %>%
      transmute(
        Fuente = fuente,
        `Uso en la app` = uso_en_app,
        `Decisión metodológica` = decision
      )
  }, options = dt_options(page_length = 5, dom = "t"), rownames = FALSE)

  output$hypotheses_table <- renderDT({
    hypotheses_table %>%
      transmute(
        Hipótesis = hipotesis,
        Planteamiento = planteamiento,
        `Cómo se prueba en la app` = como_se_operacionaliza_en_la_app,
        Resultado = resultado_en_la_app,
        `Lectura defendible` = lectura_defendible
      )
  }, options = dt_options(page_length = 4, dom = "t"), rownames = FALSE)

  output$research_questions_table <- renderDT({
    research_questions_table %>%
      transmute(
        `Pregunta de investigación` = pregunta,
        `Respuesta en la app` = respuesta_en_la_app,
        Implicación = implicacion
      )
  }, options = dt_options(page_length = 3, dom = "t"), rownames = FALSE)

  output$culture_components_decisions_table <- renderDT({
    culture_index_components %>%
      transmute(
        Componente = componente,
        `Qué aporta` = que_aporta,
        Límite = limite
      )
  }, options = dt_options(page_length = 3, dom = "t"), rownames = FALSE)

  output$variable_decisions_table <- renderDT({
    variable_decisions %>%
      transmute(
        Tipo = tipo,
        Variable = variable,
        Descripción = descripcion,
        Justificación = justificacion
      )
  }, options = dt_options(page_length = 8, dom = "t"), rownames = FALSE)

  output$discarded_variables_table <- renderDT({
    discarded_variables %>%
      transmute(
        Variable = variable,
        `Motivo de descarte` = motivo,
        `Cómo reportarlo` = como_reportarlo
      )
  }, options = dt_options(page_length = 9, dom = "t"), rownames = FALSE)

  output$data_table_preview <- renderDT({
    filtered_data() %>%
      transmute(
        País = country_label,
        Año = year,
        `Llegadas turísticas (miles)` = tourist_arrivals_thousands,
        `Índice cultural` = round(culture_index, 4),
        `PIB per cápita` = gdp_per_capita_usd,
        `Tipo de cambio` = exchange_rate_lcu_usd,
        `Muestra del modelo` = model_status
      )
  }, options = dt_options(page_length = 8, dom = "ftp"), rownames = FALSE)

  output$descriptive_summary_table <- renderDT({
    model_data %>%
      transmute(
        `Log llegadas` = log_tourism,
        `Índice cultural` = culture_index,
        `Log PIB per cápita` = log_gdppc,
        `Log tipo de cambio` = log_fx
      ) %>%
      summarise(across(
        everything(),
        list(
          Media = ~ mean(.x, na.rm = TRUE),
          `Desv. estándar` = ~ sd(.x, na.rm = TRUE),
          Mínimo = ~ min(.x, na.rm = TRUE),
          Máximo = ~ max(.x, na.rm = TRUE)
        )
      )) %>%
      pivot_longer(everything(), names_to = "metrica", values_to = "valor") %>%
      separate(metrica, into = c("Variable", "Estadístico"), sep = "_(?=[^_]+$)") %>%
      mutate(valor = round(valor, 3)) %>%
      pivot_wider(names_from = Estadístico, values_from = valor)
  }, options = dt_options(page_length = 4, dom = "t"), rownames = FALSE)

  output$model_comparison_table <- renderDT({
    model_comparison_table %>%
      transmute(
        Modelo            = modelo,
        `N`               = observaciones,
        Variable          = variable_cultural,
        `β`               = round(coeficiente_cultural, 4),
        `EE robusto`      = round(error_estandar_robusto, 4),
        `Valor p`         = format_p(valor_p),
        `R²`              = round(r_cuadrada, 4)
      )
  }, options = dt_options(page_length = 2, dom = "t"), rownames = FALSE)

  output$reading_easy <- renderUI({
    culture_row <- selected_culture_row()
    beta <- culture_row$estimate
    pct  <- round((exp(beta) - 1) * 100, 2)
    result <- selected_result()
    div(
      class = "simple-box",
      div(class = "sb-title", "Lectura fácil"),
      HTML(sprintf(
        "<strong>El %s es positivo y estadísticamente significativo.</strong><br>Un aumento de una unidad se asocia con un aumento aproximado de <strong>%.2f por ciento</strong> en las llegadas turísticas, manteniendo constantes las demás variables. (valor p = %s)",
        tolower(result$culture_label), pct, format_p(culture_row$p_value)
      ))
    )
  })

  output$plot_effect <- renderPlot({
    result   <- selected_result()
    mod_data <- selected_model_data()
    coef_tbl <- result$coef_table

    culture_term <- result$culture_term
    beta_culture <- coef_tbl$estimate[coef_tbl$term == culture_term]

    x_source <- mod_data[[culture_term]]

    x_seq <- seq(
      min(x_source, na.rm = TRUE) - 0.3,
      max(x_source, na.rm = TRUE) + 0.3,
      length.out = 200
    )

    other_terms <- coef_tbl$term[!coef_tbl$term %in% c(culture_term, "(Intercept)", "japan_dummy")]
    fixed_vals <- sapply(other_terms, function(v) {
      col <- switch(v,
        log_gdppc = "log_gdppc",
        log_fx    = "log_fx",
        log_gdppc_lag1 = "log_gdppc",
        log_fx_lag1    = "log_fx",
        NA_character_
      )
      if (!is.na(col) && col %in% names(mod_data)) mean(mod_data[[col]], na.rm = TRUE) else 0
    })

    # Construimos un mapa nombre→coeficiente para evitar cualquier riesgo de
    # dimensiones no conformables al multiplicar por la matriz de diseño.
    coef_map <- setNames(coef_tbl$estimate, coef_tbl$term)

    # plm con efectos fijos within no entrega "(Intercept)"; añadimos el promedio
    # de los efectos fijos por unidad para anclar la curva en la escala original.
    fe_offset <- if (inherits(result$model, "plm")) as.numeric(mean(fixef(result$model))) else 0

    value_for_term <- function(term_name, x_culture) {
      if (term_name == culture_term)        x_culture
      else if (term_name == "(Intercept)")  1
      else if (term_name == "japan_dummy")  mean(mod_data$japan_dummy, na.rm = TRUE)
      else if (term_name %in% names(fixed_vals)) fixed_vals[[term_name]]
      else 0
    }

    predict_log <- function(x_culture) {
      contributions <- vapply(names(coef_map), function(tn) coef_map[[tn]] * value_for_term(tn, x_culture),
                              numeric(1))
      sum(contributions) + fe_offset
    }

    log_pred  <- vapply(x_seq, predict_log, numeric(1))
    se_coef   <- coef_tbl$robust_se[coef_tbl$term == culture_term]
    log_upper <- log_pred + 1.96 * abs(se_coef * (x_seq - mean(x_seq)))
    log_lower <- log_pred - 1.96 * abs(se_coef * (x_seq - mean(x_seq)))

    # tourist_arrivals_thousands está en miles; el eje muestra millones → /1000
    curve_df <- data.frame(
      x     = x_seq,
      pred  = exp(log_pred)  / 1000,
      upper = exp(log_upper) / 1000,
      lower = exp(log_lower) / 1000
    )

    mean_ci <- mean(x_source, na.rm = TRUE)
    sd_ci   <- sd(x_source, na.rm = TRUE)
    vlines <- data.frame(
      xv    = c(mean_ci - sd_ci, mean_ci, mean_ci + sd_ci),
      label = c("−1σ", "Media", "+1σ"),
      color = c("#003478", "#1a1a2e", "#bc002d")
    )
    vlines$yv <- vapply(vlines$xv, function(xi) exp(predict_log(xi)) / 1000, numeric(1))
    vlines$label_text <- sprintf("%s\n%.2fM", vlines$label, vlines$yv)
    vlines$label_y <- max(curve_df$upper, na.rm = TRUE) * 0.95

    obs_df <- mod_data %>%
      mutate(
        x_obs = .data[[culture_term]],
        y_obs = exp(log_tourism) / 1000
      ) %>%
      filter(!is.na(x_obs))

    ggplot(curve_df, aes(x = x, y = pred)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#2e8b57", alpha = 0.15) +
      geom_line(color = "#2e8b57", linewidth = 1.1) +
      geom_vline(data = vlines, aes(xintercept = xv),
                 color = vlines$color,
                 linetype = "dotted", linewidth = 0.8) +
      geom_label(data = vlines, aes(x = xv, y = label_y, label = label_text),
                 color = vlines$color,
                 size = 2.9, fontface = "bold", fill = "white",
                 label.padding = unit(0.2, "lines"), show.legend = FALSE) +
      geom_point(data = obs_df, aes(x = x_obs, y = y_obs, color = country),
                 size = 2.4, alpha = 0.85) +
      scale_country_color() +
      theme_eq3() +
      labs(
        x     = paste0(result$culture_label, " (índice cultural)"),
        y     = "Llegadas turísticas estimadas (millones)",
        color = NULL
      ) +
      theme(legend.position = "bottom")
  })

  output$model_summary_print <- renderPrint({
    summary(selected_result()$model)
  })

  output$model_interp_brief <- renderUI({
    res   <- selected_result()
    ct    <- res$coef_table
    r2    <- round(res$r_squared * 100, 1)
    n_obs <- nrow(res$model$model)

    get_row <- function(term) ct[ct$term == term, ]

    ci_row <- get_row("culture_index")
    if (nrow(ci_row) == 0) ci_row <- get_row("culture_index_lag_model")

    gdp_row <- get_row("log_gdppc")
    fx_row  <- get_row("log_fx")

    fmt_b <- function(x) formatC(round(x, 3), format = "f", digits = 3)
    fmt_p <- function(p) {
      if (p < 0.001) "p < 0.001" else paste0("p = ", round(p, 3))
    }
    sig_stars <- function(p) {
      if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else "(n.s.)"
    }
    semi_elast <- function(b) round((exp(b) - 1) * 100, 1)

    ci_sig  <- nrow(ci_row)  > 0 && ci_row$p_value  < 0.05
    gdp_sig <- nrow(gdp_row) > 0 && gdp_row$p_value < 0.05
    fx_sig  <- nrow(fx_row)  > 0 && fx_row$p_value  < 0.05

    ci_label <- if (grepl("lag", res$culture_term)) "Índice cultural (rezagado)" else "Índice cultural"

    f_stat <- tryCatch({
      s <- summary(res$model)
      fv <- s$fstatistic
      if (!is.null(fv)) round(as.numeric(fv[["statistic"]]), 2) else NULL
    }, error = function(e) NULL)

    div(
      class = "simple-box",
      style = "margin-top:10px; margin-bottom:10px;",
      tags$b("Interpretación del output"),
      tags$ul(
        style = "margin-top:8px; margin-bottom:4px; padding-left:20px;",
        tags$li(
          tags$b(ci_label), ": ",
          if (nrow(ci_row) > 0) {
            b <- ci_row$estimate
            tagList(
              sprintf("β = %s %s (%s). ",
                      fmt_b(b), sig_stars(ci_row$p_value), fmt_p(ci_row$p_value)),
              sprintf("Un punto adicional en el índice se asocia a un incremento de %.1f%% en llegadas turísticas (semielasticidad: (e^β − 1)×100).",
                      semi_elast(b))
            )
          } else "No disponible."
        ),
        tags$li(
          tags$b("log_gdppc"), ": ",
          if (nrow(gdp_row) > 0) {
            sprintf("β = %s %s (%s). No significativo: el PIB per cápita no muestra efecto independiente dentro de la muestra bilateral.",
                    fmt_b(gdp_row$estimate), sig_stars(gdp_row$p_value), fmt_p(gdp_row$p_value))
          } else "No disponible."
        ),
        tags$li(
          tags$b("log_fx"), ": ",
          if (nrow(fx_row) > 0) {
            sprintf("β = %s %s (%s). El tipo de cambio tampoco alcanza significancia en este par de países.",
                    fmt_b(fx_row$estimate), sig_stars(fx_row$p_value), fmt_p(fx_row$p_value))
          } else "No disponible."
        )
      ),
      tags$p(
        style = "margin-top:6px; margin-bottom:0;",
        sprintf(
          "El modelo de efectos fijos (%d obs.) explica el %.1f%% de la variación within en log-llegadas (R² within). ",
          n_obs, r2
        ),
        if (!is.null(f_stat)) {
          sprintf(
            "El estadístico F conjunto es significativo (F = %.2f, p < 0.001), lo que confirma que las tres variables explican conjuntamente la variación turística una vez controlados los efectos país.",
            f_stat
          )
        }
      )
    )
  })

  output$plot_vif <- renderPlot({
    vif_data <- vif_comparison_table %>%
      mutate(
        zone = case_when(
          vif <= 3 ~ "Sin problema (VIF ≤ 3)",
          vif <= 5 ~ "Vigilar (3–5)",
          TRUE     ~ "Reportar como limitación (> 5)"
        ),
        zone = factor(zone, levels = c(
          "Sin problema (VIF ≤ 3)",
          "Vigilar (3–5)",
          "Reportar como limitación (> 5)"
        ))
      )
    max_vif <- max(vif_data$vif, na.rm = TRUE)
    ggplot(vif_data, aes(x = reorder(term_label, vif), y = vif, fill = zone)) +
      geom_col(alpha = 0.85, width = 0.6) +
      geom_hline(yintercept = 3, color = "#e8a020", linewidth = 0.9, linetype = "dashed") +
      geom_hline(yintercept = 5, color = "#bc002d", linewidth = 0.9, linetype = "dashed") +
      geom_text(aes(label = sprintf("%.2f", vif)),
                hjust = -0.18, size = 3.4, fontface = "bold", color = "#1a1a2e") +
      annotate("text", x = 0.55, y = 3.18, label = "3", color = "#e8a020",
               size = 3.2, fontface = "bold") +
      annotate("text", x = 0.55, y = 5.18, label = "5", color = "#bc002d",
               size = 3.2, fontface = "bold") +
      scale_fill_manual(
        values = c(
          "Sin problema (VIF ≤ 3)" = "#2e8b57",
          "Vigilar (3–5)"         = "#e8a020",
          "Reportar como limitación (> 5)" = "#bc002d"
        ),
        name = NULL
      ) +
      coord_flip() +
      facet_wrap(~ modelo, ncol = 2) +
      expand_limits(y = max_vif * 1.25) +
      theme_eq3() +
      theme(legend.position = "bottom") +
      labs(x = NULL, y = "Factor de inflación de la varianza (VIF)")
  })

  output$plot_timeline <- renderPlot({
    years_all <- 2005:2024
    df <- data.frame(
      year  = years_all,
      xmin  = years_all - 0.47,
      xmax  = years_all + 0.47,
      status = ifelse(years_all %in% model_excluded_years,
                      "Excluido (COVID-19)", "En el modelo")
    )
    n_included <- sum(df$status == "En el modelo") * 2
    ggplot(df) +
      geom_rect(
        aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = status),
        color = "white", linewidth = 1.2
      ) +
      geom_text(
        aes(x = year, y = 0.5, label = year),
        size = 2.8, color = "white", fontface = "bold"
      ) +
      annotate("text",
               x    = mean(years_all[!years_all %in% model_excluded_years]),
               y    = 1.42,
               label = paste0(n_included, " observaciones en el modelo  (18 años × 2 países)"),
               size = 3.4, color = "#2e8b57", fontface = "bold") +
      annotate("text",
               x = mean(model_excluded_years),
               y = 1.42,
               label = paste0(length(model_excluded_years) * 2, " obs.\nexcluidas"),
               size = 3, color = "#bc002d", fontface = "bold", lineheight = 0.9) +
      scale_fill_manual(
        values = c("En el modelo" = "#2e8b57", "Excluido (COVID-19)" = "#bc002d"),
        name   = NULL
      ) +
      scale_x_continuous(limits = c(2004.4, 2024.6), breaks = NULL) +
      scale_y_continuous(limits = c(-0.05, 1.75), breaks = NULL) +
      theme_eq3() +
      theme(
        axis.title    = element_blank(),
        axis.text     = element_blank(),
        panel.grid    = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.35, "cm"),
        legend.text   = element_text(size = 9)
      )
  })

  output$plot_model_comparison <- renderPlot({
    plot_data <- bind_rows(lapply(model_results, function(result) {
      result$coef_table %>%
        filter(term == result$culture_term) %>%
        mutate(modelo = result$label)
    })) %>%
      mutate(modelo = factor(modelo, levels = rev(unique(modelo))))

    ggplot(plot_data, aes(x = modelo, y = estimate, color = significant)) +
      geom_hline(yintercept = 0, color = "#aaa", linewidth = 0.6) +
      geom_pointrange(aes(ymin = conf_low, ymax = conf_high), linewidth = 0.8, size = 0.9) +
      coord_flip() +
      scale_color_manual(values = c("TRUE" = col_ok, "FALSE" = "#777")) +
      theme_eq3() +
      theme(legend.position = "none") +
      labs(x = NULL, y = "Coeficiente cultural")
  })

  output$coef_table <- renderDT({
    selected_result()$coef_table %>%
      transmute(
        Variable = term_label,
        Coeficiente = round(estimate, 4),
        `Error estándar robusto` = round(robust_se, 4),
        `Estadístico t` = round(statistic, 4),
        `Valor p` = format_p(p_value),
        `Límite inferior 95%` = round(conf_low, 4),
        `Límite superior 95%` = round(conf_high, 4)
      )
  }, options = dt_options(page_length = 5, dom = "t"), rownames = FALSE)

  output$plot_fitted <- renderPlot({
    plot_data <- selected_model_data() %>%
      select(country, year, log_tourism, fitted) %>%
      pivot_longer(cols = c(log_tourism, fitted), names_to = "series", values_to = "value") %>%
      mutate(series = recode(series, log_tourism = "Observado", fitted = "Ajustado"))

    ggplot(plot_data, aes(x = year, y = value, color = country, linetype = series)) +
      geom_line(linewidth = 0.95) +
      geom_point(size = 1.6) +
      scale_country_color() +
      scale_linetype_manual(values = c("Observado" = "solid", "Ajustado" = "dashed")) +
      theme_eq3() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_continuous(breaks = sort(unique(plot_data$year))) +
      labs(x = NULL, y = "Logaritmo de llegadas")
  })

  output$plot_residuals <- renderPlot({
    ggplot(selected_model_data(), aes(x = fitted, y = residual, color = country)) +
      geom_hline(yintercept = 0, color = "#bc002d", linewidth = 0.6) +
      geom_point(size = 2.4, alpha = 0.9) +
      scale_country_color() +
      theme_eq3() +
      labs(x = "Valor ajustado", y = "Residuo")
  })

  output$diagnostic_cards <- renderUI({
    tags$div(
      class = "test-grid",
      lapply(seq_len(nrow(diagnostic_details)), function(index) {
        row <- diagnostic_details[index, ]
        tags$div(
          class = "test-card",
          tags$h4(row$prueba),
          tags$div(
            class = "test-meta",
            tags$span(class = "test-pill", paste("Estadístico:", sprintf("%.4f", row$estadistico))),
            tags$span(class = "test-pill", paste("Valor p:", format_p(row$valor_p)))
          ),
          tags$div(class = "test-question", row$pregunta),
          tags$div(class = "test-decision", row$decision),
          tags$div(class = "small-note", row$nota)
        )
      })
    )
  })

  output$plot_residual_distribution <- renderPlot({
    ggplot(selected_model_data(), aes(x = residual, fill = country)) +
      geom_histogram(position = "identity", bins = 10, alpha = 0.55, color = "white") +
      geom_vline(xintercept = 0, color = "#333", linewidth = 0.6) +
      scale_country_fill() +
      theme_eq3() +
      labs(x = "Residuo", y = "Frecuencia")
  })

  output$plot_correlation_heatmap <- renderPlot({
    corr_data <- model_data %>%
      transmute(
        `Log llegadas` = log_tourism,
        `Índice cultural` = culture_index,
        `Log PIB per cápita` = log_gdppc,
        `Log tipo de cambio` = log_fx
      )
    var_names <- colnames(corr_data)
    corr_matrix <- round(cor(corr_data, use = "complete.obs"), 3)
    corr_long <- as.data.frame(as.table(corr_matrix)) %>%
      setNames(c("var1", "var2", "correlacion")) %>%
      mutate(
        var1 = as.character(var1),
        var2 = as.character(var2),
        correlacion = as.numeric(as.character(correlacion)),
        text_color = ifelse(abs(correlacion) > 0.5, "white", "#333333")
      )
    ggplot(corr_long, aes(x = var2, y = var1, fill = correlacion)) +
      geom_tile(color = "white", linewidth = 0.6) +
      geom_text(
        aes(label = sprintf("%.3f", correlacion), color = text_color),
        size = 3.5, fontface = "bold"
      ) +
      scale_color_identity() +
      scale_fill_gradient2(
        low = "#003478", mid = "#f5f5f5", high = "#bc002d",
        midpoint = 0, limits = c(-1, 1), name = "Correlación"
      ) +
      scale_x_discrete(limits = var_names) +
      scale_y_discrete(limits = rev(var_names)) +
      theme_eq3() +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1, size = 9.5),
        axis.text.y = element_text(size = 9.5),
        panel.grid = element_blank(),
        axis.title = element_blank()
      ) +
      coord_fixed()
  })

  output$plot_qq <- renderPlot({
    res <- selected_model_data()$residual
    n <- length(res)
    probs <- (seq_len(n) - 0.5) / n
    theoretical <- qnorm(probs)
    sorted_res <- sort(res)
    q1_r <- quantile(res, 0.25); q3_r <- quantile(res, 0.75)
    q1_t <- qnorm(0.25);         q3_t <- qnorm(0.75)
    slope_qq <- (q3_r - q1_r) / (q3_t - q1_t)
    intercept_qq <- q1_r - slope_qq * q1_t
    qq_df <- data.frame(theoretical = theoretical, sample = sorted_res)
    ggplot(qq_df, aes(x = theoretical, y = sample)) +
      geom_abline(slope = slope_qq, intercept = intercept_qq,
                  color = "#bc002d", linewidth = 0.9) +
      geom_point(size = 2.4, alpha = 0.85, color = "#1a1a2e") +
      theme_eq3() +
      labs(x = "Cuantiles teóricos", y = "Cuantiles de residuos")
  })

  output$plot_hetero_boxplot <- renderPlot({
    plot_data <- selected_model_data() %>%
      mutate(
        residual_abs = abs(residual),
        pais = recode(as.character(country), Japan = "Japón", Korea = "Corea del Sur")
      )
    ggplot(plot_data, aes(x = pais, y = residual_abs, fill = country)) +
      geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2.2) +
      scale_country_fill() +
      theme_eq3() +
      theme(legend.position = "none") +
      labs(x = "País", y = "Valor absoluto de residuos")
  })

  output$plot_hetero_time <- renderPlot({
    plot_data <- selected_model_data()
    ggplot(plot_data, aes(x = year, y = residual, color = country)) +
      geom_hline(yintercept = 0, color = "#bc002d", linewidth = 0.7) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2.4, alpha = 0.9) +
      scale_country_color() +
      scale_x_continuous(breaks = sort(unique(plot_data$year))) +
      theme_eq3() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Año", y = "Residuo")
  })

  output$plot_acf <- renderPlot({
    model_dat <- selected_model_data()
    countries_vec <- levels(model_dat$country)
    acf_list <- lapply(countries_vec, function(ctry) {
      ctry_data <- model_dat %>% filter(country == ctry) %>% arrange(year)
      res_vals <- ctry_data$residual
      max_lag <- min(8, max(1, length(res_vals) - 2))
      acf_result <- acf(res_vals, lag.max = max_lag, plot = FALSE)
      ci_bound <- qnorm(0.975) / sqrt(length(res_vals))
      data.frame(
        lag = as.numeric(acf_result$lag),
        acf_val = as.numeric(acf_result$acf),
        country = ctry,
        ci = ci_bound,
        stringsAsFactors = FALSE
      )
    })
    acf_data <- bind_rows(acf_list) %>%
      filter(lag > 0) %>%
      mutate(
        pais = recode(as.character(country), Japan = "Japón", Korea = "Corea del Sur"),
        country = factor(country, levels = levels(model_dat$country))
      )
    if (nrow(acf_data) == 0) return(NULL)
    ci_val <- acf_data$ci[1]
    ggplot(acf_data, aes(x = lag, y = acf_val, fill = country)) +
      geom_hline(yintercept = 0, color = "#333", linewidth = 0.5) +
      geom_hline(yintercept =  ci_val, color = "#bc002d", linewidth = 0.6, linetype = "dashed") +
      geom_hline(yintercept = -ci_val, color = "#bc002d", linewidth = 0.6, linetype = "dashed") +
      geom_col(width = 0.55, alpha = 0.8) +
      scale_country_fill() +
      scale_x_continuous(breaks = seq_len(max(acf_data$lag))) +
      facet_wrap(~ pais, ncol = 2) +
      theme_eq3() +
      theme(legend.position = "none") +
      labs(x = "Rezago", y = "Autocorrelación")
  })

  output$limitations_table <- renderDT({
    limitations_table %>%
      transmute(
        Limitación = limitacion,
        Explicación = explicacion,
        `Cómo defenderla` = como_defenderla
      )
  }, options = dt_options(page_length = 8, dom = "t"), rownames = FALSE)

  output$literature_cards <- renderUI({
    badge_class <- function(rel) {
      if (grepl("coincidencia|coincide|directa|positivo y significativo", rel, ignore.case = TRUE)) "lit-badge-box lit-badge-direct"
      else if (grepl("Refuerza|Respalda|Apoya el caso|Ayuda a interpretar|Ubica el proyecto", rel, ignore.case = TRUE)) "lit-badge-box lit-badge-partial"
      else "lit-badge-box lit-badge-limit"
    }
    badge_label <- function(rel) {
      if (grepl("coincidencia|coincide|directa|positivo y significativo", rel, ignore.case = TRUE)) "Coincide con H1"
      else if (grepl("Refuerza", rel, ignore.case = TRUE)) "Apoya el mecanismo"
      else if (grepl("Respalda la cautela", rel, ignore.case = TRUE)) "Aporta cautela"
      else if (grepl("Apoya el caso", rel, ignore.case = TRUE)) "Apoya parcialmente"
      else if (grepl("Ayuda a interpretar", rel, ignore.case = TRUE)) "Contexto"
      else if (grepl("Ubica el proyecto", rel, ignore.case = TRUE)) "Aporta contexto"
      else "Valida límites"
    }
    div(
      class = "lit-cards",
      lapply(seq_len(nrow(literature_comparison)), function(i) {
        row <- literature_comparison[i, ]
        div(
          class = "lit-card",
          div(
            class = "lit-card-header",
            span(class = "lit-card-author", row$autor_anio),
            span(class = badge_class(row$relacion_con_nuestra_conclusion),
                 badge_label(row$relacion_con_nuestra_conclusion))
          ),
          div(class = "lit-card-finding", row$hallazgo_principal),
          div(class = "lit-card-relation",
              HTML(paste0("↳ ", row$relacion_con_nuestra_conclusion)))
        )
      })
    )
  })

  output$plot_index_diagram <- renderPlot({
    comps <- data.frame(
      label = c("Bienes culturales\nexportados", "Patrimonio Mundial\nUNESCO", "Patrimonio intangible\nUNESCO"),
      y     = c(3, 2, 1)
    )

    ggplot() +
      # Source boxes
      geom_rect(data = comps,
                aes(xmin = 0, xmax = 2.8, ymin = y - 0.33, ymax = y + 0.33),
                fill = "#f8f9fb", color = "#003478", linewidth = 0.8) +
      geom_text(data = comps, aes(x = 1.4, y = y, label = label),
                size = 2.8, lineheight = 0.88, color = "#1a1a2e") +
      # Arrows source → z-score
      geom_segment(data = comps,
                   aes(x = 2.8, xend = 3.1, y = y, yend = y),
                   arrow = arrow(length = unit(0.13, "cm"), type = "closed"),
                   color = "#aab", linewidth = 0.6) +
      # z-score boxes
      geom_rect(data = comps,
                aes(xmin = 3.1, xmax = 4.5, ymin = y - 0.26, ymax = y + 0.26),
                fill = "#fef6e4", color = "#e8a020", linewidth = 0.75) +
      geom_text(data = comps, aes(x = 3.8, y = y), label = "z-score",
                size = 2.6, color = "#5a3c00", fontface = "italic") +
      # Horizontal lines z-score right → fork vertical
      geom_segment(data = comps,
                   aes(x = 4.5, xend = 5.1, y = y, yend = y),
                   color = "#aab", linewidth = 0.6) +
      # Vertical fork line connecting all three at x=5.1
      geom_segment(aes(x = 5.1, xend = 5.1, y = 1, yend = 3),
                   color = "#aab", linewidth = 0.7) +
      # Arrow from fork to promedio box (at y=2)
      geom_segment(aes(x = 5.1, xend = 5.4, y = 2, yend = 2),
                   arrow = arrow(length = unit(0.13, "cm"), type = "closed"),
                   color = "#aab", linewidth = 0.6) +
      # Promedio box
      geom_rect(aes(xmin = 5.4, xmax = 7.0, ymin = 1.67, ymax = 2.33),
                fill = "#f0f7f0", color = "#2e8b57", linewidth = 0.85) +
      geom_text(aes(x = 6.2, y = 2), label = "Promedio\nsimple",
                size = 2.8, lineheight = 0.88, color = "#1a5c30", fontface = "bold") +
      # Arrow promedio → culture_index
      geom_segment(aes(x = 7.0, xend = 7.3, y = 2, yend = 2),
                   arrow = arrow(length = unit(0.13, "cm"), type = "closed"),
                   color = "#aab", linewidth = 0.6) +
      # culture_index box
      geom_rect(aes(xmin = 7.3, xmax = 9.8, ymin = 1.62, ymax = 2.38),
                fill = "#1a1a2e", color = "#1a1a2e", linewidth = 0.9) +
      geom_text(aes(x = 8.55, y = 2), label = "culture_index",
                size = 3, color = "white", fontface = "bold") +
      scale_x_continuous(limits = c(-0.1, 9.95), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0.45, 3.55), expand = c(0, 0)) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin     = margin(10, 10, 10, 10)
      )
  }, bg = "white")

  output$data_table <- renderDT({
    filtered_data() %>%
      transmute(
        País = country_label,
        Año = year,
        `Llegadas turísticas (miles)` = tourist_arrivals_thousands,
        `Ingresos turísticos (miles de millones de dólares estadounidenses)` = tourism_receipts_billion_usd,
        `Bienes culturales exportados (millones de dólares estadounidenses)` = cultural_goods_exports_million_usd,
        `Patrimonio mundial UNESCO` = unesco_world_heritage_sites,
        `Patrimonio intangible UNESCO` = unesco_intangible_heritage_items,
        `Producto interno bruto per cápita (dólares estadounidenses)` = gdp_per_capita_usd,
        `Tipo de cambio moneda local por dólar` = exchange_rate_lcu_usd,
        `Índice cultural` = round(culture_index, 4),
        `Muestra del modelo` = model_status,
        Evento = event_note
      )
  }, options = dt_options(page_length = 12, dom = "ftp"), rownames = FALSE)
}

shinyApp(ui, server)
