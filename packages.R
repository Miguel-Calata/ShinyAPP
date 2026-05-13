options(repos = c(CRAN = "https://cloud.r-project.org"))

# Paquetes usados directamente por app.R
# zoo, car, quantmod están incluidos por si se usan como dependencias transitivas,
# pero app.R no los carga explícitamente con library().
packages <- c(
  "shiny",
  "ggplot2",
  "dplyr",
  "tidyr",
  "scales",
  "DT",
  "plm",
  "sandwich",
  "lmtest",
  "zoo",
  "car",
  "quantmod"
)

missing_packages <- packages[!packages %in% rownames(installed.packages())]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
