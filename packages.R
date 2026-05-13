options(repos = c(CRAN = "https://cloud.r-project.org"))

packages <- c(
  # Cadena de dependencias de shiny (deben resolverse antes)
  "fs",
  "sass",
  "bslib",
  # Paquetes usados directamente por app.R (library())
  "shiny",
  "ggplot2",
  "dplyr",
  "tidyr",
  "scales",
  "DT",
  "plm",
  "sandwich",
  "lmtest",
  # Dependencias transitivas incluidas por precaución
  "zoo",
  "quantmod"
)

missing_packages <- packages[!packages %in% rownames(installed.packages())]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos        = "https://cloud.r-project.org",
    dependencies = TRUE
  )
}
