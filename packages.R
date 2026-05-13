options(repos = c(CRAN = "https://cloud.r-project.org"))

install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

# Dependencias de bajo nivel que deben instalarse primero
install_if_missing(c("fs", "sass", "bslib", "nloptr"))

# Paquetes usados directamente por app.R
# zoo y quantmod se incluyen como dependencias transitivas posibles
install_if_missing(c(
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
  "quantmod"
))
