options(repos = c(CRAN = "https://cloud.r-project.org"))

# Helper para instalar dependencias cuando se ejecuta la app fuera de Docker.
required_packages <- c(
  "shiny",
  "ggplot2",
  "dplyr",
  "tidyr",
  "scales",
  "DT"
)

install.packages(required_packages, Ncpus = 1L)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0) {
  stop(
    sprintf(
      "No se pudieron instalar los paquetes requeridos: %s",
      paste(missing_packages, collapse = ", ")
    )
  )
}
