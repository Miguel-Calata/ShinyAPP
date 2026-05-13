FROM rocker/r2u:jammy

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends \
    r-cran-shiny \
    r-cran-ggplot2 \
    r-cran-dplyr \
    r-cran-tidyr \
    r-cran-scales \
    r-cran-dt \
    r-cran-plm \
    r-cran-sandwich \
    r-cran-lmtest \
    && rm -rf /var/lib/apt/lists/*

COPY . /app

EXPOSE 3838

CMD R -e "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 3838)))"
