FROM r-base:4.5.3

RUN apt-get update && apt-get install -y --no-install-recommends \
    r-cran-dplyr \
    r-cran-dt \
    r-cran-ggplot2 \
    r-cran-scales \
    r-cran-shiny \
    r-cran-tidyr \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY . /app

ENV PORT=3838

EXPOSE 3838

CMD ["R", "--no-save", "--no-restore", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', '3838')))"]
