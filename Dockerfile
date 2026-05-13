FROM rocker/r-ver:4.5.3

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

COPY packages.R /app/packages.R
RUN Rscript /app/packages.R

COPY . /app

EXPOSE 3838

CMD R -e "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 3838)))"
