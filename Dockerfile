FROM r-base:4.4.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libicu-dev \
    libjpeg-dev \
    libpng-dev \
    libssl-dev \
    libtiff5-dev \
    libuv1-dev \
    libxml2-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY packages.R /tmp/packages.R
RUN Rscript /tmp/packages.R && rm -f /tmp/packages.R

COPY . /app

ENV PORT=3838

EXPOSE 3838

CMD ["R", "--no-save", "--no-restore", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', '3838')))"]
