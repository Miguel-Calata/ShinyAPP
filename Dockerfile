FROM rocker/shiny

WORKDIR /home/app

COPY packages.R /tmp/packages.R
RUN R -e "install.packages(scan('/tmp/packages.R', what='character', sep='\n'), repos='https://cloud.r-project.org')"

COPY . /home/app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', '3838')))"]
