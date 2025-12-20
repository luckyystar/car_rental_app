FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libmysqlclient-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c(
  'shiny',
  'shinydashboard',
  'shinyjs',
  'DBI',
  'RMySQL',
  'DT',
  'dplyr',
  'lubridate',
  'plotly',
  'digest'
), repos='https://cloud.r-project.org')"

COPY . /srv/shiny-server/

EXPOSE 3838
