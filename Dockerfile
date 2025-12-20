FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libsqlite3-dev \
    libmariadb-dev-compat \
    libmariadb-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c(
  'shiny',
  'shinydashboard',
  'shinyjs',
  'DBI',
  'RSQLite',
  'DT',
  'dplyr',
  'lubridate',
  'plotly',
  'digest'
), repos='https://cloud.r-project.org')"

# Copy your app
COPY . /srv/shiny-server/
WORKDIR /srv/shiny-server/

# Expose the port that Shiny uses
EXPOSE 3838

# Run the app
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]