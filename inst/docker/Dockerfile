# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
RUN apt-get update && apt-get install -y  \
gdal-bin \
git-core \
libcurl4-openssl-dev \
libgdal-dev \
libgeos-dev \
libgeos++-dev \
libgit2-dev \
libicu-dev \
libpng-dev \
libproj-dev \
libsqlite3-dev \
sqlite3 \
libssl-dev \
libudunits2-dev \
libxml2-dev \
&& rm -rf /var/lib/apt/lists/*

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
COPY --chown=shiny:shiny app.R /srv/shiny-server/app/

# copy customised shiny-server config to run on Google Cloud Run
COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

# install required R packages
RUN R -e 'install.packages(c( \
  "devtools", "shiny"), \
  repos="http://cran.rstudio.com/")'

RUN R -e 'devtools::install_github("livelihoods-and-landscapes/maplandscape")'

# expose port
EXPOSE 3838

# set user
USER shiny

# run app
CMD ["/usr/bin/shiny-server"]
