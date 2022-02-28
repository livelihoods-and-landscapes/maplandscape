FROM jmad1v07/maplandscape-base-4.1.2

## install required R packages
RUN R -e 'install.packages(c("devtools"), \
  repos="http://cran.rstudio.com/")'

RUN R -e 'devtools::install_github("livelihoods-and-landscapes/qfieldcloudR")'
RUN R -e 'devtools::install_github("livelihoods-and-landscapes/maplandscape")'

# copy necessary files
RUN mkdir /root/app
COPY /app/server.R /root/app/
COPY /app/ui.R /root/app/
COPY /app/config.yml /root/app/
COPY /app/global.R /root/app/
COPY /app/www/* /root/app/www/
COPY /app/R/* /root/app/R/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/app', port = 3838, host = '0.0.0.0')"]
