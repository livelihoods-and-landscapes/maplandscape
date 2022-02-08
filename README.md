# maplandscape

## Overview

A package that provides functions for building [Shiny](https://shiny.rstudio.com) dashboard applications to explore and visualise spatial layers in GeoPackages. In particular, it focuses on providing browser-based tools for analysing data stored in GeoPackages in QFieldCloud. 

A pre-built [Shiny](https://shiny.rstudio.com) application is provided with this package which uses these functions and provides tools for:

* Syncing data stored in multiple GeoPackages.
* Exploring GeoPackage layers in interactive tables, web maps, and charts.
* Customising and styling charts and web maps. 
* Combining layers using spatial and non-spatial joins.
* Generating summary tables through group-by and summarise operations. 
* Generating new layers and add new columns to existing layers. 
* Authenticated access to GeoPackages stored in QFieldCloud and Google Cloud Storage.

This package was developed as part of the [Livelihoods and Landscapes](https://livelihoods-and-landscapes.com) project which is developing tools and approaches to map diverse agricultural landscapes. The initial motivation for developing the package and Shiny application was to provide analysts with tools to analyse and visualise geospatial data collected in-the-field using the [QField](https://qfield.org) mobile GIS application. 

## Setup

### Local R Package

Install as an R package:

```
install.packages("devtools")
devtools::install_github("livelihoods-and-landscapes/maplandscape")
```

Build and customise your own Shiny application using functions provided by maplandscape or launch a pre-built application:

```
dir <- system.file("app", package = "maplandscape")
setwd(dir)
library(maplandscape)
shiny::shinyAppDir(".")
```

To build the package use tools provided by the [devtools](https://github.com/r-lib/devtools) package:

```
# get package dependencies
renv::restore()

# check package
devtools::check()

# update NAMESPACE file for exports
devtools::document() 

# build docs
pkgdown::build_site()
```

The will launch a pre-built Shiny application to explore data in GeoPackages from the `inst/app` sub-directory of the package. 

### shinyapps.io

RStudio offers a free-tier hosting service for Shiny apps. Sign up at [shinyapps.io](https://www.shinyapps.io). 

Follow the above commands to launch maplandscape (or your custom Shiny application). In the top-right of the application window you will see a *Publish* button, click this button and follow the prompts to deploy the application to [shinyapps.io](https://www.shinyapps.io).

<img src="man/figures/shiny-publish.png" width="40%" style="display: block; margin: auto;"/>

RStudio provide a detailed tutorial for deploying apps to shinyapps.io [here](https://shiny.rstudio.com/articles/shinyapps.html).


### Docker and host in the cloud

This requires installing [git](https://git-scm.com/downloads) and [docker](https://www.docker.com). Clone the maplandscape GitHub repo:

```
git clone https://github.com/livelihoods-and-landscapes/maplandscape.git

cd maplandscape
```

#### Docker and Shiny Server

There is a sub-directory named `docker-shiny-server`. This contains a `Dockerfile` that lists instructions that are used to build a docker image.

```
cd /inst/docker-shiny-server

docker build -t maplandscape -f ShinyServer.Dockerfile .
```

The image is based on the `rocker/shiny:latest` image which includes [Shiny Server](https://www.rstudio.com/products/shiny/shiny-server/?_ga=2.240850435.1437924050.1628840494-908324396.1627896044) to host the maplandscape Shiny application. Shiny Server serves apps out of the `srv/shiny-server/` directory; building the docker image will install all the R packages required to run maplandscape, install the maplandscape package from github, and copy an `app.R` script into `srv/shiny-server/app` which contains the commands to launch maplandscape. 

A customised `shiny-customised.config` file is used to set the `app_dir` to  the app directory where Shiny Server will launch and serve the app from. 

Launch the docker container:

```
docker run -p 3838:3838 maplandscape
```

You can find more information about [Shiny Server](https://www.rstudio.com/products/shiny/shiny-server/?_ga=2.240850435.1437924050.1628840494-908324396.1627896044) [here](https://shiny.rstudio.com/articles/shiny-server.html), at its [GitHub repo](https://github.com/rstudio/shiny-server), and following this [tutorial](https://deanattali.com/2015/05/09/setup-rstudio-shiny-server-digital-ocean/). 

Please see the vignette Deploy: Google Cloud for a tutorial demonstrating how to deploy a containerised Shiny application on Google Cloud Run. 

#### Docker 

To deploy as a containerised Shiny application without Shiny Server (e.g. if deploying using Shiny Proxy) use the `Dockerfile` in the `/inst/` directory. This `Dockerfile` is based of a generic `maplandscape-base` image which is pre-built on top of Ubuntu 20.04 LTS and the rocker r-ver 4.1.2 image. It contains the system libraries and main R package dependencies used to build maplandscape. 

```
docker build -t maplandscape .
```
And to run:

```
docker run -p 3838:3838 maplandscape
```

