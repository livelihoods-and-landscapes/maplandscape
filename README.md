# maplandscape


## Use

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

The will launch a pre-built Shiny application to explore data in GeoPackages from the `inst/app` sub-directory of the package. 

### shinyapps.io

RStudio offers a free-tier hosting service for Shiny apps. Sign up at [shinyapps.io](https://www.shinyapps.io). 

Follow the above commands to launch maplandscape (or your custom Shiny application). 



### Docker and host in the cloud

Clone the maplandscape GitHub repo:

```
git clone https://github.com/livelihoods-and-landscapes/maplandscape.git

cd maplandscape
```

There is a sub-directory named `docker`. This contains a `Dockerfile` that lists instructions that are used to build a docker image.

```
docker build -t maplandscape .
```

The image is based on the `rocker/shiny:latest` image which includes [Shiny Server](https://www.rstudio.com/products/shiny/shiny-server/?_ga=2.240850435.1437924050.1628840494-908324396.1627896044) to host the maplandscape Shiny application. Shiny Server serves apps out of the `srv/shiny-server/` directory; building the docker image will install all the R packages required to run maplandscape, install the maplandscape package from github, and copy an `app.R` script into `srv/shiny-server/app` which contains the commands to launch maplandscape. 

A customised `shiny-customised.config` file is used to set the `app_dir` to  the app directory where Shiny Server will launch and serve the app from. 


