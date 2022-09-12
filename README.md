# maplandscape

## Overview

A package that provides functions for building [Shiny](https://shiny.rstudio.com) dashboard applications to explore and visualise layers (tables) in GeoPackages. It focuses on providing tools for quick analysis of data collected in-the-field using the [QField](https://qfield.org) mobile GIS application and stored in GeoPackages in QFieldCloud. 

A pre-built [Shiny](https://shiny.rstudio.com) application is provided with this package which uses these functions and provides tools for:

* Exploring GeoPackage layers in interactive tables, web maps, and charts.
* Customising and styling charts and web maps. 
* Combining layers using spatial and non-spatial joins.
* Generating summary tables through group-by and summarise operations. 
* Generating new layers and add new columns to existing layers. 
* Authenticated access to GeoPackages stored in QFieldCloud.

This package was developed as part of the [Livelihoods and Landscapes](https://livelihoods-and-landscapes.com) project which is developing tools and approaches to map diverse agricultural landscapes.  

## Setup

### Local R Package

Clone the maplandscape repo:

```
git clone https://github.com/livelihoods-and-landscapes/maplandscape.git
```

Move into the maplandscape directory:

```
cd maplandscape
```

maplandscape requires an instance of PostGIS. Deploy a local PostGIS instance using docker:

```
cd inst/postgis
docker compose up -d
```

Make sure the PostGIS configuration in `inst/app/config` matches what is specified in the `docker-compose.yml` file. 

Move the root of the maplandscape repo. Launch the app:

```
cd ../..
shiny::runApp("inst/app")
```

### Docker and deploy to the cloud

You can also deploy maplandscape to the cloud using Shiny Proxy, docker, and docker swarm by following the instructions [here](https://github.com/livelihoods-and-landscapes/deploy-maplandscape).
