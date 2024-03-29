% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_add_layers_leaflet.R
\name{add_layers_leaflet}
\alias{add_layers_leaflet}
\title{Add a layer to a Leaflet web map}
\usage{
add_layers_leaflet(map_object, map_active_df, map_var, map_colour, waiter)
}
\arguments{
\item{map_object}{single-element character vector of the ID of the \link[leaflet]{leaflet} map object.}

\item{map_active_df}{the spatial data frame (of class \href{https://r-spatial.github.io/sf/index.html}{sf}) of features to draw on the \link[leaflet]{leaflet} map.}

\item{map_var}{the column in \code{map_active_df} of data values to map to fill / marker colours to when drawing the features.}

\item{map_colour}{fill colour palette - \href{https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf}{RColorBrewer} palettes such as \code{"YlOrRd"}.}

\item{waiter}{\href{https://waiter.john-coene.com/#/}{waiter} object to display while the map is rendering.}
}
\value{
\link[leaflet]{leaflet} proxy object.
}
\description{
\code{add_layers_leaflet} adds a spatial layer (of class \href{https://r-spatial.github.io/sf/index.html}{sf}) to a \link[leaflet]{leaflet} web
map. \href{https://r-spatial.github.io/sf/index.html}{sf} data frames with \code{POINT}, \code{LINESTRING}, \code{MULTILINESTRING}, \code{POLYGON}, \code{MULITPOLYGON}
geometries can be added to the web map.
}
