% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_write_tables.R
\name{write_tables}
\alias{write_tables}
\title{Write a layer to a GeoPackage file}
\usage{
write_tables(df, gpkg_dir, lyr)
}
\arguments{
\item{df}{Data frame or spatial data frame (\href{https://r-spatial.github.io/sf/index.html}{sf}) to write to GeoPackage.}

\item{gpkg_dir}{Data frame generated from \code{list_layers} indicating names, file types, and paths to files uploaded tables.}

\item{lyr}{User selected layer - this is used for selecting the relevant layer from a GeoPackage with many tables.}
}
\description{
Write a (spatial - \href{https://r-spatial.github.io/sf/index.html}{sf}) data frame to layer in a GeoPackage.
}
\details{
Designed to be used with Shiny apps where a GeoPackage is in a temporary directory within the app instance.
}
