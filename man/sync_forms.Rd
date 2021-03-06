% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_sync_forms.R
\name{sync_forms}
\alias{sync_forms}
\title{Sync data collected using QField mobile GIS}
\usage{
sync_forms(template, forms)
}
\arguments{
\item{template}{A data frame of the format returned by
\code{\link[shiny]{fileInput}}. This data frame stores the path of a single
GeoPackage which can either be a central database that data collected in
the field is synced to or the "clean" QGS project with no data and an empty
GeoPackage with the same tables as the uploaded forms.}

\item{forms}{A data frame of the format returned by
\code{\link[shiny]{fileInput}}. Links to tables of data collected using
QField mobile GIS to be synced to @param template.}
}
\value{
A fourelement list \code{out_gpkg}, The first element is the path
  to a temporary GeoPackage storing data from \code{forms} synced with \code{
  template}. The second element is the date-time of the call to
  \code{sync_forms} - this is used for naming downloads of synced data. The
  third element is a data frame of the format returned by
  \code{\link[shiny]{fileInput}} listing all layers returned from syncing
  \code{forms} with \code{template}. The fourth element is a path to a
  log file.
}
\description{
\code{sync_forms} syncs data stored in GeoPackages collected using QField
mobile GIS to a central GeoPackage database or the initial "clean" QGS
project.
}
