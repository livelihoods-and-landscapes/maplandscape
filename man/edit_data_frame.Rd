% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_edit_data_frame.R
\name{edit_data_frame}
\alias{edit_data_frame}
\title{Apply edits from user to layers in a GeoPackage}
\usage{
edit_data_frame(tmp_edits, df_to_edit, df_to_edit_not_sf, layer)
}
\arguments{
\item{tmp_edits}{Data frame recording user supplied edits (derived from editing DataTables object in the UI).}

\item{df_to_edit}{Data frame or spatial data frame (sf) representing layer in GeoPacakge to apply user supplied edits to.}

\item{df_to_edit_not_sf}{Data frame representing layer in GeoPackage with sf geometry object dropped.}

\item{layer}{Name of layer that edits are being applied to.}
}
\description{
Apply edits from user to layers in a GeoPackage
}
