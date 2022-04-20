#' Add layer to a Leaflet web map - no zoom and with popups enabled.
#'
#' \code{add_layers_leaflet} adds a spatial layer (of class \href{https://r-spatial.github.io/sf/index.html}{sf}) to a \link[leaflet]{leaflet} web
#' map. \href{https://r-spatial.github.io/sf/index.html}{sf} data frames with \code{POINT}, \code{LINESTRING}, \code{MULTILINESTRING}, \code{POLYGON}, \code{MULITPOLYGON}
#' geometries can be added to the web map.
#'
#' Calling this function does not change the zoom or extent of the Leaflet map
#' to fit the contents drawn on the map. Used for keeping the zoom extent the
#' same but updating colour, opacity, and other styling options. This function will allow popup labels to be rendered.
#'
#' @param map_object single-element character vector of the ID of the \link[leaflet]{leaflet} map object.
#' @param map_active_df the spatial data frame (of class \href{https://r-spatial.github.io/sf/index.html}{sf}) of features to draw on the \link[leaflet]{leaflet} map.
#' @param map_var the column in \code{map_active_df} of data values to map fill / marker colours to when drawing the features.
#' @param map_colour fill colour palette - \href{https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf}{RColorBrewer} palettes such as \code{"YlOrRd"}.
#' @param popups character vector of column names to use as popup labels.
#' @param waiter \href{https://waiter.john-coene.com/#/}{waiter} object to display while the map is rendering.
#'
#' @return \link[leaflet]{leaflet} proxy object.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'


add_layers_leafgl_popups <- function(map_object, map_active_df, map_var, map_colour, popups, waiter) {
  supported_geometries <- c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")

  if ("sf" %in% class(map_active_df) & is.atomic(map_active_df[[map_var]]) & nrow(map_active_df) > 0) {

    # Catch GeoPackages with non-spatial tables that GeoPandas has added empty
    # GeometryCollection column to.
    if (any(is.na(sf::st_crs(map_active_df)))) {
      waiter$hide()
      return()
    }

    # make map active layer epsg 4326
    # make this an if statement
    map_df <- try(
      map_active_df %>%
        sf::st_transform(4326)
    )

    if ("try-error" %in% class(map_df)) {
      waiter$hide()
      return()
    }

    # get geometry type of map active layer
    geometry_type <- as.character(sf::st_geometry_type(map_df, by_geometry = FALSE))

    if (geometry_type %in% supported_geometries) {
      waiter$show()

      # make colour palette
      if (class(map_df[[map_var]]) != "numeric" & class(map_df[[map_var]]) != "integer") {
        pal <- leaflet::colorFactor(map_colour, map_df[[map_var]])
      } else {
        pal <- leaflet::colorNumeric(map_colour, map_df[[map_var]])
      }

      # draw polygon layers
      if (geometry_type == "POLYGON" | geometry_type == "MULTIPOLYGON") {
        if (geometry_type == "MULTIPOLYGON") {
          # cast MULTIPOLYGON to POLYGON as leafgl does not support multi*
          map_df <- sf::st_cast(map_df, "POLYGON")
        }

        proxy_map <- leaflet::leafletProxy(map_object, data = map_df) %>%
          leafgl::clearGlLayers() %>%
          leaflet::clearControls() %>%
          leaflet::clearMarkers() %>%
          leaflet::clearShapes() %>%
          leafgl::addGlPolygons(
            data = map_df,
            layerId = map_df$layer_id,
            opacity = 1,
            fillColor = ~ pal(map_df[[map_var]]),
            popup = popups
          )
      } else if (geometry_type == "LINESTRING" | geometry_type == "MULTILINESTRING") {
        if (geometry_type == "MULTILINESTRING") {
          # cast MULTILINESTRING to LINESTRING as leafgl does not support multi*
          map_df <- sf::st_cast(map_df, "LINESTRING")
        }

        proxy_map <- leaflet::leafletProxy(map_object, data = map_df) %>%
          leafgl::clearGlLayers() %>%
          leaflet::clearControls() %>%
          leaflet::clearMarkers() %>%
          leaflet::clearShapes() %>%
          leafgl::addGlPolylines(
            data = map_df,
            layerId = map_df$layer_id,
            opacity = 1,
            color = ~ pal(map_df[[map_var]]),
            popup = popups
          )
      } else {

        # cast MULTIPOINT to POINT as Leaflet does not support multipoint
        map_df <- sf::st_cast(map_df, "POINT")

        proxy_map <- leaflet::leafletProxy(map_object, data = map_df) %>%
          leafgl::clearGlLayers() %>%
          leaflet::clearControls() %>%
          leaflet::clearMarkers() %>%
          leaflet::clearShapes() %>%
          leafgl::addGlPoints(
            data = map_df,
            layerId = map_df$layer_id,
            fillColor = ~ pal(map_df[[map_var]]),
            opacity = 1,
            popup = popups
          )
      }

      waiter$hide()

      proxy_map
    }
  }
}
