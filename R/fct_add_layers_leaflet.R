#' Add a layer to a Leaflet web map
#'
#' \code{add_layers_leaflet} adds a spatial layer (of class \href{https://r-spatial.github.io/sf/index.html}{sf}) to a \link[leaflet]{leaflet} web
#' map. \href{https://r-spatial.github.io/sf/index.html}{sf} data frames with \code{POINT}, \code{LINESTRING}, \code{MULTILINESTRING}, \code{POLYGON}, \code{MULITPOLYGON}
#' geometries can be added to the web map.
#'
#' @param map_object single-element character vector of the ID of the \link[leaflet]{leaflet} map object.
#' @param map_active_df the spatial data frame (of class \href{https://r-spatial.github.io/sf/index.html}{sf}) of features to draw on the \link[leaflet]{leaflet} map.
#' @param map_var the column in \code{map_active_df} of data values to map to fill / marker colours to when drawing the features.
#' @param map_colour fill colour palette - \href{https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf}{RColorBrewer} palettes such as \code{"YlOrRd"}.
#' @param opacity feature opacity - numeric value between 0 and 1.
#' @param map_line_width stroke width for line and polygon features.
#' @param map_line_colour stroke colour for line and polygon features.
#' @param waiter \href{https://waiter.john-coene.com/#/}{waiter} object to display while the map is rendering.
#'
#' @return \link[leaflet]{leaflet} proxy object.
#'
#' @export
#'
#' @importFrom magrittr %>%


add_layers_leaflet <- function(map_object, map_active_df, map_var, map_colour, opacity, map_line_width, map_line_colour, waiter) {
  supported_geometries <- c("POINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")

  if ("sf" %in% class(map_active_df) & is.atomic(map_active_df[[map_var]]) & nrow(map_active_df) > 0) {
    waiter$show()

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
    geometry_type <- sf::st_geometry_type(map_df, by_geometry = FALSE)

    if (geometry_type %in% supported_geometries) {

      # get bounding box for the map
      bbox <- sf::st_bbox(map_df) %>%
        as.vector()

      # make colour palette
      if (class(map_df[[map_var]]) != "numeric" & class(map_df[[map_var]]) != "integer") {
        pal <- leaflet::colorFactor(map_colour, map_df[[map_var]])
      } else {
        pal <- leaflet::colorNumeric(map_colour, map_df[[map_var]])
      }

      # draw polygon layers
      if (geometry_type == "POLYGON" | geometry_type == "MULTIPOLYGON") {
        proxy_map <- leaflet::leafletProxy(map_object, data = map_df) %>%
          leaflet::clearControls() %>%
          leaflet::clearMarkers() %>%
          leaflet::clearShapes() %>%
          leaflet::addPolygons(
            data = map_df,
            layerId = map_df$layer_id,
            weight = map_line_width,
            opacity = 1.0,
            color = map_line_colour,
            fillColor = ~ pal(map_df[[map_var]]),
            fillOpacity = opacity,
            highlightOptions = leaflet::highlightOptions(
              color = "white", weight = 2,
              bringToFront = TRUE
            ),
          ) %>%
          leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      } else if (geometry_type == "LINESTRING" | geometry_type == "MULTILINESTRING") {
        proxy_map <- leaflet::leafletProxy(map_object, data = map_df) %>%
          leaflet::clearControls() %>%
          leaflet::clearMarkers() %>%
          leaflet::clearShapes() %>%
          leaflet::addPolylines(
            data = map_df,
            layerId = map_df$layer_id,
            weight = map_line_width,
            opacity = 1.0,
            color = map_line_colour,
            fillColor = ~ pal(map_df[[map_var]]),
            fillOpacity = opacity,
            highlightOptions = leaflet::highlightOptions(
              color = "white", weight = 2,
              bringToFront = TRUE
            ),
          ) %>%
          leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      } else if (geometry_type == "MULTIPOINT") {

        # cast MULTIPOINT to POINT as Leaflet does not support multipoint
        map_df <- sf::st_cast(map_df, "POINT")

        proxy_map <- leaflet::leafletProxy(map_object, data = map_df) %>%
          leaflet::clearControls() %>%
          leaflet::clearMarkers() %>%
          leaflet::clearShapes() %>%
          leaflet::addPolylines(
            data = map_df,
            layerId = map_df$layer_id,
            options = leaflet::markerOptions(clickable = TRUE)
          ) %>%
          leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      } else if (geometry_type == "POINT") {
        proxy_map <- leaflet::leafletProxy(map_object, data = map_df) %>%
          leaflet::clearControls() %>%
          leaflet::clearMarkers() %>%
          leaflet::clearShapes() %>%
          leaflet::addMarkers(
            data = map_df,
            layerId = map_df$layer_id,
            options = markerOptions(clickable = TRUE)
          ) %>%
          leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      }
      proxy_map
    }
  }

  waiter$hide()
}
