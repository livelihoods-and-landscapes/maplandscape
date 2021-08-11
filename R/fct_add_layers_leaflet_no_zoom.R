#' Add layer to \code{Leaflet} web map - no zoom
#'
#' \code{add_layers_leaflet} adds a user selected spatial layer to a Leaflet web
#' map. \code{sf} \code{POINT}, \code{LINESTRING}, \code{MULTILINESTRING},
#' \code{POLYGON}, \code{MULITPOLYGON} geometries can be added to the web map.
#' Calling this function does not change the zoom or extent of the Leaflet map
#' to fit the contents drawn on the map. Used for keeping the zoom extent the
#' same but updating colour and opacity.
#'
#' @param map_object single-element character vector of the ID of the Leaflet map object.
#' @param map_active_df the spatial data frame containing features to draw on the Leaflet map object.
#' @param map_var the column in \code{map_active_df} to map fill / marker colours to data values when drawing the features.
#' @param map_colour fill colour palette - RColorBrewer palettes such as \code{"YlOrRd"}.
#' @param opacity feature opacity - numeric value between 0 and 1.
#' @param map_line_width stroke width for line and polygon features.
#' @param map_line_colour stroke colour for line and polygon features.
#' @param waiter waiter object to display while map is rendering.
#'
#' @return Leaflet proxy object.
#'
#' @wxport
#'
#' @importFrom magrittr %>%
#'

add_layers_leaflet_no_zoom <- function(map_object, map_active_df, map_var, map_colour, opacity, map_line_width, map_line_colour, waiter) {
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
          )
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
          )
      } else if (geometry_type == "MULTIPOINT") {

        # cast MULTIPOINT to POINT as Leaflet does not support multipoint
        map_df <- st_cast(map_df, "POINT")

        proxy_map <- leaflet::leafletProxy(map_object, data = map_df) %>%
          leaflet::clearControls() %>%
          leaflet::clearMarkers() %>%
          leaflet::clearShapes() %>%
          leaflet::addPolylines(
            data = map_df,
            layerId = map_df$layer_id,
            options = markerOptions(clickable = TRUE)
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
          )
      }
      proxy_map
    }
  }

  waiter$hide()
}
