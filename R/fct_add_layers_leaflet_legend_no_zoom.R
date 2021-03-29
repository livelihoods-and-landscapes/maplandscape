
add_layers_leaflet_legend_no_zoom <- function(map_object, map_active_df, map_var, map_colour, opacity, map_line_width, map_line_colour, waiter) {
  
  supported_geometries <- c("POINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")
  
  if ("sf" %in% class(map_active_df) & is.atomic(map_active_df[[map_var]]) & nrow(map_active_df) > 0) {
    waiter$show()
    
    # make map active layer epsg 4326
    # make this an if statement
    map_df <- map_active_df %>%
      sf::st_transform(4326)
    
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
          leaflet::addLegend(
            pal = pal,
            values = map_df[[map_var]],
            position = "topright",
            title = map_var
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
          ) %>%
          leaflet::addLegend(
            pal = pal,
            values = map_df[[map_var]],
            position = "topright",
            title = map_var
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
          leaflet::addLegend(
            pal = pal,
            values = map_df[[map_var]],
            position = "topright",
            title = map_var
          )
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
          leaflet::addLegend(
            pal = pal,
            values = map_df[[map_var]],
            position = "topright",
            title = map_var
          )
      }
      proxy_map
    }
  }
  
  waiter$hide()
}