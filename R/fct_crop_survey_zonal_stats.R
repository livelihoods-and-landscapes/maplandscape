crop_survey_zonal_stats <- function(zones, crops_data) {
  
  funs_list_numeric <- list(
    mean = ~ mean(.x, na.rm = TRUE),
    sum = ~ sum(.x, na.rm = TRUE)
  )
  
  sf::st_crs(zones) <- 4326
  sf::st_crs(crops_data) <- 4326
  
  # catch empty or invalid polygons
  crops_data <- crops_data[sf::st_is_valid(crops_data) != FALSE, ]
  crops_data <- crops_data[sf::st_is_empty(crops_data) == FALSE, ]
  
  # catch empty or invalid polygons
  zones <- zones[sf::st_is_valid(zones) != FALSE, ]
  zones <- zones[sf::st_is_empty(zones) == FALSE, ]
  
  out_sdf<- sf::st_join(crops_data, zones, left = FALSE, largest = TRUE) %>%
    dplyr::select(-c("plot_id")) %>%
    dplyr::group_by(zone) %>%
    dplyr::summarise(across(where(is.numeric), funs_list_numeric), n = n(), .groups = "keep") %>%
    sf::st_drop_geometry() %>%
    as.data.frame()
  
  out_sdf <- zones %>% 
    dplyr::inner_join(out_sdf)
  
  out_sdf
}