test_that("testing list layers", {
  layers <- sf::st_layers("vavau-crop-survey.gpkg")$name
  layers_df <- list_layers("vavau-crop-survey.gpkg", "vavau-crop-survey.gpkg")
  expect_equal(layers, layers_df$layers)
})
