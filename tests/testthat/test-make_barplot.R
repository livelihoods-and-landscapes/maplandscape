test_that("make a barplot", {
  test_data_path <- paste0("vavau-crop-survey.gpkg")
  plot_boundary <- sf::st_read(test_data_path, layer="plot-boundary")

  test_barplot_ok <- make_barplot(
    plot_boundary,
    "grower_status",
    "area",
    12,
    12
  )

  expected_class <- "ggplot"
  expect_equal("ggplot" %in% expected_class, "ggplot" %in% class(test_barplot_ok))

})
