test_that("make a ggplot2 scatter plot from a (sf) data fame", {
  test_data_path <- paste0(here::here(), "/test-data/test-data/vavau-crop-survey.gpkg")
  kava <- sf::st_read(test_data_path, layer="kava")

  test_scatter_ok <- make_scatter(
    kava,

    "kava_area",
    "area_acres",
    3,
    "x_axis",
    "y_axis",
    12,
    12
  )

  test_scatter_badcol <- make_scatter(
    kava,
    "kava_area_not_here",
    "area_acres",
    3,
    "x_axis",
    "y_axis",
    12,
    12
  )

  expected_class <- "ggplot"
  expect_equal("ggplot" %in% expected_class, "ggplot" %in% class(test_scatter_ok))
  expect_error(print(test_scatter_badcol))
})
