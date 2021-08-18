test_that("make a ggplot2 histogram from a (sf) data fame", {
  test_data_path <- paste0("vavau-crop-survey.gpkg")
  kava <- sf::st_read(test_data_path, layer = "kava")

  # good column
  test_hist_ok <- make_histogram(
    kava,
    "kava_area",
    5,
    "x_axis",
    "y_axis",
    12,
    12
  )

  # bad column
  test_hist_badcol <- make_histogram(
    kava,
    "kava_area_err",
    5,
    "x_axis",
    "y_axis",
    12,
    12
  )

  expected_class <- "ggplot"
  expect_equal("ggplot" %in% expected_class, "ggplot" %in% class(test_hist_ok))
  expect_error(print(test_hist_badcol))
})
