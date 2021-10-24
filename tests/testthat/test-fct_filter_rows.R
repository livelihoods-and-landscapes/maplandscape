test_that("testing filter rows", {
  kava <- sf::st_read("vavau-crop-survey.gpkg", layer = "kava")
  expect_equal(nrow(filter_rows(kava, "kava_area > 70")), 5)
  expect_equal(filter_rows(kava, "kava_no_column > 70"), "filter error")
})
