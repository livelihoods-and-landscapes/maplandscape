test_that("testing add column", {
  kava <- sf::st_read("vavau-crop-survey.gpkg", layer = "kava")
  cols <- ncol(kava)
  expect_equal(ncol(add_column(kava, "kava_area + 10", "new_column")),cols + 1)
  expect_equal(add_column(kava, "kava_no_column + 10", "new_column"), "mutate error")
})
