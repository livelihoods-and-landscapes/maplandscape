test_that("testing table joins", {
  pb <- sf::st_read("vavau-crop-survey.gpkg", layer = "plot-boundary")
  kava <- sf::st_read("vavau-crop-survey.gpkg", layer = "kava")
  village <- sf::st_read("tonga-village-boundaries.gpkg")
  expected_class <- "sf"
  expect_equal("sf" %in% class(join_tables(pb, kava, "col_inner", "plot_id", "kava_id")), "sf" %in% expected_class)
  expect_equal("sf" %in% class(join_tables(pb, kava, "col_left", "plot_id", "kava_id")), "sf" %in% expected_class)
  expect_equal(nrow(join_tables(village, kava, "col_inner", "vid", "kava_id")), 0)
})
