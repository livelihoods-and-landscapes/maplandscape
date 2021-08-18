test_that("testing spatial joins", {
  pb <- sf::st_read("vavau-crop-survey.gpkg", layer = "plot-boundary")
  kava <- sf::st_read("vavau-crop-survey.gpkg", layer = "kava")
  village <- sf::st_read("tonga-village-boundaries.gpkg")
  expected_class <- "sf"
  expect_equal("sf" %in% class(spatial_join_tables(pb, village, "spatial_inner")), "sf" %in% expected_class)
  expect_equal("sf" %in% class(spatial_join_tables(pb, village, "spatial_left")), "sf" %in% expected_class)
  expect_error(spatial_join_tables(kava, village, "spatial_inner"))
})
