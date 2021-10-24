test_that("testing edit data frame", {
  kava <- sf::st_read("vavau-crop-survey.gpkg", layer = "kava")
  tmp_edits <- data.frame(r = 1, c = 1, e = "edit")
  edited <- edit_data_frame(tmp_edits, kava, kava, "kava")
  edited_df <- edited[[1]]
  expect_equal(edited_df[1, 1], "edit")
})
