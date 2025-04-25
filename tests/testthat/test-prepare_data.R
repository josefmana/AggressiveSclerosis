upstore <- here::here(targets::tar_config_get("store"))
targets::tar_load(raw_data, store = upstore)

test_that("each patient's demography once", {
  expect_equal(unique(table(raw_data$id$id)), 1)
})

test_that("nothing from the future", {
  expect_true(all(na.omit(raw_data$id$birth < Sys.time())))
  expect_true(all(na.omit(raw_data$treatment$start_date < Sys.time())))
  expect_true(all(na.omit(raw_data$treatment$end_date < Sys.time())))
  expect_true(all(na.omit(raw_data$relapses$relapse_date < Sys.time())))
  expect_true(all(na.omit(raw_data$edss$visit_date < Sys.time())))
  expect_true(all(na.omit(raw_data$pregnancy$start_date < Sys.time())))
  expect_true(all(na.omit(raw_data$pregnancy$end_date < Sys.time())))
  expect_true(all(na.omit(raw_data$mri$mri_date < Sys.time())))
  expect_true(all(na.omit(raw_data$spinal_cord$cord_date < Sys.time())))
  expect_true(all(na.omit(raw_data$csf$csv_date < Sys.time())))
  expect_true(all(na.omit(raw_data$cholesterol$cholesterol_date < Sys.time())))
})
