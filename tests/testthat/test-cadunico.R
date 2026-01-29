# tests for cadunico.R functions

# available_cadunico() tests --------------------------------------------------

test_that("available_cadunico returns tibble", {
  result <- available_cadunico()
  expect_s3_class(result, "tbl_df")
})

test_that("available_cadunico has correct columns", {
  result <- available_cadunico()
  expect_named(result, c("year", "description", "source", "reference_date"))
})

test_that("available_cadunico includes years 2012-2018", {
  result <- available_cadunico()
  expect_true(all(2012:2018 %in% result$year))
})

test_that("available_cadunico has 7 rows (one per year)", {
  result <- available_cadunico()
  expect_equal(nrow(result), 7)
})

test_that("available_cadunico reference dates are december", {
  result <- available_cadunico()
  expect_true(all(grepl("-12-01$", result$reference_date)))
})


# dictionary_cadunico() tests -------------------------------------------------

test_that("dictionary_cadunico returns tibble", {
  result <- suppressMessages(dictionary_cadunico(2018))
  expect_s3_class(result, "tbl_df")
})

test_that("dictionary_cadunico has expected columns", {
  result <- suppressMessages(dictionary_cadunico(2018))
  expect_true(all(c("variable", "description") %in% names(result)))
})

test_that("dictionary_cadunico validates year", {
  expect_error(dictionary_cadunico("2018"), class = "simpleError")
  expect_error(dictionary_cadunico(c(2017, 2018)), class = "simpleError")
})

test_that("dictionary_cadunico rejects invalid year", {
  # year 2010 is within bounds (2007-2100) but not in valid_years (2012-2018)
  expect_error(dictionary_cadunico(2010), "Invalid year")
  expect_error(dictionary_cadunico(2020), "Invalid year")
})


# sampling_plan_cadunico() tests ----------------------------------------------

test_that("sampling_plan_cadunico returns list", {
  result <- suppressMessages(sampling_plan_cadunico())
  expect_type(result, "list")
})

test_that("sampling_plan_cadunico has required elements", {
  result <- suppressMessages(sampling_plan_cadunico())
  expect_true("description" %in% names(result))
  expect_true("design" %in% names(result))
  expect_true("documentation_url" %in% names(result))
  expect_true("notes" %in% names(result))
})

test_that("sampling_plan_cadunico url is valid", {
  result <- suppressMessages(sampling_plan_cadunico())
  expect_match(result$documentation_url, "^https://")
  expect_match(result$documentation_url, "\\.pdf$")
})


# build_cadunico_url() tests --------------------------------------------------

test_that("build_cadunico_url returns url for valid years", {
  url <- welfarebR:::build_cadunico_url(2018)
  expect_type(url, "character")
  expect_match(url, "^https://")
})

test_that("build_cadunico_url includes year and month in url", {
  url <- welfarebR:::build_cadunico_url(2018)
  expect_match(url, "201812")
})

test_that("build_cadunico_url uses december reference", {
  url <- welfarebR:::build_cadunico_url(2015)
  expect_match(url, "201512")
})

test_that("build_cadunico_url fails for invalid year", {
  # years outside 2012-2018 range
  expect_error(welfarebR:::build_cadunico_url(2010), "No URL available")
  expect_error(welfarebR:::build_cadunico_url(2020), "No URL available")
})


# get_cadunico() validation tests ---------------------------------------------

test_that("get_cadunico validates year argument", {
  expect_error(get_cadunico("2018"), class = "simpleError")
})

test_that("get_cadunico validates as_survey argument", {
  expect_error(get_cadunico(2018, as_survey = "yes"), class = "simpleError")
})

test_that("get_cadunico validates format argument", {
  expect_error(get_cadunico(2018, format = "invalid"), class = "simpleError")
})

test_that("get_cadunico validates cache argument", {
  expect_error(get_cadunico(2018, cache = "yes"), class = "simpleError")
})

test_that("get_cadunico rejects invalid year", {
  # year 2010 is within bounds (2007-2100) but not in valid_years (2012-2018)
  expect_error(get_cadunico(2010), "Invalid year")
  expect_error(get_cadunico(2020), "Invalid year")
})


# create_cadunico_survey() tests ----------------------------------------------

test_that("create_cadunico_survey requires srvyr package", {
  skip_if_not_installed("srvyr")

  test_data <- tibble::tibble(
    peso = c(100, 200, 150),
    value = 1:3
  )

  result <- suppressMessages(welfarebR:::create_cadunico_survey(test_data))
  expect_s3_class(result, "tbl_svy")
})

test_that("create_cadunico_survey fails without weight column", {
  skip_if_not_installed("srvyr")

  test_data <- tibble::tibble(
    other_col = c(100, 200, 150),
    value = 1:3
  )

  expect_error(
    welfarebR:::create_cadunico_survey(test_data),
    "Could not find weight column"
  )
})

test_that("create_cadunico_survey works with different weight column names", {
  skip_if_not_installed("srvyr")

  test_data <- tibble::tibble(
    peso_amostral = c(100, 200, 150),
    value = 1:3
  )

  result <- suppressMessages(welfarebR:::create_cadunico_survey(test_data))
  expect_s3_class(result, "tbl_svy")
})

test_that("create_cadunico_survey uses strata and cluster when available", {
  skip_if_not_installed("srvyr")

  test_data <- tibble::tibble(
    peso = c(100, 200, 150, 250),
    estrato = c(1, 1, 2, 2),
    upa = c(1, 2, 1, 2),
    value = 1:4
  )

  result <- suppressMessages(welfarebR:::create_cadunico_survey(test_data))
  expect_s3_class(result, "tbl_svy")
})
