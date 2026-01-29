# tests for utils.R functions

test_that("build_url works with no parameters", {
  url <- build_url("https://example.com")
  expect_equal(url, "https://example.com")
})

test_that("build_url adds query parameters", {
  url <- build_url("https://example.com", foo = "bar", baz = 123)
  expect_match(url, "https://example.com\\?")
  expect_match(url, "foo=bar")
  expect_match(url, "baz=123")
})

test_that("build_url appends to existing query", {
  url <- build_url("https://example.com?existing=value", new = "param")
  expect_match(url, "existing=value")
  expect_match(url, "&new=param")
})

test_that("build_url encodes special characters", {
  url <- build_url("https://example.com", text = "hello world")
  expect_match(url, "hello%20world")
})

test_that("build_url validates base_url", {
  expect_error(build_url(123), class = "simpleError")
  expect_error(build_url(NULL), class = "simpleError")
})

test_that("validate_inputs accepts valid year", {
  expect_invisible(validate_inputs(year = 2017))
  expect_true(validate_inputs(year = 2017))
})
test_that("validate_inputs rejects invalid year type", {
  expect_error(validate_inputs(year = "2017"), class = "simpleError")
  expect_error(validate_inputs(year = c(2016, 2017)), class = "simpleError")
})

test_that("validate_inputs rejects year outside bounds", {
  expect_error(validate_inputs(year = 2000), class = "simpleError")
  expect_error(validate_inputs(year = 2200), class = "simpleError")
})

test_that("validate_inputs rejects year not in valid_years", {
  expect_error(
    validate_inputs(year = 2020, valid_years = 2011:2017),
    "Invalid year"
  )
})

test_that("validate_inputs accepts valid questionnaire", {
  expect_true(validate_inputs(questionnaire = "cras"))
})

test_that("validate_inputs rejects invalid questionnaire", {
  expect_error(
    validate_inputs(
      questionnaire = "invalid",
      valid_questionnaires = c("cras", "creas")
    ),
    "Invalid questionnaire"
  )
})

test_that("validate_inputs is case insensitive for questionnaires", {
  expect_true(
    validate_inputs(
      questionnaire = "CRAS",
      valid_questionnaires = c("cras", "creas")
    )
  )
})

test_that("read_with_format returns tibble by default", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  result <- read_with_format(df, "tibble")
  expect_s3_class(result, "tbl_df")
})

test_that("read_with_format keeps tibble as tibble", {
  tbl <- tibble::tibble(a = 1:3, b = c("x", "y", "z"))
  result <- read_with_format(tbl, "tibble")
  expect_s3_class(result, "tbl_df")
})

test_that("read_with_format validates format argument", {
  df <- data.frame(a = 1)
  expect_error(read_with_format(df, "invalid"), class = "simpleError")
})

test_that("read_with_format requires arrow package for arrow format", {
  skip_if_not_installed("arrow")
  df <- data.frame(a = 1:3)
  result <- read_with_format(df, "arrow")
  expect_s3_class(result, "ArrowTabular")
})

test_that("read_with_format requires arrow package for parquet format", {
  skip_if_not_installed("arrow")
  df <- data.frame(a = 1:3)
  result <- read_with_format(df, "parquet")
  expect_type(result, "character")
  expect_true(file.exists(result))
  unlink(result)
})
