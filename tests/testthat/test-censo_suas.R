# tests for censo_suas.R functions

# available_censo_suas() tests ------------------------------------------------

test_that("available_censo_suas returns tibble", {
  result <- available_censo_suas()
  expect_s3_class(result, "tbl_df")
})

test_that("available_censo_suas has correct columns", {
  result <- available_censo_suas()
  expect_named(result, c("year", "questionnaire", "description", "source"))
})

test_that("available_censo_suas includes years 2011-2017", {
  result <- available_censo_suas()
  years <- unique(result$year)
  expect_true(all(2011:2017 %in% years))
})

test_that("available_censo_suas includes all questionnaires", {
  result <- available_censo_suas()
  questionnaires <- unique(result$questionnaire)

  expected <- c(
    "cras", "creas", "gestao_municipal", "gestao_estadual",
    "conselho_municipal", "conselho_estadual", "centro_pop", "acolhimento"
  )
  expect_true(all(expected %in% questionnaires))
})

test_that("available_censo_suas has descriptions for all questionnaires", {
  result <- available_censo_suas()
  expect_false(any(is.na(result$description)))
})


# dictionary_censo_suas() tests -----------------------------------------------

test_that("dictionary_censo_suas returns tibble", {
  result <- suppressMessages(dictionary_censo_suas(2017, "cras"))
  expect_s3_class(result, "tbl_df")
})

test_that("dictionary_censo_suas has variable and description columns", {
  result <- suppressMessages(dictionary_censo_suas(2017, "cras"))
  expect_true("variable" %in% names(result))
  expect_true("description" %in% names(result))
})

test_that("dictionary_censo_suas loads cras dictionary with variables", {
  result <- suppressMessages(dictionary_censo_suas(2017, "cras"))
  expect_gt(nrow(result), 0)
})

test_that("dictionary_censo_suas loads creas dictionary", {
  result <- suppressMessages(dictionary_censo_suas(2017, "creas"))
  expect_gt(nrow(result), 0)
})

test_that("dictionary_censo_suas validates year", {
  expect_error(dictionary_censo_suas("2017", "cras"), class = "simpleError")
  expect_error(dictionary_censo_suas(c(2016, 2017), "cras"), class = "simpleError")
})

test_that("dictionary_censo_suas validates questionnaire", {
  expect_error(dictionary_censo_suas(2017, 123), class = "simpleError")
})

test_that("dictionary_censo_suas rejects invalid year", {
  # year 2010 is within bounds (2007-2100) but not in valid_years (2011-2017)
  expect_error(dictionary_censo_suas(2010, "cras"), "Invalid year")
})

test_that("dictionary_censo_suas rejects invalid questionnaire", {
  expect_error(dictionary_censo_suas(2017, "invalid"), "Invalid questionnaire")
})

test_that("dictionary_censo_suas is case insensitive for questionnaire", {
  result_lower <- suppressMessages(dictionary_censo_suas(2017, "cras"))
  result_upper <- suppressMessages(dictionary_censo_suas(2017, "CRAS"))
  expect_equal(nrow(result_lower), nrow(result_upper))
})


# build_censo_suas_url() tests ------------------------------------------------

test_that("build_censo_suas_url returns url for valid years", {
  url <- welfarebR:::build_censo_suas_url(2017, "cras")
  expect_type(url, "character")
  expect_match(url, "^https://")
})
test_that("build_censo_suas_url returns correct url for 2017", {
  url <- welfarebR:::build_censo_suas_url(2017, "cras")
  expect_match(url, "microdado_210\\.zip$")
})

test_that("build_censo_suas_url returns correct url for 2011", {
  url <- welfarebR:::build_censo_suas_url(2011, "cras")
  expect_match(url, "microdado_130\\.rar$")
})

test_that("build_censo_suas_url fails for invalid year", {
  expect_error(welfarebR:::build_censo_suas_url(2020, "cras"), "No URL available")
})


# filter_by_uf() tests --------------------------------------------------------

test_that("filter_by_uf filters data correctly", {
  test_data <- tibble::tibble(
    uf = c("SP", "RJ", "MG", "SP"),
    value = 1:4
  )

  result <- suppressMessages(welfarebR:::filter_by_uf(test_data, "SP"))
  expect_equal(nrow(result), 2)
  expect_true(all(result$uf == "SP"))
})

test_that("filter_by_uf is case insensitive", {
  test_data <- tibble::tibble(
    uf = c("SP", "RJ", "MG"),
    value = 1:3
  )

  result_lower <- suppressMessages(welfarebR:::filter_by_uf(test_data, "sp"))
  result_upper <- suppressMessages(welfarebR:::filter_by_uf(test_data, "SP"))
  expect_equal(nrow(result_lower), nrow(result_upper))
})

test_that("filter_by_uf works with different column names", {
  test_data <- tibble::tibble(
    sigla_uf = c("SP", "RJ", "MG"),
    value = 1:3
  )

  result <- suppressMessages(welfarebR:::filter_by_uf(test_data, "RJ"))
  expect_equal(nrow(result), 1)
})

test_that("filter_by_uf returns unfiltered data when column not found", {
  test_data <- tibble::tibble(
    other_column = c("SP", "RJ", "MG"),
    value = 1:3
  )

  result <- suppressWarnings(welfarebR:::filter_by_uf(test_data, "SP"))
  expect_equal(nrow(result), 3)
})

test_that("filter_by_uf validates uf argument", {
  test_data <- tibble::tibble(uf = c("SP", "RJ"), value = 1:2)
  expect_error(welfarebR:::filter_by_uf(test_data, 123), class = "simpleError")
})


# get_censo_suas() validation tests -------------------------------------------

test_that("get_censo_suas validates year argument", {
  expect_error(get_censo_suas("2017", "cras"), class = "simpleError")
})

test_that("get_censo_suas validates questionnaire argument", {
  expect_error(get_censo_suas(2017, 123), class = "simpleError")
})

test_that("get_censo_suas validates format argument", {
  expect_error(get_censo_suas(2017, "cras", format = "invalid"), class = "simpleError")
})

test_that("get_censo_suas validates cache argument", {
  expect_error(get_censo_suas(2017, "cras", cache = "yes"), class = "simpleError")
})

test_that("get_censo_suas rejects invalid year", {
  # year 2010 is within bounds (2007-2100) but not in valid_years (2011-2017)
  expect_error(get_censo_suas(2010, "cras"), "Invalid year")
})

test_that("get_censo_suas rejects invalid questionnaire", {
  expect_error(get_censo_suas(2017, "invalid"), "Invalid questionnaire")
})
