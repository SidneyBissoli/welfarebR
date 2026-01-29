# tests for cache.R functions

test_that("get_cache_dir returns a path", {
  path <- get_cache_dir()
  expect_type(path, "character")
  expect_true(nchar(path) > 0)
})

test_that("get_cache_dir returns default rappdirs path", {
  # reset any custom path
  if (exists("cache_dir", envir = welfarebR:::.welfarebr_env)) {
    rm("cache_dir", envir = welfarebR:::.welfarebr_env)
  }
  path <- get_cache_dir()
  expected <- rappdirs::user_cache_dir("welfarebR", "R")
  expect_equal(path, expected)
})

test_that("set_cache_dir changes cache directory", {
  old_path <- get_cache_dir()

  # set custom path
  test_path <- tempdir()
  result <- suppressMessages(set_cache_dir(test_path))

  expect_equal(result, old_path)
  expect_equal(get_cache_dir(), test_path)

  # reset to default
  suppressMessages(set_cache_dir(NULL))
})

test_that("set_cache_dir with NULL resets to default", {
  # set a custom path first
  suppressMessages(set_cache_dir(tempdir()))

  # reset
  suppressMessages(set_cache_dir(NULL))

  expected <- rappdirs::user_cache_dir("welfarebR", "R")
  expect_equal(get_cache_dir(), expected)
})

test_that("set_cache_dir validates path argument", {
  expect_error(set_cache_dir(123), class = "simpleError")
  expect_error(set_cache_dir(c("a", "b")), class = "simpleError")
})

test_that("get_cache_path builds correct path", {
  path <- get_cache_path("censo_suas", 2017, "cras")
  expect_match(path, "censo_suas")
  expect_match(path, "2017_cras\\.rds$")
})

test_that("get_cache_path respects custom extension", {
  path <- get_cache_path("cadunico", 2018, ext = "parquet")
  expect_match(path, "\\.parquet$")
})

test_that("check_cache returns NULL for non-existent file", {
  result <- check_cache("nonexistent", "fake", "path")
  expect_null(result)
})

test_that("save_to_cache and load_from_cache work correctly", {
  # use temp directory for test
  test_dir <- tempfile("welfarebr_test_")
  suppressMessages(set_cache_dir(test_dir))

  # save test data
  test_data <- tibble::tibble(a = 1:5, b = letters[1:5])
  save_to_cache(test_data, "test_dataset", "2023", "test")

  # check cache exists
  cache_path <- check_cache("test_dataset", "2023", "test")
  expect_false(is.null(cache_path))
  expect_true(file.exists(cache_path))

  # load from cache
  loaded_data <- load_from_cache(cache_path)
  expect_equal(loaded_data, test_data)

  # cleanup
  unlink(test_dir, recursive = TRUE)
  suppressMessages(set_cache_dir(NULL))
})

test_that("clear_welfarebr_cache handles non-existent directory", {
  # use non-existent temp directory
  test_dir <- tempfile("welfarebr_nonexistent_")
  suppressMessages(set_cache_dir(test_dir))

  result <- suppressMessages(clear_welfarebr_cache(ask = FALSE))
  expect_equal(result, 0)

  # cleanup
  suppressMessages(set_cache_dir(NULL))
})

test_that("clear_welfarebr_cache validates dataset argument", {
  expect_error(
    clear_welfarebr_cache(dataset = "invalid"),
    class = "simpleError"
  )
})

test_that("clear_welfarebr_cache clears specific dataset", {
  # setup test cache
  test_dir <- tempfile("welfarebr_test_")
  suppressMessages(set_cache_dir(test_dir))

  # save test data to two datasets
  test_data <- tibble::tibble(a = 1:3)
  save_to_cache(test_data, "censo_suas", "2017", "cras")
  save_to_cache(test_data, "cadunico", "2018")

  # clear only censo_suas
  result <- suppressMessages(clear_welfarebr_cache(dataset = "censo_suas", ask = FALSE))
  expect_equal(result, 1)

  # cadunico should still exist
  expect_false(is.null(check_cache("cadunico", "2018")))

  # cleanup
  unlink(test_dir, recursive = TRUE)
  suppressMessages(set_cache_dir(NULL))
})
