# utility functions for welfarebR package

#' Download file with progress bar
#'
#' @param url Character. URL to download from.
#' @param destfile Character. Destination file path. If NULL, uses temp file.
#' @param quiet Logical. Suppress progress messages? Default FALSE.
#'
#' @return Character. Path to downloaded file.
#'
#' @keywords internal
download_with_progress <- function(url, destfile = NULL, quiet = FALSE) {



  # validate inputs
 checkmate::assert_string(url)
 checkmate::assert_string(destfile, null.ok = TRUE)
  checkmate::assert_flag(quiet)

  # set destination
 if (is.null(destfile)) {
    destfile <- tempfile()
  }

 # download with httr2
  if (!quiet) {
    cli::cli_alert_info("Downloading from {.url {url}}")
  }

  req <- httr2::request(url) |>
    httr2::req_timeout(300) |>
    httr2::req_retry(max_tries = 3)

  resp <- req |>
    httr2::req_perform(path = destfile)

  if (!quiet) {
    cli::cli_alert_success("Download complete")
 }

  destfile
}


#' Download large files in chunks
#'
#' @param urls Character vector. URLs to download.
#' @param destdir Character. Destination directory.
#' @param quiet Logical. Suppress progress messages? Default FALSE.
#'
#' @return Character vector. Paths to downloaded files.
#'
#' @keywords internal
download_chunked <- function(urls, destdir = NULL, quiet = FALSE) {

 # validate inputs
  checkmate::assert_character(urls, min.len = 1)
  checkmate::assert_string(destdir, null.ok = TRUE)
  checkmate::assert_flag(quiet)

  # set destination directory
  if (is.null(destdir)) {
    destdir <- tempdir()
  }

  if (!dir.exists(destdir)) {
    dir.create(destdir, recursive = TRUE)
  }

  # download each file
  if (!quiet) {
    cli::cli_alert_info("Downloading {length(urls)} file(s)")
  }

  paths <- purrr::map_chr(urls, function(url) {
    filename <- basename(url)
    destfile <- file.path(destdir, filename)
    download_with_progress(url, destfile, quiet = TRUE)
  })

  if (!quiet) {
    cli::cli_alert_success("All downloads complete")
  }

  paths
}


#' Read data in specified format
#'
#' @param data Data frame or tibble to convert.
#' @param format Character. Output format: "tibble", "arrow", or "parquet".
#'
#' @return Data in requested format.
#'
#' @keywords internal
read_with_format <- function(data, format = "tibble") {

  # validate inputs
  checkmate::assert_choice(format, c("tibble", "arrow", "parquet"))

  if (format == "tibble") {
    # return as tibble
    if (!inherits(data, "tbl_df")) {
      data <- tibble::as_tibble(data)
    }
    return(data)
  }

  if (format == "arrow") {
    # return as arrow table
    if (!requireNamespace("arrow", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg arrow} is required for format = 'arrow'")
    }
    return(arrow::as_arrow_table(data))
  }

  if (format == "parquet") {
    # save as parquet and return path
    if (!requireNamespace("arrow", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg arrow} is required for format = 'parquet'")
    }
    path <- tempfile(fileext = ".parquet")
    arrow::write_parquet(data, path)
    cli::cli_alert_info("Data saved to {.file {path}}")
    return(path)
  }
}


#' Build URL from base and parameters
#'
#' @param base_url Character. Base URL.
#' @param ... Named parameters to append to URL.
#'
#' @return Character. Complete URL.
#'
#' @keywords internal
build_url <- function(base_url, ...) {

  # validate inputs
  checkmate::assert_string(base_url)

  params <- list(...)

  if (length(params) == 0) {
    return(base_url)
  }

  # build query string
  query <- purrr::imap_chr(params, function(value, name) {
    stringr::str_c(name, "=", utils::URLencode(as.character(value)))
  }) |>
    stringr::str_c(collapse = "&")

  # combine base and query
  if (grepl("\\?", base_url)) {
    stringr::str_c(base_url, "&", query)
  } else {
    stringr::str_c(base_url, "?", query)
  }
}


#' Validate common inputs
#'
#' @param year Integer. Year to validate.
#' @param questionnaire Character. Questionnaire type.
#' @param valid_years Integer vector. Valid years.
#' @param valid_questionnaires Character vector. Valid questionnaire types.
#'
#' @return Invisible TRUE if valid, otherwise throws error.
#'
#' @keywords internal
validate_inputs <- function(year = NULL,
                            questionnaire = NULL,
                            valid_years = NULL,
                            valid_questionnaires = NULL) {

  if (!is.null(year)) {
    checkmate::assert_integerish(year, lower = 2007, upper = 2100, len = 1)

    if (!is.null(valid_years) && !year %in% valid_years) {
      cli::cli_abort(c(
        "Invalid year: {year}",
        "i" = "Available years: {valid_years}"
      ))
    }
  }

  if (!is.null(questionnaire)) {
    checkmate::assert_string(questionnaire)

    if (!is.null(valid_questionnaires)) {
      questionnaire_lower <- tolower(questionnaire)
      valid_lower <- tolower(valid_questionnaires)

      if (!questionnaire_lower %in% valid_lower) {
        cli::cli_abort(c(
          "Invalid questionnaire: {questionnaire}",
          "i" = "Available questionnaires: {valid_questionnaires}"
        ))
      }
    }
  }

  invisible(TRUE)
}
