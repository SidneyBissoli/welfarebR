# cache management functions for welfarebR package

# package environment for storing cache directory
.welfarebr_env <- new.env(parent = emptyenv())


#' Get cache directory path
#'
#' Returns the current cache directory used by welfarebR. By default, uses
#' the user's cache directory as determined by \code{\link[rappdirs]{user_cache_dir}}.
#'
#' @return Character. Path to cache directory.
#'
#' @export
#'
#' @examples
#' get_cache_dir()
get_cache_dir <- function() {

  # check if custom directory is set
 if (exists("cache_dir", envir = .welfarebr_env)) {
    return(get("cache_dir", envir = .welfarebr_env))
  }

  # use default from rappdirs
  rappdirs::user_cache_dir("welfarebR", "R")
}


#' Set custom cache directory
#'
#' Sets a custom cache directory for welfarebR. This setting persists
#' for the current R session only.
#'
#' @param path Character. Path to cache directory. Use NULL to reset to default.
#'
#' @return Invisible. The previous cache directory path.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # set custom cache directory
#' set_cache_dir("~/my_cache")
#'
#' # reset to default
#' set_cache_dir(NULL)
#' }
set_cache_dir <- function(path) {

  # validate input
  checkmate::assert_string(path, null.ok = TRUE)

  # store previous value
 old_path <- get_cache_dir()

  if (is.null(path)) {
    # reset to default
    if (exists("cache_dir", envir = .welfarebr_env)) {
      rm("cache_dir", envir = .welfarebr_env)
    }
    cli::cli_alert_info("Cache directory reset to default: {.path {get_cache_dir()}}")
  } else {
    # set custom path
    assign("cache_dir", path, envir = .welfarebr_env)
    cli::cli_alert_info("Cache directory set to: {.path {path}}")
  }

  invisible(old_path)
}


#' Clear welfarebR cache
#'
#' Removes all cached files from the welfarebR cache directory.
#'
#' @param dataset Character. Dataset to clear: "all", "censo_suas", or "cadunico".
#'   Default is "all".
#' @param ask Logical. Ask for confirmation before deleting? Default TRUE.
#'
#' @return Invisible. Number of files deleted.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # clear all cache
#' clear_welfarebr_cache()
#'
#' # clear only censo_suas cache
#' clear_welfarebr_cache(dataset = "censo_suas")
#' }
clear_welfarebr_cache <- function(dataset = "all", ask = TRUE) {

  # validate inputs
  checkmate::assert_choice(dataset, c("all", "censo_suas", "cadunico"))
  checkmate::assert_flag(ask)

  cache_dir <- get_cache_dir()

  # check if cache directory exists
  if (!dir.exists(cache_dir)) {
    cli::cli_alert_info("Cache directory does not exist. Nothing to clear.")
    return(invisible(0))
 }

  # determine which files to delete
  if (dataset == "all") {
    target_dir <- cache_dir
    pattern <- ".*"
  } else {
    target_dir <- file.path(cache_dir, dataset)
    pattern <- ".*"
  }

  if (!dir.exists(target_dir)) {
    cli::cli_alert_info("No cache found for {.val {dataset}}.")
    return(invisible(0))
  }

  # list files
  files <- list.files(target_dir, pattern = pattern, full.names = TRUE, recursive = TRUE)

  if (length(files) == 0) {
    cli::cli_alert_info("Cache is empty. Nothing to clear.")
    return(invisible(0))
  }

  # ask for confirmation
  if (ask) {
    cli::cli_alert_warning("This will delete {length(files)} cached file(s).")
    confirm <- readline("Are you sure? (y/n): ")
    if (!tolower(confirm) %in% c("y", "yes")) {
      cli::cli_alert_info("Operation cancelled.")
      return(invisible(0))
    }
  }

  # delete files
  unlink(files, recursive = TRUE)

  cli::cli_alert_success("Deleted {length(files)} cached file(s).")

  invisible(length(files))
}


#' Build cache file path
#'
#' @param dataset Character. Dataset name ("censo_suas" or "cadunico").
#' @param ... Additional path components (e.g., year, questionnaire).
#' @param ext Character. File extension. Default "rds".
#'
#' @return Character. Full path to cache file.
#'
#' @keywords internal
get_cache_path <- function(dataset, ..., ext = "rds") {

  checkmate::assert_string(dataset)
  checkmate::assert_string(ext)

  # build path components
  components <- c(list(...))
  filename <- stringr::str_c(
    stringr::str_c(components, collapse = "_"),
    ".",
    ext
  )

  file.path(get_cache_dir(), dataset, filename)
}


#' Check if data exists in cache
#'
#' @param dataset Character. Dataset name.
#' @param ... Additional identifiers (year, questionnaire, etc.).
#'
#' @return Character or NULL. Path to cached file if exists, NULL otherwise.
#'
#' @keywords internal
check_cache <- function(dataset, ...) {

  path <- get_cache_path(dataset, ...)

  if (file.exists(path)) {
    return(path)
  }

  NULL
}


#' Save data to cache
#'
#' @param data Data to cache.
#' @param dataset Character. Dataset name.
#' @param ... Additional identifiers (year, questionnaire, etc.).
#'
#' @return Invisible. Path to cached file.
#'
#' @keywords internal
save_to_cache <- function(data, dataset, ...) {

  path <- get_cache_path(dataset, ...)

  # ensure directory exists
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  # save data
 saveRDS(data, path)

  invisible(path)
}


#' Load data from cache
#'
#' @param path Character. Path to cached file.
#'
#' @return Cached data.
#'
#' @keywords internal
load_from_cache <- function(path) {

  checkmate::assert_file_exists(path)

  readRDS(path)
}
