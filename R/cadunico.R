# cadunico functions for welfarebR package

# cadunico sample data url patterns
.cadunico_base_url <- "https://aplicacoes.mds.gov.br/sagi/pesquisas/documentos/microdado"

# available years for cadunico sample data
.cadunico_years <- 2012:2018

# documentation urls
.cadunico_sampling_plan_url <- "https://aplicacoes.mds.gov.br/sagirmps/noticias/arquivos/files/Plano_amostral_microdados_cadastro_unico.pdf"
.cadunico_dictionary_url <- "https://dados.gov.br/dados/conjuntos-dados/microdados-amostrais-do-cadastro-unico"


#' Download CadUnico sample data
#'
#' Downloads and processes deidentified sample microdata from Cadastro Unico
#' (CadUnico), Brazil's unified registry for social programs. The data is a
#' complex sample and requires survey weights for proper inference.
#'
#' @param year Integer. Year of the sample (2012-2018). Reference date is
#'   December of each year.
#' @param as_survey Logical. Return as survey design object? Default FALSE.
#'   If TRUE, returns a \code{\link[srvyr]{as_survey_design}} object with
#'   appropriate weights.
#' @param uf Character. Optional. Filter by state (UF code, e.g., "SP", "RJ").
#'   Default NULL returns all states.
#' @param format Character. Output format: "tibble" (default), "arrow", or
#'   "parquet". Ignored if as_survey = TRUE.
#' @param cache Logical. Use cached data if available? Default TRUE.
#'
#' @return A tibble, survey design object, arrow table, or parquet path,
#'   depending on parameters.
#'
#' @details
#' CadUnico is a complex sample survey. When analyzing the data, you should
#' use survey weights. Set \code{as_survey = TRUE} to get a properly weighted
#' survey design object that works with the \pkg{srvyr} and \pkg{survey}
#' packages.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # download cadunico 2018 as tibble
#' cad_2018 <- get_cadunico(2018)
#'
#' # download as survey design object for proper inference
#' cad_survey <- get_cadunico(2018, as_survey = TRUE)
#'
#' # calculate weighted mean using srvyr
#' library(srvyr)
#' cad_survey |>
#'   summarise(mean_income = survey_mean(renda_per_capita))
#' }
get_cadunico <- function(year,
                         as_survey = FALSE,
                         uf = NULL,
                         format = "tibble",
                         cache = TRUE) {

  # validate inputs
  checkmate::assert_integerish(year, len = 1)
  checkmate::assert_flag(as_survey)
  checkmate::assert_string(uf, null.ok = TRUE)
  checkmate::assert_choice(format, c("tibble", "arrow", "parquet"))
  checkmate::assert_flag(cache)

  # validate against available data
  available <- available_cadunico()
  valid_years <- available$year

  validate_inputs(
    year = year,
    valid_years = valid_years
  )

  # check cache
  if (cache) {
    cache_path <- check_cache("cadunico", year)
    if (!is.null(cache_path)) {
      cli::cli_alert_info("Loading from cache...")
      data <- load_from_cache(cache_path)

      # filter by uf if requested
      if (!is.null(uf)) {
        data <- filter_by_uf(data, uf)
      }

      # return as survey if requested
      if (as_survey) {
        return(create_cadunico_survey(data))
      }

      return(read_with_format(data, format))
    }
  }

  # build download url
  url <- build_cadunico_url(year)

  # download data
  cli::cli_alert_info("Downloading CadUnico {year}...")
  temp_file <- download_with_progress(url, quiet = TRUE)

  # read and process data
  data <- read_cadunico_file(temp_file, year)

  # save to cache
  if (cache) {
    save_to_cache(data, "cadunico", year)
    cli::cli_alert_success("Data cached for future use")
  }

  # cleanup temp file
  unlink(temp_file)

  # filter by uf if requested
  if (!is.null(uf)) {
    data <- filter_by_uf(data, uf)
  }

  cli::cli_alert_success("Done! {nrow(data)} rows loaded")

  # return as survey if requested
  if (as_survey) {
    return(create_cadunico_survey(data))
  }

  # return in requested format
  read_with_format(data, format)
}


#' List available CadUnico datasets
#'
#' Returns a tibble with all available CadUnico sample datasets.
#'
#' @return A tibble with columns: year, description, source, reference_date.
#'
#' @export
#'
#' @examples
#' available_cadunico()
available_cadunico <- function() {

  # define available datasets (2012-2018)
  tibble::tibble(
    year = .cadunico_years,
    description = stringr::str_c(
      "Amostra desidentificada do Cadastro Unico - dez/",
      year
    ),
    source = "microdados_sagi",
    reference_date = stringr::str_c(year, "-12-01")
  )
}


#' Get CadUnico data dictionary
#'
#' Returns the data dictionary (variable descriptions) for a specific
#' CadUnico sample dataset.
#'
#' @param year Integer. Year of the sample.
#'
#' @return A tibble with columns: variable, description, type, values.
#'
#' @details
#' The official data dictionary is available at the Brazilian Open Data Portal:
#' \url{https://dados.gov.br/dados/conjuntos-dados/microdados-amostrais-do-cadastro-unico}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # get dictionary for cadunico 2018
#' dict <- dictionary_cadunico(2018)
#' }
dictionary_cadunico <- function(year) {

  # validate inputs
  checkmate::assert_integerish(year, len = 1)

  validate_inputs(
    year = year,
    valid_years = available_cadunico()$year
  )

  # TODO: implement dictionary loading from dados.gov.br
  cli::cli_alert_warning("Dictionary not yet implemented for CadUnico {year}")
  cli::cli_alert_info("Official dictionary available at: {.url {(.cadunico_dictionary_url)}}")

  # return empty structure for now
  tibble::tibble(
    variable = character(),
    description = character(),
    type = character(),
    values = character()
  )
}


#' Get CadUnico sampling plan information
#'
#' Returns information about the CadUnico sampling methodology, including
#' the URL to the official sampling plan document.
#'
#' @return A list with sampling plan information and documentation URL.
#'
#' @details
#' The CadUnico sample is a complex survey design. For proper statistical
#' inference, you should use survey weights and account for the sampling
#' design. Use \code{get_cadunico(year, as_survey = TRUE)} to get a
#' properly weighted survey design object.
#'
#' @export
#'
#' @examples
#' # get sampling plan info
#' sampling_info <- sampling_plan_cadunico()
#' sampling_info$url
sampling_plan_cadunico <- function() {

  cli::cli_alert_info("CadUnico uses a complex sample design with stratification")
  cli::cli_alert_info("Official sampling plan document: {.url {(.cadunico_sampling_plan_url)}}")

  list(
    description = "Amostra complexa estratificada do Cadastro Unico",
    design = "Stratified sample with expansion weights",
    documentation_url = .cadunico_sampling_plan_url,
    notes = c(
      "Use as_survey = TRUE in get_cadunico() for proper inference",
      "Weights are required for population estimates",
      "See official document for full methodology"
    )
  )
}


# internal helper functions ------------------------------------------------

#' Build CadUnico download URL
#'
#' @param year Integer. Year.
#'
#' @return Character. Download URL.
#'
#' @keywords internal
build_cadunico_url <- function(year) {

  # validate year
  if (!year %in% .cadunico_years) {
    cli::cli_abort(c(
      "No URL available for year {year}",
      "i" = "Available years: {(.cadunico_years)}"
    ))
  }

  # url pattern: base_amostra_cad_YYYYMM.zip (MM = 12 for december)
  stringr::str_c(
    .cadunico_base_url,
    "/base_amostra_cad_",
    year,
    "12.zip"
  )
}


#' Read and process CadUnico file
#'
#' @param path Character. Path to downloaded file.
#' @param year Integer. Year of data.
#'
#' @return A tibble with processed data.
#'
#' @keywords internal
read_cadunico_file <- function(path, year) {

  # detect file type
  ext <- tolower(tools::file_ext(path))

  # handle compressed files
  if (ext == "zip") {
    # extract to temp directory
    temp_dir <- tempdir()
    utils::unzip(path, exdir = temp_dir)

    # find data file (usually .txt or .csv)
    files <- list.files(
      temp_dir,
      pattern = "\\.(txt|csv)$",
      ignore.case = TRUE,
      full.names = TRUE
    )

    if (length(files) == 0) {
      cli::cli_abort("Could not find data file in archive")
    }

    path <- files[1]
    ext <- tolower(tools::file_ext(path))
  }

  # read based on file type
  # cadunico typically uses semicolon delimiter
  data <- if (ext == "csv") {
    readr::read_csv2(path, show_col_types = FALSE)
  } else if (ext == "txt") {
    readr::read_delim(path, delim = ";", show_col_types = FALSE)
  } else {
    cli::cli_abort("Unsupported file format: {ext}")
  }

  # clean column names
  data <- data |>
    janitor::clean_names()

  data
}


#' Create survey design object for CadUnico
#'
#' @param data A tibble with CadUnico data.
#'
#' @return A srvyr survey design object.
#'
#' @keywords internal
create_cadunico_survey <- function(data) {

  # check if srvyr is available
 if (!requireNamespace("srvyr", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg srvyr} is required for as_survey = TRUE. Install with: install.packages('srvyr')"
    )
  }

  # identify weight column
  # common names: peso, peso_amostral, weight, fator_expansao
  weight_cols <- c("peso", "peso_amostral", "weight", "fator_expansao", "peso_fam")
  found_weight <- intersect(names(data), weight_cols)

  if (length(found_weight) == 0) {
    cli::cli_abort(c(
      "Could not find weight column in data.",
      "i" = "Expected one of: {weight_cols}"
    ))
  }

  weight_col <- found_weight[1]
  cli::cli_alert_info("Using weight column: {.var {weight_col}}")

  # identify strata and cluster columns if present
  strata_cols <- c("estrato", "stratum", "v4617")
  cluster_cols <- c("upa", "cluster", "v4618", "conglomerado")

  found_strata <- intersect(names(data), strata_cols)
  found_cluster <- intersect(names(data), cluster_cols)

  # create survey design
  if (length(found_strata) > 0 && length(found_cluster) > 0) {
    # full complex design
    cli::cli_alert_info(
      "Creating complex survey design with strata ({found_strata[1]}) and clusters ({found_cluster[1]})"
    )
    survey_design <- data |>
      srvyr::as_survey_design(
        weights = !!rlang::sym(weight_col),
        strata = !!rlang::sym(found_strata[1]),
        ids = !!rlang::sym(found_cluster[1]),
        nest = TRUE
      )
  } else {
    # simple weighted design
    cli::cli_alert_info("Creating weighted survey design (no strata/cluster info found)")
    survey_design <- data |>
      srvyr::as_survey_design(weights = !!rlang::sym(weight_col))
  }

  survey_design
}
