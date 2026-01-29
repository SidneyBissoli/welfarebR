# censo suas functions for welfarebR package

# valid questionnaires for censo suas
.censo_suas_questionnaires <- c(
  "cras",
  "creas",
  "gestao_municipal",
  "gestao_estadual",
  "conselho_municipal",
  "conselho_estadual",
  "centro_pop",
  "acolhimento"
)

# url mapping for censo suas microdados (2011-2017)
.censo_suas_urls <- c(
  "2017" = "https://aplicacoes.mds.gov.br/sagi/pesquisas/documentos/microdado/microdado_210.zip",
  "2016" = "https://aplicacoes.mds.gov.br/sagi/pesquisas/documentos/microdado/microdado_209.zip",
 "2015" = "https://aplicacoes.mds.gov.br/sagi/pesquisas/documentos/pdf/microdados_2015.zip",
  "2014" = "https://aplicacoes.mds.gov.br/sagi/pesquisas/documentos/pdf/microdados_2014.zip",
  "2013" = "https://aplicacoes.mds.gov.br/sagi/pesquisas/documentos/pdf/microdado_137.rar",
  "2012" = "https://aplicacoes.mds.gov.br/sagi/pesquisas/documentos/pdf/microdado_134.rar",
  "2011" = "https://aplicacoes.mds.gov.br/sagi/pesquisas/documentos/pdf/microdado_130.rar"
)

# folder patterns for each questionnaire (used to find files in zip)
.censo_suas_folder_patterns <- c(
  "cras" = "CRAS",
  "creas" = "CREAS",
  "gestao_municipal" = "Gest.o_Municipal|Gestao_Municipal",
  "gestao_estadual" = "Gest.o_Estadual|Gestao_Estadual",
  "conselho_municipal" = "Conselho_Municipal",
  "conselho_estadual" = "Conselho_Estadual",
  "centro_pop" = "Centro_POP",
  "acolhimento" = "Unidades_Acolhimento|Acolhimento"
)


#' Download Censo SUAS data
#'
#' Downloads and processes data from Censo SUAS (Sistema Unico de Assistencia
#' Social census). Data includes information about CRAS, CREAS, and municipal
#' management of social assistance services.
#'
#' @param year Integer. Year of the census (2011-2017).
#' @param questionnaire Character. Type of questionnaire. One of: "cras",
#'   "creas", "gestao_municipal", "gestao_estadual", "conselho_municipal",
#'   "conselho_estadual", "centro_pop", "acolhimento".
#' @param uf Character. Optional. Filter by state (UF code, e.g., "SP", "RJ").
#'   Default NULL returns all states.
#' @param format Character. Output format: "tibble" (default), "arrow", or
#'   "parquet".
#' @param cache Logical. Use cached data if available? Default TRUE.
#'
#' @return A tibble (or arrow table/parquet path) with Censo SUAS data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # download cras data for 2017
#' cras_2017 <- get_censo_suas(2017, "cras")
#'
#' # download only for sao paulo state
#' cras_sp <- get_censo_suas(2017, "cras", uf = "SP")
#'
#' # return as arrow table
#' cras_arrow <- get_censo_suas(2017, "cras", format = "arrow")
#' }
get_censo_suas <- function(year,
                           questionnaire,
                           uf = NULL,
                           format = "tibble",
                           cache = TRUE) {

  # validate inputs
  checkmate::assert_integerish(year, len = 1)
  checkmate::assert_string(questionnaire)
  checkmate::assert_string(uf, null.ok = TRUE)
  checkmate::assert_choice(format, c("tibble", "arrow", "parquet"))
  checkmate::assert_flag(cache)

  # normalize questionnaire name
  questionnaire <- tolower(questionnaire)

  # validate against available data
  available <- available_censo_suas()
  valid_years <- unique(available$year)
  valid_questionnaires <- unique(available$questionnaire)

  validate_inputs(
    year = year,
    questionnaire = questionnaire,
    valid_years = valid_years,
    valid_questionnaires = valid_questionnaires
  )

  # check cache
  if (cache) {
    cache_path <- check_cache("censo_suas", year, questionnaire)
    if (!is.null(cache_path)) {
      cli::cli_alert_info("Loading from cache...")
      data <- load_from_cache(cache_path)

      # filter by uf if requested
      if (!is.null(uf)) {
        data <- filter_by_uf(data, uf)
      }

      return(read_with_format(data, format))
    }
  }

  # build download url
  url <- build_censo_suas_url(year, questionnaire)

  # download data
  cli::cli_alert_info("Downloading Censo SUAS {questionnaire} {year}...")
  temp_file <- download_with_progress(url, quiet = TRUE)

  # read and process data
  data <- read_censo_suas_file(temp_file, year, questionnaire)

  # save to cache
  if (cache) {
    save_to_cache(data, "censo_suas", year, questionnaire)
    cli::cli_alert_success("Data cached for future use")
  }

  # cleanup temp file
  unlink(temp_file)

  # filter by uf if requested
  if (!is.null(uf)) {
    data <- filter_by_uf(data, uf)
  }

  cli::cli_alert_success("Done! {nrow(data)} rows loaded")

  # return in requested format
  read_with_format(data, format)
}


#' List available Censo SUAS datasets
#'
#' Returns a tibble with all available Censo SUAS datasets, including
#' years and questionnaire types.
#'
#' @return A tibble with columns: year, questionnaire, description, source.
#'
#' @export
#'
#' @examples
#' available_censo_suas()
available_censo_suas <- function() {

  # define available datasets
  # microdados sagi: 2011-2017
  tibble::tibble(
    year = rep(2011:2017, each = 8),
    questionnaire = rep(.censo_suas_questionnaires, times = 7),
    description = dplyr::case_when(
      questionnaire == "cras" ~ "Centro de Referencia de Assistencia Social",
      questionnaire == "creas" ~ "Centro de Referencia Especializado de Assistencia Social",
      questionnaire == "gestao_municipal" ~ "Gestao Municipal do SUAS",
      questionnaire == "gestao_estadual" ~ "Gestao Estadual do SUAS",
      questionnaire == "conselho_municipal" ~ "Conselho Municipal de Assistencia Social",
      questionnaire == "conselho_estadual" ~ "Conselho Estadual de Assistencia Social",
      questionnaire == "centro_pop" ~ "Centro de Referencia para Populacao em Situacao de Rua",
      questionnaire == "acolhimento" ~ "Unidades de Acolhimento",
      TRUE ~ NA_character_
    ),
    source = "microdados_sagi"
  )
}


#' Get Censo SUAS data dictionary
#'
#' Returns the data dictionary (variable descriptions) for a specific
#' Censo SUAS dataset. Dictionaries are available for 2017 and are used
#' as reference for all years (variable structure is similar across years).
#'
#' @param year Integer. Year of the census (2011-2017).
#' @param questionnaire Character. Type of questionnaire.
#'
#' @return A tibble with columns: variable, description.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # get dictionary for cras
#' dict <- dictionary_censo_suas(2017, "cras")
#' head(dict)
#' }
dictionary_censo_suas <- function(year, questionnaire) {

  # validate inputs
  checkmate::assert_integerish(year, len = 1)
  checkmate::assert_string(questionnaire)

  questionnaire <- tolower(questionnaire)

  validate_inputs(
    year = year,
    questionnaire = questionnaire,
    valid_years = unique(available_censo_suas()$year),
    valid_questionnaires = .censo_suas_questionnaires
  )

  # build dictionary file path
  # dictionaries are from 2017 but apply to all years
  dict_file <- stringr::str_c("dict_", questionnaire, "_2017.csv")
  dict_path <- system.file(
    "extdata", "dictionaries", "censo_suas_2017", dict_file,
    package = "welfarebR"
  )

  # check if dictionary exists
  if (dict_path == "" || !file.exists(dict_path)) {
    cli::cli_alert_warning("Dictionary not available for {questionnaire}")
    return(tibble::tibble(
      variable = character(),
      description = character()
    ))
  }

  # read dictionary
  dict <- readr::read_csv2(
    dict_path,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "Latin1")
  ) |>
    janitor::clean_names()

  # standardize column names
  if ("variavel" %in% names(dict)) {
    dict <- dict |> dplyr::rename(variable = variavel)
  }
  if ("descricao" %in% names(dict)) {
    dict <- dict |> dplyr::rename(description = descricao)
  }

  cli::cli_alert_success("Dictionary loaded: {nrow(dict)} variables")

  dict
}


# internal helper functions ------------------------------------------------

#' Build Censo SUAS download URL
#'
#' @param year Integer. Year.
#' @param questionnaire Character. Questionnaire type (not used for 2011-2017,
#'   as all questionnaires are in a single archive).
#'
#' @return Character. Download URL.
#'
#' @keywords internal
build_censo_suas_url <- function(year, questionnaire) {

  year_char <- as.character(year)

  # use url mapping for 2011-2017
  if (year_char %in% names(.censo_suas_urls)) {
    return(.censo_suas_urls[[year_char]])
  }

  # future: add urls for more recent years
  cli::cli_abort(c(
    "No URL available for year {year}",
    "i" = "Available years: {names(.censo_suas_urls)}"
  ))
}


#' Read and process Censo SUAS file
#'
#' @param path Character. Path to downloaded file.
#' @param year Integer. Year of data.
#' @param questionnaire Character. Questionnaire type.
#'
#' @return A tibble with processed data.
#'
#' @keywords internal
read_censo_suas_file <- function(path, year, questionnaire) {

  # detect file type
  ext <- tolower(tools::file_ext(path))

  # handle compressed files
  if (ext %in% c("zip", "rar")) {
    # create unique temp directory for extraction
    temp_dir <- file.path(tempdir(), stringr::str_c("censo_suas_", year))
    if (dir.exists(temp_dir)) {
      unlink(temp_dir, recursive = TRUE)
    }
    dir.create(temp_dir, recursive = TRUE)

    if (ext == "zip") {
      cli::cli_alert_info("Extracting archive...")
      utils::unzip(path, exdir = temp_dir)
    } else {
      # rar requires archive package
      if (!requireNamespace("archive", quietly = TRUE)) {
        cli::cli_abort(c(
          "RAR files require the {.pkg archive} package",
          "i" = "Install with: install.packages('archive')"
        ))
      }
      cli::cli_alert_info("Extracting RAR archive...")
      archive::archive_extract(path, dir = temp_dir)
    }

    # find the questionnaire folder using pattern
    folder_pattern <- .censo_suas_folder_patterns[[questionnaire]]
    if (is.null(folder_pattern)) {
      cli::cli_abort("Unknown questionnaire: {questionnaire}")
    }

    # list all files recursively
    all_files <- list.files(temp_dir, recursive = TRUE, full.names = TRUE)

    # find base de dados csv file (not RH, not dicionario)
    data_files <- all_files[
      grepl(folder_pattern, all_files, ignore.case = TRUE) &
      grepl("Base de dados\\.csv$", all_files, ignore.case = TRUE) &
      !grepl("_RH_", all_files, ignore.case = TRUE)
    ]

    if (length(data_files) == 0) {
      # try alternative patterns
      data_files <- all_files[
        grepl(folder_pattern, all_files, ignore.case = TRUE) &
        grepl("divulga.*\\.csv$", all_files, ignore.case = TRUE) &
        !grepl("Dicion", all_files, ignore.case = TRUE) &
        !grepl("_RH_", all_files, ignore.case = TRUE)
      ]
    }

    if (length(data_files) == 0) {
      cli::cli_abort(c(
        "Could not find data file for {questionnaire} in archive",
        "i" = "Pattern used: {folder_pattern}"
      ))
    }

    path <- data_files[1]
    cli::cli_alert_info("Found: {basename(path)}")
    ext <- tolower(tools::file_ext(path))
  }

  # read based on file type
  cli::cli_alert_info("Reading data...")
  data <- if (ext == "csv") {
    readr::read_csv2(path, show_col_types = FALSE, locale = readr::locale(encoding = "Latin1"))
  } else if (ext == "txt") {
    readr::read_delim(path, delim = ";", show_col_types = FALSE, locale = readr::locale(encoding = "Latin1"))
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg readxl} is required to read Excel files")
    }
    readxl::read_excel(path)
  } else {
    cli::cli_abort("Unsupported file format: {ext}")
  }

  # clean column names
  data <- data |>
    janitor::clean_names()

  data
}


#' Filter data by UF (state)
#'
#' @param data A tibble.
#' @param uf Character. UF code.
#'
#' @return Filtered tibble.
#'
#' @keywords internal
filter_by_uf <- function(data, uf) {

  checkmate::assert_string(uf)
  uf_value <- toupper(uf)

  # try common column names for uf
  uf_cols <- c("uf", "sigla_uf", "sg_uf", "estado", "co_uf")
  found_col <- intersect(names(data), uf_cols)

  if (length(found_col) == 0) {
    cli::cli_alert_warning("Could not find UF column. Returning unfiltered data.")
    return(data)
  }

  col_name <- found_col[1]
  data |>
    dplyr::filter(toupper(!!rlang::sym(col_name)) == uf_value)
}
