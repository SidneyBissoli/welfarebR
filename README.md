
<!-- README.md is generated from README.Rmd. Please edit that file -->

# welfarebR

<!-- badges: start -->
<!-- badges: end -->

welfarebR provides functions to download and process Brazilian social
assistance data in tidy format. The package gives easy access to data
from: - **Censo SUAS**: Annual census of the Unified Social Assistance
System (CRAS, CREAS, Municipal Management) - **CadUnico**: Deidentified
sample from the Unified Registry for Social Programs

Data sources are from the Ministry of Social Development (MDS).

## Installation

You can install the development version of welfarebR from GitHub:

``` r
# install.packages("pak")
pak::pak("sidneybissoli/welfarebR")
```

## Usage

### Censo SUAS

``` r
library(welfarebR)

# list available datasets
available_censo_suas()

# download CRAS data for 2023
cras_2023 <- get_censo_suas(year = 2023, questionnaire = "cras")

# download only for Sao Paulo state
cras_sp <- get_censo_suas(year = 2023, questionnaire = "cras", uf = "SP")
```

### CadUnico

``` r
# list available years
available_cadunico()

# download CadUnico sample for 2023
cad_2023 <- get_cadunico(year = 2023)

# download as survey design object (for proper weighted analysis)
cad_survey <- get_cadunico(year = 2023, as_survey = TRUE)

# example: calculate weighted statistics
library(srvyr)
cad_survey |>
  summarise(mean_income = survey_mean(renda_per_capita, na.rm = TRUE))
```

### Cache

Data is cached locally by default to avoid repeated downloads:

``` r
# check cache directory
get_cache_dir()

# set custom cache directory
set_cache_dir("~/my_cache")

# clear cache
clear_welfarebr_cache()
```

## Data sources

| Dataset    | Description                       | Years     | Source   |
|------------|-----------------------------------|-----------|----------|
| Censo SUAS | Census of social assistance units | 2011-2023 | MDS/SAGI |
| CadUnico   | Unified Registry sample           | 2012-2024 | MDS/SAGI |

## Related packages

-   [healthbR](https://github.com/sidneybissoli/healthbR): Brazilian
    public health data
-   [educabR](https://github.com/sidneybissoli/educabR): Brazilian
    education data

## License

MIT
