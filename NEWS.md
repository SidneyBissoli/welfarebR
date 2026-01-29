# welfarebR 0.1.0

Initial CRAN release.
## Censo SUAS

* Added `get_censo_suas()` to download Censo SUAS data (2011-2017).
* Added `available_censo_suas()` to list available datasets.
* Added `dictionary_censo_suas()` to get variable descriptions.
* Supported questionnaires: CRAS, CREAS, Gestao Municipal, Gestao Estadual, Conselho Municipal, Conselho Estadual, Centro POP, Acolhimento.

## CadUnico

* Added `get_cadunico()` to download CadUnico sample data (2012-2018).
* Added `available_cadunico()` to list available years.
* Added `dictionary_cadunico()` to get variable descriptions.
* Added `sampling_plan_cadunico()` for sampling methodology information.
* Added survey design support via `as_survey = TRUE` parameter.

## Cache System

* Added `get_cache_dir()` to get current cache directory.
* Added `set_cache_dir()` to set custom cache directory.
* Added `clear_welfarebr_cache()` to clear cached data.

## Output Formats

* Support for tibble (default), arrow, and parquet output formats.
