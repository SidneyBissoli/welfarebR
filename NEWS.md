# welfarebR (development version)

## welfarebR 0.0.0.9000

* Initial development version.
* Added `get_censo_suas()` to download Censo SUAS data (CRAS, CREAS, etc.).
* Added `available_censo_suas()` to list available Censo SUAS datasets.
* Added `dictionary_censo_suas()` to get variable descriptions.
* Added `get_cadunico()` to download CadUnico sample data.
* Added `available_cadunico()` to list available CadUnico years.
* Added `dictionary_cadunico()` to get variable descriptions.
* Added survey design support via `as_survey` parameter in `get_cadunico()`.
* Added caching system with `get_cache_dir()`, `set_cache_dir()`, and `clear_welfarebr_cache()`.
* Support for multiple output formats: tibble, arrow, parquet.
