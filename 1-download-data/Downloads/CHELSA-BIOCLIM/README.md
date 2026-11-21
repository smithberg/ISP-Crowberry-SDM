# CHELSA-BIOCLIM+ (Iceland clip)

Climate rasters from CHELSA-BIOCLIM+, clipped to Iceland. They are used as predictors in the crowberry MaxEnt model.

## What is here

- 390 GeoTIFFs: 39 variables x 10 conditions
  - 1981-2010 baseline
  - GFDL-ESM4 under SSP1-2.6, SSP3-7.0 and SSP5-8.5, each for 2011-2040, 2041-2070 and 2071-2100
- Clipped to lon -25.0 to -12.5, lat 63.0 to 67.0 (includes some ocean margin)
- Native CHELSA resolution (30 arc-seconds, about 1 km), WGS84, Float32, LZW-compressed
- File names:
  - Baseline: `<variable>_1981-2010_iceland.tif`, e.g. `bio01_1981-2010_iceland.tif`
  - Future: `gfdl-esm4_<ssp>_<variable>_<period>_iceland.tif`, e.g. `gfdl-esm4_ssp126_bio01_2041-2070_iceland.tif`

## How it was made

Files were produced by `1-download-data/Scripts/chelsa-bioclim-download-iceland.R`.

- The script reads the URL list in `1-download-data/Input data/CHELSA_bioclim_download_list_GFDL-ESM4.txt`.
- For each URL it reads only the Iceland window straight from CHELSA's server, so the global files are never downloaded, and saves the clipped GeoTIFF here.
- It skips files that are already complete, so you can stop and rerun it.
- Files that fail after three tries are listed in `failed_downloads.txt` (this file exists only if something failed). Rerun the script to retry them.
- To regenerate this folder, run the script from the project. The URL list is the only input needed.

Downloaded on: 15 October 2026

## Source and citation

Brun, P., Zimmermann, N. E., Hari, C., Pellissier, L., & Karger, D. N. (2022). Global climate-related predictors at kilometer resolution for the past and future. Earth System Science Data, 14, 5573-5603. https://doi.org/10.5194/essd-14-5573-2022

Dataset: CHELSA-BIOCLIM+, https://doi.org/10.16904/envidat.332

Data: https://chelsa-climate.org

Licence: CC0 1.0 (public domain dedication), as listed on the EnviDat dataset page. Please still cite the sources above.