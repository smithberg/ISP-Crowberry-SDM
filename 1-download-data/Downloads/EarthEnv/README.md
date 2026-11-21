# EarthEnv topography (Iceland clip)

Terrain layers from EarthEnv, clipped to Iceland. They are used as predictors in the crowberry MaxEnt model.

## What is here

Six GeoTIFFs, one per variable:

- `elevation_1KMmd_GMTEDmd_iceland.tif`
- `slope_1KMmd_GMTEDmd_iceland.tif`
- `northness_1KMmd_GMTEDmd_iceland.tif`
- `eastness_1KMmd_GMTEDmd_iceland.tif`
- `tpi_1KMmd_GMTEDmd_iceland.tif` (topographic position index)
- `roughness_1KMmd_GMTEDmd_iceland.tif`

Details:
- Derived from the 250 m GMTED2010 elevation model and summarised to 1 km cells using the median (`md` in the file names). The SRTM-based layers were not used because they stop at 60 N and do not cover Iceland.
- Clipped to lon -25.0 to -12.5, lat 63.0 to 67.0, the same window as the CHELSA clip.
- Float32, LZW-compressed.
- Topography has no time dimension, so the same files are used for the current climate and every future scenario.

## How it was made

Files were produced by `1-download-data/Scripts/earthenv-topo-download-iceland.R`.

- For each variable the script reads only the Iceland window straight from `https://data.earthenv.org/topography`, so the global files are never downloaded, and saves it here.
- It skips files that are already complete, so you can stop and rerun it.
- Files that fail after three tries are listed in `failed_downloads_topography.txt` (this file exists only if something failed). Rerun the script to retry them.
- At the end it checks that the topography grid matches the CHELSA clips and reports if it doesn't.
- To regenerate this folder, run the script from `1-download-data/Scripts/`. No input files are needed.

Downloaded on: 15 October 2026

## Source and citation

Amatulli, G., Domisch, S., Tuanmu, M.-N., Parmentier, B., Ranipeta, A., Malczyk, J., & Jetz, W. (2018). A suite of global, cross-scale topographic variables for environmental and biodiversity modeling. Scientific Data, 5, 180040. https://doi.org/10.1038/sdata.2018.40

Dataset: https://doi.org/10.1594/PANGAEA.867115

Data: https://www.earthenv.org/topography

Licence: CC BY 3.0, as listed for this dataset on PANGAEA. The EarthEnv website itself states no licence. Please cite the sources above.