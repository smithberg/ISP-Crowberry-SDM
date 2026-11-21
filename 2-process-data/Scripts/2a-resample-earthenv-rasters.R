# 2a-resample-earthenv-to-chelsa.R
# Resamples the six EarthEnv topography layers onto the CHELSA grid so every
# predictor shares exactly the same extent, resolution and cell alignment.
#
# Reads:  1-download-data/Downloads/EarthEnv/*_iceland.tif      (6 layers)
#         1-download-data/Downloads/CHELSA-BIOCLIM/**/*.tif      (grid reference only)
# Writes: 2-process-data/Outputs/EarthEnv resampled/<name>_resampled.tif
# The original EarthEnv and CHELSA files are never modified.

# install.packages("terra")
library(terra)

# ---- Settings ---------------------------------------------------------------
# All six layers are continuous, so bilinear is appropriate. Change a layer to
# "near" here if you ever add a categorical EarthEnv layer.
resample_method <- "bilinear"
overwrite       <- TRUE   # only ever overwrites files in the output folder

# ---- Locate project root ----------------------------------------------------
find_project_root <- function(name = "ISP-Crowberry-SDM", marker = "1-download-data") {
  is_root <- function(d) dir.exists(file.path(d, marker))
  
  script_dir <- tryCatch({
    f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    if (length(f)) {
      dirname(normalizePath(f))
    } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      dirname(rstudioapi::getSourceEditorContext()$path)
    } else NULL
  }, error = function(e) NULL)
  
  for (s in c(script_dir, getwd())) {
    if (is.null(s) || !nzchar(s)) next
    d <- normalizePath(s, mustWork = FALSE)
    repeat {
      if (is_root(d)) return(d)
      parent <- dirname(d)
      if (parent == d) break
      d <- parent
    }
  }
  
  home <- path.expand("~")
  tops <- list.dirs(home, recursive = FALSE)
  tops <- tops[basename(tops) != "Library"]
  patterns <- c(file.path(home, name),
                unlist(lapply(0:5, function(k) paste0(tops, "/", strrep("*/", k), name))))
  hits <- unique(Sys.glob(patterns))
  hits <- hits[vapply(hits, is_root, logical(1))]
  
  if (length(hits) == 0) stop("Couldn't find '", name, "' under ", home)
  normalizePath(hits[1])
}

proj_root <- find_project_root()
message("Project root: ", proj_root)
p <- function(...) file.path(proj_root, ...)

downloads_dir <- p("1-download-data", "Downloads")
out_dir       <- p("2-process-data", "Outputs", "EarthEnv resampled")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Find inputs ------------------------------------------------------------
topo_files <- list.files(file.path(downloads_dir, "EarthEnv"),
                         pattern = "_iceland\\.tif$", full.names = TRUE)
if (length(topo_files) == 0) stop("No *_iceland.tif files in ", file.path(downloads_dir, "EarthEnv"))
if (length(topo_files) != 6) warning("Expected 6 EarthEnv layers but found ", length(topo_files))

chelsa_files <- list.files(file.path(downloads_dir, "CHELSA-BIOCLIM"),
                           pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
if (length(chelsa_files) == 0) stop("No .tif files in ", file.path(downloads_dir, "CHELSA-BIOCLIM"))

# Any CHELSA layer works as the reference (they share one grid); prefer bio01.
ref_file <- grep("bio01_1981-2010_iceland\\.tif$", chelsa_files, value = TRUE)[1]
if (is.na(ref_file)) ref_file <- chelsa_files[1]
ref <- terra::rast(ref_file)[[1]]
message("Grid reference: ", basename(ref_file),
        "  (", terra::ncol(ref), " x ", terra::nrow(ref), " cells)")
message("EarthEnv layers: ", length(topo_files), "\n")

# ---- Resample ---------------------------------------------------------------
results <- lapply(topo_files, function(f) {
  src <- terra::rast(f)
  var <- sub("_1KM.*$", "", basename(f))        # e.g. "eastness"
  
  if (!isTRUE(terra::same.crs(src, ref))) {
    warning(basename(f), ": CRS differs from the CHELSA reference")
  }
  
  out_file <- file.path(out_dir, sub("\\.tif$", "_resampled.tif", basename(f)))
  message("Resampling ", basename(f), " -> ", basename(out_file))
  
  res <- terra::resample(src, ref, method = resample_method)
  names(res) <- var
  terra::writeRaster(res, out_file, overwrite = overwrite,
                     datatype = "FLT4S", gdal = c("COMPRESS=LZW"))
  
  # Re-read the saved file and verify it really matches the CHELSA grid
  chk <- terra::rast(out_file)
  same_grid <- isTRUE(tryCatch(
    terra::compareGeom(ref, chk, crs = TRUE, ext = TRUE, rowcol = TRUE, res = TRUE,
                       stopOnError = FALSE),
    error = function(e) FALSE))
  rng <- terra::global(chk, c("min", "max"), na.rm = TRUE)
  
  data.frame(
    layer      = var,
    file       = basename(out_file),
    same_grid  = same_grid,
    na_cells   = as.numeric(terra::global(chk, "isNA")[1, 1]),
    min        = round(rng$min, 4),
    max        = round(rng$max, 4),
    stringsAsFactors = FALSE
  )
})
summary_tbl <- do.call(rbind, results)

# ---- Report -----------------------------------------------------------------
message("\n=== Resampling summary ===")
print(summary_tbl, row.names = FALSE)

if (all(summary_tbl$same_grid)) {
  message("\nGRID: all ", nrow(summary_tbl), " resampled layers match the CHELSA grid.")
} else {
  message("\nGRID PROBLEM: these layers still do not match the CHELSA grid:")
  message(paste0("  - ", summary_tbl$file[!summary_tbl$same_grid], collapse = "\n"))
}

message("NA cells: the CHELSA grid has one more row than EarthEnv, so each layer gets ",
        "a row of NA at the top (ocean north of Iceland). Nothing is masked here; ",
        "build the land mask in a separate step.")
message("\nSaved to: ", out_dir)