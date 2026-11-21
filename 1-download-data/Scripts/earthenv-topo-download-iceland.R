# =============================================================================
# Download EarthEnv topography layers (1 km), clipped to Iceland
#
# Data: Amatulli, G., Domisch, S., Tuanmu, M.-N., Parmentier, B., Ranipeta, A.,
#   Malczyk, J., and Jetz, W. (2018). A suite of global, cross-scale topographic
#   variables for environmental and biodiversity modeling. Scientific Data 5,
#   180040. https://doi.org/10.1038/sdata.2018.40   (https://www.earthenv.org/topography)
#
# What it does (same approach as the CHELSA script)
#   - Reads ONLY the Iceland window straight from the EarthEnv server and saves it
#     as a small GeoTIFF in "1-download-data/Downloads/EarthEnv"
#   - One file at a time; files already done are skipped, so you can stop and
#     re-run at any time; failed files are retried, logged, and skipped
#   - At the end, checks that the topography grid lines up with your CHELSA clips
#     (the ones in "1-download-data/Downloads")
#
# Topography has no time dimension (it is one snapshot of the terrain), so there
# are NO separate files per period or scenario. Use these same files for the
# current climate and for every future scenario.
#
# How to use
#   1. Keep this script in ISP-Crowberry-SDM/1-download-data/Scripts. It finds the project
#      folder from its own location, so files always go into that project (opening the
#      .Rproj in RStudio works too). The first lines of output print the folder it chose.
#   2. Run the whole script (Source button, or Cmd/Ctrl + Shift + Enter)
# =============================================================================

# --- Packages ----------------------------------------------------------------
for (pkg in c("terra", "here")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(terra)
library(here)

# --- Find the project folder -------------------------------------------------
# here() alone uses the folder R was started in. If this script is opened by itself (not
# through the .Rproj), that can be your home folder, and files end up in the wrong place.
# So the project folder is located from this script's own location first, and it must
# contain "1-download-data/Scripts".
find_project_root <- function() {
  is_root <- function(d) dir.exists(file.path(d, "1-download-data", "Scripts"))
  starts <- character(0)
  
  # 1. where this script is saved (RStudio editor, or Rscript --file=)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (length(p) == 1 && !is.na(p) && nzchar(p)) starts <- c(starts, dirname(path.expand(p)))
  }
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f) > 0) starts <- c(starts, dirname(f[1]))
  
  # 2. what here() and the working directory point to
  starts <- c(starts, here::here(), getwd())
  
  for (s in starts) {
    d <- normalizePath(path.expand(s), winslash = "/", mustWork = FALSE)
    repeat {                     # look in this folder, then each folder above it
      if (is_root(d)) return(d)
      up <- dirname(d)
      if (up == d) break
      d <- up
    }
  }
  stop("Could not find your ISP-Crowberry-SDM project folder (a folder that contains ",
       "1-download-data/Scripts).\n  Fix: save this script in 1-download-data/Scripts, or open ",
       "the project's .Rproj file in RStudio, or run setwd(\"<path to ISP-Crowberry-SDM>\") ",
       "and then run this script again.", call. = FALSE)
}
project_root <- find_project_root()
message("Project folder: ", project_root)

# --- Settings (edit these if needed) -----------------------------------------

# Variables to download. Available at 1 km:
#   elevation, slope, eastness, northness, aspectcosine, aspectsine, roughness,
#   tpi (topographic position index), tri (terrain ruggedness index), vrm (vector
#   ruggedness measure), dx, dxx, dy, dyy (partial derivatives), pcurv, tcurv (curvature)
# Note: elevation is strongly tied to CHELSA's temperature layers, so check
# correlations before keeping it (see our earlier discussion).
variables <- c("elevation", "slope", "northness", "eastness", "tpi", "roughness")

# How each 1 km cell summarises the original 250 m terrain: "md" = median, "mn" = mean
statistic <- "md"

base_url <- "https://data.earthenv.org/topography"

# Iceland bounding box in degrees: xmin, xmax, ymin, ymax (same window as the CHELSA clip)
iceland_ext <- ext(-25.0, -12.5, 63.0, 67.0)

downloads_dir <- file.path(project_root, "1-download-data", "Downloads")   # CHELSA clips live here
output_dir    <- file.path(downloads_dir, "EarthEnv")                        # topography goes here

max_tries <- 3
retry_wait_seconds <- 10
allow_full_download <- FALSE   # if TRUE and the partial read fails, downloads each whole global file

# --- Build the list of URLs --------------------------------------------------
# We use the GMTED2010-based files. (The SRTM-based files stop at 60 N and do not cover Iceland.)
# File names follow EarthEnv's own scheme:
#   elevation:       elevation_1KM<stat>_GMTED<stat>.tif   e.g. elevation_1KMmd_GMTEDmd.tif
#   other variables: <variable>_1KM<stat>_GMTEDmd.tif       e.g. slope_1KMmd_GMTEDmd.tif
if (!statistic %in% c("md", "mn")) stop("statistic must be \"md\" (median) or \"mn\" (mean)")

file_names <- ifelse(variables == "elevation",
                     sprintf("elevation_1KM%s_GMTED%s.tif", statistic, statistic),
                     sprintf("%s_1KM%s_GMTEDmd.tif", variables, statistic))
urls <- file.path(base_url, file_names)
n <- length(urls)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
unlink(list.files(output_dir, pattern = "\\.part$", full.names = TRUE))   # leftovers of an interrupted write
message("Output dir: ", output_dir)
message("Files to get: ", n, "  (e.g. ", urls[1], ")\n")

# --- Helpers -----------------------------------------------------------------
out_name <- function(url) sub("\\.tif$", "_iceland.tif", basename(url))

is_valid <- function(path) {
  file.exists(path) && file.size(path) > 0 &&
    !inherits(try(rast(path), silent = TRUE), "try-error")
}

# Crop a raster source (web path or local file) to Iceland and save it. Written to a
# temporary ".part" file and renamed only when complete, so a crash cannot leave a
# half-written file that looks finished.
clip_and_save <- function(src, out) {
  tmp <- paste0(out, ".part")
  if (file.exists(tmp)) file.remove(tmp)
  r <- rast(src)
  r <- crop(r, iceland_ext, snap = "out")
  writeRaster(r, tmp, filetype = "GTiff", datatype = "FLT4S",
              gdal = c("COMPRESS=LZW"), overwrite = TRUE)
  if (!file.rename(tmp, out)) stop("Could not rename ", tmp)
  invisible(TRUE)
}

setGDALconfig(c("GDAL_HTTP_MAX_RETRY=3",
                "GDAL_HTTP_RETRY_DELAY=5",
                "GDAL_HTTP_TIMEOUT=120",
                "GDAL_DISABLE_READDIR_ON_OPEN=EMPTY_DIR",
                "CPL_VSIL_CURL_ALLOWED_EXTENSIONS=.tif"))

# --- Main loop ---------------------------------------------------------------
n_done <- 0; n_skipped <- 0; failed <- character(0)
t0 <- Sys.time()

for (i in seq_along(urls)) {
  url   <- urls[i]
  out   <- file.path(output_dir, out_name(url))
  label <- sprintf("[%d/%d] %s", i, n, basename(out))
  
  if (is_valid(out)) {
    message(label, "  -> already downloaded, skipping")
    n_skipped <- n_skipped + 1
    next
  }
  
  ok <- FALSE
  for (attempt in seq_len(max_tries)) {
    res <- try(clip_and_save(paste0("/vsicurl/", url), out), silent = TRUE)
    if (!inherits(res, "try-error")) { ok <- TRUE; break }
    message(label, "  -> attempt ", attempt, " failed: ",
            trimws(sub("^Error[^:]*:", "", as.character(res))))
    if (attempt < max_tries) Sys.sleep(retry_wait_seconds * attempt)
  }
  
  if (!ok && allow_full_download) {
    message(label, "  -> trying full download (large)...")
    old_timeout <- options(timeout = 7200)
    full <- tempfile(fileext = ".tif")
    res <- try({
      download.file(url, full, mode = "wb", quiet = TRUE)
      clip_and_save(full, out)
    }, silent = TRUE)
    unlink(full)
    options(old_timeout)
    ok <- !inherits(res, "try-error")
  }
  
  if (ok) {
    has_data <- global(rast(out), "notNA")[1, 1] > 0
    if (!has_data) warning(basename(out), " saved but contains no data - check it")
    message(label, "  -> saved")
    n_done <- n_done + 1
  } else {
    message(label, "  -> FAILED, will skip (re-run the script to retry)")
    failed <- c(failed, url)
  }
}

# --- Summary -----------------------------------------------------------------
message("\n=== Finished in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min ===")
message("Saved this run:   ", n_done)
message("Already had:      ", n_skipped)
message("Failed:           ", length(failed))
fail_log <- file.path(output_dir, "failed_downloads_topography.txt")
if (length(failed) > 0) {
  writeLines(failed, fail_log)
  message("Failed URLs listed in: ", fail_log, "\nRe-run this script to retry them.")
} else if (file.exists(fail_log)) {
  file.remove(fail_log)
}

# --- Check: does the topography grid line up with the CHELSA clips? ----------
# Both are 30-arcsecond global grids, so they should match. If they do not, the layers
# must be resampled onto one common grid before modelling.
topo_files   <- file.path(output_dir, out_name(urls))
topo_files   <- topo_files[file.exists(topo_files)]
chelsa_files <- list.files(downloads_dir, pattern = "_1981-2010_iceland\\.tif$", full.names = TRUE)
if (length(topo_files) > 0 && length(chelsa_files) > 0) {
  same_grid <- isTRUE(tryCatch(
    compareGeom(rast(topo_files[1]), rast(chelsa_files[1]), stopOnError = FALSE, res = TRUE),
    error = function(e) FALSE))
  if (same_grid) {
    message("\nGrid check: topography and CHELSA clips share the same grid (extent, cell size, projection). Good.")
  } else {
    message("\nGrid check: topography and CHELSA clips are NOT on exactly the same grid.",
            "\n  Resample them onto one common grid before modelling.",
            "\n  Topography: ", paste(round(c(ext(rast(topo_files[1])) |> as.vector(), res(rast(topo_files[1]))), 5), collapse = " "),
            "\n  CHELSA:     ", paste(round(c(ext(rast(chelsa_files[1])) |> as.vector(), res(rast(chelsa_files[1]))), 5), collapse = " "))
  }
} else if (length(chelsa_files) == 0) {
  message("\n(No CHELSA clips found in Downloads yet, so the grid check was skipped.)")
}

if (length(failed) == 0) message("\nAll ", n, " topography files are in: ", output_dir)