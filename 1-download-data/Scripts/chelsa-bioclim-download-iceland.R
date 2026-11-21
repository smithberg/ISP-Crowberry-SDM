# =============================================================================
# Download CHELSA-BIOCLIM+ rasters, clipped to Iceland
#
# What it does
#   - Reads the list of CHELSA URLs from "1-download-data/Input data"
#   - For each URL, reads ONLY the Iceland window straight from CHELSA's server
#     (the global file is never downloaded) and saves it as a small GeoTIFF in
#     "1-download-data/Downloads/CHELSA-BIOCLIM"
#   - Goes one file at a time
#   - Skips files that are already done, so you can stop and re-run at any time
#     (after a crash, lost wifi, closing RStudio...) and it picks up where it left off
#   - Files that fail are retried, then logged and skipped; re-run the script
#     to try the failed ones again
#
# How to use
#   1. Keep this script in ISP-Crowberry-SDM/1-download-data/Scripts. It finds the project
#      folder from its own location (or from R's working directory), so files always go into
#      that project; no RStudio Project (.Rproj) is needed. The first lines of output print
#      the folder it chose.
#   2. Put CHELSA_bioclim_download_list*.txt in ISP-Crowberry-SDM/1-download-data/Input data
#   3. Run the whole script (Source button, or Cmd/Ctrl + Shift + Enter)
# =============================================================================

# --- Packages ----------------------------------------------------------------
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
library(terra)

# --- Locate project root ------------------------------------------------------
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

# --- Settings (edit these if needed) -----------------------------------------
input_dir  <- file.path(proj_root, "1-download-data", "Input data")
output_dir <- file.path(proj_root, "1-download-data", "Downloads", "CHELSA-BIOCLIM")

# Iceland bounding box in degrees: xmin, xmax, ymin, ymax (a little ocean margin included)
iceland_ext <- ext(-25.0, -12.5, 63.0, 67.0)

max_tries <- 3             # attempts per file before giving up on it (for this run)
retry_wait_seconds <- 10   # waits 10 s after attempt 1, 20 s after attempt 2, ...

# If reading just the Iceland window from the server fails, you can allow the script to
# download the whole global file instead (slow and large, ~hundreds of MB each). Default: off.
allow_full_download <- FALSE

# --- Find the URL list -------------------------------------------------------
url_file <- list.files(input_dir, pattern = "^CHELSA_bioclim_download_list.*\\.txt$",
                       full.names = TRUE)
if (length(url_file) == 0) stop("No CHELSA_bioclim_download_list*.txt found in: ", input_dir)
if (length(url_file) > 1) {
  warning("More than one URL list found; using: ", basename(url_file[1]))
}
url_file <- url_file[1]

urls <- trimws(readLines(url_file, warn = FALSE))
urls <- urls[grepl("^https?://", urls)]   # drops blank lines and # comments
n <- length(urls)
if (n == 0) stop("No URLs found in ", url_file)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Remove leftovers of an interrupted write (unfinished ".part" files)
unlink(list.files(output_dir, pattern = "\\.part$", full.names = TRUE))

message("URL list:   ", url_file)
message("Output dir: ", output_dir)
message("Files to get: ", n, "\n")

# --- Helpers -----------------------------------------------------------------

# Short output name, e.g. bio01_1981-2010_iceland.tif or
# gfdl-esm4_ssp126_bio01_2041-2070_iceland.tif
out_name <- function(url) {
  f <- basename(url)
  f <- sub("^CHELSA_", "", f)
  f <- sub("_V\\.2\\.1", "", f)
  sub("\\.tif$", "_iceland.tif", f)
}

# TRUE only if the file exists and can be opened as a raster
is_valid <- function(path) {
  file.exists(path) && file.size(path) > 0 &&
    !inherits(try(rast(path), silent = TRUE), "try-error")
}

# Crop a raster source (a /vsicurl/ web path or a local file) to Iceland and save it.
# It is written to a temporary ".part" file first and renamed only when complete,
# so a crash in the middle can never leave a half-written file that looks finished.
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

# Settings that make reading from the web more robust
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
  
  # Optional last resort: download the whole global file, then clip it
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
    # sanity check: the saved window should not be entirely empty
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
if (length(failed) > 0) {
  fail_log <- file.path(output_dir, "failed_downloads.txt")
  writeLines(failed, fail_log)
  message("Failed URLs listed in: ", fail_log, "\nRe-run this script to retry them.")
} else {
  fail_log <- file.path(output_dir, "failed_downloads.txt")
  if (file.exists(fail_log)) file.remove(fail_log)
  message("All ", n, " files are in: ", output_dir)
}