# 2c-clip-rasters-to-land.R
# Clips every CHELSA and EarthEnv layer to the IS 50V land polygon.
# Rule: a cell is kept if ANY part of it touches land (terra: touches = TRUE).
# One land mask is built once on the CHELSA grid and applied to every layer, so all
# outputs share the same extent and the same land cells.
#
# CHELSA variables listed by 2b-filter-chelsa-variables.R are skipped, so they never reach the
# clipped folder, the fill step or the predictor screening. Run 2b first.
#
# Put this file in: ISP-Crowberry-SDM/2-process-data/Scripts/
# Reads:
#   1-download-data/Downloads/CHELSA-BIOCLIM/**/*.tif            CHELSA layers (all scenarios)
#   2-process-data/Outputs/Predictor filtering/excluded_variables.csv   variables to skip (from 2b)
#   2-process-data/Outputs/EarthEnv resampled/*.tif               EarthEnv layers
#   1-download-data/Downloads/LMI-IS50V/is50v_land_polygon.gpkg   land polygon
# Writes (inputs are never changed):
#   2-process-data/Outputs/CHELSA-BIOCLIM clipped/                same file names as the inputs
#   2-process-data/Outputs/EarthEnv clipped+resampled/            same file names as the inputs
#   2-process-data/Outputs/Raster clip/land_mask_touches.tif                  the mask (1 = land cell)
#   2-process-data/Outputs/Raster clip/raster_clip_report.csv                 cells with data per layer
#
# Cell values are not changed and NA cells are not filled. Layers that already had NA
# blocks (gdd10, swe, gst ...) keep them; the report shows which layers those are.
#
# NOT TESTED on your data. Check the report and one clipped layer before using the output.

# install.packages("terra")
library(terra)
terraOptions(progress = 0)

# ---- Settings ---------------------------------------------------------------
overwrite <- FALSE   # FALSE = skip layers whose clipped file already exists (safe to re-run)

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

# ---- Paths ------------------------------------------------------------------
chelsa_in  <- p("1-download-data", "Downloads", "CHELSA-BIOCLIM")
earth_in   <- p("2-process-data", "Outputs", "EarthEnv resampled")
land_file  <- p("1-download-data", "Downloads", "LMI-IS50V", "is50v_land_polygon.gpkg")
filter_file <- p("2-process-data", "Outputs", "Predictor filtering", "excluded_variables.csv")

chelsa_out <- p("2-process-data", "Outputs", "CHELSA-BIOCLIM clipped")
earth_out  <- p("2-process-data", "Outputs", "EarthEnv clipped+resampled")
mask_file  <- p("2-process-data", "Outputs", "Raster clip/land_mask_touches.tif")
report_file <- p("2-process-data", "Outputs", "Raster clip/raster_clip_report.csv")

for (d in c(chelsa_out, earth_out)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

chelsa_all   <- list.files(chelsa_in, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
earth_files  <- list.files(earth_in,  pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

# ---- Leave out the variables excluded in 2b ---------------------------------
if (!file.exists(filter_file)) {
  stop("Missing ", filter_file, "\nRun 2b-filter-chelsa-variables.R first.")
}
excl <- read.csv(filter_file, stringsAsFactors = FALSE)
if (!("variable" %in% names(excl))) stop("excluded_variables.csv needs a 'variable' column.")
excl_vars <- unique(excl$variable)

# The variable name sits between underscores (or at the start), so "gdd10" does not match "gddlgd10".
var_pat <- function(v) paste0("(^|_)", v, "_")
is_excluded <- function(files) {
  if (length(excl_vars) == 0) return(rep(FALSE, length(files)))
  Reduce(`|`, lapply(excl_vars, function(v) grepl(var_pat(v), basename(files))))
}
skip <- is_excluded(chelsa_all)
chelsa_files <- chelsa_all[!skip]

message("CHELSA layers found:   ", length(chelsa_all))
message("  skipped (2b filter): ", sum(skip), "  (", paste(excl_vars, collapse = ", "), ")")
message("  to clip:             ", length(chelsa_files))
message("EarthEnv layers found: ", length(earth_files))
if (length(chelsa_files) == 0) stop("No CHELSA layers left to clip in ", chelsa_in)
if (length(earth_files) == 0) warning("No .tif files in ", earth_in, "; only CHELSA will be clipped.")
if (!file.exists(land_file)) {
  stop("Land polygon not found: ", land_file, "\nRun lmi-is50v-coastline-download.R first.")
}

# Old clipped copies of the skipped variables (from before the filter existed) would otherwise
# still be picked up by the later scripts, so they are removed here.
old_clipped <- list.files(chelsa_out, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
old_clipped <- old_clipped[is_excluded(old_clipped)]
if (length(old_clipped) > 0) {
  unlink(old_clipped)
  message("Removed ", length(old_clipped), " old clipped file(s) of the excluded variables.")
}

# ---- Reference grid and land mask (built once) ------------------------------
ref      <- rast(chelsa_files[1])[[1]]
template <- rast(ref)                       # grid only, no values
message("Reference grid: ", basename(chelsa_files[1]), "  (", nrow(ref), " x ", ncol(ref), " cells)")

land <- vect(land_file)
land <- project(land, crs(ref))

land_mask   <- rasterize(land, template, field = 1, touches = TRUE)   # 1 where any land touches the cell
land_mask_c <- trim(land_mask)                                        # shared extent for every layer
ext_c       <- ext(land_mask_c)
mask_cells  <- as.numeric(global(land_mask_c, "notNA")[1, 1])

message("Land cells (any part touching land): ", format(mask_cells, big.mark = ","),
        "  of ", format(ncell(ref), big.mark = ","), " in the full grid")
message("Clipped grid: ", nrow(land_mask_c), " x ", ncol(land_mask_c), " cells")

dir.create(dirname(mask_file), showWarnings = FALSE, recursive = TRUE)
writeRaster(land_mask_c, mask_file, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
message("Saved mask: ", mask_file)

# ---- Clip one layer ---------------------------------------------------------
clip_one <- function(f, out_file) {
  r <- rast(f)
  
  same_grid <- isTRUE(tryCatch(
    compareGeom(template, r, crs = TRUE, ext = TRUE, rowcol = TRUE, res = TRUE, stopOnError = FALSE),
    error = function(e) FALSE))
  resampled <- FALSE
  if (!same_grid) {
    r <- resample(r, template, method = "bilinear")   # continuous layers only
    resampled <- TRUE
  }
  
  r <- crop(r, ext_c)
  r <- mask(r, land_mask_c)
  
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(out_file, ".part")
  writeRaster(r, tmp, filetype = "GTiff", overwrite = TRUE, gdal = c("COMPRESS=LZW"))
  file.rename(tmp, out_file)
  
  list(non_NA = as.numeric(global(r, "notNA")[1, 1]), resampled = resampled)
}

# ---- Main loop --------------------------------------------------------------
jobs <- rbind(
  data.frame(src = "CHELSA",
             in_file = chelsa_files,
             out_file = file.path(chelsa_out, substring(chelsa_files, nchar(chelsa_in) + 2)),
             stringsAsFactors = FALSE),
  if (length(earth_files) > 0)
    data.frame(src = "EarthEnv",
               in_file = earth_files,
               out_file = file.path(earth_out, substring(earth_files, nchar(earth_in) + 2)),
               stringsAsFactors = FALSE)
)
n <- nrow(jobs)

rows   <- vector("list", n)
failed <- character(0)
t0 <- Sys.time()
unlink(list.files(c(chelsa_out, earth_out), pattern = "\\.part$", recursive = TRUE, full.names = TRUE))

for (i in seq_len(n)) {
  f   <- jobs$in_file[i]
  out <- jobs$out_file[i]
  nm  <- basename(f)
  
  if (file.exists(out) && !overwrite) {
    res <- tryCatch({
      rr <- rast(out)
      list(non_NA = as.numeric(global(rr, "notNA")[1, 1]), resampled = NA)
    }, error = function(e) NULL)
    if (!is.null(res)) {
      rows[[i]] <- data.frame(file = nm, source = jobs$src[i], non_NA_cells = res$non_NA,
                              mask_cells = mask_cells, status = "already done", stringsAsFactors = FALSE)
      if (i %% 25 == 0 || i == n) message(sprintf("[%d/%d] %s", i, n, "(skipping existing files...)"))
      next
    }
  }
  
  res <- tryCatch(clip_one(f, out), error = function(e) {
    message(sprintf("[%d/%d] %s  -> FAILED: %s", i, n, nm, conditionMessage(e)))
    NULL
  })
  
  if (is.null(res)) {
    failed <- c(failed, f)
    rows[[i]] <- data.frame(file = nm, source = jobs$src[i], non_NA_cells = NA_real_,
                            mask_cells = mask_cells, status = "FAILED", stringsAsFactors = FALSE)
  } else {
    rows[[i]] <- data.frame(file = nm, source = jobs$src[i], non_NA_cells = res$non_NA,
                            mask_cells = mask_cells,
                            status = if (isTRUE(res$resampled)) "clipped (resampled first)" else "clipped",
                            stringsAsFactors = FALSE)
    if (i %% 10 == 0 || i == n) message(sprintf("[%d/%d] %s  -> done", i, n, nm))
  }
}

# ---- Report -----------------------------------------------------------------
report <- do.call(rbind, rows)
report$fraction_of_land_cells_with_data <- round(report$non_NA_cells / report$mask_cells, 4)
dir.create(dirname(report_file), showWarnings = FALSE, recursive = TRUE)
write.csv(report, report_file, row.names = FALSE)

n_holes <- sum(report$fraction_of_land_cells_with_data < 1, na.rm = TRUE)
n_resampled <- sum(report$status == "clipped (resampled first)")

message("\n=== Finished in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min ===")
message("Layers clipped or already done: ", sum(report$status != "FAILED"), " of ", n)
message("Layers skipped by the 2b filter: ", sum(skip))
message("Layers resampled to the CHELSA grid first: ", n_resampled)
message("Layers with NA cells inside the land mask: ", n_holes, "  (see raster_clip_report.csv)")
message("Failed: ", length(failed))
if (length(failed) > 0) {
  fail_log <- p("2-process-data", "Outputs", "failed_clips.txt")
  writeLines(failed, fail_log)
  message("Failed files listed in: ", fail_log, "\nRe-run this script to retry them.")
}
message("Clipped CHELSA layers:   ", chelsa_out)
message("Clipped EarthEnv layers: ", earth_out)
message("Report:                  ", report_file)