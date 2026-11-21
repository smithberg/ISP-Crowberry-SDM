# 1b-gbif-spatial-thin.R
# Spatially thins the land-filtered GBIF crowberry records so no two kept points are < 5 km apart.
#
# Put this file in: ISP-Crowberry-SDM/2-process-data/Scripts/
# Reads:  2-process-data/Outputs/**/crowberry_occurrences_land_only.csv   (made by gbif-filter-to-land.R)
#         2-process-data/Outputs/land_mask_touches.tif                    (optional, for a final check)
# Writes: 2-process-data/Outputs/GBIF thinned/crowberry_occurrences_thinned_5km.csv
#         2-process-data/Outputs/GBIF thinned/gbif_thinning_map_5km.png
# Inputs are only read, never changed.
#
# NOT TESTED on your data. Check the counts and the map before using the output.

# install.packages(c("readr", "dplyr", "spThin", "terra"))

# ---- Settings ---------------------------------------------------------------
thin_km <- 5
reps    <- 100
seed    <- 42

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
outputs_dir <- p("2-process-data", "Outputs")
out_dir     <- file.path(outputs_dir, "GBIF thinned")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_file <- file.path(out_dir, "crowberry_occurrences_thinned_5km.csv")
map_file <- file.path(out_dir, "gbif_thinning_map_5km.png")
mask_file <- file.path(outputs_dir, "land_mask_touches.tif")

# ---- Read land-filtered occurrences -----------------------------------------
in_file <- list.files(outputs_dir, pattern = "^crowberry_occurrences_land_only\\.csv$",
                      recursive = TRUE, full.names = TRUE)
if (length(in_file) == 0) stop("Run gbif-filter-to-land.R first; no crowberry_occurrences_land_only.csv under ", outputs_dir)
if (length(in_file) > 1) {
  stop("Found more than one crowberry_occurrences_land_only.csv:\n  ", paste(in_file, collapse = "\n  "),
       "\nDelete or move the extra copy.")
}
message("Reading: ", in_file)

occ <- readr::read_csv(in_file, show_col_types = FALSE)

lat_col <- intersect(c("decimalLatitude", "latitude", "lat"), names(occ))[1]
lon_col <- intersect(c("decimalLongitude", "longitude", "lon", "long"), names(occ))[1]
if (is.na(lat_col) || is.na(lon_col)) {
  stop("Couldn't find lat/lon columns. Columns are: ", paste(names(occ), collapse = ", "))
}

occ <- occ[!is.na(occ[[lat_col]]) & !is.na(occ[[lon_col]]), ]
occ$thin_species <- "crowberry"   # spThin needs a species column
n_before <- nrow(occ)
message("Records before thinning: ", n_before)

# ---- Spatial thinning -------------------------------------------------------
# spThin:: is written out because the terra package also has a thin().
set.seed(seed)
thinned_list <- spThin::thin(
  loc.data = as.data.frame(occ),
  lat.col  = lat_col,
  long.col = lon_col,
  spec.col = "thin_species",
  thin.par = thin_km,   # km
  reps     = reps,
  locs.thinned.list.return = TRUE,
  write.files = FALSE,
  write.log.file = FALSE
)

kept_per_rep <- vapply(thinned_list, nrow, integer(1))
message("Records kept per repetition: min ", min(kept_per_rep), ", max ", max(kept_per_rep))
best <- thinned_list[[which.max(kept_per_rep)]]

# Join back so the other columns (year, etc.) are kept
occ_thin <- dplyr::inner_join(
  occ, best,
  by = stats::setNames(c("Latitude", "Longitude"), c(lat_col, lon_col))
)
occ_thin <- occ_thin[!duplicated(occ_thin[, c(lat_col, lon_col)]), ]
occ_thin$thin_species <- NULL

if (nrow(occ_thin) != nrow(best)) {
  warning("Joined table has ", nrow(occ_thin), " rows but spThin kept ", nrow(best),
          ". Some coordinates may not have matched exactly; check the output.")
}
message("Records after ", thin_km, " km thinning: ", nrow(occ_thin),
        "  (", round(100 * nrow(occ_thin) / n_before, 1), "% of the input)")

# ---- Check: every kept point sits in a land cell of the clipped rasters -----
if (file.exists(mask_file) && requireNamespace("terra", quietly = TRUE)) {
  m  <- terra::rast(mask_file)
  xy <- as.matrix(occ_thin[, c(lon_col, lat_col)])
  v  <- terra::extract(m, xy)[, 1]
  n_off <- sum(is.na(v))
  if (n_off > 0) {
    warning(n_off, " thinned point(s) fall outside the land mask (cells with no data). ",
            "Re-check gbif-filter-to-land.R and the clipped rasters.")
  } else {
    message("Check passed: all thinned points fall in land cells of the clipped rasters.")
  }
} else {
  message("(Skipped the land-mask check: land_mask_touches.tif not found. Run clip-rasters-to-land.R.)")
}

# ---- Save -------------------------------------------------------------------
readr::write_csv(occ_thin, out_file)
message("Saved: ", out_file)

png(map_file, width = 1400, height = 900, res = 130)
plot(occ[[lon_col]], occ[[lat_col]], pch = 16, cex = 0.4, col = "grey70", asp = 1 / cos(65 * pi / 180),
     xlab = "Longitude", ylab = "Latitude",
     main = paste0("GBIF crowberry: ", n_before, " land records, ", nrow(occ_thin), " kept after ", thin_km, " km thinning"))
points(occ_thin[[lon_col]], occ_thin[[lat_col]], pch = 16, cex = 0.5, col = "darkgreen")
legend("topright", legend = c("removed", "kept"), pch = 16, col = c("grey70", "darkgreen"), bty = "n")
invisible(dev.off())
message("Saved: ", map_file)
