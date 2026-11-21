# 1a-gbif-filter-to-land.R
# Removes (or optionally snaps) GBIF records that fall outside the IS 50V land polygon.
# Run this BEFORE spatial thinning, so the thinning chooses among valid points only.
#
# Put this file in: ISP-Crowberry-SDM/2-process-data/Scripts/
# Reads:  1-download-data/Downloads/GBIF/crowberry_occurrences_clean_*.csv
#         1-download-data/Downloads/LMI-IS50V/is50v_land_polygon.gpkg
# Writes: 2-process-data/Outputs/GBIF filtered/crowberry_occurrences_land_only.csv   (filtered records)
#         2-process-data/Outputs/GBIF filtered/gbif_off_land_report.csv              (every off-land point)
#         2-process-data/Outputs/GBIF filtered/gbif_off_land_map.png                 (where they are)
# The original cleaned CSV is only read, never changed.
#
# NOT TESTED on your data. Check the report and the map before using the output.

# install.packages(c("sf", "readr"))
suppressPackageStartupMessages({
  library(sf)
  library(readr)
})

# ---- Settings ---------------------------------------------------------------
# 0    = remove every point outside the land polygon (default)
# 1000 = snap points within 1000 m of land to the nearest land, remove the rest
snap_distance_m <- 1000
nudge_m         <- 10    # snapped points are placed this far inland so they are clearly inside

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

gbif_dir  <- p("1-download-data", "Downloads", "GBIF")
land_file <- p("1-download-data", "Downloads", "LMI-IS50V", "is50v_land_polygon.gpkg")
out_dir   <- p("2-process-data", "Outputs/GBIF filtered")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

out_file    <- file.path(out_dir, "crowberry_occurrences_land_only.csv")
report_file <- file.path(out_dir, "gbif_off_land_report.csv")
map_file    <- file.path(out_dir, "gbif_off_land_map.png")

# ---- Read records -----------------------------------------------------------
in_file <- list.files(gbif_dir, pattern = "^crowberry_occurrences_clean.*\\.csv$", full.names = TRUE)
if (length(in_file) != 1) {
  stop("Expected exactly 1 cleaned crowberry CSV in ", gbif_dir, " but found ", length(in_file))
}
if (!file.exists(land_file)) {
  stop("Land polygon not found: ", land_file, "\nRun lmi-is50v-coastline-download.R first.")
}
message("Reading records: ", in_file)
occ <- read_csv(in_file, show_col_types = FALSE)

lat_col <- intersect(c("decimalLatitude", "latitude", "lat"), names(occ))[1]
lon_col <- intersect(c("decimalLongitude", "longitude", "lon", "long"), names(occ))[1]
if (is.na(lat_col) || is.na(lon_col)) {
  stop("Couldn't find lat/lon columns. Columns are: ", paste(names(occ), collapse = ", "))
}

n_input <- nrow(occ)
occ$.row_id <- seq_len(n_input)   # row number in the cleaned CSV, used in the report
has_xy <- !is.na(occ[[lat_col]]) & !is.na(occ[[lon_col]])
if (any(!has_xy)) message("Dropping ", sum(!has_xy), " rows with missing coordinates")
occ <- occ[has_xy, ]
message("Records with coordinates: ", nrow(occ))

# ---- Read land polygon (projected to ISN93, metres, so distances are in metres) ----
land   <- st_read(land_file, quiet = TRUE)
land   <- st_transform(st_make_valid(land), 3057)
land_u <- st_union(st_geometry(land))   # a single land geometry

pts <- st_as_sf(occ[, c(".row_id", lon_col, lat_col)],
                coords = c(lon_col, lat_col), crs = 4326)
pts <- st_transform(pts, 3057)

# ---- Which points are on land? ----------------------------------------------
inside  <- lengths(st_intersects(pts, land_u)) > 0
out_idx <- which(!inside)
message("Inside land polygon: ", sum(inside), "   outside: ", length(out_idx))

keep <- inside
report <- NULL

if (length(out_idx) > 0) {
  dist_m   <- as.numeric(st_distance(pts[out_idx, ], land_u))
  orig_lon <- occ[[lon_col]][out_idx]
  orig_lat <- occ[[lat_col]][out_idx]
  action   <- ifelse(snap_distance_m > 0 & dist_m <= snap_distance_m, "snap", "remove")
  new_lon  <- rep(NA_real_, length(out_idx))
  new_lat  <- rep(NA_real_, length(out_idx))

  # ---- Optional snapping --------------------------------------------------
  for (k in which(action == "snap")) {
    i     <- out_idx[k]
    ln    <- st_nearest_points(st_geometry(pts)[i], land_u)   # line: point -> nearest land
    xy    <- st_coordinates(ln)[, 1:2, drop = FALSE]
    start <- xy[1, ]
    end   <- xy[2, ]
    d     <- sqrt(sum((end - start)^2))
    inward  <- end + (end - start) / d * nudge_m              # a little further inland
    snapped <- st_sfc(st_point(inward), crs = 3057)
    if (lengths(st_intersects(snapped, land_u)) > 0) {
      ll <- st_coordinates(st_transform(snapped, 4326))
      new_lon[k] <- ll[1, 1]
      new_lat[k] <- ll[1, 2]
      occ[[lon_col]][i] <- new_lon[k]
      occ[[lat_col]][i] <- new_lat[k]
      keep[i] <- TRUE
    } else {
      action[k] <- "remove"   # the nudge landed back in the sea (narrow inlet or islet)
    }
  }

  # ---- Report -------------------------------------------------------------
  report <- data.frame(
    row_in_clean_csv   = occ$.row_id[out_idx],
    longitude          = orig_lon,
    latitude           = orig_lat,
    distance_to_land_m = round(dist_m, 1),
    action             = ifelse(action == "snap", "snapped", "removed"),
    new_longitude      = new_lon,
    new_latitude       = new_lat,
    stringsAsFactors   = FALSE
  )
  extra <- intersect(c("gbifID", "coordinateUncertaintyInMeters", "year", "locality", "datasetName"),
                     names(occ))
  if (length(extra) > 0) {
    report <- cbind(report, as.data.frame(occ[out_idx, extra, drop = FALSE]))
  }
  report <- report[order(-report$distance_to_land_m), ]
  write.csv(report, report_file, row.names = FALSE)

  message("\nOff-land points (largest distance first):")
  print(report[, c("row_in_clean_csv", "longitude", "latitude", "distance_to_land_m", "action")],
        row.names = FALSE)
} else {
  message("No points fall outside the land polygon.")
}

# ---- Map of the off-land points ---------------------------------------------
png(map_file, width = 1400, height = 900, res = 130)
plot(land_u, col = "grey90", border = "grey40", lwd = 0.3,
     main = paste0("GBIF points outside the land polygon (n = ", length(out_idx), ")"))
if (length(out_idx) > 0) {
  plot(st_geometry(pts)[out_idx], add = TRUE, pch = 21, bg = "red", cex = 1.3)
  text(st_coordinates(pts[out_idx, ]), labels = occ$.row_id[out_idx], pos = 3, cex = 0.7, col = "red")
}
invisible(dev.off())

# ---- Save filtered records --------------------------------------------------
occ_out <- occ[keep, ]
occ_out$.row_id <- NULL
write_csv(occ_out, out_file)

n_snapped <- if (is.null(report)) 0 else sum(report$action == "snapped")
n_removed <- if (is.null(report)) 0 else sum(report$action == "removed")

message("\n=== Summary ===")
message("Records in cleaned CSV:        ", n_input)
message("  missing coordinates removed: ", sum(!has_xy))
message("  outside land, removed:       ", n_removed)
message("  outside land, snapped:       ", n_snapped)
message("Records written:               ", nrow(occ_out))
message("Saved: ", out_file)
if (!is.null(report)) {
  message("Saved: ", report_file)
  message("Saved: ", map_file)
}
if (n_removed / max(nrow(occ), 1) > 0.05) {
  warning("More than 5% of records were removed. Check the map; the polygon or its CRS may be wrong.")
}
