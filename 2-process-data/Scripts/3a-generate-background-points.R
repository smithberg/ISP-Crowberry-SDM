# 3a-generate-background-points.R
# Draws random background points over Iceland's land cells for MaxEnt (presence-background).
# One point per raster cell (cell centres), so there are no duplicate points.
#
# Put this file in: ISP-Crowberry-SDM/2-process-data/Scripts/
# Reads:  2-process-data/Outputs/Raster clip/land_mask_touches.tif                       (from clip-rasters-to-land.R)
#         2-process-data/Outputs/CHELSA-BIOCLIM clipped/  bio*_1981-2010*.tif (historic bioclim layers)
#         2-process-data/Outputs/EarthEnv clipped+resampled/*.tif            (topography layers)
#         2-process-data/Outputs/GBIF thinned/crowberry_occurrences_thinned_<thin_km>km.csv
# Writes: 2-process-data/Outputs/Background points/background_points_<n>.csv
#         2-process-data/Outputs/Background points/background_points_map.png
#         2-process-data/Outputs/Background points/valid_cells_mask.tif
# Inputs are only read, never changed.
#
# Background cells are restricted to cells that have data in the land mask AND in every
# historic bio layer and every EarthEnv layer. The gdd/gst/swe layers are NOT used for this
# check yet, because you haven't decided whether to use them. If you do use them, MaxEnt
# will treat their NA cells as missing, so rerun this script after deciding.
#
# NOT TESTED on your data. Check the console messages and the map before using the output.

# install.packages(c("terra", "readr"))
library(terra)

# ---- Settings ---------------------------------------------------------------
thin_km      <- 5        # which thinned presence file to use (5 or 2)
n_background <- 10000    # standard MaxEnt value; Iceland has ~100k land cells at ~1 km
seed         <- 42
# Optional: a bias raster (higher value = more likely to be sampled). NULL = uniform random.
# Example later: bias_file <- p("2-process-data", "Outputs", "Bias layer", "bias.tif")
bias_file    <- NULL

# Map appearance
map_width_px <- 1800                # width of the map png (the height follows from the shape of Iceland)
col_land     <- "grey93"            # land
col_bg       <- "grey15"            # background points
col_pres     <- "#E41A1C"           # presence points (fill; they have a thin black edge)
cex_bg       <- 0.22
cex_pres     <- 0.6
legend_cex   <- 1.1
map_pad      <- 0.04                # zoom out: extra space around the map, as a fraction of its width / height
legend_x     <- 1.9                 # legend: degrees of longitude from the right edge of the map to its left side
legend_y     <- 0.6                 # legend: degrees of latitude from the bottom edge of the map up to its top

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
chelsa_dir  <- file.path(outputs_dir, "CHELSA-BIOCLIM clipped")
earth_dir   <- file.path(outputs_dir, "EarthEnv clipped+resampled")
mask_file   <- file.path(outputs_dir, "Raster clip/land_mask_touches.tif")
pres_file   <- file.path(outputs_dir, "GBIF thinned",
                         paste0("crowberry_occurrences_thinned_", thin_km, "km.csv"))

out_dir <- file.path(outputs_dir, "Background points")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
bg_file    <- file.path(out_dir, paste0("background_points_", n_background, ".csv"))
map_file   <- file.path(out_dir, "background_points_map.png")
valid_file <- file.path(out_dir, "valid_cells_mask.tif")

if (!file.exists(mask_file)) stop("Run clip-rasters-to-land.R first; missing ", mask_file)
if (!file.exists(pres_file)) stop("Thinned presence file not found: ", pres_file)

# ---- Valid cells: land AND data in the predictors ---------------------------
land <- rast(mask_file)
valid <- !is.na(land)
message("Land cells in mask: ", format(sum(values(valid), na.rm = TRUE), big.mark = ","))

pred_files <- c(
  list.files(chelsa_dir, pattern = "^bio[0-9]+_1981-2010.*\\.tif$", recursive = TRUE, full.names = TRUE),
  list.files(earth_dir,  pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
)
message("Predictor layers used for the data check: ", length(pred_files),
        "  (historic bio layers + EarthEnv)")

if (length(pred_files) > 0) {
  s <- rast(pred_files)
  if (!isTRUE(compareGeom(land, s, stopOnError = FALSE))) {
    stop("The clipped predictor layers don't share the land mask's grid. Re-run clip-rasters-to-land.R.")
  }
  has_data <- sum(is.na(s)) == 0
  n_lost <- sum(values(valid), na.rm = TRUE) - sum(values(valid & has_data), na.rm = TRUE)
  valid <- valid & has_data
  message("Land cells dropped because a predictor is NA there: ", n_lost)
} else {
  warning("No clipped predictor layers found; using the land mask only.")
}
valid_cells <- which(values(valid, mat = FALSE) %in% 1)
message("Cells available for background: ", format(length(valid_cells), big.mark = ","))
if (length(valid_cells) == 0) stop("No valid cells found. Check the clipped rasters.")

writeRaster(ifel(valid, 1, NA), valid_file, overwrite = TRUE, gdal = c("COMPRESS=LZW"))

# ---- Sampling weights (uniform unless a bias raster is given) ---------------
w <- NULL
if (!is.null(bias_file)) {
  if (!file.exists(bias_file)) stop("bias_file not found: ", bias_file)
  b <- rast(bias_file)[[1]]
  if (!isTRUE(compareGeom(land, b, stopOnError = FALSE))) b <- resample(b, land, method = "bilinear")
  w <- values(b, mat = FALSE)[valid_cells]
  w[is.na(w) | w < 0] <- 0
  if (sum(w > 0) < n_background) stop("Fewer cells with positive bias weight than n_background.")
  message("Sampling weighted by: ", bias_file)
} else {
  message("Sampling uniformly at random over valid cells.")
}

# ---- Draw background cells --------------------------------------------------
set.seed(seed)
n_draw <- min(n_background, length(valid_cells))
if (n_draw < n_background) {
  warning("Only ", length(valid_cells), " valid cells; using all of them as background.")
}
pick <- sample.int(length(valid_cells), n_draw, replace = FALSE, prob = w)
bg_cells <- valid_cells[pick]
xy <- xyFromCell(land, bg_cells)

bg <- data.frame(longitude = xy[, 1], latitude = xy[, 2], cell = bg_cells)
readr::write_csv(bg, bg_file)
message("Background points: ", nrow(bg))
message("Saved: ", bg_file)

# ---- Check the presences against the same valid cells -----------------------
occ <- readr::read_csv(pres_file, show_col_types = FALSE)
lat_col <- intersect(c("decimalLatitude", "latitude", "lat"), names(occ))[1]
lon_col <- intersect(c("decimalLongitude", "longitude", "lon", "long"), names(occ))[1]
if (is.na(lat_col) || is.na(lon_col)) stop("Couldn't find lat/lon columns in ", pres_file)

pxy  <- as.matrix(occ[, c(lon_col, lat_col)])
pval <- terra::extract(ifel(valid, 1, NA), pxy)[, 1]
n_bad <- sum(is.na(pval))
message("Presence points: ", nrow(occ), "   in valid cells: ", nrow(occ) - n_bad)
if (n_bad > 0) {
  warning(n_bad, " presence point(s) fall in cells without data (or outside the mask). ",
          "MaxEnt would drop them. Check them before modelling.")
}
pcells <- terra::cellFromXY(land, pxy)
message("Presence cells that also contain a background point: ", sum(pcells %in% bg_cells),
        "  (normal and fine for MaxEnt)")

# ---- Map --------------------------------------------------------------------
# The map is drawn with the shape corrected for latitude (a degree of longitude is shorter than a degree of latitude
# at 65 N), so Iceland is not stretched sideways. The png height follows from that shape.
asp_map <- 1 / cos(65 * pi / 180)
e       <- terra::ext(land)
px      <- map_pad * (e[2] - e[1]); py <- map_pad * (e[4] - e[3])
xl      <- c(e[1] - px, e[2] + px); yl <- c(e[3] - py, e[4] + py)   # plotted area: the raster extent plus a margin
shape   <- ((xl[2] - xl[1]) / asp_map) / (yl[2] - yl[1])    # width / height of the map area
map_h   <- round(map_width_px / shape) + 260              # + room for the title and axes

png(map_file, width = map_width_px, height = map_h, res = 150)
par(mar = c(3.2, 3.6, 3.2, 1))
plot(ifel(valid, 1, NA), col = col_land, legend = FALSE, axes = TRUE, asp = asp_map, xlim = xl, ylim = yl, mar = c(3.2, 3.6, 3.2, 1),
     main = paste0("Background (n = ", nrow(bg), ") and thinned presences (n = ", nrow(occ), ", ", thin_km, " km)"))
points(bg$longitude, bg$latitude, pch = 16, cex = cex_bg, col = col_bg)
points(pxy[, 1], pxy[, 2], pch = 21, cex = cex_pres, bg = col_pres, col = "black", lwd = 0.4)
# Legend in the empty sea off the south-east coast. It is placed by map coordinates (degrees) measured from the corner of the
# raster extent, so it stays inside the frame. Move it with legend_x / legend_y (distance in degrees from the right / bottom edge).
legend(x = e[2] - legend_x, y = e[3] + legend_y, legend = c("Background", "Presence"), pch = c(16, 21),
       col = c(col_bg, "black"), pt.bg = c(NA, col_pres), pt.cex = c(0.9, 1.1),
       bty = "n", cex = legend_cex, xpd = NA)
invisible(dev.off())
message("Saved: ", map_file)