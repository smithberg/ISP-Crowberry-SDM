# 4a-assign-spatial-blocks.R
# Assigns every presence and background point to one of 4 spatial folds (the "block" method used
# by Wallace / ENMeval): split at the median latitude, then split each half at its own median
# longitude. The median lines are computed ONCE from a reference presence set and then applied to
# any presence set and to the background points, so the 2 km and 5 km runs use identical blocks.
#
# Fold numbering:  1 = south-west   2 = south-east   3 = north-west   4 = north-east
#
# Put this file in: ISP-Crowberry-SDM/2-process-data/Scripts/
# Reads:  2-process-data/Outputs/GBIF thinned/crowberry_occurrences_thinned_<km>km.csv
#         2-process-data/Outputs/Background points/background_points_<n>.csv
# Writes: 2-process-data/Outputs/Spatial blocks/
#           occurrences_thinned_<thin_km>km_folds.csv     presences + fold column
#           background_points_<n>_folds.csv               background + fold column
#           block_boundaries.csv                          the median lines used
#           fold_summary_<thin_km>km.csv                  points per fold
#           spatial_blocks_map_<thin_km>km.png            map of the blocks
# Inputs are only read, never changed.
#
# The scripts that use the fold columns (tuning in 5a, stepwise removal in 5b, final variable importance in 6b,
# and in 3-modelling the fold test 1b, the baseline model 1a, cross-validation 1c and the binary maps) read the
# two *_folds.csv files from the Spatial blocks folder.
#
# Later, in the model script: use these fold columns as user-defined partitions
# (ENMeval: partitions = "user", user.grp = list(occs.grp = <presence fold>, bg.grp = <background fold>)).
#
# NOT TESTED on your data. Check the summary and the map before using the output.

# install.packages("readr")

# ---- Settings ---------------------------------------------------------------
thin_km           <- 5   # which presence file to assign folds to (5 or 2)
reference_thin_km <- 5   # which presence set defines the block lines. Keep this fixed across runs.

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

# ---- Paths and inputs -------------------------------------------------------
outputs_dir <- p("2-process-data", "Outputs")
out_dir     <- file.path(outputs_dir, "Spatial blocks")   # everything this script writes goes straight in here
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

pres_file <- function(km) file.path(outputs_dir, "GBIF thinned",
                                    paste0("crowberry_occurrences_thinned_", km, "km.csv"))
bg_files  <- list.files(file.path(outputs_dir, "Background points"),
                        pattern = "^background_points_[0-9]+\\.csv$", full.names = TRUE)

for (f in c(pres_file(thin_km), pres_file(reference_thin_km))) {
  if (!file.exists(f)) stop("Thinned presence file not found: ", f)
}
if (length(bg_files) != 1) {
  stop("Expected exactly 1 background_points_<n>.csv in 'Background points' but found ", length(bg_files),
       ". Run generate-background-points.R, or delete the extra file.")
}

get_xy <- function(df, what) {
  lat <- intersect(c("decimalLatitude", "latitude", "lat"), names(df))[1]
  lon <- intersect(c("decimalLongitude", "longitude", "lon", "long"), names(df))[1]
  if (is.na(lat) || is.na(lon)) stop("Couldn't find lat/lon columns in the ", what, " file.")
  list(lon = lon, lat = lat)
}

pres <- readr::read_csv(pres_file(thin_km), show_col_types = FALSE)
ref  <- readr::read_csv(pres_file(reference_thin_km), show_col_types = FALSE)
bg   <- readr::read_csv(bg_files, show_col_types = FALSE)
pc <- get_xy(pres, "presence"); rc <- get_xy(ref, "reference presence"); bc <- get_xy(bg, "background")

pres <- pres[!is.na(pres[[pc$lon]]) & !is.na(pres[[pc$lat]]), ]
ref  <- ref[!is.na(ref[[rc$lon]]) & !is.na(ref[[rc$lat]]), ]
message("Presences to assign: ", nrow(pres), " (", thin_km, " km set)   background: ", nrow(bg))
message("Block lines from:    ", nrow(ref), " presences (", reference_thin_km, " km set)")

# ---- Block lines (from the reference presences) -----------------------------
lat_med    <- median(ref[[rc$lat]])
south      <- ref[[rc$lat]] <= lat_med
lon_med_s  <- median(ref[[rc$lon]][south])
lon_med_n  <- median(ref[[rc$lon]][!south])

assign_fold <- function(lon, lat) {
  is_south <- lat <= lat_med
  is_east  <- ifelse(is_south, lon > lon_med_s, lon > lon_med_n)
  ifelse(is_south, ifelse(is_east, 2L, 1L), ifelse(is_east, 4L, 3L))
}

pres$fold <- assign_fold(pres[[pc$lon]], pres[[pc$lat]])
bg$fold   <- assign_fold(bg[[bc$lon]],   bg[[bc$lat]])

boundaries <- data.frame(
  line  = c("median latitude", "median longitude, southern half", "median longitude, northern half"),
  value = c(lat_med, lon_med_s, lon_med_n),
  reference_presence_set = paste0(reference_thin_km, " km"),
  stringsAsFactors = FALSE
)
write.csv(boundaries, file.path(out_dir, "block_boundaries.csv"), row.names = FALSE)

# ---- Summary ----------------------------------------------------------------
labels <- c("south-west", "south-east", "north-west", "north-east")
summ <- data.frame(
  fold       = 1:4,
  region     = labels,
  presences  = as.integer(table(factor(pres$fold, levels = 1:4))),
  background = as.integer(table(factor(bg$fold,   levels = 1:4)))
)
summ$presence_share   <- round(summ$presences  / sum(summ$presences), 3)
summ$background_share <- round(summ$background / sum(summ$background), 3)
write.csv(summ, file.path(out_dir, paste0("fold_summary_", thin_km, "km.csv")), row.names = FALSE)

message("\n=== Points per fold ===")
print(summ, row.names = FALSE)
if (any(summ$presences < 20)) {
  warning("A fold has fewer than 20 presences. Evaluation on that fold will be noisy.")
}
if (any(summ$background == 0)) {
  warning("A fold has no background points. Check the block lines and the background file.")
}
if (thin_km != reference_thin_km) {
  message("\nNote: the block lines come from the ", reference_thin_km, " km set, so the ", thin_km,
          " km presences are not exactly equal across folds. That is expected.")
}

# ---- Save -------------------------------------------------------------------
pres_out <- file.path(out_dir, paste0("occurrences_thinned_", thin_km, "km_folds.csv"))
bg_out   <- file.path(out_dir, sub("\\.csv$", "_folds.csv", basename(bg_files)))
readr::write_csv(pres, pres_out)
readr::write_csv(bg,   bg_out)
message("\nSaved: ", pres_out)
message("Saved: ", bg_out)
message("Saved: ", file.path(out_dir, "block_boundaries.csv"))
message("Saved: ", file.path(out_dir, paste0("fold_summary_", thin_km, "km.csv")))

# ---- Map --------------------------------------------------------------------
cols <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")
map_file <- file.path(out_dir, paste0("spatial_blocks_map_", thin_km, "km.png"))
png(map_file, width = 1400, height = 900, res = 130)
plot(bg[[bc$lon]], bg[[bc$lat]], pch = 16, cex = 0.15, col = adjustcolor(cols[bg$fold], 0.4),
     asp = 1 / cos(65 * pi / 180), xlab = "Longitude", ylab = "Latitude",
     main = paste0("Spatial blocks: ", nrow(pres), " presences (", thin_km, " km), ", nrow(bg), " background"))
points(pres[[pc$lon]], pres[[pc$lat]], pch = 21, bg = cols[pres$fold], col = "black", cex = 0.6, lwd = 0.3)
abline(h = lat_med, lty = 2)
segments(lon_med_s, par("usr")[3], lon_med_s, lat_med, lty = 2)
segments(lon_med_n, lat_med, lon_med_n, par("usr")[4], lty = 2)
legend("topright", legend = paste0(1:4, " ", labels), pch = 21, pt.bg = cols, bty = "n", cex = 0.9)
invisible(dev.off())
message("Saved: ", map_file)