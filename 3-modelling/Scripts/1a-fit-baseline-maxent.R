# 1a-fit-baseline-maxent.R
# Fits the baseline MaxEnt model (the 8 variables kept by 5b, feature classes and RM from 5a/5b) on the
# thinned crowberry presences and the 10,000 background points, then maps the predicted habitat suitability
# across Iceland (cloglog output, 0 to 1).
#
# Put this file in: ISP-Crowberry-SDM/3-modelling/Scripts/
# Reads:  2-process-data/Outputs/Predictor screening/selected_predictors.csv     (raster paths)
#         2-process-data/Outputs/<historic_file column>                           (the rasters)
#         2-process-data/Outputs/Spatial blocks/occurrences_thinned_<thin_km>km_folds.csv
#         2-process-data/Outputs/Spatial blocks/background_points_<n>_folds.csv
#         2-process-data/Outputs/Background points/valid_cells_mask.tif           (optional, used if present)
#         2-process-data/Outputs/MaxEnt tuning/Stepwise removal/stepwise_path.csv (optional, only for a sanity check)
#         1-download-data/Downloads/LMI-IS50V/is50v_land_polygon.gpkg             (optional coastline outline)
# Writes: 3-modelling/Outputs/Baseline model/
#           baseline_maxent_model.rds        the fitted maxnet model (reload it instead of refitting)
#           suitability_baseline.tif         suitability raster (cloglog, 0 to 1), for later steps
#           suitability_map.png              the map
#           suitability_map_with_points.png      the same map with the thinned presences (red) and the background points on top
#           model_summary.txt                settings, AICc, suitability at presences vs background
# Inputs are only read, never changed.
#
# How it works: one model is fitted with ENMeval (algorithm = "maxnet"), exactly as in 5a and 5b, so the AICc
# printed here should match the lowest AICc in stepwise_path.csv (16511.6 in your notes). If it doesn't, the
# inputs (presence set, rasters, variables) are not the ones 5b used. ENMeval also returns the prediction
# raster over the whole study area, which is what gets mapped.
#
# Reading the map: cloglog suitability from presence-background data is a RELATIVE index of how much the
# climate and terrain of a cell resemble those where crowberry has been recorded (given the background
# you gave it). It is not a probability of occurrence, and it inherits the sampling bias in GBIF. The map is
# also "in-sample": the model was fitted on these presences. Judge performance with the spatial-block
# cross-validation (4a folds), not by how this map looks.

# install.packages(c("terra", "readr", "ENMeval", "maxnet"))
library(terra)

# ---- Settings ---------------------------------------------------------------
thin_km      <- 5                          # which thinned presence set (must have a *_folds.csv from 4a)
n_background <- 10000                      # must match the background_points_<n>_folds.csv file
final_vars   <- c("bio02", "bio15", "bio18", "swe", "tpi", "roughness", "northness", "ngd10")  # from 5b
fc           <- "QHPT"                      # feature classes from 5a
rm_value     <- 1.5                        # regularization multiplier from 5a
overwrite    <- TRUE                      # FALSE = reuse baseline_maxent_model.rds if it exists (just redraws the map)
col_pres     <- "#E41A1C"                  # map: presence points (fill; thin black edge)
col_bg       <- "#000000A0"                # map: background points
cex_pres     <- 0.5
cex_bg       <- 0.15
map_width    <- 2400                       # map PNG size in pixels (300 dpi); the extra height leaves room for the caption
map_height   <- 1800
cex_caption  <- 0.6

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

# ---- Packages ---------------------------------------------------------------
for (pkg in c("ENMeval", "maxnet", "readr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Package '", pkg, "' is needed. install.packages(\"", pkg, "\")")
}
if (utils::packageVersion("ENMeval") < "2.0.0") stop("ENMeval 2.0.0 or newer is needed (ENMevaluate with tune.args).")

# ---- Paths ------------------------------------------------------------------
outputs_dir <- p("2-process-data", "Outputs")
sel_file    <- file.path(outputs_dir, "Predictor screening", "selected_predictors.csv")
assign_dir  <- file.path(outputs_dir, "Spatial blocks")
pres_file   <- file.path(assign_dir, paste0("occurrences_thinned_", thin_km, "km_folds.csv"))
bg_file     <- file.path(assign_dir, paste0("background_points_", n_background, "_folds.csv"))
valid_file  <- file.path(outputs_dir, "Background points", "valid_cells_mask.tif")
path_file   <- file.path(outputs_dir, "MaxEnt tuning", "Stepwise removal", "stepwise_path.csv")
coast_file  <- p("1-download-data", "Downloads", "LMI-IS50V", "is50v_land_polygon.gpkg")
out_dir     <- p("3-modelling", "Outputs", "Baseline model")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
model_file  <- file.path(out_dir, "baseline_maxent_model.rds")
pred_file   <- file.path(out_dir, "suitability_baseline.tif")

for (f in c(sel_file, pres_file, bg_file)) if (!file.exists(f)) stop("Missing input: ", f)

# ---- Predictor rasters (the 8 final variables only) -------------------------
sel <- read.csv(sel_file, stringsAsFactors = FALSE)
if (!all(c("variable", "historic_file") %in% names(sel))) {
  stop("selected_predictors.csv needs the columns 'variable' and 'historic_file'.")
}
not_in_sel <- setdiff(final_vars, sel$variable)
if (length(not_in_sel) > 0) stop("These variables are not in selected_predictors.csv: ", paste(not_in_sel, collapse = ", "))
sel <- sel[match(final_vars, sel$variable), ]
pred_files <- p(sel$historic_file)
if (any(!file.exists(pred_files))) {
  stop("Predictor raster(s) not found:\n  ", paste(pred_files[!file.exists(pred_files)], collapse = "\n  "))
}
envs <- rast(pred_files)
names(envs) <- sel$variable
message("Predictors (", nlyr(envs), "): ", paste(names(envs), collapse = ", "))

# Restrict to cells with data in every layer, the same cells the background was drawn from.
valid <- NULL
if (file.exists(valid_file)) {
  valid <- rast(valid_file)
  if (isTRUE(compareGeom(envs, valid, stopOnError = FALSE))) {
    envs <- mask(envs, valid)
  } else {
    warning("valid_cells_mask.tif is not on the predictors' grid; not applied.")
    valid <- NULL
  }
}

# ---- Points -----------------------------------------------------------------
pres <- readr::read_csv(pres_file, show_col_types = FALSE)
bg   <- readr::read_csv(bg_file,   show_col_types = FALSE)
get_xy <- function(df, what) {
  lat <- intersect(c("decimalLatitude", "latitude", "lat"), names(df))[1]
  lon <- intersect(c("decimalLongitude", "longitude", "lon", "long"), names(df))[1]
  if (is.na(lat) || is.na(lon)) stop("Couldn't find lat/lon columns in the ", what, " file.")
  data.frame(longitude = df[[lon]], latitude = df[[lat]])
}
pxy <- get_xy(pres, "presence")
bxy <- get_xy(bg, "background")

# Drop points with no predictor data, as in 5a and 5b.
pv <- terra::extract(envs, as.matrix(pxy))[, names(envs), drop = FALSE]
bv <- terra::extract(envs, as.matrix(bxy))[, names(envs), drop = FALSE]
ok_p <- stats::complete.cases(pv) & !is.na(pxy$longitude) & !is.na(pxy$latitude)
ok_b <- stats::complete.cases(bv)
if (any(!ok_p) || any(!ok_b)) {
  message("Dropping points with a missing predictor value: ", sum(!ok_p), " presences, ", sum(!ok_b), " background")
}
pxy <- pxy[ok_p, ]; bxy <- bxy[ok_b, ]
rownames(pxy) <- NULL; rownames(bxy) <- NULL
message("Presences: ", nrow(pxy), "   background: ", nrow(bxy))

# ---- Fit the model and get the prediction raster ----------------------------
aicc <- NA_real_
if (file.exists(model_file) && file.exists(pred_file) && !overwrite) {
  message("Found a saved model and suitability raster, so the model is NOT refitted:\n  ", model_file,
          "\nSet overwrite <- TRUE (or delete those files) to refit, e.g. after changing predictors or settings.")
  mod  <- readRDS(model_file)
  pred <- rast(pred_file)
} else {
  message("Fitting the model: ", length(final_vars), " variables, fc = ", fc, ", RM = ", rm_value, " ...")
  e <- NULL
  utils::capture.output(
    e <- suppressMessages(ENMeval::ENMevaluate(
      occs = pxy, envs = envs, bg = bxy,
      tune.args = list(fc = fc, rm = rm_value),
      algorithm = "maxnet", partitions = "none", parallel = FALSE))
  )
  res  <- ENMeval::eval.results(e)
  aicc <- res$AICc[1]
  mod  <- e@models[[1]]
  pred <- e@predictions[[1]]           # cloglog suitability over the whole study area
  names(pred) <- "suitability"
  # ENMeval's prediction can come back with a differently written (or missing) CRS even though it is on the same grid as
  # the predictors. Copy the predictors' CRS onto it, so the saved raster is georeferenced and mask() doesn't warn.
  if (!isTRUE(terra::same.crs(pred, envs))) terra::crs(pred) <- terra::crs(envs)
  if (!is.null(valid)) pred <- mask(pred, valid)
  saveRDS(mod, model_file)
  writeRaster(pred, pred_file, overwrite = TRUE)
  message(sprintf("Fitted. AICc = %.1f, non-zero coefficients = %s", aicc,
                  if ("ncoef" %in% names(res)) res$ncoef[1] else "NA"))
  
  # Sanity check against 5b: same inputs should give the same AICc.
  if (file.exists(path_file)) {
    sp <- read.csv(path_file, stringsAsFactors = FALSE)
    ref <- sp$AICc[sp$n_vars == length(final_vars)]
    if (length(ref) == 1 && !is.na(ref)) {
      if (abs(aicc - ref) < 0.5) {
        message(sprintf("Matches 5b (AICc %.1f for %d variables), so the inputs are the same.", ref, length(final_vars)))
      } else {
        warning(sprintf("AICc here (%.1f) differs from 5b's %d-variable model (%.1f). Check the presence set, rasters and variables.",
                        aicc, length(final_vars), ref))
      }
    }
  }
}

# ---- Suitability at presences vs background ---------------------------------
sp_pres <- terra::extract(pred, as.matrix(pxy))[, 1]
sp_bg   <- terra::extract(pred, as.matrix(bxy))[, 1]
q <- function(x) round(quantile(x, c(0.1, 0.5, 0.9), na.rm = TRUE), 3)
message("\nSuitability (cloglog) at the points, 10th / 50th / 90th percentile:")
message("  presences:  ", paste(q(sp_pres), collapse = " / "))
message("  background: ", paste(q(sp_bg),   collapse = " / "))
message("(Presences should sit clearly higher than background. This is in-sample, so it will look good.)")

# ---- Map --------------------------------------------------------------------
coast <- NULL
if (file.exists(coast_file)) coast <- tryCatch(project(vect(coast_file), crs(pred)), error = function(e) NULL)

draw_map <- function(show_points = FALSE) {
  map_mar <- c(3.2, 1, 3.5, 5)          # bottom, left, top, right (lines); the bottom strip is kept free for the caption
  par(mar = map_mar)
  plot(pred, col = hcl.colors(100, "YlGnBu", rev = TRUE), range = c(0, 1), axes = FALSE, box = FALSE,
       main = "Predicted habitat suitability for crowberry (Empetrum nigrum) in Iceland",
       cex.main = 1, mar = map_mar, plg = list(title = "Suitability\n(cloglog)", title.cex = 0.8, cex = 0.8))
  if (!is.null(coast)) lines(coast, lwd = 0.4, col = "grey25")
  if (show_points) {
    points(bxy$longitude, bxy$latitude, pch = 16, cex = cex_bg, col = col_bg)                                   # background first,
    points(pxy$longitude, pxy$latitude, pch = 21, cex = cex_pres, bg = col_pres, col = "black", lwd = 0.4)      # presences on top
  }
  
  # Caption. It is placed in whole-image (NDC) coordinates rather than with mtext(side = 1): mtext measures from the plot
  # margins as they stand after terra::plot() has drawn, and that left the caption on or past the bottom edge of the PNG.
  # NDC positions don't depend on the margins, so the caption stays inside the image.
  caption <- c(
    sprintf("Baseline MaxEnt (maxnet): %d variables, %s, RM %s; %d presences (%d km thinning), %d background points.",
            length(final_vars), fc, rm_value, nrow(pxy), thin_km, nrow(bxy)),
    if (show_points) "Red = thinned presences, black dots = background points."
  )
  op <- par(xpd = NA); on.exit(par(op), add = TRUE)
  cap_x <- grconvertX(0.5, from = "ndc", to = "user")
  cap_y <- grconvertY(c(0.062, 0.030)[seq_along(caption)], from = "ndc", to = "user")   # first line higher, second below it
  text(cap_x, cap_y, labels = caption, cex = cex_caption, col = "grey30")
}

png(file.path(out_dir, "suitability_map.png"), width = map_width, height = map_height, res = 300)
draw_map(FALSE); invisible(dev.off())
png(file.path(out_dir, "suitability_map_with_points.png"), width = map_width, height = map_height, res = 300)
draw_map(TRUE); invisible(dev.off())

writeLines(c(
  paste("Run finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste("R:", R.version.string),
  paste("ENMeval:", as.character(utils::packageVersion("ENMeval"))),
  paste("maxnet:", as.character(utils::packageVersion("maxnet"))),
  paste("terra:", as.character(utils::packageVersion("terra"))),
  paste0("Variables: ", paste(final_vars, collapse = ", ")),
  paste0("Feature classes: ", fc, "   RM: ", rm_value),
  paste0("Presences: ", nrow(pxy), "   Background: ", nrow(bxy), "   thin_km: ", thin_km),
  paste0("AICc: ", if (is.na(aicc)) "not recalculated (model loaded from file)" else round(aicc, 1)),
  paste0("Suitability at presences, 10/50/90th percentile: ", paste(q(sp_pres), collapse = " / ")),
  paste0("Suitability at background, 10/50/90th percentile: ", paste(q(sp_bg), collapse = " / "))
), file.path(out_dir, "model_summary.txt"))

message("\nSaved: ", model_file)
message("Saved: ", pred_file)
message("Saved: ", file.path(out_dir, "suitability_map.png"), " (+ _with_points.png)")
message("Saved: ", file.path(out_dir, "model_summary.txt"))