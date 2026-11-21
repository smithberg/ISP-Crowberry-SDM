# 2a-scenario-change-grid.R
# Six figures, all from GFDL-ESM4. Figures 1, 2 and 2c have a 3 x 3 grid of maps for 2011-2040, 2041-2070 and 2071-2100
# under each of SSP1-2.6, SSP3-7.0 and SSP5-8.5 (rows = scenario, columns = period) plus the baseline suitability map
# (1981-2010). In figures 1 and 2 the baseline is at the bottom right, next to the colour bar (bottom left); figure 1 has
# exactly the same layout and size as figure 2. Figure 2b is a 2 x 2 version of figure 2 with only the baseline and the 2071-2100 change
# under each scenario. Figure 2c is figure 2 with a line plot of the mean suitability over time underneath, and figure 4 is
# that line plot on its own. Figure 3 maps the prediction uncertainty across the 9 projections.
#   1.  scenario_suitability_grid.png              the projected SUITABILITY in each of the 9 scenario-periods
#   2.  scenario_change_grid.png                   the CHANGE in suitability (future minus baseline) in each of them
#   2b. scenario_change_grid_2071-2100.png         the same change maps, but only 2071-2100, as a 2 x 2 grid with the baseline
#   2c. scenario_change_grid_line_plot.png         figure 2, exactly as it is, with the line plot of mean suitability below it
#   3.  scenario_uncertainty_grid.png              the UNCERTAINTY across the 9 projections (spread and agreement on direction)
#   4.  scenario_mean_suitability_over_time.png    the line plot of mean suitability over time, on its own
#
# Put this file in: ISP-Crowberry-SDM/3-modelling/Scripts/   (run 1a first)
# Reads:  3-modelling/Outputs/Baseline model/baseline_maxent_model.rds           (the model from 1a)
#         2-process-data/Outputs/Predictor screening/selected_predictors.csv      (raster paths)
#         2-process-data/Outputs/<historic_file column>                            (1981-2010 rasters)
#         2-process-data/Outputs/CHELSA-BIOCLIM filled/<gcm>_<ssp>_<var>_<period>_iceland.tif  (future rasters,
#              found next to each historic CHELSA file; 9 scenario-periods x the CHELSA variables in the model)
#         2-process-data/Outputs/Background points/valid_cells_mask.tif            (optional, used if present)
#         1-download-data/Downloads/LMI-IS50V/is50v_land_polygon.gpkg              (optional coastline outline)
# Writes: 3-modelling/Outputs/Projection/Scenario grid/
#           scenario_suitability_grid.png               figure 1: suitability, baseline + 9 projections
#           scenario_change_grid.png                    figure 2: change in suitability, 9 maps + baseline
#           scenario_change_grid_2071-2100.png          figure 2b: baseline + the 2071-2100 change in each scenario
#           scenario_change_grid_line_plot.png          figure 2c: figure 2 + the line plot of mean suitability over time
#           scenario_uncertainty_grid.png               figure 3: uncertainty across the 9 projections
#           scenario_mean_suitability_over_time.png     figure 4: the line plot on its own
#           scenario_summary.csv                        one row per scenario and period (see below)
#           uncertainty_summary.csv                     one row per uncertainty measure (see below)
#           suitability_<gcm>_<ssp>_<period>.tif        the 9 future suitability rasters (cloglog, 0 to 1)
#           suitability_baseline_1981-2010.tif          the baseline suitability raster, same code and model
#           uncertainty_sd_all9.tif                     SD of suitability across all 9 projections
#           uncertainty_extrapolation_<ssp>_<period>.tif  the 9 per-projection extrapolation distances
#           uncertainty_direction_agreement.tif         share of the 9 projections that agree on the direction of change
#           uncertainty_mean_change_all9.tif            mean change in suitability across the 9 projections
# Inputs are only read, never changed.
#
# How it works: the same fitted model predicts on the 1981-2010 stack and on each of the 9 future stacks with
# identical code, and each change map is future minus baseline. Terrain variables (tpi, roughness, northness,
# elevation, slope, eastness) don't change with climate, so the historic rasters are reused for them.
#
# Text size: in figures 1 and 2 all the text (titles, panel letters, map titles, colour bar numbers and labels) is 10% bigger
# than before (grid_text_scale = 1.1), except the "Suitability" title and the numbers on the baseline legend next to panel (j),
# which are 15% bigger (grid_legend_scale = 1.15). Both figures also have grid_bottom_pad_px (5) extra pixels of white space
# under (j). The other figures (2b, 2c, 3, 4) keep their old text sizes.
#
# In figure 1 all ten panels share the same 0-1 suitability scale (colour bar bottom left, as in figure 2). In figure 2 all
# nine change panels share ONE colour scale (set by the 2nd-98th percentile of all nine changes pooled, so a few
# extreme cells don't dominate), so panels can be compared directly. Purple = suitability falls, green = rises.
# Figure 2b uses that same colour scale, so its three maps look exactly as they do in figure 2.
# Grey shading = cells where at least one variable is outside the range the model was trained on. maxnet clamps
# those values to the training edge, so the suitability change there is not trustworthy. It is drawn on figure 3 only.
# Figure 1 (show_outside_on_suitability) and the change maps (figures 2, 2b and 2c; show_outside_on_change) leave it off
# unless those settings are TRUE.
#
# Colour bar on the change maps: it runs from -lim to +lim, where lim is the larger of the absolute 2nd and 98th percentile
# of all nine changes pooled (so zero stays at the white middle). The bar is marked with a bold tick and a number at the
# actual 2nd and 98th percentile values, and at the bar's ends (-lim, +lim) where those differ. Cells that change by more
# than lim are drawn in the end colour.
#
# Line plot (figures 2c and 4): the mean suitability over Iceland's land for the 1981-2010 baseline and for each of the
# three future periods, one line per scenario. It is the area-weighted mean of the cloglog suitability over all land
# cells, the same number as mean_suitability in scenario_summary.csv. All three lines start at the same baseline point
# (the open circle) and then diverge. Points sit at the middle of each 30-year period (1995.5, 2025.5, 2055.5, 2085.5)
# and the lines join them straight, so nothing is implied between periods. The scenario colours and markers are the same
# as in 2f. The y-axis is rounded out to the tick spacing (e.g. 0.40 to 0.80), so there is a numbered tick (0.40, 0.50,
# ...) over the whole range shown. Figure 2c is figure 2 with this plot centred underneath it (panel k); the maps are
# drawn by the same code as figure 2 and are the same size. Figure 4 is the same plot on its own, with a title only
# (no subtitle); its size is set by ts_fig_px below, and its legend shrinks to fit the width if it would otherwise be cut off.
#
# Uncertainty (figure 3): with one fitted model and one climate model there is no statistical error on the
# predictions, so two things are mapped:
#   - Top row: SD of suitability across all 9 projections, and the share of the 9 that agree on the direction of
#     change (1 = all 9 agree, about 0.56 = a 5 to 4 split).
#   - 3 x 3 grid (same layout as figures 1 and 2): one extrapolation-uncertainty map for EACH of the 9 projections.
#     For every cell this is the largest distance by which any variable lies outside the range the model was
#     trained on, as a fraction of that variable's training range (0 = inside the range everywhere, 0.5 = at
#     least one variable half a training range beyond the edge). It shows where each projection is
#     less reliable. All nine panels share one colour scale.
# Grey shading on the top-row maps = at least one of the 9 projections extrapolates in that cell.
#
# scenario_summary.csv columns: area-weighted mean suitability, area-weighted mean change, % of cells down by
# more than 0.1, % up by more than 0.1, the largest increase and the largest decrease in suitability in any single
# cell (max_increase = the biggest future-minus-baseline value, max_decrease = the most negative one, so it is
# normally below 0; both are on the 0-1 cloglog scale and are not area-weighted, they are just the extreme cells),
# % of cells outside the training range, the variable most often outside it and the area-weighted mean
# extrapolation distance.
# uncertainty_summary.csv columns: area-weighted mean SD, % of cells with SD above 0.1, % of cells where all 9
# projections agree on the direction and % where at least 8 of 9 do.
#
# Limits: one climate model (GFDL-ESM4), so climate-model uncertainty is not captured; fitting uncertainty
# (e.g. from bootstrapped or resampled models) is not included either; suitability is a relative index, not
# abundance, and ignores dispersal and soils; cells are not equal area (WGS84 degrees), so the means are
# area-weighted. 2011-2040 differs little from the 1981-2010 baseline, so small changes there are expected. The mean in
# the line plot is over all of Iceland, so it can hide places that gain and places that lose cancelling out; the maps
# show where. Cells outside the training range (grey on figure 3) are included in the mean, and also in
# max_increase and max_decrease, so the single most extreme cell can be one where the model is extrapolating.
#
# NOT TESTED on your data. Check the console messages and the figures before using the output.

# install.packages(c("terra", "maxnet"))
library(terra)
library(maxnet)

# ---- Settings ---------------------------------------------------------------
gcm        <- "gfdl-esm4"
scenarios  <- c(ssp126 = "SSP1-2.6", ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")   # rows, in this order
periods    <- c("2011-2040", "2041-2070", "2071-2100")                            # columns, in this order
baseline_period <- "1981-2010"                                                    # used for the line plot's labels
period_2x2 <- "2071-2100"                                                          # period shown in the 2 x 2 change figure (2b); one of 'periods'
stopifnot(period_2x2 %in% periods)
static_vars <- c("elevation", "slope", "northness", "eastness", "tpi", "roughness")   # terrain: reuse the historic raster
show_outside_range <- TRUE   # grey shading where the model extrapolates (figure 3; the two settings below control the others)
show_outside_on_suitability <- FALSE   # also draw the grey shading on the suitability maps (figure 1); FALSE = leave it off
show_outside_on_change <- FALSE   # also draw the grey shading on the change maps (figures 2, 2b, 2c); FALSE = leave it off
overwrite <- TRUE          # FALSE = reuse saved suitability rasters if they exist (just redraws the figures)
grid_text_scale <- 1.1       # text size multiplier for figures 1 and 2 only (1 = the old sizes): titles, panel letters, colour bar text.
# The other figures (2b, 2c, 3, 4) are unchanged.
grid_legend_scale <- 1.15    # figures 1 and 2 only: size multiplier for the baseline legend's "Suitability" title and numbers (right of panel j)
grid_bottom_pad_px <- 5      # figures 1 and 2 only: extra pixels of white space below the bottom row, so the (j) isn't close to clipping
panel_letters <- letters     # letter centred under each map: a, b, c ... (LETTERS for A, B, C)
letter_fmt    <- "(%s)"      # "(%s)" gives (a); "%s" gives a
letter_cex    <- 1.1         # size of the panel letters (map titles are 0.8-1.0)
ts_cols    <- c(ssp126 = "#0072B2", ssp370 = "#E69F00", ssp585 = "#B2182B")   # line plot colours: blue, orange, dark red (as in 2f)
ts_pch     <- c(ssp126 = 16, ssp370 = 17, ssp585 = 15)                        # line plot markers, so the scenarios don't rely on colour alone
ts_y_from_zero <- FALSE      # line plot y-axis: FALSE = fitted to the data (with a margin), TRUE = starts at 0
ts_fig_px  <- c(2800, 1500)  # figure 4 size in pixels (width, height) at 300 dpi; it was 2400 x 1800

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
sel_file    <- file.path(outputs_dir, "Predictor screening", "selected_predictors.csv")
valid_file  <- file.path(outputs_dir, "Background points", "valid_cells_mask.tif")
model_file  <- p("3-modelling", "Outputs", "Baseline model", "baseline_maxent_model.rds")
coast_file  <- p("1-download-data", "Downloads", "LMI-IS50V", "is50v_land_polygon.gpkg")
out_dir     <- p("3-modelling", "Outputs", "Projection", "Scenario grid")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (f in c(sel_file, model_file)) if (!file.exists(f)) stop("Missing input: ", f, if (f == model_file) "\nRun 1a-fit-baseline-maxent.R first." else "")

mod <- readRDS(model_file)
model_vars <- names(mod$varmin)
if (is.null(model_vars)) stop("The saved model has no varmin/varmax, so its variables can't be identified. Refit with 1a.")
message("Model variables (", length(model_vars), "): ", paste(model_vars, collapse = ", "))

# ---- Predictor rasters ------------------------------------------------------
sel <- read.csv(sel_file, stringsAsFactors = FALSE)
if (!all(c("variable", "historic_file") %in% names(sel))) stop("selected_predictors.csv needs the columns 'variable' and 'historic_file'.")
missing_v <- setdiff(model_vars, sel$variable)
if (length(missing_v) > 0) stop("Model variables not found in selected_predictors.csv: ", paste(missing_v, collapse = ", "))
sel <- sel[match(model_vars, sel$variable), ]
hist_files <- p(sel$historic_file)

future_files <- function(ssp, per) {
  ifelse(sel$variable %in% static_vars, hist_files,
         file.path(dirname(hist_files), sprintf("%s_%s_%s_%s_iceland.tif", gcm, ssp, sel$variable, per)))
}
# Check every file up front, so a missing one stops the run before any slow step.
all_needed <- unique(c(hist_files, unlist(lapply(names(scenarios), function(s) unlist(lapply(periods, function(per) future_files(s, per)))))))
if (any(!file.exists(all_needed))) {
  stop(sum(!file.exists(all_needed)), " raster(s) not found, for example:\n  ",
       paste(utils::head(all_needed[!file.exists(all_needed)], 8), collapse = "\n  "))
}

valid <- NULL
masked <- function(r) if (is.null(valid)) r else mask(r, valid)
envs_hist <- rast(hist_files); names(envs_hist) <- sel$variable
if (file.exists(valid_file)) {
  v <- rast(valid_file)
  if (isTRUE(compareGeom(envs_hist, v, stopOnError = FALSE))) valid <- v else warning("valid_cells_mask.tif is not on the predictors' grid; not applied.")
}
envs_hist <- masked(envs_hist)

load_future <- function(ssp, per) {
  r <- rast(future_files(ssp, per)); names(r) <- sel$variable
  if (!isTRUE(compareGeom(envs_hist, r, stopOnError = FALSE))) stop("Future rasters for ", ssp, " ", per, " are not on the same grid as the historic ones.")
  masked(r)
}

# ---- Helpers ----------------------------------------------------------------
predict_suit <- function(r) {                      # cloglog suitability for every cell with data in all layers
  v  <- terra::values(r, mat = TRUE)[, model_vars, drop = FALSE]
  ok <- stats::complete.cases(v)
  pr <- rep(NA_real_, nrow(v))
  idx <- which(ok)
  for (ch in split(idx, ceiling(seq_along(idx) / 20000))) {
    pr[ch] <- as.numeric(predict(mod, as.data.frame(v[ch, , drop = FALSE]), type = "cloglog", clamp = TRUE))
  }
  out <- terra::rast(r, nlyrs = 1)
  terra::values(out) <- pr
  names(out) <- "suitability"
  out
}
outside_flags <- function(r) lapply(model_vars, function(v) terra::ifel(r[[v]] < mod$varmin[[v]] | r[[v]] > mod$varmax[[v]], 1, 0))
wmean <- function(r) {                              # area-weighted mean over land cells
  w <- terra::cellSize(r, unit = "km", mask = TRUE)
  terra::global(r * w, "sum", na.rm = TRUE)[1, 1] / terra::global(w, "sum", na.rm = TRUE)[1, 1]
}

# ---- Baseline ---------------------------------------------------------------
base_file <- file.path(out_dir, "suitability_baseline_1981-2010.tif")
if (file.exists(base_file) && !overwrite) {
  pred_base <- rast(base_file)
} else {
  message("Predicting baseline (1981-2010) suitability ...")
  pred_base <- masked(predict_suit(envs_hist))
  writeRaster(pred_base, base_file, overwrite = TRUE)
}
mean_base <- wmean(pred_base)

# ---- The 9 projections --------------------------------------------------------
preds <- list(); changes <- list(); outmasks <- list(); extrap <- list(); summ <- list()
extrap_dist <- function(r) {                        # largest distance outside the training range, as a fraction of that range
  d <- lapply(model_vars, function(v) {
    lo <- mod$varmin[[v]]; hi <- mod$varmax[[v]]; rg <- hi - lo
    terra::ifel(r[[v]] < lo, (lo - r[[v]]) / rg, terra::ifel(r[[v]] > hi, (r[[v]] - hi) / rg, 0))
  })
  out <- max(rast(d)); names(out) <- "extrapolation"
  out
}
t0 <- Sys.time(); k <- 0; n_total <- length(scenarios) * length(periods)
for (s in names(scenarios)) for (per in periods) {
  k <- k + 1
  key <- paste(s, per, sep = "_")
  envs_f <- load_future(s, per)
  f_pred <- file.path(out_dir, sprintf("suitability_%s_%s_%s.tif", gcm, s, per))
  if (file.exists(f_pred) && !overwrite) {
    pred_f <- rast(f_pred)
    note <- "loaded"
  } else {
    pred_f <- masked(predict_suit(envs_f))
    writeRaster(pred_f, f_pred, overwrite = TRUE)
    note <- "predicted"
  }
  flags   <- outside_flags(envs_f)
  n_out   <- masked(sum(rast(flags)))
  share_v <- vapply(flags, function(fl) terra::global(masked(fl), "mean", na.rm = TRUE)[1, 1], numeric(1))
  names(share_v) <- model_vars
  ch <- pred_f - pred_base
  preds[[key]]    <- pred_f
  changes[[key]]  <- ch
  outmasks[[key]] <- terra::ifel(n_out > 0, 1, NA)
  ex <- masked(extrap_dist(envs_f))
  extrap[[key]] <- ex
  writeRaster(ex, file.path(out_dir, sprintf("uncertainty_extrapolation_%s_%s.tif", s, per)), overwrite = TRUE)
  cv <- terra::values(ch, mat = FALSE); cv <- cv[is.finite(cv)]
  summ[[key]] <- data.frame(
    scenario = scenarios[[s]], ssp = s, period = per,
    mean_suitability = round(wmean(pred_f), 4), mean_change = round(wmean(pred_f) - mean_base, 4),
    pct_cells_down_0.1 = round(100 * mean(cv < -0.1), 1), pct_cells_up_0.1 = round(100 * mean(cv > 0.1), 1),
    max_increase = round(max(cv), 4), max_decrease = round(min(cv), 4),   # the single largest rise and fall in any cell (future minus baseline)
    pct_cells_outside_range = round(100 * terra::global(n_out > 0, "mean", na.rm = TRUE)[1, 1], 1),
    most_often_outside = if (max(share_v) > 0) names(which.max(share_v)) else NA_character_,
    mean_extrapolation = round(wmean(ex), 4),
    stringsAsFactors = FALSE)
  message(sprintf("[%s] %d of %d: %s %s %s (%.1f min elapsed)", format(Sys.time(), "%H:%M"), k, n_total,
                  scenarios[[s]], per, note, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
summary_tab <- do.call(rbind, summ); rownames(summary_tab) <- NULL
write.csv(summary_tab, file.path(out_dir, "scenario_summary.csv"), row.names = FALSE)
message("\n=== Scenario summary (baseline area-weighted mean suitability: ", round(mean_base, 3), ") ===")
print(summary_tab, row.names = FALSE)

# ---- Uncertainty across the 9 projections ---------------------------------------
n_proj     <- length(preds)
pred_stack <- rast(preds);   names(pred_stack) <- names(preds)
chg_stack  <- rast(changes); names(chg_stack)  <- names(changes)

union_out <- function(keys) terra::ifel(sum(rast(outmasks[keys]), na.rm = TRUE) > 0, 1, NA)   # any projection extrapolating

# (a) SD of suitability across all 9 projections (scenario + time)
unc_all <- terra::stdev(pred_stack, pop = FALSE, na.rm = TRUE); names(unc_all) <- "sd_all9"
out_all <- union_out(names(preds))

# (b) Agreement on the direction of change: share of the 9 projections on the majority side (0.56 to 1)
n_down <- sum(chg_stack < 0)
n_up   <- sum(chg_stack > 0)
agree  <- max(n_down, n_up) / n_proj; names(agree) <- "direction_agreement"
mean_change_all <- mean(chg_stack, na.rm = TRUE); names(mean_change_all) <- "mean_change_all9"

writeRaster(unc_all, file.path(out_dir, "uncertainty_sd_all9.tif"), overwrite = TRUE)
writeRaster(agree, file.path(out_dir, "uncertainty_direction_agreement.tif"), overwrite = TRUE)
writeRaster(mean_change_all, file.path(out_dir, "uncertainty_mean_change_all9.tif"), overwrite = TRUE)

pct <- function(r) round(100 * terra::global(r, "mean", na.rm = TRUE)[1, 1], 1)
unc_tab <- data.frame(
  measure = "SD across all 9 projections",
  mean_sd = round(wmean(unc_all), 4),
  pct_cells_sd_above_0.1 = pct(unc_all > 0.1),
  pct_cells_all9_agree_on_direction = pct(agree > 1 - 1e-9),
  pct_cells_8of9_agree_on_direction = pct(agree > 8 / 9 - 1e-9),
  stringsAsFactors = FALSE)
write.csv(unc_tab, file.path(out_dir, "uncertainty_summary.csv"), row.names = FALSE)
message("\n=== Uncertainty summary ===")
print(unc_tab, row.names = FALSE)

# ---- Figures ----------------------------------------------------------------
# Shared colour scale for the change maps, set by the 2nd-98th percentile of all nine changes pooled.
all_ch <- unlist(lapply(changes, function(r) { v <- terra::values(r, mat = FALSE); v[is.finite(v)] }))
chg_q <- stats::quantile(all_ch, c(0.02, 0.98), names = FALSE)   # the 2nd and 98th percentile, marked on the colour bar
lim <- max(abs(chg_q))
if (!is.finite(lim) || lim == 0) lim <- max(abs(all_ch))
message(sprintf("\nShared change colour scale: +/-%.2f (2nd percentile %.3f, 98th percentile %.3f of all nine changes pooled)",
                lim, chg_q[1], chg_q[2]))

# Colour scale for the all-9 SD map: 0 to its 98th percentile.
all_sd <- terra::values(unc_all, mat = FALSE); all_sd <- all_sd[is.finite(all_sd)]
sd_max <- as.numeric(stats::quantile(all_sd, 0.98))
if (!is.finite(sd_max) || sd_max == 0) sd_max <- max(all_sd)
message(sprintf("SD colour scale: 0 to %.2f (98th percentile)", sd_max))

# Shared colour scale for the nine extrapolation maps: 0 to the 98th percentile of the cells that extrapolate at all.
all_ex <- unlist(lapply(extrap, function(r) { v <- terra::values(r, mat = FALSE); v[is.finite(v) & v > 0] }))
ex_max <- if (length(all_ex) > 0) as.numeric(stats::quantile(all_ex, 0.98)) else 1
if (!is.finite(ex_max) || ex_max == 0) ex_max <- 1
message(sprintf("Shared extrapolation colour scale: 0 to %.2f of the training range", ex_max))

# Mean suitability for the line plot: the baseline, then the three periods, for each scenario (rows = baseline + periods,
# columns = scenarios). These are the area-weighted means already worked out above (mean_base and scenario_summary.csv).
mid_year <- function(per) mean(as.numeric(strsplit(per, "-")[[1]]))
ts_x   <- vapply(c(baseline_period, periods), mid_year, numeric(1))              # 1995.5, 2025.5, 2055.5, 2085.5
ts_lab <- c(paste0(baseline_period, "\n(baseline)"), periods)
ts_vals <- sapply(names(scenarios), function(s) {
  r <- summary_tab[summary_tab$ssp == s, ]
  c(mean_base, r$mean_suitability[match(periods, r$period)])
})
colnames(ts_vals) <- names(scenarios)
if (any(!is.finite(ts_vals))) stop("Couldn't get a mean suitability for every scenario and period (needed for the line plot).")
ts_xlim <- range(ts_x) + c(-6, 6)
ts_yr   <- range(ts_vals)
ts_pad  <- 0.12 * diff(ts_yr); if (!is.finite(ts_pad) || ts_pad == 0) ts_pad <- 0.02
# y-axis ticks: pretty values that cover the data plus a margin (starting at 0 if ts_y_from_zero). The plot's y-range is
# then set to exactly the lowest and highest tick, so the numbers (0.40, 0.50, ...) run over the whole range shown.
ts_yt   <- pretty(c(if (ts_y_from_zero) 0 else ts_yr[1] - ts_pad, ts_yr[2] + ts_pad))
ts_yt   <- ts_yt[ts_yt >= -1e-9 & ts_yt <= 1 + 1e-9]
ts_ylim <- range(ts_yt)
message("\n=== Mean suitability over time (area-weighted, all land) ===")
print(data.frame(scenario = rep(unname(scenarios), each = length(ts_x)),
                 period = rep(c(baseline_period, periods), times = length(scenarios)),
                 mean_suitability = round(as.vector(ts_vals), 4)), row.names = FALSE)

coast <- NULL
if (file.exists(coast_file)) coast <- tryCatch(project(vect(coast_file), crs(pred_base)), error = function(e) NULL)
suit_cols  <- hcl.colors(100, "YlGnBu", rev = TRUE)
chg_cols   <- hcl.colors(100, "Purple-Green")
sd_cols    <- hcl.colors(100, "YlOrRd", rev = TRUE)
agr_cols   <- hcl.colors(100, "Blues 3", rev = TRUE)
ex_cols    <- hcl.colors(100, "Reds 3", rev = TRUE)
add_coast  <- function() if (!is.null(coast)) lines(coast, lwd = 0.3, col = "grey25")
# Panel letters: bold, centred under each map, just below the southern edge of the land (keeps the gap between rows small).
land_xy <- as.vector(terra::ext(terra::trim(pred_base)))   # xmin, xmax, ymin, ymax of the land
map_letter <- function(i, sc = 1) text((land_xy[1] + land_xy[2]) / 2, land_xy[3] - 0.01 * (land_xy[4] - land_xy[3]),
                                       sprintf(letter_fmt, panel_letters[i]), adj = c(0.5, 1), font = 2, cex = letter_cex * sc, xpd = NA)   # sc = text size multiplier
add_outside <- function(om) {
  if (show_outside_range && isTRUE(terra::global(om, "sum", na.rm = TRUE)[1, 1] > 0)) {
    plot(om, add = TRUE, col = "#4d4d4d80", legend = FALSE)
  }
}

# ---- Layouts and drawing helpers ---------------------------------------------
chg_lab    <- "Suitability change from baseline (2nd-98th percentile)"
out_note   <- "Grey shading = at least one variable outside the model's training range"
scen_title <- function(s, per) paste0(scenarios[[s]], ",  ", per)

# Baseline suitability map. With legend = TRUE its own 0-1 colour bar sits in the right margin.
plot_baseline <- function(legend, mar = c(1.4, 0.5, 2.2, 5), cex.main = 1, leg_title = "Suitability\n(cloglog)", leg_cex = 1) {
  plot(pred_base, col = suit_cols, range = c(0, 1), axes = FALSE, box = FALSE, legend = legend,
       main = "Baseline suitability (1981-2010)", cex.main = cex.main, mar = mar,
       plg = list(title = leg_title, title.cex = 0.8 * leg_cex, cex = 0.8 * leg_cex))   # leg_cex = size multiplier for the legend title and numbers
  add_coast()
}

# One change map (future minus baseline) on the shared +/-lim scale, with grey shading where the model extrapolates.
plot_change <- function(key, ttl, mar = c(1.2, 0.3, 1.8, 0.3), cex.main = 0.85) {
  plot(clamp(changes[[key]], -lim, lim, values = TRUE), col = chg_cols, range = c(-lim, lim),
       legend = FALSE, axes = FALSE, box = FALSE, main = ttl, cex.main = cex.main, mar = mar)
  if (show_outside_on_change) add_outside(outmasks[[key]])
  add_coast()
}

# Horizontal purple-green colour bar for the change maps. Under the usual numbered ticks there is a second row with the
# actual 2nd and 98th percentile values (bold tick + number), and the bar's end values (-lim, +lim) where they are not
# already marked. An end label is skipped if it would sit within 15% of lim of a percentile label, so the numbers
# don't overlap. The bottom margin (4.6 lines) holds the tick numbers, the percentile row, the label and (if shown) the grey note.
change_bar <- function(sc = 1) {   # sc = text size multiplier (1 = the sizes used by figures 2b and 2c)
  vals <- seq(-lim, lim, length.out = 200)
  par(mar = c(4.6, 2.8, 0.4, 1.8))   # wider left/right margins so the end labels (e.g. -0.53) aren't cut off at the panel edge
  image(vals, 1, matrix(vals, ncol = 1), col = chg_cols, yaxt = "n", xlab = "", cex.axis = 0.9 * sc)
  mk_x   <- chg_q
  mk_lab <- c(sprintf("2nd: %.2f", chg_q[1]), sprintf("98th: %.2f", chg_q[2]))
  for (e in c(-lim, lim)) {
    if (all(abs(e - mk_x) > 0.15 * lim)) { mk_x <- c(mk_x, e); mk_lab <- c(mk_lab, sprintf("%.2f", e)) }
  }
  axis(1, at = mk_x, labels = FALSE, tck = -0.1, lwd = 0, lwd.ticks = 1.8)                      # bold ticks
  axis(1, at = mk_x, labels = mk_lab, tick = FALSE, line = 0.9, font = 2, cex.axis = 0.8 * sc)  # their numbers, one row below
  mtext(chg_lab, side = 1, line = 3.4, cex = 0.8 * sc)
  if (show_outside_on_change && show_outside_range) mtext(out_note, side = 1, line = 4.3, cex = 0.6 * sc, col = "grey30")
}

# Line plot of the mean suitability over time: one line per scenario, all starting at the baseline point (open circle).
# The caller sets the margins (par(mar = ...)) and the text and marker sizes. Draws into the current panel.
# yaxs = "i" makes the y-range exactly ts_ylim (no extra 4% each side), so the axis line and the numbered ticks run over
# the whole plotted range, from the bottom of the plot to the top.
plot_timeseries <- function(cex_axis = 1, cex_lab = 1, pt_cex = 1.5) {
  graphics::plot(NA, xlim = ts_xlim, ylim = ts_ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", yaxs = "i")
  abline(h = ts_yt, v = ts_x, col = "grey92")
  axis(1, at = ts_x, labels = ts_lab, padj = 0.4, cex.axis = cex_axis)
  axis(2, at = ts_yt, labels = formatC(ts_yt, format = "f", digits = 2), las = 1, cex.axis = cex_axis)
  mtext("Mean habitat suitability (cloglog)", side = 2, line = 3.3, cex = cex_lab)
  for (s in names(scenarios)) {
    lines(ts_x, ts_vals[, s], col = ts_cols[[s]], lwd = 2.8)
    points(ts_x[-1], ts_vals[-1, s], col = ts_cols[[s]], pch = ts_pch[[s]], cex = pt_cex)
  }
  points(ts_x[1], ts_vals[1, 1], pch = 21, bg = "white", col = "grey20", lwd = 2, cex = pt_cex * 1.15)   # the shared baseline point
}

# Legend for the line plot, in a panel of its own (as in 5e): the three scenarios and the baseline point. If the legend at
# the requested size would be wider than max_w of the panel (so its ends would be cut off), cex is reduced to fit.
ts_legend <- function(cex = 1.1, max_w = 0.96) {
  par(mar = c(0, 0, 0, 0)); plot.new()
  leg <- list(x = "center", horiz = TRUE, bty = "n",
              legend = c(unname(scenarios), paste0(baseline_period, " baseline")),
              col = c(unname(ts_cols[names(scenarios)]), "grey20"), pch = c(unname(ts_pch[names(scenarios)]), 21), pt.bg = "white",
              lty = c(1, 1, 1, NA), lwd = 2.8)
  w <- do.call(graphics::legend, c(leg, list(cex = cex, plot = FALSE)))$rect$w   # width as a fraction of the panel
  if (is.finite(w) && w > max_w) cex <- cex * max_w / w
  do.call(graphics::legend, c(leg, list(cex = cex)))
}

# Figure 2 layout: 12 columns. Rows 1-3: the 3 x 3 change maps, 4 columns wide each. Bottom row (same height as a map
# row): the baseline map on the right (panel 11, 5 columns wide = the 4 columns of a map plus one for its legend, so the
# map itself comes out the same size as the others) and the colour bar on the left (panel 10, in the middle of three
# thin sub-rows so it stays thin and level with the middle of the baseline).
lay_chg <- rbind(c(1,  1,  1,  1,  2,  2,  2,  2,  3,  3,  3,  3),
                 c(4,  4,  4,  4,  5,  5,  5,  5,  6,  6,  6,  6),
                 c(7,  7,  7,  7,  8,  8,  8,  8,  9,  9,  9,  9),
                 c(0,  0,  0,  0,  0,  0,  0,  11, 11, 11, 11, 11),
                 c(10, 10, 10, 10, 10, 10, 0,  11, 11, 11, 11, 11),
                 c(0,  0,  0,  0,  0,  0,  0,  11, 11, 11, 11, 11))
ht_chg  <- c(1, 1, 1, 0.27, 0.46, 0.27)   # the 3 bottom sub-rows add up to 1 = the height of a map row

# Figure 2c layout: figure 2 exactly as above, plus two rows underneath: the line plot (panel 12) centred over columns 3 to
# 10, and its legend (panel 13) below it. The extra rows are 1.3 and 0.16 map-row heights tall. The png is made taller by
# the same factor (see below), so the maps come out the same size as in figure 2.
lay_chg_ts <- rbind(lay_chg,
                    c(0, 0, 12, 12, 12, 12, 12, 12, 12, 12, 0, 0),
                    c(0, 0, 13, 13, 13, 13, 13, 13, 13, 13, 0, 0))
ht_chg_ts  <- c(ht_chg, 1.3, 0.16)

# Figure 2b layout: 4 columns. Baseline and the three change maps in a 2 x 2 grid, every panel 2 columns wide, with the
# colour bar centred underneath.
lay_2x2 <- rbind(c(1, 1, 2, 2),
                 c(3, 3, 4, 4),
                 c(0, 5, 5, 0))

# Figure 1: same layout as figure 2 (lay_chg): the 9 suitability maps on one shared 0-1 scale, then the colour bar (bottom left,
# same size and place as figure 2's change bar) and the baseline suitability map with its own legend (bottom right).
suitability_bar <- function(sc = 1) {   # sc = text size multiplier
  vals <- seq(0, 1, length.out = 200)
  par(mar = c(4.6, 2.8, 0.4, 1.8))   # same margins as the change bar, so it comes out the same size
  image(vals, 1, matrix(vals, ncol = 1), col = suit_cols, yaxt = "n", xlab = "", cex.axis = 0.9 * sc)
  mtext("Suitability (cloglog)", side = 1, line = 2.6, cex = 0.8 * sc)
  if (show_outside_on_suitability && show_outside_range) mtext(out_note, side = 1, line = 3.6, cex = 0.6 * sc, col = "grey30")
}

# Extra white space below the bottom row, in pixels (at 300 dpi). The png is made this much taller (see the png() calls),
# so the maps stay the same size and the pad is all new space under the panel letter (j).
pad_bottom <- function(px) {
  o <- par("omi")
  par(omi = c(px / 300, o[2], o[3], o[4]))
}

# Figure 1. All text is grid_text_scale times its old size, except the baseline legend's "Suitability" title and numbers,
# which are grid_legend_scale times their old size.
draw_suitability_grid <- function(sc = grid_text_scale, leg_cex = grid_legend_scale) {
  layout(lay_chg, heights = ht_chg)
  par(oma = c(0, 0, 3, 0))
  pad_bottom(grid_bottom_pad_px)
  
  pn <- 0
  for (s in names(scenarios)) for (per in periods) {
    key <- paste(s, per, sep = "_")
    plot(preds[[key]], col = suit_cols, range = c(0, 1), legend = FALSE, axes = FALSE, box = FALSE,
         main = scen_title(s, per), cex.main = 0.85 * sc, mar = c(1.2, 0.3, 1.8, 0.3))
    if (show_outside_on_suitability) add_outside(outmasks[[key]])
    add_coast()
    pn <- pn + 1; map_letter(pn, sc)
  }
  
  suitability_bar(sc)               # panel 10
  # the "\n" after the legend title adds an empty line under it, which lifts it clear of the "1.0" at the top of the bar
  plot_baseline(legend = TRUE, mar = c(1.2, 0.3, 1.8, 4.5), cex.main = 0.85 * sc, leg_title = "Suitability\n", leg_cex = leg_cex)   # panel 11
  pn <- pn + 1; map_letter(pn, sc)
  
  mtext(sprintf("Projected crowberry habitat suitability, %s", toupper(gcm)),
        outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.1 * sc)
}

# Panels 1 to 11 of figure 2: the 9 change maps (future minus baseline) on one shared +/-lim scale, then the colour bar
# (left) and the baseline suitability map with its own legend (right). Used by both figure 2 and figure 2c, so the maps are
# identical in the two. The baseline is drawn last, so it gets the last panel letter. Returns the number of letters used.
draw_change_panels <- function(sc = 1, leg_cex = 1) {   # sc, leg_cex = text size multipliers: 1 for figure 2c, grid_text_scale and grid_legend_scale for figure 2
  pn <- 0
  for (s in names(scenarios)) for (per in periods) {
    plot_change(paste(s, per, sep = "_"), scen_title(s, per), cex.main = 0.85 * sc)
    pn <- pn + 1; map_letter(pn, sc)
  }
  
  change_bar(sc)                    # panel 10
  # panel 11: same margins and title size as the change maps (so the map is the same size), except for a wider right
  # margin that holds the legend; one-line legend title so it fits in the top margin. The legend's "Suitability" title and
  # numbers have their own multiplier (leg_cex).
  # (figure 2 only: its enlarged legend text would otherwise run into the title, so the "\n" adds an empty line under the
  # title, which lifts it clear of the "1.0" at the top of the bar. Figure 2c keeps its title as it was.)
  plot_baseline(legend = TRUE, mar = c(1.2, 0.3, 1.8, 4.5), cex.main = 0.85 * sc,
                leg_title = if (leg_cex > 1) "Suitability\n" else "Suitability", leg_cex = leg_cex)
  pn <- pn + 1; map_letter(pn, sc)
  pn
}

# Figure 2: the change maps, colour bar and baseline. All text is grid_text_scale times its old size, except the baseline
# legend's "Suitability" title and numbers, which are grid_legend_scale times their old size.
draw_change_grid <- function(sc = grid_text_scale, leg_cex = grid_legend_scale) {
  layout(lay_chg, heights = ht_chg)
  par(oma = c(0, 0, 3, 0))
  pad_bottom(grid_bottom_pad_px)
  draw_change_panels(sc, leg_cex)
  mtext(sprintf("Projected change in crowberry habitat suitability, %s", toupper(gcm)),
        outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.1 * sc)
}

# Figure 2c: figure 2 (same panels, same title) with the line plot of mean suitability over time underneath it (panel k) and
# its legend below that. The text sizes are set a little above 1 because a layout this big shrinks them to 0.66 of normal;
# the panel letter is sized the same way as the map letters.
draw_change_grid_line_plot <- function() {
  layout(lay_chg_ts, heights = ht_chg_ts)
  par(oma = c(0, 0, 3, 0))
  pn <- draw_change_panels()
  
  par(mar = c(5.6, 4.8, 2.4, 1.2))                                   # panel 12
  plot_timeseries(cex_axis = 1.15, cex_lab = 0.85, pt_cex = 2)
  title(main = "Mean suitability across Iceland over time", cex.main = 1.1, font.main = 2)
  pn <- pn + 1
  mtext(sprintf(letter_fmt, panel_letters[pn]), side = 1, line = 4.5, at = mean(par("usr")[1:2]),
        font = 2, cex = letter_cex * par("cex"))
  ts_legend(cex = 1.1)                                               # panel 13
  
  mtext(sprintf("Projected change in crowberry habitat suitability, %s", toupper(gcm)),
        outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.1)
}

# Figure 4: the line plot on its own, with a title (no subtitle notes), and the legend underneath. The bottom margin (4.2 lines) is
# just deep enough for the two-line x-axis labels, which keeps the figure compact; the legend is fitted to the width.
draw_timeseries_fig <- function() {
  layout(matrix(1:2, ncol = 1), heights = c(1, 0.14))
  par(oma = c(0, 0, 2.4, 0), mar = c(4.2, 5.2, 1.2, 1.6))
  plot_timeseries(cex_axis = 1, cex_lab = 1, pt_cex = 1.6)
  ts_legend(cex = 1.05)
  mtext(sprintf("Mean crowberry habitat suitability under each SSP scenario (%s)", toupper(gcm)),
        outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.1)
}

# Figure 2b: baseline (top left) and the change in one period (period_2x2) under SSP1-2.6 (top right), SSP3-7.0
# (bottom left) and SSP5-8.5 (bottom right). All four maps get identical margins so they come out the same size; the
# baseline's legend sits in its right margin. Same +/-lim colour scale as figure 2.
draw_change_2x2 <- function(per = period_2x2) {
  layout(lay_2x2, heights = c(1, 1, 0.45))
  par(oma = c(0, 0, 3, 0))
  mar4 <- c(1.4, 0.5, 2.2, 5)
  
  plot_baseline(legend = TRUE, mar = mar4)
  pn <- 1; map_letter(pn)
  
  for (s in names(scenarios)) {
    plot_change(paste(s, per, sep = "_"), scen_title(s, per), mar = mar4, cex.main = 1)
    pn <- pn + 1; map_letter(pn)
  }
  
  change_bar()
  
  mtext(sprintf("Projected change in crowberry habitat suitability, %s, %s", per, toupper(gcm)),
        outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.1)
}

# Uncertainty figure. Row 1: SD across all 9 projections (left) and direction agreement (right), each with its own
# legend. Rows 2-4: the extrapolation distance of each of the 9 projections (rows = scenario, columns = period).
# Row 5: shared colour bar for the 9 extrapolation maps.
lay_u <- rbind(c(1, 1, 1, 2, 2, 2),
               c(3, 3, 4, 4, 5, 5),
               c(6, 6, 7, 7, 8, 8),
               c(9, 9, 10, 10, 11, 11),
               c(0, 12, 12, 12, 12, 0))

draw_uncertainty <- function() {
  layout(lay_u, heights = c(1.5, 1, 1, 1, 0.45))
  par(oma = c(0, 0, 3, 0))
  
  plot(clamp(unc_all, 0, sd_max, values = TRUE), col = sd_cols, range = c(0, sd_max), axes = FALSE, box = FALSE,
       main = "SD of suitability across all 9 projections", cex.main = 0.95, mar = c(1.4, 0.5, 2.2, 5),
       plg = list(title = "SD", title.cex = 0.8, cex = 0.8))
  add_outside(out_all); add_coast()
  pn <- 1; map_letter(pn)
  
  plot(agree, col = agr_cols, range = c(0.5, 1), axes = FALSE, box = FALSE,
       main = "Agreement on direction of change", cex.main = 0.95, mar = c(1.4, 0.5, 2.2, 5),
       plg = list(title = "Share of 9\nagreeing", title.cex = 0.8, cex = 0.8))
  add_outside(out_all); add_coast()
  pn <- pn + 1; map_letter(pn)
  
  for (s in names(scenarios)) for (per in periods) {
    key <- paste(s, per, sep = "_")
    plot(clamp(extrap[[key]], 0, ex_max, values = TRUE), col = ex_cols, range = c(0, ex_max), legend = FALSE,
         axes = FALSE, box = FALSE, main = paste0(scenarios[[s]], ",  ", per), cex.main = 0.85,
         mar = c(1.2, 0.3, 1.8, 0.3))
    add_coast()
    pn <- pn + 1; map_letter(pn)
  }
  
  vals <- seq(0, ex_max, length.out = 200)
  par(mar = c(3.6, 1, 0.4, 1))
  image(vals, 1, matrix(vals, ncol = 1), col = ex_cols, yaxt = "n", xlab = "", cex.axis = 0.9)
  mtext("Extrapolation distance in each projection (fraction of the training range; 0 = within range)", side = 1, line = 2.3, cex = 0.8)
  if (show_outside_range) mtext("Top-row grey shading = at least one of the 9 projections has a variable outside the training range", side = 1, line = 3.3, cex = 0.6, col = "grey30")
  
  mtext(sprintf("Prediction uncertainty, %s projections", toupper(gcm)),
        outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.1)
}

f_2x2 <- sprintf("scenario_change_grid_%s.png", period_2x2)
png(file.path(out_dir, "scenario_suitability_grid.png"), width = 3000, height = 3500 + grid_bottom_pad_px, res = 300)   # + the pad below (j)
draw_suitability_grid(); invisible(dev.off())
png(file.path(out_dir, "scenario_change_grid.png"), width = 3000, height = 3500 + grid_bottom_pad_px, res = 300)
draw_change_grid(); invisible(dev.off())
png(file.path(out_dir, f_2x2), width = 3000, height = 2950, res = 300)
draw_change_2x2(); invisible(dev.off())
# 4730 px = figure 2's 3500 px stretched by (4 + 1.3 + 0.16) / 4 for the two extra rows (the 3-line top margin stays the same)
png(file.path(out_dir, "scenario_change_grid_line_plot.png"), width = 3000, height = 4730, res = 300)
draw_change_grid_line_plot(); invisible(dev.off())
png(file.path(out_dir, "scenario_uncertainty_grid.png"), width = 3000, height = 4300, res = 300)
draw_uncertainty(); invisible(dev.off())
png(file.path(out_dir, "scenario_mean_suitability_over_time.png"), width = ts_fig_px[1], height = ts_fig_px[2], res = 300)
draw_timeseries_fig(); invisible(dev.off())

message("\nSaved: ", file.path(out_dir, "scenario_suitability_grid.png"))
message("Saved: ", file.path(out_dir, "scenario_change_grid.png"))
message("Saved: ", file.path(out_dir, f_2x2))
message("Saved: ", file.path(out_dir, "scenario_change_grid_line_plot.png"))
message("Saved: ", file.path(out_dir, "scenario_uncertainty_grid.png"))
message("Saved: ", file.path(out_dir, "scenario_mean_suitability_over_time.png"))
message("Saved: ", file.path(out_dir, "scenario_summary.csv"))
message("Saved: ", file.path(out_dir, "uncertainty_summary.csv"))
message("Saved: 10 suitability rasters and 12 uncertainty rasters in ", out_dir)