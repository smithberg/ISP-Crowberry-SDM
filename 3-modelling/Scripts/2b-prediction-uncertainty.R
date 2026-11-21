# 2b-prediction-uncertainty.R
# Prediction uncertainty for the 9 GFDL-ESM4 projections (SSP1-2.6, SSP3-7.0, SSP5-8.5 x 2011-2040, 2041-2070,
# 2071-2100), and which variables drive it. Figures:
#   1. uncertainty_across_projections.png        SD of suitability across all 9 projections, and how many of the 9
#                                                agree on the direction of change (5 to 9 of 9; 4 or fewer = grey)
#   1b. uncertainty_across_scenarios.png         spread of suitability across the 3 scenarios in ONE period (scenario_period,
#                                                2071-2100 by default), and how many of the 3 agree on the direction of change
#   1c. uncertainty_across_scenarios_by_period.png   the same spread map for each of the 3 periods side by side, on one
#                                                colour scale (shows the scenarios fanning out over time)
#   2. uncertainty_extrapolation_grid.png        3 x 3 grid, extrapolation distance of each projection
#   3. extrapolation_dominant_variable_grid.png  3 x 3 grid, which variable is furthest outside its training range
#   4. extrapolation_variable_count_grid.png     3 x 3 grid, how many variables are outside their training range
#   5. change_attribution_grid.png               3 x 3 grid, which variable drives the change in suitability
#                                                (one-at-a-time attribution; only if run_attribution = TRUE)
# (rows = scenario, columns = period in the 3 x 3 grids)
#
# Put this file in: ISP-Crowberry-SDM/3-modelling/Scripts/   (run 1a first; 2a is not required)
# Reads:  3-modelling/Outputs/Baseline model/baseline_maxent_model.rds           (the model from 1a)
#         2-process-data/Outputs/Predictor screening/selected_predictors.csv      (raster paths)
#         2-process-data/Outputs/<historic_file column>                            (1981-2010 rasters)
#         2-process-data/Outputs/CHELSA-BIOCLIM filled/<gcm>_<ssp>_<var>_<period>_iceland.tif  (future rasters)
#         2-process-data/Outputs/Background points/valid_cells_mask.tif            (optional, used if present)
#         1-download-data/Downloads/LMI-IS50V/is50v_land_polygon.gpkg              (optional coastline outline)
#         3-modelling/Outputs/Projection/Scenario grid/suitability_*.tif           (optional: the suitability rasters
#              written by 2a are reused if they exist, otherwise the suitability is predicted here)
# Writes: 3-modelling/Outputs/Uncertainty/
#           README.txt                                        the folder layout below, written on each run
#           Figures/
#             the five figures above
#           Tables/
#             uncertainty_summary.csv                         across-projection measures (all 9)
#             uncertainty_summary_across_scenarios.csv        across-scenario measures, one row per period (3 scenarios)
#             extrapolation_summary.csv                       one row per projection
#             extrapolation_by_variable.csv                   one row per projection and variable
#             extrapolation_variable_count_summary.csv        how many variables are out of range, per projection
#             change_attribution_summary.csv                  one row per projection and variable
#           Rasters/
#             Suitability/                                    suitability rasters predicted here (none if 2a's are reused)
#             Across projections/                             scenarios_<period>_range.tif, scenarios_<period>_sd.tif and
#                                                             scenarios_<period>_direction_agreement_n_of_3.tif (one set per
#                                                             period; the 3 scenarios of that period), plus the all-9 rasters:
#                                                             uncertainty_sd_all9.tif, uncertainty_direction_agreement.tif
#                                                             (a fraction, 0.56 to 1), uncertainty_direction_agreement_n_of_9.tif
#                                                             (a count, 0 to 9), uncertainty_mean_change_all9.tif
#             Extrapolation/
#               Distance/                 extrapolation_distance_<ssp>_<period>.tif  (9 rasters)
#               Dominant variable/        extrapolation_dominant_<ssp>_<period>.tif  (9 rasters, coded, see below)
#               Variable count/           extrapolation_nvars_<ssp>_<period>.tif     (1, 2, 3 = 3 or more)
#             Change attribution/
#               Dominant variable/        change_attribution_dominant_<ssp>_<period>.tif  (9 rasters)
#               One-at-a-time effects/    effect_<ssp>_<period>_<var>.tif  (reused on re-runs)
# Inputs are only read, never changed.
#
# What is measured (one fitted model and one climate model, so there is no statistical error on the predictions):
#   - SD across all 9 projections: how much a cell's predicted suitability varies over scenarios AND periods, so it
#     includes the time trend as well as the scenario differences.
#   - Spread across the 3 scenarios, one period at a time (figures 1b and 1c): within one period the three projections
#     differ only by scenario, so this measures scenario uncertainty without the time trend. With only 3 values an SD
#     is a rough descriptive spread (it is about 0.5 to 0.58 times the range), so the default is the RANGE (max minus
#     min, in suitability units: the largest difference between any two scenarios). Set spread_metric <- "sd" for the SD
#     instead; both are saved as rasters. The agreement map counts how many of the 3 scenarios agree on whether
#     suitability falls or rises relative to baseline (3 of 3, 2 of 3, or 1 or fewer = no majority, for example where
#     some scenarios show no change). Both are also summarised per period in uncertainty_summary_across_scenarios.csv.
#   - Direction agreement: how many of the 9 projections agree on whether suitability falls or rises relative to the
#     1981-2010 baseline, shown as a count out of 9 (9 of 9 = all agree, 5 of 9 = a 5 to 4 split). Where 4 or fewer
#     agree, no direction is shared by a majority: some of the 9 show no change at all in that cell (for example where
#     suitability is zero in the baseline and in the future). Those cells are drawn light grey. The count is also saved
#     as a fraction of 9 (0.56 to 1) in uncertainty_direction_agreement.tif.
#   - Extrapolation distance, per projection: for each cell, the largest distance by which any variable lies outside
#     the range the model was trained on, as a fraction of that variable's training range (0 = inside the range for
#     every variable). It is checked one variable at a time, so it does not detect unusual COMBINATIONS of values
#     that are each inside their range, and it says nothing about how strongly the prediction responds to the
#     inputs. That is why it can look flat where the SD and agreement maps show structure.
#
# Which variables contribute, and what happens when several do:
#   - Dominant variable (extrapolation): the variable with the largest distance outside its range in the cell.
#     Where the runner-up is at least `mixed_ratio` times as far out as the dominant one (default 0.5), the cell
#     is classed as "mixed" instead of being given to one variable. The number of variables outside the range
#     is mapped separately (1, 2, 3 or more).
#   - Change attribution: for each variable that changes with climate, the model predicts with ONLY that variable
#     switched from its 1981-2010 value to its future value (all others at baseline). The effect is that
#     prediction minus the baseline suitability. The dominant variable is the one with the largest absolute
#     effect, again with a "mixed" class when the runner-up is at least `mixed_ratio` times as large. Cells where
#     the largest effect is below `attr_min` are left blank. The model is not additive (cloglog output, variables
#     interact), so the one-at-a-time effects will not sum to the total change; the maps show which variable
#     matters most, not a decomposition. Correlated climate variables (e.g. several temperature variables) move
#     together, so the "dominant" one may only be the one furthest out among a group that shifts together.
#   - Coding of the *_dominant_*.tif rasters: 1..K = the K variables in the order of the model (a colour table is
#     stored in the file), K+1 = mixed.
# On the figure 1 maps, dark grey shading = at least one of the 9 projections extrapolates in that cell. On the agreement
# map, LIGHT grey = 4 or fewer of the 9 agree on a direction (a different thing from the dark extrapolation shading).
#
# Limits: one climate model (GFDL-ESM4), so climate-model uncertainty is not captured; fitting uncertainty (for
# example from bootstrapped models) is not included; suitability is a relative index, not abundance; cells are not
# equal area (WGS84 degrees), so the means are area-weighted. The attribution step predicts the model once per
# climate variable and projection, so it is the slow part of the script; the effect rasters are saved and reused.
#
# NOT TESTED on your data. Check the console messages and the figures before using the output.

# install.packages(c("terra", "maxnet"))
library(terra)
library(maxnet)

# ---- Settings ---------------------------------------------------------------
gcm        <- "gfdl-esm4"
scenarios  <- c(ssp126 = "SSP1-2.6", ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")   # rows, in this order
periods    <- c("2011-2040", "2041-2070", "2071-2100")                            # columns, in this order
static_vars <- c("elevation", "slope", "northness", "eastness", "tpi", "roughness")   # terrain: reuse the historic raster
show_outside_range <- TRUE   # grey shading on the uncertainty maps where any projection shown extrapolates
scenario_period <- "2071-2100"   # figure 1b: the period whose 3 scenarios are compared
spread_metric   <- "range"       # "range" (max - min across the 3 scenarios) or "sd" (SD across the 3), figures 1b and 1c
spread_by_period_figure <- TRUE  # also make figure 1c (the spread map for each of the 3 periods)
mixed_ratio <- 0.5           # runner-up at least this fraction of the largest -> cell is "mixed"
run_attribution <- TRUE      # one-at-a-time attribution of the change (slow: one prediction per climate variable and projection)
attr_min   <- 0.01           # attribution: cells whose largest one-at-a-time effect is below this are left blank
overwrite  <- TRUE          # FALSE = reuse saved suitability and effect rasters if they exist
panel_letters <- letters     # letter centred under each map: a, b, c ... (LETTERS for A, B, C)
letter_fmt    <- "(%s)"      # "(%s)" gives (a); "%s" gives a
letter_cex    <- 1.1         # size of the panel letters (map titles are 0.8-1.0)

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
scen_dir    <- p("3-modelling", "Outputs", "Projection", "Scenario grid")   # suitability rasters from 2a, reused if present
out_dir     <- p("3-modelling", "Outputs", "Uncertainty")
fig_dir     <- file.path(out_dir, "Figures")
tab_dir     <- file.path(out_dir, "Tables")
ras_dir     <- file.path(out_dir, "Rasters")
suit_dir    <- file.path(ras_dir, "Suitability")
across_dir  <- file.path(ras_dir, "Across projections")
ext_dist_dir <- file.path(ras_dir, "Extrapolation", "Distance")
ext_dom_dir  <- file.path(ras_dir, "Extrapolation", "Dominant variable")
ext_n_dir    <- file.path(ras_dir, "Extrapolation", "Variable count")
att_dom_dir  <- file.path(ras_dir, "Change attribution", "Dominant variable")
attr_dir     <- file.path(ras_dir, "Change attribution", "One-at-a-time effects")
all_dirs <- c(fig_dir, tab_dir, suit_dir, across_dir, ext_dist_dir, ext_dom_dir, ext_n_dir)
if (run_attribution) all_dirs <- c(all_dirs, att_dom_dir, attr_dir)
for (d in all_dirs) dir.create(d, showWarnings = FALSE, recursive = TRUE)

for (f in c(sel_file, model_file)) if (!file.exists(f)) stop("Missing input: ", f, if (f == model_file) "\nRun 1a-fit-baseline-maxent.R first." else "")

mod <- readRDS(model_file)
model_vars <- names(mod$varmin)
if (is.null(model_vars)) stop("The saved model has no varmin/varmax, so its variables can't be identified. Refit with 1a.")
message("Model variables (", length(model_vars), "): ", paste(model_vars, collapse = ", "))
clim_vars <- setdiff(model_vars, static_vars)    # variables that change with climate
message("Variables that change with climate (", length(clim_vars), "): ", paste(clim_vars, collapse = ", "))

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
# Suitability raster: reuse the one written by 2a or by an earlier run of this script, otherwise predict and save it here.
get_suit <- function(fname, envs) {
  for (d in c(scen_dir, suit_dir)) {
    f <- file.path(d, fname)
    if (file.exists(f) && !overwrite) {
      r <- rast(f)
      if (isTRUE(compareGeom(envs, r, stopOnError = FALSE))) return(list(r = r, note = paste("loaded from", basename(d))))
    }
  }
  r <- masked(predict_suit(envs))
  writeRaster(r, file.path(suit_dir, fname), overwrite = TRUE)
  list(r = r, note = "predicted")
}
extrap_layers <- function(r) {                      # per variable: distance outside the training range, as a fraction of that range
  d <- lapply(model_vars, function(v) {
    lo <- mod$varmin[[v]]; hi <- mod$varmax[[v]]; rg <- hi - lo
    terra::ifel(r[[v]] < lo, (lo - r[[v]]) / rg, terra::ifel(r[[v]] > hi, (r[[v]] - hi) / rg, 0))
  })
  out <- rast(d); names(out) <- model_vars
  out
}
# Classify the dominant layer of a stack of non-negative layers. 1..K = that layer, K+1 = mixed (runner-up is at
# least `ratio` times the largest), NA where the largest value is not above `min_val`.
dominant_class <- function(st, ratio, min_val = 0) {
  K    <- terra::nlyr(st)
  mx   <- max(st)
  dom  <- terra::which.max(st)
  rest <- max(rast(lapply(seq_len(K), function(i) terra::ifel(dom == i, 0, st[[i]]))))
  cls  <- terra::ifel(mx <= min_val, NA, terra::ifel(rest >= ratio * mx, K + 1, dom))
  names(cls) <- "dominant_variable"
  cls
}
with_cols <- function(r, cols) { terra::coltab(r) <- data.frame(value = seq_along(cols), col = cols); r }
wmean <- function(r) {                              # area-weighted mean over land cells
  w <- terra::cellSize(r, unit = "km", mask = TRUE)
  terra::global(r * w, "sum", na.rm = TRUE)[1, 1] / terra::global(w, "sum", na.rm = TRUE)[1, 1]
}
pct <- function(r) round(100 * terra::global(r, "mean", na.rm = TRUE)[1, 1], 1)
cell_sum <- function(r) terra::global(r, "sum", na.rm = TRUE)[1, 1]

# Colours: one per model variable (same variable, same colour in every figure), grey for "mixed".
var_cols   <- setNames(hcl.colors(length(model_vars), "Dark 3"), model_vars)
mixed_col  <- "#9e9e9e"
ext_cls_cols  <- c(unname(var_cols), mixed_col)                  # classes of the extrapolation dominant map
att_cls_cols  <- c(unname(var_cols[clim_vars]), mixed_col)       # classes of the attribution dominant map
count_cols    <- c("#fdd49e", "#fc8d59", "#b30000")              # 1, 2, 3+ variables outside the range

# ---- Baseline and the 9 projections -------------------------------------------
b <- get_suit("suitability_baseline_1981-2010.tif", envs_hist)
pred_base <- b$r
message("Baseline suitability: ", b$note)

preds <- list(); extrap <- list(); ext_dom <- list(); ext_cnt <- list(); eff <- list(); att_dom <- list()
ext_summ <- list(); var_summ <- list(); cnt_summ <- list(); att_summ <- list()
t0 <- Sys.time(); k <- 0; n_total <- length(scenarios) * length(periods)
for (s in names(scenarios)) for (per in periods) {
  k <- k + 1
  key    <- paste(s, per, sep = "_")
  envs_f <- load_future(s, per)
  g      <- get_suit(sprintf("suitability_%s_%s_%s.tif", gcm, s, per), envs_f)
  preds[[key]] <- g$r
  
  # -- Extrapolation: distance, dominant variable, number of variables
  dist_st <- masked(extrap_layers(envs_f))
  ex      <- max(dist_st); names(ex) <- "extrapolation"
  extrap[[key]] <- ex
  writeRaster(ex, file.path(ext_dist_dir, sprintf("extrapolation_distance_%s_%s.tif", s, per)), overwrite = TRUE)
  
  n_out  <- sum(dist_st > 0); names(n_out) <- "n_vars_outside"
  cnt    <- terra::ifel(n_out == 0, NA, terra::ifel(n_out >= 3, 3, n_out))
  names(cnt) <- "n_vars_outside"
  cls    <- dominant_class(dist_st, mixed_ratio, 0)
  ext_cnt[[key]] <- with_cols(cnt, count_cols)
  ext_dom[[key]] <- with_cols(cls, ext_cls_cols)
  writeRaster(ext_cnt[[key]], file.path(ext_n_dir, sprintf("extrapolation_nvars_%s_%s.tif", s, per)), overwrite = TRUE, datatype = "INT1U")
  writeRaster(ext_dom[[key]], file.path(ext_dom_dir, sprintf("extrapolation_dominant_%s_%s.tif", s, per)), overwrite = TRUE, datatype = "INT1U")
  
  n_ext <- cell_sum(!is.na(cls))
  share <- function(r, val) if (n_ext > 0) round(100 * cell_sum(r == val) / n_ext, 1) else NA_real_
  ext_summ[[key]] <- data.frame(
    scenario = scenarios[[s]], ssp = s, period = per,
    mean_extrapolation = round(wmean(ex), 4),
    max_extrapolation = round(terra::global(ex, "max", na.rm = TRUE)[1, 1], 4),
    pct_cells_extrapolating = pct(ex > 0), stringsAsFactors = FALSE)
  cnt_summ[[key]] <- data.frame(
    scenario = scenarios[[s]], ssp = s, period = per,
    pct_cells_extrapolating = pct(ex > 0),
    pct_of_extrapolating_1_variable = share(n_out, 1),
    pct_of_extrapolating_2_variables = share(n_out, 2),
    pct_of_extrapolating_3plus_variables = if (n_ext > 0) round(100 * cell_sum(n_out >= 3) / n_ext, 1) else NA_real_,
    pct_of_extrapolating_mixed_class = share(cls, length(model_vars) + 1), stringsAsFactors = FALSE)
  var_summ[[key]] <- do.call(rbind, lapply(seq_along(model_vars), function(i) {
    d <- terra::values(dist_st[[i]], mat = FALSE); d <- d[is.finite(d) & d > 0]
    data.frame(scenario = scenarios[[s]], ssp = s, period = per, variable = model_vars[i],
               pct_cells_outside_range = pct(dist_st[[i]] > 0),
               mean_distance_where_outside = if (length(d)) round(mean(d), 4) else NA_real_,
               pct_of_extrapolating_cells_dominant = share(cls, i), stringsAsFactors = FALSE)
  }))
  
  # -- Change attribution: one climate variable at a time switched to its future value
  note_a <- "skipped"
  if (run_attribution && length(clim_vars) > 0) {
    eff_list <- lapply(clim_vars, function(v) {
      f_eff <- file.path(attr_dir, sprintf("effect_%s_%s_%s.tif", s, per, v))
      if (file.exists(f_eff) && !overwrite) {
        r <- rast(f_eff)
        if (isTRUE(compareGeom(envs_hist, r, stopOnError = FALSE))) return(r)
      }
      sw <- envs_hist; sw[[v]] <- envs_f[[v]]
      r <- masked(predict_suit(sw)) - pred_base
      names(r) <- v
      writeRaster(r, f_eff, overwrite = TRUE)
      r
    })
    eff_st <- rast(eff_list); names(eff_st) <- clim_vars
    eff[[key]] <- eff_st
    acls <- dominant_class(abs(eff_st), mixed_ratio, attr_min)
    att_dom[[key]] <- with_cols(acls, att_cls_cols)
    writeRaster(att_dom[[key]], file.path(att_dom_dir, sprintf("change_attribution_dominant_%s_%s.tif", s, per)), overwrite = TRUE, datatype = "INT1U")
    n_att <- cell_sum(!is.na(acls))
    att_summ[[key]] <- do.call(rbind, lapply(c(seq_along(clim_vars), length(clim_vars) + 1), function(i) {
      is_mixed <- i > length(clim_vars)
      data.frame(scenario = scenarios[[s]], ssp = s, period = per,
                 variable = if (is_mixed) "mixed" else clim_vars[i],
                 mean_effect = if (is_mixed) NA_real_ else round(wmean(eff_st[[i]]), 4),
                 mean_abs_effect = if (is_mixed) NA_real_ else round(wmean(abs(eff_st[[i]])), 4),
                 pct_of_cells_dominant = if (n_att > 0) round(100 * cell_sum(acls == i) / n_att, 1) else NA_real_,
                 stringsAsFactors = FALSE)
    }))
    note_a <- "done"
  }
  message(sprintf("[%s] %d of %d: %s %s, suitability %s, attribution %s (%.1f min elapsed)", format(Sys.time(), "%H:%M"), k, n_total,
                  scenarios[[s]], per, g$note, note_a, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
bind <- function(l) { x <- do.call(rbind, l); rownames(x) <- NULL; x }
ext_tab <- bind(ext_summ); var_tab <- bind(var_summ); cnt_tab <- bind(cnt_summ)
write.csv(ext_tab, file.path(tab_dir, "extrapolation_summary.csv"), row.names = FALSE)
write.csv(var_tab, file.path(tab_dir, "extrapolation_by_variable.csv"), row.names = FALSE)
write.csv(cnt_tab, file.path(tab_dir, "extrapolation_variable_count_summary.csv"), row.names = FALSE)
message("\n=== Extrapolation summary ===");              print(ext_tab, row.names = FALSE)
message("\n=== Variables outside range, per projection ==="); print(cnt_tab, row.names = FALSE)
message("\n=== Extrapolation by variable ===");           print(var_tab, row.names = FALSE)
if (length(att_summ) > 0) {
  att_tab <- bind(att_summ)
  write.csv(att_tab, file.path(tab_dir, "change_attribution_summary.csv"), row.names = FALSE)
  message("\n=== One-at-a-time change attribution ==="); print(att_tab, row.names = FALSE)
}

# ---- Uncertainty across the 9 projections ---------------------------------------
n_proj     <- length(preds)
pred_stack <- rast(preds); names(pred_stack) <- names(preds)
chg_stack  <- pred_stack - pred_base                      # each projection minus the baseline

# Cells with a baseline value but no value in a projection. These come out white on the spread and agreement maps
# (the maps do not fill them in), so this shows how many there are and whether it is the same cells in every projection.
na_chk <- data.frame(projection = names(pred_stack),
                     cells_missing_vs_baseline = vapply(seq_len(terra::nlyr(pred_stack)), function(i)
                       cell_sum(is.na(pred_stack[[i]]) & !is.na(pred_base)), numeric(1)),
                     stringsAsFactors = FALSE)
na_chk$cells_with_baseline_value <- cell_sum(!is.na(pred_base))
write.csv(na_chk, file.path(tab_dir, "suitability_missing_cells.csv"), row.names = FALSE)
message("\n=== Cells with a baseline value but none in the projection ===")
print(na_chk, row.names = FALSE)

unc_all <- terra::stdev(pred_stack, pop = FALSE, na.rm = TRUE); names(unc_all) <- "sd_all9"
n_down  <- sum(chg_stack < 0)
n_up    <- sum(chg_stack > 0)
n_agree <- max(n_down, n_up); names(n_agree) <- "n_agreeing_of_9"       # how many of the 9 share the more common direction
agree   <- n_agree / n_proj; names(agree) <- "direction_agreement"       # the same as a fraction
mean_change_all <- mean(chg_stack, na.rm = TRUE); names(mean_change_all) <- "mean_change_all9"
any_extrap <- terra::ifel(max(rast(extrap)) > 0, 1, NA)    # at least one projection extrapolates

writeRaster(unc_all, file.path(across_dir, "uncertainty_sd_all9.tif"), overwrite = TRUE)
writeRaster(agree, file.path(across_dir, "uncertainty_direction_agreement.tif"), overwrite = TRUE)
writeRaster(n_agree, file.path(across_dir, "uncertainty_direction_agreement_n_of_9.tif"), overwrite = TRUE, datatype = "INT1U")
writeRaster(mean_change_all, file.path(across_dir, "uncertainty_mean_change_all9.tif"), overwrite = TRUE)

unc_tab <- data.frame(
  mean_sd = round(wmean(unc_all), 4),
  pct_cells_sd_above_0.1 = pct(unc_all > 0.1),
  pct_cells_all9_agree_on_direction = pct(agree > 1 - 1e-9),
  pct_cells_8of9_agree_on_direction = pct(agree > 8 / 9 - 1e-9),
  pct_cells_4_or_fewer_agree_on_direction = pct(n_agree <= 4),
  pct_cells_any_projection_extrapolating = pct(max(rast(extrap)) > 0),
  stringsAsFactors = FALSE)
write.csv(unc_tab, file.path(tab_dir, "uncertainty_summary.csv"), row.names = FALSE)
message("\n=== Uncertainty across the 9 projections ===")
print(unc_tab, row.names = FALSE)

# ---- Uncertainty across the 3 scenarios, one period at a time -------------------------
# The SD across all 9 mixes scenario differences with the change over time. Within ONE period the three projections
# differ only by scenario, so this isolates scenario uncertainty. With 3 values the SD is a rough descriptive spread, so
# the range is the default (spread_metric); both are saved.
if (!scenario_period %in% periods) stop("scenario_period must be one of: ", paste(periods, collapse = ", "))
if (!spread_metric %in% c("range", "sd")) stop("spread_metric must be \"range\" or \"sd\".")
spread_by_per <- list(); sc_agree <- list(); sc_extrap <- list(); sc_tab <- list()
for (per in periods) {
  keys_p <- paste(names(scenarios), per, sep = "_")
  st_p   <- pred_stack[[keys_p]]
  rg_p   <- max(st_p) - min(st_p);                              names(rg_p) <- "range_3scenarios"
  sd_p   <- terra::stdev(st_p, pop = FALSE, na.rm = TRUE);      names(sd_p) <- "sd_3scenarios"
  ch_p   <- chg_stack[[keys_p]]
  na_p   <- max(sum(ch_p < 0), sum(ch_p > 0));                  names(na_p) <- "n_agreeing_of_3"
  ex_p   <- max(rast(extrap[keys_p]))
  writeRaster(rg_p, file.path(across_dir, sprintf("scenarios_%s_range.tif", per)), overwrite = TRUE)
  writeRaster(sd_p, file.path(across_dir, sprintf("scenarios_%s_sd.tif", per)), overwrite = TRUE)
  writeRaster(na_p, file.path(across_dir, sprintf("scenarios_%s_direction_agreement_n_of_3.tif", per)), overwrite = TRUE, datatype = "INT1U")
  spread_by_per[[per]] <- if (spread_metric == "sd") sd_p else rg_p
  sc_agree[[per]]  <- na_p
  sc_extrap[[per]] <- terra::ifel(ex_p > 0, 1, NA)
  sc_tab[[per]] <- data.frame(
    period = per, mean_range = round(wmean(rg_p), 4), mean_sd = round(wmean(sd_p), 4),
    pct_cells_range_above_0.1 = pct(rg_p > 0.1),
    pct_cells_3_of_3_agree_on_direction = pct(na_p >= 3), pct_cells_2_of_3_agree_on_direction = pct(na_p == 2),
    pct_cells_1_or_fewer_agree_on_direction = pct(na_p <= 1),
    pct_cells_any_scenario_extrapolating = pct(ex_p > 0), stringsAsFactors = FALSE)
}
sc_tab <- do.call(rbind, sc_tab)
write.csv(sc_tab, file.path(tab_dir, "uncertainty_summary_across_scenarios.csv"), row.names = FALSE)
message("\n=== Spread across the 3 scenarios, by period ===")
print(sc_tab, row.names = FALSE)
spread_label <- if (spread_metric == "sd") "SD" else "Range"

# ---- Figures ----------------------------------------------------------------
# SD colour scale: 0 to the 98th percentile, so a few extreme cells don't dominate.
all_sd <- terra::values(unc_all, mat = FALSE); all_sd <- all_sd[is.finite(all_sd)]
sd_max <- as.numeric(stats::quantile(all_sd, 0.98))
if (!is.finite(sd_max) || sd_max == 0) sd_max <- max(all_sd)
message(sprintf("\nSD colour scale: 0 to %.2f (98th percentile)", sd_max))

# Extrapolation colour scale shared by the nine maps: 0 to the 98th percentile of the cells that extrapolate at all.
all_ex <- unlist(lapply(extrap, function(r) { v <- terra::values(r, mat = FALSE); v[is.finite(v) & v > 0] }))
ex_max <- if (length(all_ex) > 0) as.numeric(stats::quantile(all_ex, 0.98)) else 1
if (!is.finite(ex_max) || ex_max == 0) ex_max <- 1
message(sprintf("Shared extrapolation colour scale: 0 to %.2f of the training range", ex_max))

coast <- NULL
if (file.exists(coast_file)) coast <- tryCatch(project(vect(coast_file), crs(pred_base)), error = function(e) NULL)
sd_cols   <- hcl.colors(100, "YlOrRd", rev = TRUE)
agr_levels <- 4:9                                                          # 4 stands for "4 or fewer"
agr_cols  <- c("#d0d0d0", hcl.colors(7, "Blues 3", rev = TRUE)[3:7])        # light grey, then light to dark blue for 5 to 9
agr_lab   <- c("4 or fewer", "5 of 9", "6 of 9", "7 of 9", "8 of 9", "9 of 9")
agr_cls   <- terra::ifel(n_agree <= 4, 4, n_agree); names(agr_cls) <- "agreement_class"
terra::coltab(agr_cls) <- data.frame(value = agr_levels, col = agr_cols)
ex_cols   <- hcl.colors(100, "Reds 3", rev = TRUE)
add_coast <- function() if (!is.null(coast)) lines(coast, lwd = 0.3, col = "grey25")
# Panel letters: bold, centred under each map, just below the southern edge of the land (keeps the gap between rows small).
land_xy <- as.vector(terra::ext(terra::trim(pred_base)))   # xmin, xmax, ymin, ymax of the land
map_letter <- function(i, cex = letter_cex) text((land_xy[1] + land_xy[2]) / 2, land_xy[3] - 0.01 * (land_xy[4] - land_xy[3]),
                                                 sprintf(letter_fmt, panel_letters[i]), adj = c(0.5, 1), font = 2, cex = cex, xpd = NA)
add_outside <- function(om) {
  if (show_outside_range && isTRUE(cell_sum(om) > 0)) plot(om, add = TRUE, col = "#4d4d4d80", legend = FALSE)
}

# Figure 1: SD across the 9 projections (left) and how many of the 9 agree on the direction of change (right).
draw_across <- function() {
  layout(matrix(c(1, 2, 0, 3), nrow = 2, byrow = TRUE), heights = c(1, 0.17))
  par(oma = c(2, 0, 3, 0))
  plot(clamp(unc_all, 0, sd_max, values = TRUE), col = sd_cols, range = c(0, sd_max), axes = FALSE, box = FALSE,
       main = "SD of suitability across all 9 projections", cex.main = 0.95, mar = c(1.4, 0.5, 2.2, 5),
       plg = list(title = "SD", title.cex = 0.8, cex = 0.8))
  add_outside(any_extrap); add_coast(); map_letter(1)
  plot(agr_cls, legend = FALSE, axes = FALSE, box = FALSE,
       main = "Agreement on direction of change", cex.main = 0.95, mar = c(1.4, 0.5, 2.2, 5))
  add_outside(any_extrap); add_coast(); map_letter(2)
  par(mar = c(0, 0, 0, 0)); plot.new()
  legend("top", legend = agr_lab, fill = agr_cols, border = "grey40", horiz = TRUE, bty = "n", cex = 0.8, x.intersp = 0.6,
         title = "Number of the 9 projections that agree on whether suitability falls or rises", title.cex = 0.8)
  text(0.5, 0.06, "Light grey (4 or fewer): no direction is shared by a majority, because some projections show no change in that cell",
       cex = 0.62, col = "grey30")
  if (show_outside_range) mtext("Dark grey shading = at least one of the 9 projections has a variable outside the model's training range",
                                outer = TRUE, side = 1, line = 0.5, cex = 0.6, col = "grey30")
  mtext(sprintf("Prediction uncertainty across the 9 %s projections", toupper(gcm)),
        outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.1)
}

# 3 x 3 grids (rows = scenario, columns = period) with a bottom slot for a colour bar or a legend.
lay_e <- rbind(c(1, 1, 2, 2, 3, 3),
               c(4, 4, 5, 5, 6, 6),
               c(7, 7, 8, 8, 9, 9),
               c(0, 10, 10, 10, 10, 0))
panel_title <- function(s, per) paste0(scenarios[[s]], ",  ", per)

# Figure 2: extrapolation distance, one shared colour bar.
draw_extrap <- function() {
  layout(lay_e, heights = c(1, 1, 1, 0.45))
  par(oma = c(0, 0, 3, 0))
  pn <- 0
  for (s in names(scenarios)) for (per in periods) {
    key <- paste(s, per, sep = "_")
    plot(clamp(extrap[[key]], 0, ex_max, values = TRUE), col = ex_cols, range = c(0, ex_max), legend = FALSE,
         axes = FALSE, box = FALSE, main = panel_title(s, per), cex.main = 0.85, mar = c(1.2, 0.3, 1.8, 0.3))
    add_coast()
    pn <- pn + 1; map_letter(pn)
  }
  vals <- seq(0, ex_max, length.out = 200)
  par(mar = c(3.6, 1, 0.4, 1))
  image(vals, 1, matrix(vals, ncol = 1), col = ex_cols, yaxt = "n", xlab = "", cex.axis = 0.9)
  mtext("Extrapolation distance (fraction of the training range; 0 = within range for every variable)", side = 1, line = 2.3, cex = 0.8)
  mtext(sprintf("Extrapolation uncertainty of each %s projection", toupper(gcm)),
        outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.1)
}

# Figures 3-5: categorical 3 x 3 grids (colour table stored in each raster) with a shared legend underneath.
draw_class_grid <- function(rlist, labels, cols, legend_title, main_title, ncol_legend) {
  layout(lay_e, heights = c(1, 1, 1, 0.45))
  par(oma = c(0, 0, 3, 0))
  pn <- 0
  for (s in names(scenarios)) for (per in periods) {
    key <- paste(s, per, sep = "_")
    r <- rlist[[key]]
    if (isTRUE(cell_sum(!is.na(r)) > 0)) {
      plot(r, legend = FALSE, axes = FALSE, box = FALSE, main = panel_title(s, per), cex.main = 0.85, mar = c(1.2, 0.3, 1.8, 0.3))
    } else {
      # empty panel: set up the same map coordinates so the "none" label and the letter sit where they would on a real map
      par(mar = c(1.2, 0.3, 1.8, 0.3)); plot.new(); plot.window(xlim = land_xy[1:2], ylim = land_xy[3:4], asp = 1)
      title(main = panel_title(s, per), cex.main = 0.85)
      text(mean(land_xy[1:2]), mean(land_xy[3:4]), "none", col = "grey50", cex = 0.9)
    }
    add_coast()
    pn <- pn + 1; map_letter(pn)
  }
  par(mar = c(0, 0, 0, 0)); plot.new()
  legend("center", legend = labels, fill = cols, ncol = ncol_legend, bty = "n", cex = 0.85, title = legend_title)
  mtext(main_title, outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.1)
}

# Spread colour scale shared by the maps in figures 1b and 1c: 0 to the 98th percentile over the 3 periods.
all_sp <- unlist(lapply(spread_by_per, function(r) { v <- terra::values(r, mat = FALSE); v[is.finite(v)] }))
sp_max <- as.numeric(stats::quantile(all_sp, 0.98))
if (!is.finite(sp_max) || sp_max == 0) sp_max <- max(all_sp)
message(sprintf("Spread colour scale (figures 1b, 1c): 0 to %.2f (98th percentile over the 3 periods)", sp_max))
agr3_cols <- c("#d0d0d0", "#6baed6", "#08519c")
agr3_lab  <- c("1 or fewer", "2 of 3", "3 of 3")

# Figure 1b: spread across the 3 scenarios (left) and how many of the 3 agree on the direction of change (right).
draw_scenarios <- function() {
  per <- scenario_period
  agr3 <- terra::ifel(sc_agree[[per]] <= 1, 1, sc_agree[[per]]); names(agr3) <- "agreement_class_3"
  terra::coltab(agr3) <- data.frame(value = 1:3, col = agr3_cols)
  layout(matrix(c(1, 2, 0, 3), nrow = 2, byrow = TRUE), heights = c(1, 0.17))
  par(oma = c(2, 0, 3, 0))
  plot(clamp(spread_by_per[[per]], 0, sp_max, values = TRUE), col = sd_cols, range = c(0, sp_max), axes = FALSE, box = FALSE,
       main = sprintf("%s of suitability across the 3 scenarios, %s", spread_label, per), cex.main = 0.95, mar = c(1.4, 0.5, 2.2, 5),
       plg = list(title = spread_label, title.cex = 0.8, cex = 0.8))
  add_outside(sc_extrap[[per]]); add_coast(); map_letter(1)
  plot(agr3, legend = FALSE, axes = FALSE, box = FALSE,
       main = sprintf("Agreement on direction of change, %s", per), cex.main = 0.95, mar = c(1.4, 0.5, 2.2, 5))
  add_outside(sc_extrap[[per]]); add_coast(); map_letter(2)
  par(mar = c(0, 0, 0, 0)); plot.new()
  legend("top", legend = agr3_lab, fill = agr3_cols, border = "grey40", horiz = TRUE, bty = "n", cex = 0.8, x.intersp = 0.6,
         title = "Number of the 3 scenarios that agree on whether suitability falls or rises", title.cex = 0.8)
  text(0.5, 0.06, "Light grey (1 or fewer): no direction is shared by a majority, because some scenarios show no change in that cell",
       cex = 0.62, col = "grey30")
  if (show_outside_range) mtext(sprintf("Dark grey shading = at least one of the 3 %s projections has a variable outside the model's training range", per),
                                outer = TRUE, side = 1, line = 0.5, cex = 0.6, col = "grey30")
  mtext(sprintf("Scenario uncertainty in %s (%s)", per, toupper(gcm)),
        outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.1)
}

# Figure 1c: the spread map for each period, one colour scale.
draw_scenarios_by_period <- function() {
  n_per <- length(periods)
  # Only the last panel needs room on the right for the colour bar, so it gets a wider slot (widths) and a wider right
  # margin (mar); the other panels have narrow side margins, which brings the maps closer together.
  # To tighten or loosen the gap further, change the 1.17 below (and the figure width in the png() call to match).
  layout(matrix(seq_len(n_per), nrow = 1), widths = c(rep(1, n_per - 1), 1.17))
  par(oma = c(0.5, 0, 3.2, 0))
  for (i in seq_len(n_per)) {
    per <- periods[i]
    plot(clamp(spread_by_per[[per]], 0, sp_max, values = TRUE), col = sd_cols, range = c(0, sp_max), axes = FALSE, box = FALSE,
         legend = (i == n_per), main = per, cex.main = 0.95 * 1.2,                          # period labels 20% bigger
         mar = if (i == n_per) c(1.4, 0.5, 2.2, 5) else c(1.4, 0.5, 2.2, 0.5),
         plg = list(title = spread_label, title.cex = 0.8, cex = 0.8))
    add_coast(); map_letter(i, cex = letter_cex * 1.2)                                      # panel letters 20% bigger
  }
  mtext(sprintf("%s of suitability across the 3 scenarios, by period (%s)", spread_label, toupper(gcm)),
        outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.1 * 1.1)                      # title 10% bigger
}

png(file.path(fig_dir, "uncertainty_across_projections.png"), width = 3000, height = 1700, res = 300)
draw_across(); invisible(dev.off())
png(file.path(fig_dir, "uncertainty_across_scenarios.png"), width = 3000, height = 1700, res = 300)
draw_scenarios(); invisible(dev.off())
if (spread_by_period_figure) {
  png(file.path(fig_dir, "uncertainty_across_scenarios_by_period.png"), width = 4400, height = 1500, res = 300)
  draw_scenarios_by_period(); invisible(dev.off())
}
png(file.path(fig_dir, "uncertainty_extrapolation_grid.png"), width = 3000, height = 3400, res = 300)
draw_extrap(); invisible(dev.off())

png(file.path(fig_dir, "extrapolation_dominant_variable_grid.png"), width = 3000, height = 3500, res = 300)
draw_class_grid(ext_dom, c(model_vars, "mixed (runner-up close)"), ext_cls_cols,
                "Variable furthest outside its training range (blank = within range)",
                sprintf("Which variable drives the extrapolation, %s projections", toupper(gcm)), 3)
invisible(dev.off())

png(file.path(fig_dir, "extrapolation_variable_count_grid.png"), width = 3000, height = 3400, res = 300)
draw_class_grid(ext_cnt, c("1 variable", "2 variables", "3 or more"), count_cols,
                "Number of variables outside their training range (blank = none)",
                sprintf("How many variables are outside the training range, %s projections", toupper(gcm)), 3)
invisible(dev.off())

if (length(att_dom) > 0) {
  png(file.path(fig_dir, "change_attribution_grid.png"), width = 3000, height = 3500, res = 300)
  draw_class_grid(att_dom, c(clim_vars, "mixed (runner-up close)"), att_cls_cols,
                  sprintf("Variable with the largest one-at-a-time effect on suitability (blank = effect below %.2f)", attr_min),
                  sprintf("Which variable drives the change in suitability, %s projections", toupper(gcm)), 3)
  invisible(dev.off())
}

# ---- README describing the folder layout ----------------------------------------
writeLines(c(
  "Uncertainty outputs (written by 2b-prediction-uncertainty.R)",
  "",
  "Figures/                      the PNG figures",
  "Tables/                       summary CSVs (across-projection, across-scenario, extrapolation, variable counts, change attribution)",
  "Rasters/",
  "  Suitability/                suitability rasters predicted by this script (2a's are reused when present)",
  "  Across projections/         SD of suitability, direction agreement (fraction and count of 9), mean change across the 9 projections;",
  "                              and, for each period, the range, SD and direction agreement (count of 3) across the 3 scenarios",
  "  Extrapolation/",
  "    Distance/                 extrapolation distance per projection (fraction of the training range)",
  "    Dominant variable/        variable furthest outside its range; classes 1..K = model variables, K+1 = mixed",
  "    Variable count/           number of variables outside range (1, 2, 3 = 3 or more)",
  "  Change attribution/",
  "    Dominant variable/        variable with the largest one-at-a-time effect on suitability; K+1 = mixed",
  "    One-at-a-time effects/    effect of switching each climate variable alone (reused on re-runs)",
  "",
  paste0("Model variables, in class order: ", paste(model_vars, collapse = ", ")),
  paste0("Climate variables used for change attribution: ", paste(clim_vars, collapse = ", "))
), file.path(out_dir, "README.txt"))

message("\nSaved figures in ", fig_dir)
message("Saved tables in ", tab_dir)
message("Saved rasters in ", ras_dir)