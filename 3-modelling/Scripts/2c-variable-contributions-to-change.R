# 2c-variable-contributions-to-change.R
# How much each CHELSA climate variable contributes to the projected CHANGE in suitability itself (signed: which
# variables push suitability up, which push it down), as opposed to 2b's "dominant variable" map, which only shows the
# single largest contributor and drops its sign. (2d, the next script, is about contributions to the UNCERTAINTY.)
# Figures:
#   1. contribution_maps_<ssp>_<period>.png      one figure per scenario, for ONE period (map_period): a map of the effect
#                                                of each climate variable, plus a last panel with the suitability change, all on
#                                                one diverging colour scale (purple = suitability falls, green = rises)
#   2. contribution_bars.png                     area-weighted mean effect of each variable, per scenario and period, next to
#                                                the total change
#   3. contribution_additivity_scatter.png       total change against the sum of the one-at-a-time effects, cell by cell, for
#                                                map_period: how far the variables' effects are from adding up
#   4. contribution_dominant_variable_<period>.png             3 maps (one per scenario), for map_period: the variable
#                                                with the largest effect on the change in suitability between the baseline
#                                                and that period in each cell
#      contribution_dominant_variable_with_bars_<period>.png   the same 3 maps with horizontal stacked bars below them: the
#                                                % of Iceland's land where each variable is dominant (one bar per scenario)
#      contribution_dominant_variable_bars_<period>.png        the bars on their own
# The panels of figures 1 (each variable's map, then the total change), 2 (one per scenario) and 4 (the maps, and in the
# version with bars the bar panel as well) are lettered (a), (b), ..., centred under each panel; panel_letters and
# letter_fmt in the settings change the style. Figure 3 and the bars-only figure have no letters.
#
# Put this file in: ISP-Crowberry-SDM/3-modelling/Scripts/   (run 1a and 2b first, with run_attribution = TRUE in 2b)
# Reads:  3-modelling/Outputs/Baseline model/baseline_maxent_model.rds                       (the model from 1a)
#         3-modelling/Outputs/Uncertainty/Rasters/Change attribution/One-at-a-time effects/   (effect_<ssp>_<period>_<var>.tif, 2b)
#         3-modelling/Outputs/Projection/Scenario grid/suitability_*.tif                      (baseline and 9 future suitability
#              rasters from 2a), or 3-modelling/Outputs/Uncertainty/Rasters/Suitability/ (written by 2b if 2a was not run)
#         3-modelling/Outputs/Uncertainty/Rasters/Extrapolation/Distance/                    (optional, for the grey shading)
#         1-download-data/Downloads/LMI-IS50V/is50v_land_polygon.gpkg                         (optional coastline outline)
# Writes: 3-modelling/Outputs/Change attribution/
#           Figures/    the figures above
#           Tables/     contribution_to_change_summary.csv   one row per scenario, period and variable (plus two summary rows)
#                       dominant_variable_summary_<period>.csv   share of Iceland's land where each variable is dominant (figure 4)
# Inputs are only read, never changed. Nothing here refits the model or re-predicts: it reuses 2a's and 2b's rasters.
#
# What is plotted:
#   2b predicts with ONE climate variable at a time switched from its 1981-2010 value to its future value for a given
#   scenario and period (every other variable at its 1981-2010 value) and saves effect = that prediction minus the baseline
#   prediction. These effects are signed. The total change is the full future suitability (all variables changed together)
#   minus the baseline suitability.
#   - Maps: the effect of each variable on one colour scale, plus the total change. Where a single variable's panel looks
#     like the total panel, that variable accounts for most of the change there.
#   - Bars: the area-weighted mean effect of each variable. Means can hide opposite signs in different places, so the
#     table also gives the mean absolute effect and the % of cells where the effect is above +change_min or below
#     -change_min. "total change" is the mean total change; "sum of effects" is the sum of the variables' means.
#   - Scatter: if the variables acted independently, every cell would sit on the 1:1 line (total change = sum of the
#     one-at-a-time effects). Distance from the line is the interaction between variables (the model is not additive:
#     cloglog output, and the variables are combined inside the model).
#   - Dominant variable (figure 4): in each cell, the variable whose one-at-a-time effect has the largest ABSOLUTE size, so
#     it is the variable that moves suitability the most, whether up or down (the sign is in figure 1). This is the same
#     rule and the same colours as 2b's change_attribution_grid.png: where the runner-up is at least mixed_ratio times as
#     large as the largest effect the cell is classed "mixed", and cells where the largest effect is below dominant_min are
#     left blank. It uses the same period (map_period) as figures 1 and 3. Keep mixed_ratio and dominant_min equal to
#     2b's mixed_ratio and attr_min if you want the maps to match 2b's. The table gives the area-weighted % of land in
#     each class, and the bars of figure 4 show the same numbers (blank cells are the white part of each bar).
#   The effects come from predictions with maxnet's clamping on, so where a variable is outside the training range its
#   effect reflects the clamped value, not the raw future value (grey shading marks those cells).
#   Correlated variables (several temperature variables, say) move together, so one-at-a-time effects can over- or
#   under-credit a variable within a correlated group, and the "dominant" variable may only be the largest in a group.
#
# Limits: one climate model (GFDL-ESM4); fitting uncertainty is not included; suitability is a relative index, not
# abundance; cells are not equal area (WGS84 degrees), so the means are area-weighted.
#
# NOT TESTED on your data. Check the console messages and the figures before using the output.

# install.packages(c("terra", "maxnet"))
library(terra)
library(maxnet)

# ---- Settings ---------------------------------------------------------------
gcm        <- "gfdl-esm4"
scenarios  <- c(ssp126 = "SSP1-2.6", ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")   # same order as 2b
periods    <- c("2011-2040", "2041-2070", "2071-2100")
static_vars <- c("elevation", "slope", "northness", "eastness", "tpi", "roughness")   # terrain: no climate effect
map_period  <- "2071-2100"   # figures 1, 3 and 4: the period shown
show_outside_range <- FALSE  # grey shading where the projection has a variable outside the training range (needs 2b's distance rasters)
change_min  <- 0.01          # an effect above +change_min or below -change_min counts as a rise or a fall in the table
mixed_ratio <- 0.5           # figure 4: runner-up at least this fraction of the largest effect -> cell is "mixed" (2b's value)
dominant_min <- 0.01         # figure 4: cells whose largest one-at-a-time effect is below this are left blank (2b's attr_min)
scatter_n   <- 40000         # cells sampled for the scatter (figure 3)
panel_letters <- letters     # letter centred under each panel of figures 1, 2 and 4: a, b, c, ... (LETTERS for A, B, C, ...)
letter_fmt    <- "(%s)"      # "(%s)" gives (a); "%s" gives a
letter_cex    <- 1.0         # size of the panel letters (the panel titles are larger)
bar_legend_cex <- 0.72 * 1.4 * 1.4 * 1.2   # figure 2: size of the legend text (0.72 was the original size; 40% bigger twice over, then another 20%)
bar_ylab_cex   <- 1.2 * 1.25 # figure 2: size of the y axis label "Area-weighted mean change..." (was 1.2; now 25% bigger)
bar_axis_cex   <- 1.3        # figure 2: size of the period labels (2011-2040, ...) and the 0.00, 0.05, ... tick labels (30% bigger than the default of 1)
bar_title_cex  <- 1.1 * 1.2  # figure 2: size of the title across the top (was 1.1; now 20% bigger)
bar_letter_cex <- 1.1 * letter_cex   # figure 2: size of its panel letters (a)-(c) (10% bigger than letter_cex; figures 1 and 4 are unchanged)
dom_letter_cex <- 1.3 * 1.15 * letter_cex   # figure 4: size of its panel letters (a)-(d) (30% bigger than in figures 1 and 2, then another 15%)
dom_legend_cex <- 1.76        # figure 4: size of the legend labels (shrinks automatically if the legend would not fit the width)
bar_label_min <- 4           # figure 4 bars: segments narrower than this % of the land are left unlabelled
dom_bar_label_cex <- 1.2 * 1.2   # figure 4 bars: size of the % labels inside the segments (20% bigger than 1.2)
dom_bar_axis_cex  <- 1.1 * 1.2   # figure 4 bars: size of the 0%, 20%, ... labels under the bars (20% bigger than 1.1)
dom_bar_ssp_cex   <- 1.4 * 1.3   # figure 4 bars: size of the scenario names (SSP1-2.6, ...) left of the bars (30% bigger than 1.4)
dom_bar_xlab_cex  <- 1.3 * 1.2 * 1.15   # figure 4 bars: size of "Share of land area" (20% bigger than 1.3, then another 15%; the "(d)" uses dom_letter_cex)
dom_map_title_cex <- 1.3 * 1.3   # figure 4: size of the scenario names (SSP1-2.6, ...) above the maps (30% bigger than 1.3)
dom_title_cex     <- 1.1 * 1.1  # figure 4 (maps, and maps with bars): size of the title across the top (10% bigger than 1.1)
dom_row_px   <- c(maps = 1000, bars = 586, legend = 140)   # figure 4 with bars: height in pixels of the map row, the bar row and the legend (one row of entries, no title)
dom_fig_w    <- NULL         # figure 4 with bars: width in pixels. NULL = just wide enough for the three maps at their row height (no empty space between them); a number overrides it
dom_panel_pad_px <- 30       # figure 4 with bars: extra space in pixels added to each map's width, which sets the gap between the maps (used when dom_fig_w is NULL)
dom_map_mar  <- c(1.4, 0.5, 2.2, 0.5)   # figure 4: margins around each map, in lines: bottom (panel letter), left, top (panel title), right
dom_legend_order <- c("ngd10", "swe", "bio02", "bio15", "bio18")   # figure 4: order of the legend entries and of the bar segments (then mixed, then blank); any variable not listed goes last.
dom_bars_only_px <- c(bars = 650, legend = 160)   # figure 4, bars-only version: height in pixels of the bar row and the legend
map_row_px    <- NULL        # figure 1: height of one row of maps in pixels, counting each map's title and letter margins. NULL = just tall enough for the maps (they are limited by the panel width, so extra height would only be white space between the rows); a number overrides it
colbar_px     <- 100         # figure 1: thickness of the colour bar itself in pixels (it was about 200)
map_title     <- TRUE        # figure 1: FALSE leaves out the title across the top of the figure (saves about 100 px of height, e.g. if the caption says the same)

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
model_file <- p("3-modelling", "Outputs", "Baseline model", "baseline_maxent_model.rds")
coast_file <- p("1-download-data", "Downloads", "LMI-IS50V", "is50v_land_polygon.gpkg")
unc_dir    <- p("3-modelling", "Outputs", "Uncertainty")                 # 2b's output folder (read only)
attr_dir   <- file.path(unc_dir, "Rasters", "Change attribution", "One-at-a-time effects")
ext_dist_dir <- file.path(unc_dir, "Rasters", "Extrapolation", "Distance")
suit_dirs  <- c(p("3-modelling", "Outputs", "Projection", "Scenario grid"), file.path(unc_dir, "Rasters", "Suitability"))
out_dir    <- p("3-modelling", "Outputs", "Change attribution")
fig_dir    <- file.path(out_dir, "Figures")
tab_dir    <- file.path(out_dir, "Tables")
for (d in c(fig_dir, tab_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(model_file)) stop("Missing input: ", model_file, "\nRun 1a-fit-baseline-maxent.R first.")
mod <- readRDS(model_file)
model_vars <- names(mod$varmin)
if (is.null(model_vars)) stop("The saved model has no varmin/varmax, so its variables can't be identified. Refit with 1a.")
clim_vars <- setdiff(model_vars, static_vars)
if (length(clim_vars) == 0) stop("The model has no climate variables.")
message("Climate variables (", length(clim_vars), "): ", paste(clim_vars, collapse = ", "))
if (!map_period %in% periods) stop("map_period must be one of: ", paste(periods, collapse = ", "))

# ---- Check that the rasters exist ------------------------------------------------
find_suit <- function(fname) {
  for (d in suit_dirs) { f <- file.path(d, fname); if (file.exists(f)) return(f) }
  NA_character_
}
eff_file <- function(s, per, v) file.path(attr_dir, sprintf("effect_%s_%s_%s.tif", s, per, v))
suit_names <- c("suitability_baseline_1981-2010.tif",
                unlist(lapply(names(scenarios), function(s) sprintf("suitability_%s_%s_%s.tif", gcm, s, periods))))
suit_found <- vapply(suit_names, find_suit, character(1))
if (any(is.na(suit_found))) {
  stop(sum(is.na(suit_found)), " suitability raster(s) not found, for example:\n  ",
       paste(utils::head(suit_names[is.na(suit_found)], 6), collapse = "\n  "),
       "\nRun 2a-scenario-change-grid.R (or 2b) first. Looked in:\n  ", paste(suit_dirs, collapse = "\n  "))
}
eff_needed <- unlist(lapply(names(scenarios), function(s) unlist(lapply(periods, function(per)
  vapply(clim_vars, function(v) eff_file(s, per, v), character(1))))))
if (any(!file.exists(eff_needed))) {
  stop(sum(!file.exists(eff_needed)), " effect raster(s) from 2b are missing, for example:\n  ",
       paste(utils::head(eff_needed[!file.exists(eff_needed)], 6), collapse = "\n  "),
       "\nRun 2b-prediction-uncertainty.R with run_attribution <- TRUE first.")
}

# ---- Helpers ---------------------------------------------------------------------
wmean <- function(r) {                              # area-weighted mean over land cells
  w <- terra::cellSize(r, unit = "km", mask = TRUE)
  terra::global(r * w, "sum", na.rm = TRUE)[1, 1] / terra::global(w, "sum", na.rm = TRUE)[1, 1]
}
pct <- function(r) round(100 * terra::global(r, "mean", na.rm = TRUE)[1, 1], 1)
cell_sum <- function(r) terra::global(r, "sum", na.rm = TRUE)[1, 1]
samp_vals <- function(r, n = 200000) {              # a regular sample of cell values (keeps memory low)
  v <- terra::spatSample(r, n, method = "regular", na.rm = TRUE, as.raster = FALSE, values = TRUE)
  v <- as.numeric(v[[1]]); v[is.finite(v)]
}
# Classify the dominant layer of a stack of non-negative layers (same as 2b and 2d). 1..K = that layer, K+1 = mixed
# (runner-up is at least `ratio` times the largest), NA where the largest value is not above `min_val`.
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

# Same colour for the same variable as in 2b and 2d (hcl.colors over ALL model variables, in model order).
var_cols  <- setNames(hcl.colors(length(model_vars), "Dark 3"), model_vars)
chg_cols  <- hcl.colors(100, "PRGn")                # purple = suitability falls, green = rises
mixed_col <- "#9e9e9e"                              # figure 4: "mixed" class, grey as in 2b and 2d
dom_cols  <- c(unname(var_cols[clim_vars]), mixed_col)

# ---- Load the baseline, the total change and the one-at-a-time effects ------------------
base <- rast(suit_found[["suitability_baseline_1981-2010.tif"]])
chg <- list(); eff <- list(); sum_eff <- list(); tab <- list()
add_rows <- function(s, per, label, r) data.frame(
  scenario = scenarios[[s]], ssp = s, period = per, variable = label,
  mean_effect = round(wmean(r), 4), mean_abs_effect = round(wmean(abs(r)), 4),
  pct_cells_up = pct(r > change_min), pct_cells_down = pct(r < -change_min), stringsAsFactors = FALSE)
for (s in names(scenarios)) for (per in periods) {
  key <- paste(s, per, sep = "_")
  fut <- rast(suit_found[[sprintf("suitability_%s_%s_%s.tif", gcm, s, per)]])
  chg[[key]] <- fut - base; names(chg[[key]]) <- "total_change"
  st <- rast(lapply(clim_vars, function(v) rast(eff_file(s, per, v)))); names(st) <- clim_vars
  if (!isTRUE(compareGeom(base, st, stopOnError = FALSE))) stop("Effect rasters for ", s, " ", per, " are not on the same grid as the suitability rasters.")
  eff[[key]] <- st
  sum_eff[[key]] <- sum(st); names(sum_eff[[key]]) <- "sum_of_effects"
  tab[[key]] <- rbind(
    do.call(rbind, lapply(seq_along(clim_vars), function(i) add_rows(s, per, clim_vars[i], st[[i]]))),
    add_rows(s, per, "sum of one-at-a-time effects", sum_eff[[key]]),
    add_rows(s, per, "total change", chg[[key]]))
  message(sprintf("[%s] %s %s loaded", format(Sys.time(), "%H:%M"), scenarios[[s]], per))
}
tab <- do.call(rbind, tab); rownames(tab) <- NULL
write.csv(tab, file.path(tab_dir, "contribution_to_change_summary.csv"), row.names = FALSE)
message("\n=== Contribution of each variable to the change in suitability (area-weighted means) ===")
print(tab, row.names = FALSE)

# ---- Plot helpers ------------------------------------------------------------------------
coast <- NULL
if (file.exists(coast_file)) coast <- tryCatch(project(vect(coast_file), crs(base)), error = function(e) NULL)
add_coast <- function() if (!is.null(coast)) lines(coast, lwd = 0.3, col = "grey25")
outside_mask <- function(s, per) {                  # 1 where this projection has any variable outside the training range
  f <- file.path(ext_dist_dir, sprintf("extrapolation_distance_%s_%s.tif", s, per))
  if (show_outside_range && file.exists(f)) terra::ifel(rast(f) > 0, 1, NA) else NULL
}
add_outside <- function(om) {
  if (!is.null(om) && isTRUE(cell_sum(om) > 0)) plot(om, add = TRUE, col = "#4d4d4d80", legend = FALSE)
}

# ---- Figure 1: one map per variable plus the total change, one figure per scenario ---------
# One symmetric colour scale for all of them: the 98th percentile of |value| over every panel in every scenario at
# map_period, so the maps can be compared across variables and scenarios.
pool <- unlist(lapply(names(scenarios), function(s) {
  key <- paste(s, map_period, sep = "_")
  c(unlist(lapply(seq_along(clim_vars), function(i) abs(samp_vals(eff[[key]][[i]])))), abs(samp_vals(chg[[key]])))
}))
lim <- as.numeric(stats::quantile(pool, 0.98))
if (!is.finite(lim) || lim == 0) lim <- max(pool)
message(sprintf("\nChange colour scale: -%.3f to +%.3f (98th percentile of |value|, %s, all scenarios)", lim, lim, map_period))

# Layout of figure 1, all in pixels at 300 dpi. One line of margin text is 40 px here (R draws text at 0.66 of its normal size
# in a layout with 3 or more rows or columns: 0.66 x 0.2 in x 300 dpi). The map rows, the colour bar and the margins are all
# sized from that, so the colour bar thickness and the space between the rows are set by the numbers below.
fig1_w  <- 3000
line_px <- 40
n_col   <- min(3, length(clim_vars) + 1)                  # maps per row
n_row   <- ceiling((length(clim_vars) + 1) / n_col)       # rows of maps
map_mar <- c(1.2, 0.3, 1.6, 0.3)   # margins around each map, in lines: bottom (panel letter), left, top (panel title), right
cb_mar  <- c(3.6, 3, 0.4, 3)       # margins around the colour bar, in lines: the bottom holds the tick labels and two lines of text
oma_top <- if (map_title) 2.4 else 0.2                    # lines above the maps, for the figure title
ext_b   <- as.vector(terra::ext(base))                    # xmin, xmax, ymin, ymax of the area each map is drawn over
map_asp <- (ext_b[2] - ext_b[1]) * (if (terra::is.lonlat(base)) cos(mean(ext_b[3:4]) * pi / 180) else 1) / (ext_b[4] - ext_b[3])   # width / height of a map as plotted (a degree of longitude is shorter than a degree of latitude)
row_px  <- if (is.null(map_row_px)) {
  ceiling((fig1_w / n_col - sum(map_mar[c(2, 4)]) * line_px) / map_asp + sum(map_mar[c(1, 3)]) * line_px)   # map height from the panel width, plus the title and letter margins
} else map_row_px
cb_px   <- colbar_px + sum(cb_mar[c(1, 3)]) * line_px     # height of the colour bar row: the bar plus its margins
message(sprintf("Figure 1 size: %d x %d px (map rows %d px, colour bar %d px thick)", fig1_w,
                round(oma_top * line_px + n_row * row_px + cb_px), row_px, colbar_px))

draw_maps <- function(s) {
  key <- paste(s, map_period, sep = "_")
  panel_names <- c(clim_vars, "suitability change")
  panel_rast  <- c(lapply(seq_along(clim_vars), function(i) eff[[key]][[i]]), list(chg[[key]]))
  np <- length(panel_rast); ncol_g <- min(3, np); nrow_g <- ceiling(np / ncol_g)
  m <- matrix(c(seq_len(np), rep(0, nrow_g * ncol_g - np)), nrow = nrow_g, ncol = ncol_g, byrow = TRUE)
  layout(rbind(m, rep(np + 1, ncol_g)), heights = c(rep(row_px, nrow_g), cb_px))   # heights in pixels; the figure height below is their sum plus the title margin
  par(oma = c(0, 0, oma_top, 0))
  om <- outside_mask(s, map_period)
  land_ext <- terra::ext(terra::trim(chg[[key]]))                  # extent of the land (no empty sea around it), for placing the letters
  for (i in seq_len(np)) {
    plot(clamp(panel_rast[[i]], -lim, lim, values = TRUE), col = chg_cols, range = c(-lim, lim), legend = FALSE,
         axes = FALSE, box = FALSE, main = panel_names[i], cex.main = 1.3, font.main = if (i == np) 4 else 2,
         mar = map_mar)                                              # small margins: room for the panel letter below, the title above
    add_outside(om); add_coast()
    # Panel letter, centred just under the southern tip of the land (in map coordinates, so it sits close to the map
    # however the map was fitted into the panel).
    op <- par(xpd = NA)
    text((land_ext[1] + land_ext[2]) / 2, land_ext[3] - 0.01 * (land_ext[4] - land_ext[3]),
         labels = sprintf(letter_fmt, panel_letters[i]), adj = c(0.5, 1), font = 2, cex = letter_cex)
    par(op)
  }
  vals <- seq(-lim, lim, length.out = 200)
  par(mar = cb_mar)                                          # the bar is colbar_px thick: the row height minus these margins
  image(vals, 1, matrix(vals, ncol = 1), col = chg_cols, yaxt = "n", xlab = "", cex.axis = 0.9)
  mtext("Change in suitability (future minus baseline): one variable changing, or all together in the last panel", side = 1, line = 1.9, cex = 0.8)
  if (show_outside_range) mtext("Dark grey shading = at least one variable outside the model's training range in this projection",
                                side = 1, line = 2.6, cex = 0.55, col = "grey30")
  if (map_title) mtext(sprintf("Contribution of each variable to the change in suitability, %s, %s (%s)", scenarios[[s]], map_period, toupper(gcm)),
                       outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.1)
}
for (s in names(scenarios)) {
  f <- file.path(fig_dir, sprintf("contribution_maps_%s_%s.png", s, map_period))
  png(f, width = fig1_w, height = round(oma_top * line_px + n_row * row_px + cb_px), res = 300)   # title margin + map rows + colour bar row
  draw_maps(s); invisible(dev.off())
  message("Saved: ", f)
}

# ---- Figure 2: mean effect per variable, per scenario and period -----------------------
bar_rows <- c(clim_vars, "total change")
bar_cols <- c(unname(var_cols[clim_vars]), "#3d3d3d")
bar_data <- lapply(names(scenarios), function(s) {
  sapply(periods, function(per) vapply(bar_rows, function(v) tab$mean_effect[tab$ssp == s & tab$period == per & tab$variable == v][1], numeric(1)))
})
all_b <- unlist(bar_data)
y_lo <- min(0, min(all_b, na.rm = TRUE)); y_hi <- max(0, max(all_b, na.rm = TRUE))
ylim <- c(y_lo, y_hi + 0.3 * (y_hi - y_lo))
png(file.path(fig_dir, "contribution_bars.png"), width = 4200, height = 1700, res = 300)
par(mfrow = c(1, length(scenarios)), mar = c(4.4, 5.0, 3.6, 0.8), oma = c(0, 0, 2.4, 0),   # bottom margin leaves room for the panel letter; left margin room for the bigger tick labels and y label
    mgp = c(3.5, 1, 0))                                                                      # y label sits on line 3.5 (default 3) so it clears the bigger tick labels
for (k in seq_along(scenarios)) {
  barplot(bar_data[[k]], beside = TRUE, col = bar_cols, names.arg = periods, border = NA, las = 1, ylim = ylim,
          ylab = if (k == 1) "Area-weighted mean change in suitability" else "", cex.lab = bar_ylab_cex,
          cex.axis = bar_axis_cex, cex.names = bar_axis_cex,
          main = unname(scenarios[k]), cex.main = 1.6)
  abline(h = 0, col = "grey40")
  mtext(sprintf(letter_fmt, panel_letters[k]), side = 1, line = 2.7, at = mean(par("usr")[1:2]), font = 2, cex = bar_letter_cex)   # panel letter, centred under the plot
  if (k == 1) legend("topleft", legend = bar_rows, fill = bar_cols, border = NA, bty = "n", cex = bar_legend_cex, ncol = 2)
}
mtext("Mean contribution of each bioclimatic variable to suitability change", outer = TRUE, side = 3, line = 0.6, font = 2, cex = bar_title_cex)
invisible(dev.off())

# ---- Figure 3: do the effects add up? Total change against the sum of the one-at-a-time effects ------
png(file.path(fig_dir, "contribution_additivity_scatter.png"), width = 4200, height = 1500, res = 300)
par(mfrow = c(1, length(scenarios)), mar = c(4.2, 4.4, 3, 0.8), oma = c(0, 0, 2.4, 0))
for (s in names(scenarios)) {
  key <- paste(s, map_period, sep = "_")
  xy <- terra::spatSample(c(sum_eff[[key]], chg[[key]]), size = scatter_n, method = "regular", na.rm = TRUE, as.raster = FALSE, values = TRUE)
  xy <- as.data.frame(xy); names(xy) <- c("sum_effects", "total_change")
  xy <- xy[stats::complete.cases(xy) & is.finite(xy$sum_effects) & is.finite(xy$total_change), , drop = FALSE]
  rg <- range(c(xy$sum_effects, xy$total_change))
  plot(xy$sum_effects, xy$total_change, pch = 16, cex = 0.35, col = adjustcolor("grey20", 0.12), xlim = rg, ylim = rg, asp = 1,
       xlab = "Sum of one-at-a-time effects", ylab = if (s == names(scenarios)[1]) "Total change (all variables together)" else "",
       main = scenarios[[s]], las = 1)
  abline(0, 1, col = "#c0392b", lwd = 1.5); abline(h = 0, v = 0, col = "grey80")
  legend("topleft", legend = sprintf("%d cells sampled; r = %.2f", nrow(xy), stats::cor(xy$sum_effects, xy$total_change)),
         bty = "n", cex = 0.8, text.col = "grey25")
}
mtext(sprintf("Do the variables' effects add up? %s (%s). Red line = perfectly additive", map_period, toupper(gcm)),
      outer = TRUE, side = 3, line = 0.6, font = 2, cex = 1.05)
invisible(dev.off())

# ---- Figure 4: dominant variable of the change in suitability, one map per scenario ----------------------
# Dominant = the variable with the largest absolute one-at-a-time effect in the cell (same rule as 2b). The sign is not
# shown here; figure 1 shows whether that variable's effect is a rise or a fall.
dom_r <- list(); dom_tab <- list(); dom_pct <- list()
for (s in names(scenarios)) {
  key <- paste(s, map_period, sep = "_")
  cls <- dominant_class(abs(eff[[key]]), mixed_ratio, dominant_min)       # 1..K = climate variables, K+1 = mixed, NA = blank
  dom_r[[s]] <- with_cols(cls, dom_cols)
  # Area-weighted % of land cells in each class (cells are not equal area). Blank = largest effect below dominant_min.
  in_class <- function(i) 100 * wmean(terra::mask(terra::ifel(is.na(cls), 0, terra::ifel(cls == i, 1, 0)), chg[[key]]))
  pct_class <- vapply(seq_along(dom_cols), in_class, numeric(1))
  pct_blank <- 100 * wmean(terra::mask(terra::ifel(is.na(cls), 1, 0), chg[[key]]))
  pct_classified <- 100 - pct_blank
  dom_pct[[s]] <- c(pct_class, pct_blank)                  # unrounded % of land, for the bars of figure 4
  dom_tab[[s]] <- data.frame(
    scenario = scenarios[[s]], ssp = s, period = map_period,
    variable = c(clim_vars, "mixed", sprintf("blank (largest effect below %.2f)", dominant_min)),
    pct_of_land = round(c(pct_class, pct_blank), 1),
    pct_of_classified_cells = round(c(100 * pct_class / pct_classified, NA_real_), 1),
    stringsAsFactors = FALSE)
}
dom_tab <- do.call(rbind, dom_tab); rownames(dom_tab) <- NULL
write.csv(dom_tab, file.path(tab_dir, sprintf("dominant_variable_summary_%s.csv", map_period)), row.names = FALSE)
message(sprintf("\n=== Dominant variable of the change in suitability, %s (area-weighted %% of land) ===", map_period))
print(dom_tab, row.names = FALSE)

# Dominant-variable figures. 4a: the three maps and a legend. 4b: the maps, then horizontal bars of the % of land that each
# variable dominates, then the legend. 4c: the bars and the legend alone.
land_xy <- as.vector(terra::ext(terra::trim(chg[[paste(names(scenarios)[1], map_period, sep = "_")]])))   # xmin, xmax, ymin, ymax of the land
dom_labs   <- c(clim_vars, sprintf("Mixed (runner-up ≥ %d%%)", round(100 * mixed_ratio)))
blank_lab  <- sprintf("Blank (largest effect below %.2f)", dominant_min)
bar_pct    <- do.call(cbind, dom_pct)       # % of land: rows = the climate variables, mixed, blank; columns = scenarios
bar_fill   <- c(dom_cols, "white")          # blank = white, as on the maps
leg_vars   <- c(intersect(dom_legend_order, clim_vars), setdiff(clim_vars, dom_legend_order))   # climate variables in legend order
leg_idx    <- c(match(leg_vars, clim_vars), length(clim_vars) + 1)    # positions in dom_labs / dom_cols: the variables in legend order, then mixed
leg_idx_bl <- c(leg_idx, length(clim_vars) + 2)                       # the same, then blank (positions in c(dom_labs, blank_lab) / bar_fill)
bar_pct    <- bar_pct[leg_idx_bl, , drop = FALSE]    # the bars stack in legend order ...
bar_fill   <- bar_fill[leg_idx_bl]                    # ... with their colours, blank last
txt_col    <- function(col) if (sum(col2rgb(col)[, 1] / 255 * c(0.299, 0.587, 0.114)) < 0.5) "white" else "black"   # readable label colour
maps_title <- sprintf("Which variable contributes most to the change in suitability, baseline (1981-2010) to %s (%s)", map_period, toupper(gcm))

# The three maps (one layout panel each), lettered under each map.
draw_dom_maps <- function() {
  for (i in seq_along(scenarios)) {
    s <- names(scenarios)[i]
    plot(dom_r[[s]], legend = FALSE, axes = FALSE, box = FALSE, main = scenarios[[s]], cex.main = dom_map_title_cex, mar = dom_map_mar)
    add_coast()                                                      # no extrapolation shading on this figure
    op <- par(xpd = NA)                                              # panel letter, centred just under the southern tip of the land
    text((land_xy[1] + land_xy[2]) / 2, land_xy[3] - 0.01 * (land_xy[4] - land_xy[3]),
         labels = sprintf(letter_fmt, panel_letters[i]), adj = c(0.5, 1), font = 2, cex = dom_letter_cex)
    par(op)
  }
}

# Legend, drawn by hand so the entries can sit close together: one row, each entry = colour swatch + label, with a fixed
# small gap between entries (legend() spaces every column as wide as the longest label). There is no title; the row sits in
# the middle of the panel. The text size is reduced in steps if the row would be wider than the panel.
draw_dom_legend <- function(labs, cols) {
  par(mar = c(0, 0, 0, 0)); plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  entry_w <- function(cx) strwidth("M", cex = cx) * 1.5 + strwidth(labs, cex = cx)            # swatch + gap + label
  row_w   <- function(cx) sum(entry_w(cx)) + strwidth("M", cex = cx) * (length(labs) - 1)     # entries + 1 em between them
  cx <- dom_legend_cex;       while (row_w(cx) > 0.96 && cx > 0.5) cx <- cx * 0.95
  em <- strwidth("M", cex = cx); sw_h <- strheight("M", cex = cx) * 1.1
  y_row <- 0.5
  x0 <- (1 - row_w(cx)) / 2
  for (j in seq_along(labs)) {
    rect(x0, y_row - sw_h / 2, x0 + em, y_row + sw_h / 2, col = cols[j], border = "grey30")
    text(x0 + 1.5 * em, y_row, labs[j], adj = c(0, 0.5), cex = cx)
    x0 <- x0 + entry_w(cx)[j] + em
  }
}

# Horizontal stacked bars, one per scenario (first scenario on top): the % of Iceland's land where each variable is dominant,
# plus mixed and blank, so every bar adds up to 100%. Segments narrower than bar_label_min % are left unlabelled.
# letter_i = the panel letter for the bars (NULL = none). With a letter, the axis label is one line, "(d) Share of land area",
# centred on the figure (under map b); the "(d)" is the same size as the map letters, the words are dom_bar_xlab_cex.
draw_dom_bars <- function(letter_i = NULL) {
  ns <- length(scenarios)
  par(mar = c(4.2, 8.2, 0.6, 1.8))     # left and right margins widened a little to fit the bigger scenario names and the "100%" label
  plot.new(); plot.window(xlim = c(0, 100), ylim = c(0.45, ns + 0.55), xaxs = "i", yaxs = "i")
  for (i in seq_len(ns)) {
    y  <- ns - i + 1
    v  <- bar_pct[, i]
    xs <- c(0, cumsum(v))
    rect(xs[-length(xs)], y - 0.38, xs[-1], y + 0.38, col = bar_fill, border = "grey25", lwd = 0.6)
    mids <- (xs[-length(xs)] + xs[-1]) / 2
    for (j in which(v >= bar_label_min))
      text(mids[j], y, sprintf("%.1f%%", v[j]), col = txt_col(bar_fill[j]), cex = dom_bar_label_cex)
  }
  axis(1, at = seq(0, 100, 20), labels = paste0(seq(0, 100, 20), "%"), cex.axis = dom_bar_axis_cex)
  axis(2, at = rev(seq_len(ns)), labels = unname(scenarios), las = 1, tick = FALSE, font = 2, cex.axis = dom_bar_ssp_cex)
  xlab_line <- 3.5                     # margin line of the "Share of land area" label (moved down from 2.8 so the bigger text clears the axis numbers)
  if (is.null(letter_i)) {
    mtext("Share of land area", side = 1, line = xlab_line, cex = dom_bar_xlab_cex)
  } else {
    # text() rather than mtext(), so that the size is scaled exactly like the map letters in this multi-panel layout
    lab1 <- sprintf(letter_fmt, panel_letters[letter_i]); lab2 <- " Share of land area"
    w1 <- strwidth(lab1, cex = dom_letter_cex, font = 2); w2 <- strwidth(lab2, cex = dom_bar_xlab_cex)
    x0 <- grconvertX(0.5, "ndc", "user") - (w1 + w2) / 2                       # centre of the whole figure
    y0 <- par("usr")[3] - xlab_line * (par("mai")[1] / par("mar")[1]) * diff(par("usr")[3:4]) / par("pin")[2]   # margin line xlab_line, in user units
    op <- par(xpd = NA)
    text(x0, y0, lab1, adj = c(0, 0), font = 2, cex = dom_letter_cex)
    text(x0 + w1, y0, lab2, adj = c(0, 0), cex = dom_bar_xlab_cex)
    par(op)
  }
}

# 4a: maps + legend (as before)
draw_dominant <- function() {
  np <- length(scenarios)
  layout(matrix(c(seq_len(np), rep(np + 1, np)), nrow = 2, byrow = TRUE), heights = c(1, 0.3))
  par(oma = c(0.5, 0, 3.2, 0))
  draw_dom_maps()
  draw_dom_legend(c(dom_labs, blank_lab)[leg_idx_bl], bar_fill)      # with a "blank" entry, since the title that used to explain it is gone
  mtext(maps_title, outer = TRUE, side = 3, line = 0.8, font = 2, cex = dom_title_cex)
}

# 4b: maps, then bars, then the legend (the legend has a "blank" entry because the bars show the blank share)
draw_dominant_with_bars <- function() {
  np <- length(scenarios)
  layout(rbind(seq_len(np), rep(np + 1, np), rep(np + 2, np)), heights = unname(dom_row_px))
  par(oma = c(0.5, 0, 3.2, 0))
  draw_dom_maps()
  draw_dom_bars(letter_i = np + 1)
  draw_dom_legend(c(dom_labs, blank_lab)[leg_idx_bl], bar_fill)      # one row; the text shrinks only if it would not fit
  mtext(maps_title, outer = TRUE, side = 3, line = 0.8, font = 2, cex = dom_title_cex)
}

# 4c: the bars and the legend alone (no panel letter: it is a single plot)
draw_dominant_bars_only <- function() {
  layout(rbind(1, 2), heights = unname(dom_bars_only_px))
  par(oma = c(0.5, 0, 3, 0))
  draw_dom_bars()
  draw_dom_legend(c(dom_labs, blank_lab)[leg_idx_bl], bar_fill)      # one row; the text shrinks only if it would not fit
  mtext(sprintf("Share of land where each variable has the largest effect on the change in suitability, baseline to %s (%s)", map_period, toupper(gcm)),
        outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.05)
}

# Width of the version with bars. layout() stretches the three rows to fill the height left after the outer margins (0.5 + 3.2
# lines), and each map is as large as its row allows (limited by height, not width), so the width that just fits three maps is
# worked out from the map row's real height and the maps' aspect ratio (map_asp, from figure 1). Anything wider is empty space.
dom_h_with_bars <- round(230 + sum(dom_row_px))                                  # 230 px = the title and bottom margins
dom_w_with_bars <- if (!is.null(dom_fig_w)) dom_fig_w else {
  row_scale <- (dom_h_with_bars - 3.7 * line_px) / sum(dom_row_px)
  map_h     <- dom_row_px[["maps"]] * row_scale - sum(dom_map_mar[c(1, 3)]) * line_px
  ceiling(length(scenarios) * (map_h * map_asp + sum(dom_map_mar[c(2, 4)]) * line_px + dom_panel_pad_px))
}
message(sprintf("Figure 4 with bars: %d x %d px", dom_w_with_bars, dom_h_with_bars))
dom_figs <- list(
  list(file = sprintf("contribution_dominant_variable_%s.png", map_period),
       w = 4500, h = 1900, draw = draw_dominant),
  list(file = sprintf("contribution_dominant_variable_with_bars_%s.png", map_period),
       w = dom_w_with_bars, h = dom_h_with_bars, draw = draw_dominant_with_bars),
  list(file = sprintf("contribution_dominant_variable_bars_%s.png", map_period),
       w = 3600, h = round(230 + sum(dom_bars_only_px)), draw = draw_dominant_bars_only))
for (fg in dom_figs) {
  f <- file.path(fig_dir, fg$file)
  png(f, width = fg$w, height = fg$h, res = 300)
  fg$draw(); invisible(dev.off())
  message("Saved: ", f)
}

message("\nSaved figures in ", fig_dir)
message("Saved tables in ", tab_dir)