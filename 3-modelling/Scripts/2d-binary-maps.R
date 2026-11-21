# 2d-binary-maps.R
# Turns the suitability maps into suitable / unsuitable maps with a MaxSSS threshold, then maps and measures where
# suitable habitat is lost, gained or stays suitable for each scenario and period (GFDL-ESM4: SSP1-2.6, SSP3-7.0 and
# SSP5-8.5; 2011-2040, 2041-2070 and 2071-2100) compared with the 1981-2010 baseline. A second figure and a table
# add WHEN each cell gains or loses suitability (for example gained by 2011-2040, or gained and then lost again).
#
# Put this file in: ISP-Crowberry-SDM/3-modelling/Scripts/   (run 2a first)
# Reads:  3-modelling/Outputs/Projection/Scenario grid/suitability_baseline_1981-2010.tif        (from 2a)
#         3-modelling/Outputs/Projection/Scenario grid/suitability_<gcm>_<ssp>_<period>.tif      (9 rasters, from 2a)
#         2-process-data/Outputs/Spatial blocks/occurrences_thinned_<thin_km>km_folds.csv
#         2-process-data/Outputs/Spatial blocks/background_points_<n>_folds.csv
#         1-download-data/Downloads/LMI-IS50V/is50v_land_polygon.gpkg                            (optional coastline)
# Writes: 3-modelling/Outputs/Projection/Binary maps/
#         (Sorted into subfolders: .png in Figures/, .csv and .txt in Tables/, .tif in Rasters/.)
#           binary_change_grid.png                      baseline map + 3 x 3 grid of lost / gained / stable maps
#           binary_change_timing_grid.png               four maps in a 2 x 2 grid (one per scenario, baseline to 2071-2100,
#                                                       and the baseline for reference), coloured by WHEN gains and losses
#                                                       happen, with three stacked bars below (one per scenario, each
#                                                       adding up to 100% of the land)
#           thresholds.csv                              the thresholds used (MaxSSS and the 10th-percentile check)
#           area_summary.csv                            km2 suitable, lost, gained and stable, per threshold,
#                                                       scenario and period
#           change_timing_summary.csv                   km2 and % of land in each timing category, per threshold,
#                                                       scenario and period view (see below)
#           change_timing_classes.csv                   the timing categories: code, name and figure colour
#           change_class_<gcm>_<ssp>_<period>.tif       the 9 class rasters for the figure's threshold (see codes)
#           timing_class_<gcm>_<ssp>.tif                the 3 timing rasters (whole trajectory to 2071-2100) for the
#                                                       figure's threshold; codes as in change_timing_classes.csv
#           binary_baseline.tif                         baseline suitable (1) / unsuitable (0)
# Inputs are only read, never changed.
#
# Class codes in the change rasters: 0 = unsuitable in both, 1 = lost (suitable at baseline, not in the future),
# 2 = gained (not suitable at baseline, suitable in the future), 3 = stable suitable.
#
# Timing categories (binary_change_timing_grid.png, change_timing_summary.csv, timing_class_*.tif): each cell has a
# suitable / unsuitable value at the baseline and at each period. A panel (or "period view") follows the cell from the
# baseline up to that panel's period and puts it in one category:
#   1  Unsuitable throughout
#   2  Suitable throughout
#   3-5  Gained by 2011-2040 / 2041-2070 / 2071-2100: unsuitable at baseline, became suitable in that period, and stayed
#        suitable through the panel's period (so in the 2041-2070 panel, "gained by 2011-2040" is a cell that became
#        suitable in the first period and still is)
#   6-8  Lost by 2011-2040 / 2041-2070 / 2071-2100: suitable at baseline, became unsuitable in that period and stayed so
#   9  Gained, then lost again: unsuitable at baseline, suitable at some period, unsuitable again by the panel's period
#   10 Lost, then regained: suitable at baseline, unsuitable at some period, suitable again by the panel's period
#   11 Flips more than once: suitability changes three times (only possible by 2071-2100)
# The 2011-2040 view therefore matches binary_change_grid.png (gained, lost, stable), and the 2071-2100 view is the
# full trajectory from baseline to the end of the century. A view only has the categories that its period allows.
# The timing figure shows only the 2071-2100 view (one map and one bar per scenario); the csv has all three views.
#
# How the threshold is chosen (MaxSSS, Liu et al. 2013, 2016): on the BASELINE suitability raster, take the
# suitability at the thinned presences and at the background points, and pick the threshold that gives the highest
# sensitivity + specificity. Sensitivity = share of presences at or above the threshold. Specificity here is the
# share of BACKGROUND points below it (there are no true absences), so it is a pseudo-specificity: some background
# is places where crowberry grows. The threshold therefore depends on the background you used (its extent and
# any sampling bias), and it is a threshold for this model and background, not a property of crowberry.
# The same single threshold is then applied to the baseline and to every future map. It is NOT recomputed for each
# scenario, because the point is to ask what happens to the same cut-off as the climate changes.
# The 10th-percentile threshold (10% of baseline presences fall below it) is a second, more generous cut-off,
# written to the tables as a check on how much the area numbers depend on the choice.
#
# Areas are true cell areas in km2 (cells are 30 arc-seconds, so they get narrower toward the north), not cell counts.
#
# Limits:
#  - One climate model (GFDL-ESM4). Suitable habitat is climate and terrain similarity to today's records, not
#    where crowberry will actually grow (dispersal, soils and land use are ignored).
#  - Gains and losses in cells where a future variable is outside the range the model was trained on rest on clamped
#    values and are not trustworthy. This script does not shade those cells (2a does in its figures), so look at the
#    grey areas there before reading too much into a gain or loss. gddlgd10 is the likeliest culprit.
#  - A change only shows up if suitability crosses the threshold. Cells that move a lot but stay on one side look
#    unchanged here, and cells just around the threshold can flip with a small change. The continuous change maps
#    from 2a show the whole picture.
#  - The "gained, then lost again", "lost, then regained" and "flips" categories are mostly cells sitting close to the
#    threshold, where a small difference between periods (or between the SSP's year-to-year climate) is enough to cross
#    it. Treat them as unstable cells, not as a real boom-and-bust in the habitat.
#
# NOT TESTED on your data. Check the console messages and the figures before using the output.

# install.packages("terra")
library(terra)

# ---- Settings ---------------------------------------------------------------
thin_km      <- 5                          # which thinned presence set (must have a *_folds.csv from 4a)
n_background <- 10000                      # must match the background_points_<n>_folds.csv file
gcm          <- "gfdl-esm4"
scenarios    <- c(ssp126 = "SSP1-2.6", ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")   # rows, in this order
periods      <- c("2011-2040", "2041-2070", "2071-2100")                            # columns, in this order
figure_threshold <- "maxsss"               # which threshold the figures and class rasters use: "maxsss" or "p10"
p10_quantile <- 0.10                       # the second threshold: this quantile of baseline suitability at presences
panel_letters <- letters                   # letter centred under each map: a, b, c ... (LETTERS for A, B, C)
letter_fmt    <- "(%s)"                    # "(%s)" gives (a); "%s" gives a
letter_cex    <- 1.1                       # size of the panel letters (map titles are 0.8-1.0)

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

if (!(figure_threshold %in% c("maxsss", "p10"))) stop("figure_threshold must be \"maxsss\" or \"p10\".")
if (length(periods) != 3) stop("The timing figure is set up for exactly three periods.")
if (!requireNamespace("readr", quietly = TRUE)) stop("Package 'readr' is needed. install.packages(\"readr\")")

# ---- Paths ------------------------------------------------------------------
outputs_dir <- p("2-process-data", "Outputs")
blocks_dir  <- file.path(outputs_dir, "Spatial blocks")
# fold CSVs from 4a: use the Assignments/Tables subfolder if the files are there, otherwise the Spatial blocks folder itself
assign_dir  <- file.path(blocks_dir, "Assignments", "Tables")
if (!file.exists(file.path(assign_dir, paste0("occurrences_thinned_", thin_km, "km_folds.csv")))) assign_dir <- blocks_dir
pres_file  <- file.path(assign_dir, paste0("occurrences_thinned_", thin_km, "km_folds.csv"))
bg_file     <- file.path(assign_dir, paste0("background_points_", n_background, "_folds.csv"))
grid_dir    <- p("3-modelling", "Outputs", "Projection", "Scenario grid")
base_file   <- file.path(grid_dir, "suitability_baseline_1981-2010.tif")
coast_file  <- p("1-download-data", "Downloads", "LMI-IS50V", "is50v_land_polygon.gpkg")
out_dir     <- p("3-modelling", "Outputs", "Projection", "Binary maps")
for (d in file.path(out_dir, c("Tables", "Figures", "Rasters"))) dir.create(d, showWarnings = FALSE, recursive = TRUE)

fut_file <- function(s, per) file.path(grid_dir, sprintf("suitability_%s_%s_%s.tif", gcm, s, per))
needed <- c(pres_file, bg_file, base_file,
            unlist(lapply(names(scenarios), function(s) vapply(periods, function(per) fut_file(s, per), character(1)))))
if (any(!file.exists(needed))) {
  stop(sum(!file.exists(needed)), " input file(s) not found, for example:\n  ",
       paste(utils::head(needed[!file.exists(needed)], 6), collapse = "\n  "),
       "\nThe *_folds.csv files come from 4a-assign-spatial-blocks.R; the suitability rasters come from 2a-scenario-change-grid.R.")
}

# ---- Suitability rasters ------------------------------------------------------
pred_base <- rast(base_file)
preds <- list()
for (s in names(scenarios)) for (per in periods) {
  r <- rast(fut_file(s, per))
  if (!isTRUE(compareGeom(pred_base, r, stopOnError = FALSE))) stop("Raster ", basename(fut_file(s, per)), " is not on the same grid as the baseline.")
  preds[[paste(s, per, sep = "_")]] <- r
}

# ---- Points: baseline suitability at presences and background ---------------
get_xy <- function(df, what) {
  lat <- intersect(c("decimalLatitude", "latitude", "lat"), names(df))[1]
  lon <- intersect(c("decimalLongitude", "longitude", "lon", "long"), names(df))[1]
  if (is.na(lat) || is.na(lon)) stop("Couldn't find lat/lon columns in the ", what, " file.")
  data.frame(longitude = df[[lon]], latitude = df[[lat]])
}
pxy <- get_xy(readr::read_csv(pres_file, show_col_types = FALSE), "presence")
bxy <- get_xy(readr::read_csv(bg_file,   show_col_types = FALSE), "background")
s_pres <- terra::extract(pred_base, as.matrix(pxy))[, 1]
s_bg   <- terra::extract(pred_base, as.matrix(bxy))[, 1]
if (any(is.na(s_pres)) || any(is.na(s_bg))) {
  message("Dropping points with no baseline suitability: ", sum(is.na(s_pres)), " presences, ", sum(is.na(s_bg)), " background")
}
s_pres <- s_pres[is.finite(s_pres)]; s_bg <- s_bg[is.finite(s_bg)]
message("Presences: ", length(s_pres), "   background: ", length(s_bg))

# ---- Thresholds ---------------------------------------------------------------
maxsss <- function(pos, neg) {                       # maximise sensitivity + specificity over every observed score
  cand <- sort(unique(c(pos, neg)))
  sens <- 1 - findInterval(cand, sort(pos), left.open = TRUE) / length(pos)   # share of presences >= threshold
  spec <- findInterval(cand, sort(neg), left.open = TRUE) / length(neg)        # share of background < threshold
  i <- which.max(sens + spec)
  c(threshold = cand[i], sensitivity = sens[i], specificity = spec[i])
}
ms  <- maxsss(s_pres, s_bg)
p10 <- unname(stats::quantile(s_pres, p10_quantile))
w   <- terra::cellSize(pred_base, unit = "km", mask = TRUE)            # km2 of each land cell
land_km2 <- terra::global(w, "sum", na.rm = TRUE)[1, 1]
km2_of   <- function(r01) terra::global(w * r01, "sum", na.rm = TRUE)[1, 1]

thr_tab <- data.frame(
  method = c("maxsss", "p10"),
  description = c("MaxSSS: maximum sensitivity + specificity (specificity against background)",
                  sprintf("%.0fth percentile of baseline suitability at presences", 100 * p10_quantile)),
  threshold = c(ms[["threshold"]], p10),
  sensitivity = c(ms[["sensitivity"]], mean(s_pres >= p10)),
  pseudo_specificity = c(ms[["specificity"]], mean(s_bg < p10)),
  pct_land_suitable_baseline = round(100 * c(km2_of(pred_base >= ms[["threshold"]]), km2_of(pred_base >= p10)) / land_km2, 1),
  stringsAsFactors = FALSE)
thr_tab[, c("threshold", "sensitivity", "pseudo_specificity")] <- round(thr_tab[, c("threshold", "sensitivity", "pseudo_specificity")], 4)
write.csv(thr_tab, file.path(out_dir, "Tables", "thresholds.csv"), row.names = FALSE)
message("\n=== Thresholds (cloglog scale) ===")
print(thr_tab[, c("method", "threshold", "sensitivity", "pseudo_specificity", "pct_land_suitable_baseline")], row.names = FALSE)
message("(pseudo_specificity = share of background points below the threshold; there are no true absences.)")
if (thr_tab$pct_land_suitable_baseline[1] > 80) {
  message("NOTE: MaxSSS marks more than 80% of the land as suitable at baseline. That fits a widespread species, but it means\n",
          "      the binary maps will show little contrast; losses and gains happen in the margins.")
}

# ---- Classify and measure -----------------------------------------------------
classify <- function(pred_f, t) (pred_base >= t) + 2 * (pred_f >= t)   # 0 none, 1 lost, 2 gained, 3 stable suitable
rows <- list(); classes_fig <- list()
for (m in thr_tab$method) {
  t <- thr_tab$threshold[thr_tab$method == m]
  for (s in names(scenarios)) for (per in periods) {
    key  <- paste(s, per, sep = "_")
    code <- classify(preds[[key]], t)
    lost <- km2_of(code == 1); gained <- km2_of(code == 2); stable <- km2_of(code == 3)
    base_suit <- lost + stable
    rows[[length(rows) + 1]] <- data.frame(
      threshold_method = m, threshold = round(t, 4), scenario = scenarios[[s]], ssp = s, period = per,
      km2_suitable_baseline = round(base_suit), km2_suitable_future = round(gained + stable),
      km2_stable = round(stable), km2_lost = round(lost), km2_gained = round(gained),
      km2_net_change = round(gained - lost),
      pct_net_change = if (base_suit > 0) round(100 * (gained - lost) / base_suit, 1) else NA_real_,
      pct_land_suitable_future = round(100 * (gained + stable) / land_km2, 1),
      stringsAsFactors = FALSE)
    if (m == figure_threshold) classes_fig[[key]] <- code
  }
}
area_tab <- do.call(rbind, rows); rownames(area_tab) <- NULL
write.csv(area_tab, file.path(out_dir, "Tables", "area_summary.csv"), row.names = FALSE)
message("\n=== Suitable area (km2): ", figure_threshold, " threshold ===")
print(area_tab[area_tab$threshold_method == figure_threshold,
               c("scenario", "period", "km2_suitable_baseline", "km2_suitable_future", "km2_lost", "km2_gained", "pct_net_change")],
      row.names = FALSE)
message("Land area in the rasters: ", round(land_km2), " km2")

# Save the rasters
thr_fig <- thr_tab$threshold[thr_tab$method == figure_threshold]
writeRaster(pred_base >= thr_fig, file.path(out_dir, "Rasters", "binary_baseline.tif"), overwrite = TRUE, datatype = "INT1U")
for (key in names(classes_fig)) {
  s <- sub("_.*$", "", key); per <- sub("^[^_]*_", "", key)
  writeRaster(classes_fig[[key]], file.path(out_dir, "Rasters", sprintf("change_class_%s_%s_%s.tif", gcm, s, per)),
              overwrite = TRUE, datatype = "INT1U")
}

# ---- Timing of gains and losses -----------------------------------------------
# Each cell has a suitable (1) / unsuitable (0) value at the baseline and at each period. For a "view" that runs up to
# period k, the sequence (baseline, period 1, ..., period k) is put into one of the categories described in the header.
n_per <- length(periods)
cat_names <- c("Unsuitable throughout", "Suitable throughout",
               sprintf("Gained by %s", periods), sprintf("Lost by %s", periods),
               "Gained, then lost again", "Lost, then regained", "Flips more than once")
cat_cols  <- c("#F0F0F0", "#009E73",
               "#08519C", "#4292C6", "#9ECAE1",          # gained by the 1st, 2nd, 3rd period (earlier = darker)
               "#A63603", "#E6550D", "#FDAE6B",          # lost by the 1st, 2nd, 3rd period (earlier = darker)
               "#6A3D9A", "#FFD92F", "#555555")          # gained then lost, lost then regained, flips more than once
n_cat <- length(cat_names)
cat_of_seq <- function(b) {                              # b = 0/1 vector: baseline, then each period up to the view's period
  tr <- which(diff(b) != 0)                              # index j = suitability changes going into period j
  if (length(tr) == 0) return(if (b[1] == 1) 2 else 1)
  if (length(tr) == 1) return(if (b[1] == 0) 2 + tr else 2 + n_per + tr)
  if (length(tr) == 2) return(if (b[1] == 0) 3 + 2 * n_per else 4 + 2 * n_per)
  5 + 2 * n_per
}
lookup_for_view <- function(k) vapply(0:(2^(k + 1) - 1), function(cd) cat_of_seq(rev(as.integer(intToBits(cd))[1:(k + 1)])), numeric(1))

timing_of <- function(s, k, t) {                         # category raster for scenario s, following the cell up to period k
  code <- (pred_base >= t) * 2^k
  for (j in seq_len(k)) code <- code + (preds[[paste(s, periods[j], sep = "_")]] >= t) * 2^(k - j)
  out <- terra::subst(code, from = 0:(2^(k + 1) - 1), to = lookup_for_view(k))
  names(out) <- "timing"
  out
}

trows <- list(); timing_fig <- list()
for (m in thr_tab$method) {
  t <- thr_tab$threshold[thr_tab$method == m]
  for (s in names(scenarios)) for (k in seq_len(n_per)) {
    tim <- timing_of(s, k, t)
    z   <- terra::zonal(w, tim, fun = "sum", na.rm = TRUE)
    km2 <- stats::setNames(numeric(n_cat), seq_len(n_cat))
    km2[as.character(z[[1]])] <- z[[2]]
    trows[[length(trows) + 1]] <- data.frame(
      threshold_method = m, threshold = round(t, 4), scenario = scenarios[[s]], ssp = s,
      period_view = sprintf("baseline to %s", periods[k]),
      category_code = seq_len(n_cat), category = cat_names,
      km2 = round(unname(km2)), pct_of_land = round(100 * unname(km2) / land_km2, 2), stringsAsFactors = FALSE)
    if (m == figure_threshold) timing_fig[[paste(s, k, sep = "_")]] <- tim
  }
}
timing_tab <- do.call(rbind, trows); rownames(timing_tab) <- NULL
write.csv(timing_tab, file.path(out_dir, "Tables", "change_timing_summary.csv"), row.names = FALSE)
write.csv(data.frame(code = seq_len(n_cat), category = cat_names, colour = cat_cols), file.path(out_dir, "Tables", "change_timing_classes.csv"), row.names = FALSE)
for (s in names(scenarios)) {
  writeRaster(timing_fig[[paste(s, n_per, sep = "_")]], file.path(out_dir, "Rasters", sprintf("timing_class_%s_%s.tif", gcm, s)),
              overwrite = TRUE, datatype = "INT1U")
}
message("\n=== Timing of gains and losses, baseline to ", periods[n_per], " (km2): ", figure_threshold, " threshold ===")
fin <- timing_tab[timing_tab$threshold_method == figure_threshold & timing_tab$period_view == sprintf("baseline to %s", periods[n_per]), ]
print(reshape(fin[, c("scenario", "category", "km2")], idvar = "category", timevar = "scenario", direction = "wide"), row.names = FALSE)

# ---- Figures ----------------------------------------------------------------
coast <- NULL
if (file.exists(coast_file)) coast <- tryCatch(project(vect(coast_file), crs(pred_base)), error = function(e) NULL)
add_coast <- function() if (!is.null(coast)) lines(coast, lwd = 0.3, col = "grey25")

# Panel letters: bold, centred under each map, just below the southern edge of the land (so the gap between rows stays small).
land_xy <- as.vector(terra::ext(terra::trim(pred_base)))   # xmin, xmax, ymin, ymax of the land
map_letter <- function(i) text((land_xy[1] + land_xy[2]) / 2, land_xy[3] - 0.01 * (land_xy[4] - land_xy[3]),
                               sprintf(letter_fmt, panel_letters[i]), adj = c(0.5, 1), font = 2, cex = letter_cex, xpd = NA)

# One-row legend with the same gap between every item (legend(horiz = TRUE) gives every item the width of the longest label,
# which looks uneven). gap_in = space between one label's end and the next colour box, in inches.
even_legend <- function(labels, cols, cex = 1.0, gap_in = 0.5) {
  n <- length(labels)
  bw <- xinch(0.16) * cex; bh <- yinch(0.16) * cex; pad <- xinch(0.08) * cex; gp <- xinch(gap_in)
  tw <- strwidth(labels, cex = cex)
  item <- bw + pad + tw
  x0 <- mean(par("usr")[1:2]) - (sum(item) + gp * (n - 1)) / 2
  xs <- x0 + c(0, cumsum(item[-n] + gp))
  y  <- mean(par("usr")[3:4])
  rect(xs, y - bh / 2, xs + bw, y + bh / 2, col = cols, border = "grey40")
  text(xs + bw + pad, y, labels, adj = c(0, 0.5), cex = cex)
}

cls_cols  <- c("#F0F0F0", "#D55E00", "#0072B2", "#009E73")        # unsuitable, lost, gained, stable suitable
cls_names <- c("Unsuitable in both", "Lost", "Gained", "Stable suitable")
brk       <- c(-0.5, 0.5, 1.5, 2.5, 3.5)
fig_label <- if (figure_threshold == "maxsss") "MaxSSS" else sprintf("%.0fth-percentile", 100 * p10_quantile)
base_title <- sprintf("Baseline (1981-2010): suitable (suitability >= %.2f), %.0f%% of land",
                      thr_fig, thr_tab$pct_land_suitable_baseline[thr_tab$method == figure_threshold])
panel_title_cex <- 1.4                     # size of the "SSP, period" title above each map
panel_net <- function(s, per) sprintf("%s,  %s", scenarios[[s]], per)   # title only; the area change is quantified in the line plots (2d)

lay <- rbind(c(0, 1, 1, 1, 1, 0),
             c(2, 2, 3, 3, 4, 4),
             c(5, 5, 6, 6, 7, 7),
             c(8, 8, 9, 9, 10, 10),
             c(0, 11, 11, 11, 11, 0))

# Figure 1: lost / gained / stable
png(file.path(out_dir, "Figures", "binary_change_grid.png"), width = 3000, height = 4300, res = 300)   # taller than before: room for the letters under each map
layout(lay, heights = c(1.5, 1, 1, 1, 0.45))
par(oma = c(0, 0, 3, 0))

plot((pred_base >= thr_fig) * 3, col = cls_cols[c(1, 4)], breaks = c(-0.5, 1.5, 3.5), legend = FALSE,
     axes = FALSE, box = FALSE, mar = c(1.4, 0.5, 2.4, 0.5), cex.main = 1, main = base_title)
add_coast()
pn <- 1; map_letter(pn)

for (s in names(scenarios)) for (per in periods) {
  key <- paste(s, per, sep = "_")
  plot(classes_fig[[key]], col = cls_cols, breaks = brk, legend = FALSE, axes = FALSE, box = FALSE,
       mar = c(1.2, 0.3, 2.2, 0.3), cex.main = panel_title_cex, main = panel_net(s, per))
  add_coast()
  pn <- pn + 1; map_letter(pn)
}

par(mar = c(0.5, 1, 0.5, 1))
plot.new()
even_legend(cls_names, cls_cols, cex = 1.0, gap_in = 0.55)
mtext(sprintf("Habitat suitability gained and lost against the baseline (%s threshold, GFDL-ESM4)", fig_label),
      outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.1)
invisible(dev.off())

# Figure 2: when gains and losses happen. Top: four maps in a 2 x 2 grid, one per scenario (each follows every cell from
# the baseline to the end of the century) plus the baseline for reference in the bottom-right corner. Below: one stacked
# horizontal bar per scenario, in the same colours as the maps. Each bar is the whole land area (100%) split into the timing
# categories. A shared legend sits at the bottom.
n_scen     <- length(scenarios)
final_view <- sprintf("baseline to %s", periods[n_per])
bar_codes  <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1)        # left to right: suitable throughout | gained | lost | gained and lost again | unsuitable throughout
bar_tab    <- timing_tab[timing_tab$threshold_method == figure_threshold & timing_tab$period_view == final_view, ]
bar_pct    <- lapply(names(scenarios), function(s) {
  x <- bar_tab[bar_tab$ssp == s, ]
  v <- x$pct_of_land[match(bar_codes, x$category_code)]
  v[is.na(v)] <- 0
  v
})
names(bar_pct) <- names(scenarios)
bar_y      <- rev(seq_len(n_scen))                         # first scenario at the top
bar_label_min <- 3.5                                       # segments narrower than this (% of land) get no label inside
is_dark    <- function(col) colSums(col2rgb(col) * c(0.299, 0.587, 0.114)) < 140   # white text on dark fills, black on light
map_mar    <- c(1.4, 0.3, 2.2, 0.3)                       # bottom (room for the panel letter), left, top (room for the title), right; in lines
map_titles <- function(top) mtext(top, side = 3, line = 0.2, font = 2, cex = 1.15)   # one-line title above a map
bar_mar    <- c(4.6, 6.2, 0.3, 1.5)                        # margins of the bar panel, in lines (the bar area itself is not resized)

# Panels 1-4 are the maps (2 x 2, six columns each); panel 5 is the stacked bars and panel 6 the legend.
lay2 <- rbind(c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
              c(3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4),
              rep(5, 12),
              rep(6, 12))

# Figure height. Nothing is resized: the maps, bars, legend and text keep their size, and the height is just the sum of what
# each part needs. The maps are cropped to the land (no blank sea above/below), and each map row is as tall as the land
# at the map width, plus its margins. Heights are in inches; one margin line = line_in (asked from a throwaway device that
# has the same layout, because R shrinks margin lines when a layout has more than 2 rows or columns).
land_ext <- terra::ext(land_xy[1], land_xy[2], land_xy[3], land_xy[4])
line_in  <- tryCatch({ pdf(NULL); layout(lay2); par(mar = c(1, 0, 0, 0)); x <- par("mai")[1]; dev.off(); x },
                     error = function(e) 0.2 * 0.66)
png_w_in <- 3000 / 300; png_h_res <- 300
map_w_in <- png_w_in / 2 - sum(map_mar[c(2, 4)]) * line_in                     # width of one map (two columns across the figure)
asp_map  <- if (terra::is.lonlat(pred_base)) 1 / cos(mean(land_xy[3:4]) * pi / 180) else 1   # how terra stretches lon/lat maps
map_h_in <- map_w_in * (land_xy[4] - land_xy[3]) * asp_map / (land_xy[2] - land_xy[1])
row_h_in  <- map_h_in + sum(map_mar[c(1, 3)]) * line_in                        # the maps keep their size; raise map_mar[1] / map_mar[3] to space the rows out
bars_h_in <- 1.94 + sum(bar_mar[c(1, 3)]) * line_in                            # 1.94 in = the height the bar area had before
leg_h_in  <- 0.95
oma_lines <- 3
fig_h_in  <- oma_lines * line_in + 2 * row_h_in + bars_h_in + leg_h_in
png(file.path(out_dir, "Figures", "binary_change_timing_grid.png"), width = 3000, height = round(fig_h_in * png_h_res), res = png_h_res)
layout(lay2, heights = c(row_h_in, row_h_in, bars_h_in, leg_h_in))
par(oma = c(0, 0, oma_lines, 0))

# Maps: SSP1-2.6, SSP3-7.0 (top row), SSP5-8.5 and the baseline (bottom row)
for (i in seq_len(n_scen)) {
  s <- names(scenarios)[i]
  plot(terra::crop(timing_fig[[paste(s, n_per, sep = "_")]], land_ext), col = cat_cols, breaks = seq(0.5, n_cat + 0.5, by = 1), legend = FALSE,
       axes = FALSE, box = FALSE, mar = map_mar, main = "")
  add_coast()
  map_titles(scenarios[[i]])
  map_letter(i)
}
plot(terra::crop((pred_base >= thr_fig) * 3, land_ext), col = cls_cols[c(1, 4)], breaks = c(-0.5, 1.5, 3.5), legend = FALSE,
     axes = FALSE, box = FALSE, mar = map_mar, main = "")
add_coast()
map_titles("Baseline (for reference)")
map_letter(n_scen + 1)

# Stacked bars: share of the land area in each timing category, baseline to the end of the century
par(mar = bar_mar)
plot.new(); plot.window(xlim = c(0, 100), ylim = c(0.45, n_scen + 0.55), xaxs = "i", yaxs = "i")
for (i in seq_len(n_scen)) {
  v  <- bar_pct[[i]]
  v  <- v * 100 / sum(v)                                   # sums are 100 already apart from rounding; this makes each bar exactly full width
  x1 <- cumsum(v); x0 <- x1 - v
  rect(x0, bar_y[i] - 0.36, x1, bar_y[i] + 0.36, col = cat_cols[bar_codes], border = "grey40", lwd = 0.6)
  lab <- v >= bar_label_min
  if (any(lab)) text((x0 + x1)[lab] / 2, bar_y[i], sprintf("%.1f%%", v[lab]), cex = 0.85,
                     col = ifelse(is_dark(cat_cols[bar_codes][lab]), "white", "black"))
}
axis(2, at = bar_y, labels = unname(scenarios), las = 1, tick = FALSE, font = 2, cex.axis = 1.0)
axis(1, at = seq(0, 100, by = 20), labels = paste0(seq(0, 100, by = 20), "%"), cex.axis = 0.85)
mtext("Share of land area", side = 1, line = 2.4, cex = 0.9)
mtext(sprintf(letter_fmt, panel_letters[n_scen + 2]), side = 1, line = 3.9, font = 2, cex = letter_cex)

# Legend in four columns (filled down each column): stable states | gained | lost | gained and lost again. NA = empty slot.
par(mar = c(0.5, 1, 0.5, 1))
plot.new()
leg_order <- c(2, 1, NA,
               3, 4, 5,
               6, 7, 8,
               9, 10, 11)
leg_lab <- cat_names[leg_order]; leg_lab[is.na(leg_lab)] <- ""
leg_fill <- cat_cols[leg_order]
legend("center", legend = leg_lab, fill = leg_fill, border = ifelse(is.na(leg_fill), NA, "grey40"),
       ncol = 4, bty = "n", cex = 1.6, x.intersp = 0.6)

mtext(sprintf("Timing of suitability gain and loss from baseline (%s threshold = %.4f, GFDL-ESM4)", fig_label, thr_fig),
      outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.21)   # 1.21 = the old 1.1, 10% bigger
invisible(dev.off())

message("\nSaved: ", file.path(out_dir, "Figures", "binary_change_grid.png"))
message("Saved: ", file.path(out_dir, "Figures", "binary_change_timing_grid.png"))
message("Saved: ", file.path(out_dir, "Tables", "thresholds.csv"))
message("Saved: ", file.path(out_dir, "Tables", "area_summary.csv"))
message("Saved: ", file.path(out_dir, "Tables", "change_timing_summary.csv"))
message("Saved: ", file.path(out_dir, "Tables", "change_timing_classes.csv"))
message("Saved: class rasters (change_class_*.tif, timing_class_*.tif) and binary_baseline.tif in ", out_dir)