# 5b-corine-class-suitability.R
# Shows how suitable each CORINE land cover class is for crowberry, now and under each emissions scenario, as four figures:
#   1. Figures/5b_current_suitability_<threshold>.png   CURRENT (1981-2010) habitat, three panels side by side, one row per class:
#        left:   the share of all suitable habitat that lies in the class (solid bar) next to the share of all land in the
#                class (pale outlined bar), so you can see which classes hold more or less suitable habitat than their area
#        middle: the percentage of the class that is suitable
#        right:  the spread of suitability values in the class (10th-90th percentile, 25th-75th percentile and median)
#                against the threshold
#   2. Figures/5b_suitability_overlay_<threshold>.png   one panel per scenario (SSP1-2.6, SSP3-7.0, SSP5-8.5): the percentage of
#        each class that is suitable for 2011-2040, 2041-2070 and 2071-2100, overlaid on a subtle grey baseline (1981-2010)
#   3. Figures/5b_suitable_area_change_<threshold>.png  the change in suitable area (km2) in each class from the baseline, for
#        the three periods, with the three scenarios side by side (one panel each, sharing one x-axis scale). This figure
#        only shows the classes that make up at least min_pct_land_change % of Iceland's land (2% by default)
#   4. Figures/5b_suitability_change_<threshold>.png    the same layout and the same classes as figure 3, but showing the change
#        in the AVERAGE suitability of each class (no threshold applied) instead of the change in suitable area. A narrow
#        panel on the left shows each class's average suitability in the baseline, next to the threshold, so you can see
#        whether a class was suitable to begin with as well as how much it changes
#
# Put this file in: ISP-Crowberry-SDM/3-modelling/Scripts/   (run 2a and 2b first)
# Reads:  3-modelling/Outputs/Projection/Scenario grid/suitability_baseline_1981-2010.tif   (from 2a)
#         3-modelling/Outputs/Projection/Binary maps/Tables/thresholds.csv                         (from 2b)
#         3-modelling/Outputs/Projection/Scenario grid/suitability_<gcm>_<ssp>_<period>.tif  (9 rasters, from 2a)
#         1-download-data/Downloads/CORINE/corine_2018_iceland.gpkg                         (from the CORINE download script)
#         3-modelling/Outputs/CORINE/Tables/5a_class_comparison_<thin_km>km.csv             (from 5a; gives the number of
#                                                                                            presences per class)
# Writes: 3-modelling/Outputs/CORINE/   (all four CORINE scripts, 5a to 5d, write here; files start with the script's number)
#           Figures/5b_current_suitability_<threshold>.png     figure 1 (current habitat)
#           Figures/5b_suitability_overlay_<threshold>.png     figure 2 (% of each class suitable, by scenario)
#           Figures/5b_suitable_area_change_<threshold>.png    figure 3 (change in suitable area, by scenario)
#           Figures/5b_suitability_change_<threshold>.png      figure 4 (change in average suitability, by scenario)
#           Tables/5b_suitability_by_class_<threshold>.csv     the numbers for each class, current habitat
#           Tables/5b_suitability_by_scenario_<threshold>.csv  the numbers for each class, baseline and all 9 projections
# Inputs are only read, never changed.
#
# How the polygons and the raster are lined up: the suitability map is a grid of roughly 1 km cells and CORINE is a set of
# polygons, so a cell can hold several classes. Rather than give each cell ONE class (which would lose the small classes
# and misplace the edges), the script works out, for every suitability cell, what FRACTION of it lies in each class. It does
# this by drawing the polygons on a grid that is fine_factor times finer than the suitability grid (each suitability cell
# is split into fine_factor x fine_factor pieces; each piece gets the class under its centre) and counting the pieces.
# Every area below is then the sum, over cells, of (fraction of the cell in the class) x (cell area in km2) x (the cell's
# value). So a cell half in heath and half in bare rock counts half toward each. A cell is suitable if its suitability is
# at or above the threshold from 2b (MaxSSS by default; set threshold_method <- "p10" for the 10th-percentile version),
# and the whole cell is treated as suitable or not, so the class numbers are averages over mixed cells, not a finer-scale
# map. Cell areas are true areas in km2 (cells get narrower toward the north).
# The average suitability of a class (figure 4 and the mean_suitability column in the csv files) is the same kind of
# average: the suitability values of the cells, weighted by how much of each cell is in the class. No threshold is used.
#
# Reading it:
#  - Only land covered by BOTH a land class in CORINE (the sea class, 523, is left out) and a suitability value is counted.
#    The console reports how much of the suitability grid's land area that covers; the shortfall is coast and island
#    cells where the two datasets disagree about where the land stops.
#  - The model is built from climate and terrain, not land cover, so this is an independent check: if the model puts habitat
#    in heath and shrub and almost none in glaciers or bare rock, it is picking up the pattern you would expect.
#  - The model's predictors are nearly all climate and terrain at about 1 km, so differences between classes mostly reflect
#    where the classes sit (lowland or highland, warm or cold), not something the model knows about the vegetation.
#  - Classes in figures 1 and 2 are those with at least min_km2 of land AND at least min_points presences (from 5a; every
#    class is in the csv files). A class with no presences is left out of those figures because the model has no records in
#    it to support what it says there. Classes with only a few presences are still weakly supported: see n_points in the
#    csv files. Figures 3 and 4 instead show the classes that make up at least min_pct_land_change % of Iceland's land,
#    whatever their number of presences, so check n_points in the csv for any of them with few records.
#  - The scenario figures apply the SAME baseline threshold to every future map, so differences come from the changed
#    climate, not from a changed cut-off (figure 4 uses no threshold at all, apart from the reference line in its baseline
#    panel). The classes are held fixed at their 2018 extent: vegetation does not change, glaciers do not retreat, and the
#    land does not change in any way. Only the suitability values change. Read "area of heath that becomes suitable" as
#    "area where the 2018 heath is, in climate that becomes suitable", not as a prediction that the heath will be there.
#  - Average suitability is not the same as the share of a class that is suitable. A class whose average is below the
#    threshold can still have a good share of suitable cells (a few high cells pull the average up), and a class whose
#    average is above it can still contain unsuitable cells. Use the baseline panel of figure 4 as a guide to where each
#    class starts, and figures 1 and 2 for the share above the threshold.
#  - Habitat in future cells with climate beyond what the model saw in the records rests on extrapolation (see the grey
#    areas in the 2a figures).
#
# NOT TESTED on your data. Check the console messages and the figures before using the output.

# install.packages("terra")
library(terra)

# ---- Settings ---------------------------------------------------------------
threshold_method <- "maxsss"      # "maxsss" or "p10" (as in 2b)
fine_factor      <- 5             # each suitability cell is split into fine_factor x fine_factor pieces to find class fractions
exclude_codes    <- "523"         # classes that are not land. 523 = Sea and ocean
min_km2          <- 100           # figures 1 and 2: leave out classes with less land than this (every class is in the csv files)
min_points       <- 1             # figures 1 and 2: also leave out classes with fewer presences than this
min_pct_land_change <- 2          # figures 3 and 4: show only classes that make up at least this % of Iceland's land
thin_km          <- 5             # only used to find the 5a table, for the presence counts
gcm        <- "gfdl-esm4"         # for the scenario figures (as in the future raster file names)
scenarios  <- c(ssp126 = "SSP1-2.6", ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")      # one panel each, in this order
periods    <- c("2011-2040", "2041-2070", "2071-2100")
baseline_period <- "1981-2010"
line_cols  <- c(ssp126 = "#0072B2", ssp370 = "#E69F00", ssp585 = "#B2182B")         # each scenario's colour (light to dark by period)
col_other  <- "grey60"            # colour for a class with no colour set below
# Class colours (by CORINE code), the same as in 5a. Any class not listed gets a colour from a fallback palette.
class_cols <- c(
  "322" = "#8E6BBE",   # Moors and heathland
  "321" = "#A8D68B",   # Natural grasslands
  "324" = "#2E8B57",   # Transitional woodland-shrub
  "311" = "#0B5D2A", "312" = "#0B5D2A", "313" = "#0B5D2A",   # forests
  "333" = "#D9B382",   # Sparsely vegetated areas
  "332" = "#8A8A8A",   # Bare rocks
  "331" = "#EFE29A",   # Beaches, dunes, sands
  "335" = "#9ED8F0",   # Glaciers and perpetual snow
  "411" = "#4FB5A5", "412" = "#1B7F79",   # marshes, peat bogs
  "231" = "#F2D335", "211" = "#E0A800", "242" = "#F4A259",   # farmland
  "511" = "#4A90C8", "512" = "#1F5FA8",   # water courses, water bodies
  "112" = "#D7301F", "121" = "#B30000", "122" = "#E57373", "132" = "#8C2D04"   # built-up
)

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

# ---- Inputs -----------------------------------------------------------------
base_file    <- p("3-modelling", "Outputs", "Projection", "Scenario grid", "suitability_baseline_1981-2010.tif")
thr_file     <- p("3-modelling", "Outputs", "Projection", "Binary maps", "Tables", "thresholds.csv")
corine_file  <- p("1-download-data", "Downloads", "CORINE", "corine_2018_iceland.gpkg")
out_dir      <- p("3-modelling", "Outputs", "CORINE")
fig_dir      <- file.path(out_dir, "Figures")
tab_dir      <- file.path(out_dir, "Tables")
for (d in c(fig_dir, tab_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
pres_file    <- file.path(tab_dir, sprintf("5a_class_comparison_%dkm.csv", thin_km))

if (!file.exists(base_file))   stop("Missing input: ", base_file, "\nRun 2a-scenario-change-grid.R first.")
if (!file.exists(thr_file))    stop("Missing input: ", thr_file, "\nRun 2b-binary-maps.R first.")
if (!file.exists(corine_file)) stop("Missing input: ", corine_file, "\nRun corine-landcover-download-iceland.R first.")

thr_tab <- read.csv(thr_file, stringsAsFactors = FALSE)
if (!(threshold_method %in% thr_tab$method)) stop("No threshold \"", threshold_method, "\" in thresholds.csv.")
thr <- thr_tab$threshold[thr_tab$method == threshold_method]
thr_label <- if (threshold_method == "maxsss") "MaxSSS" else threshold_method
message("Threshold (", threshold_method, "): ", round(thr, 4))

suit <- rast(base_file)

# ---- CORINE polygons ----------------------------------------------------------------
cv <- terra::vect(corine_file)
cd <- as.data.frame(cv)
code_col <- names(cd)[grepl("^code_?18$", names(cd), ignore.case = TRUE)][1]
if (is.na(code_col)) code_col <- names(cd)[grepl("^code", names(cd), ignore.case = TRUE)][1]
if (is.na(code_col)) stop("Couldn't find the CORINE code column. Columns are: ", paste(names(cd), collapse = ", "))
name_col <- names(cd)[grepl("^code_text_en$", names(cd), ignore.case = TRUE)][1]
clc <- as.character(as.integer(as.character(cd[[code_col]])))
nm_tab <- if (!is.na(name_col)) tapply(as.character(cd[[name_col]]), clc, function(x) x[1]) else NULL
message("CORINE polygons: ", format(nrow(cd), big.mark = ","), "   code column: ", code_col)

land_poly <- !clc %in% exclude_codes
cv  <- cv[land_poly, ]
clc <- clc[land_poly]
codes <- sort(unique(clc))
K <- length(codes)
cv$idx <- match(clc, codes)
cv <- terra::makeValid(cv)                          # some polygons in the download are self-intersecting
cv <- terra::project(cv, crs(suit))
class_name <- function(code) {
  nm <- if (!is.null(nm_tab)) unname(nm_tab[code]) else rep(NA_character_, length(code))
  nm[is.na(nm)] <- "(unnamed class)"
  nm
}
message(K, " land classes.")

# ---- Class fractions in every suitability cell ---------------------------------------------
fine_tpl <- terra::rast(terra::ext(suit), resolution = terra::res(suit) / fine_factor, crs = crs(suit))
message("Drawing the polygons on a grid ", fine_factor, "x finer than the suitability grid (",
        format(terra::ncell(fine_tpl), big.mark = ","), " cells); this can take a few minutes...")
cls_fine <- terra::rasterize(cv, fine_tpl, field = "idx", background = 0)

w <- terra::cellSize(suit, unit = "km", mask = FALSE)          # km2 of each suitability cell
v <- data.frame(suit = terra::values(suit, mat = FALSE), w = terra::values(w, mat = FALSE))
keep <- is.finite(v$suit)
cell_idx <- which(keep)
v <- v[keep, ]
if (nrow(v) == 0) stop("The suitability raster has no values.")

F <- matrix(0, nrow = nrow(v), ncol = K, dimnames = list(NULL, codes))     # fraction of each cell in each class
km2_class_total <- numeric(K)                                              # class area over ALL cells, with or without suitability
w_all <- terra::values(w, mat = FALSE)
for (k in seq_len(K)) {
  fk <- terra::aggregate(cls_fine == k, fact = fine_factor, fun = "mean")
  if (!isTRUE(terra::compareGeom(suit, fk, stopOnError = FALSE))) stop("The class fractions did not line up with the suitability grid.")
  fv <- terra::values(fk, mat = FALSE)
  fv[is.na(fv)] <- 0
  F[, k] <- fv[cell_idx]
  km2_class_total[k] <- sum(fv * w_all)
}
Fw <- F * v$w                                                              # km2 of each class in each cell
km2_land <- colSums(Fw)                                                    # class area that has a suitability value
cover_pct <- 100 * sum(km2_land) / sum(v$w)
message(sprintf("\nCORINE land classes cover %.1f%% of the area of the cells that have a suitability value (the rest is sea, or cells where CORINE and the land mask disagree).", cover_pct))
lost <- sum(km2_class_total) - sum(km2_land)
message(sprintf("CORINE land area with no suitability value: %s km2 (%.1f%% of CORINE land area).",
                format(round(lost), big.mark = ","), 100 * lost / sum(km2_class_total)))

# ---- Numbers for one suitability map ------------------------------------------------------
case_stats <- function(ssp, scenario, period, sv) {              # sv = suitability value of each kept cell (NA = no value)
  suitable <- !is.na(sv) & sv >= thr
  ks   <- as.numeric(crossprod(Fw, as.numeric(suitable)))        # km2 suitable in each class
  msv  <- as.numeric(crossprod(Fw, ifelse(is.na(sv), 0, sv))) / pmax(km2_land, 1e-9)
  data.frame(ssp = ssp, scenario = scenario, period = period, code = codes, class_name = class_name(codes),
             km2_land = km2_land, km2_suitable = ks,
             pct_of_class_suitable = ifelse(km2_land > 0, 100 * ks / km2_land, NA_real_),
             pct_of_suitable_area = if (sum(ks) > 0) 100 * ks / sum(ks) else NA_real_,
             pct_of_land_area = 100 * km2_land / sum(km2_land),
             mean_suitability = msv, stringsAsFactors = FALSE)
}
wquant <- function(x, wt, probs) {
  ok <- is.finite(x) & wt > 0
  x <- x[ok]; wt <- wt[ok]
  if (length(x) == 0) return(rep(NA_real_, length(probs)))
  o <- order(x); cw <- cumsum(wt[o]) / sum(wt)
  vapply(probs, function(q) x[o][which(cw >= q)[1]], numeric(1))
}

# ---- Current habitat --------------------------------------------------------------------
now <- case_stats("baseline", "Baseline", baseline_period, v$suit)
qs  <- t(vapply(seq_len(K), function(k) wquant(v$suit, Fw[, k], c(0.1, 0.25, 0.5, 0.75, 0.9)), numeric(5)))
colnames(qs) <- c("q10", "q25", "median", "q75", "q90")
now <- cbind(now, qs)
tot_land <- sum(km2_land); tot_suit <- sum(now$km2_suitable)
now$ratio_suitable_to_land <- now$pct_of_suitable_area / now$pct_of_land_area
if (!file.exists(pres_file)) stop("Missing input: ", pres_file, "\nRun 5a-classify-presences-by-corine.R first (it gives the number of presences in each class).")
pt <- read.csv(pres_file, stringsAsFactors = FALSE, colClasses = c(code = "character"))
now$n_points <- pt$n_points[match(now$code, pt$code)]
now$n_points[is.na(now$n_points)] <- 0                  # a class 5a didn't list has no presences
now_out <- now[order(-now$km2_land), ]
for (cn in c("km2_land", "km2_suitable")) now_out[[cn]] <- round(now_out[[cn]])
for (cn in c("pct_of_class_suitable", "pct_of_suitable_area", "pct_of_land_area", "ratio_suitable_to_land")) now_out[[cn]] <- round(now_out[[cn]], 2)
for (cn in c("mean_suitability", colnames(qs))) now_out[[cn]] <- round(now_out[[cn]], 3)
now_out$ssp <- NULL; now_out$scenario <- NULL; now_out$period <- NULL
write.csv(now_out, file.path(tab_dir, paste0("5b_suitability_by_class_", threshold_method, ".csv")), row.names = FALSE)

message(sprintf("\nSuitable area: %s km2 (%.0f%% of %s km2 of land that has both a class and a suitability value)",
                format(round(tot_suit), big.mark = ","), 100 * tot_suit / tot_land, format(round(tot_land), big.mark = ",")))
message("\n=== Current habitat by CORINE class ===")
print(now_out[now_out$km2_land >= min_km2 & now_out$n_points >= min_points, c("code", "class_name", "n_points", "km2_land", "pct_of_land_area", "pct_of_class_suitable", "pct_of_suitable_area", "median")], row.names = FALSE)

# ---- Scenarios ------------------------------------------------------------------------------
fut_file <- function(s, per) file.path(dirname(base_file), sprintf("suitability_%s_%s_%s.tif", gcm, s, per))
fut_all  <- unlist(lapply(names(scenarios), function(s) vapply(periods, function(per) fut_file(s, per), character(1))))
if (any(!file.exists(fut_all))) {
  stop(sum(!file.exists(fut_all)), " future suitability raster(s) not found, for example:\n  ",
       paste(utils::head(fut_all[!file.exists(fut_all)], 4), collapse = "\n  "), "\nRun 2a-scenario-change-grid.R first.")
}
cases <- list(baseline = now[, names(case_stats("a", "a", "a", v$suit))])
for (s in names(scenarios)) for (per in periods) {
  r <- rast(fut_file(s, per))
  if (!isTRUE(compareGeom(suit, r, stopOnError = FALSE))) stop("Raster ", basename(fut_file(s, per)), " is not on the same grid as the baseline.")
  sv <- terra::values(r, mat = FALSE)[cell_idx]
  if (anyNA(sv)) message("Note: ", sum(is.na(sv)), " cell(s) have no value in ", basename(fut_file(s, per)), "; counted as unsuitable (and as zero in the average suitability).")
  cases[[paste(s, per, sep = "_")]] <- case_stats(s, scenarios[[s]], per, sv)
}
case_tab <- do.call(rbind, cases); rownames(case_tab) <- NULL
base_ks  <- stats::setNames(cases[["baseline"]]$km2_suitable, codes)
base_ms  <- stats::setNames(cases[["baseline"]]$mean_suitability, codes)
case_tab$change_km2_vs_baseline <- case_tab$km2_suitable - base_ks[case_tab$code]
case_tab$change_mean_suitability_vs_baseline <- case_tab$mean_suitability - base_ms[case_tab$code]
case_tab$n_points <- now$n_points[match(case_tab$code, now$code)]
case_out <- case_tab
for (cn in c("km2_land", "km2_suitable", "change_km2_vs_baseline")) case_out[[cn]] <- round(case_out[[cn]])
for (cn in c("pct_of_class_suitable", "pct_of_suitable_area", "pct_of_land_area")) case_out[[cn]] <- round(case_out[[cn]], 2)
for (cn in c("mean_suitability", "change_mean_suitability_vs_baseline")) case_out[[cn]] <- round(case_out[[cn]], 3)
write.csv(case_out, file.path(tab_dir, paste0("5b_suitability_by_scenario_", threshold_method, ".csv")), row.names = FALSE)

tot_by_case <- tapply(case_tab$km2_suitable, paste(case_tab$ssp, case_tab$period), sum)
message("\n=== Total suitable area (km2), baseline and projections ===")
print(round(tot_by_case))

# ---- Which classes go in the figures --------------------------------------------------------
show   <- now$km2_land >= min_km2 & now$n_points >= min_points
if (!any(show)) stop("No class has at least min_km2 = ", min_km2, " km2 of land and at least min_points = ", min_points, " presences.")
ord    <- order(-now$km2_land)
ord    <- ord[show[ord]]
n_rows <- length(ord)
y_pos  <- rev(seq_len(n_rows))                          # the biggest class at the top
labels <- substr(paste(now$code[ord], now$class_name[ord]), 1, 38)
cls_col <- stats::setNames(rep(NA_character_, K), codes)
kn <- intersect(codes, names(class_cols)); cls_col[kn] <- class_cols[kn]
mi <- names(cls_col)[is.na(cls_col)]
if (length(mi) > 0) cls_col[mi] <- grDevices::hcl.colors(length(mi), "Dark 3")
bar_col <- unname(cls_col[now$code[ord]])
n_hidden <- sum(!show)
foot <- if (n_hidden > 0) sprintf("%d class(es) with under %g km2 of land or fewer than %g presences are left out of the figures.", n_hidden, min_km2, min_points) else ""

# ---- Figure 1: current habitat -----------------------------------------------------------------
png(file.path(fig_dir, paste0("5b_current_suitability_", threshold_method, ".png")), width = 3200, height = max(1800, 700 + 190 * n_rows), res = 300)
layout(matrix(1:3, nrow = 1), widths = c(1.9, 1, 1))
par(oma = c(0, 0, 3.5, 0))
gridlines <- function(at) abline(v = at, col = "grey92", lwd = 0.8)

# Left: share of suitable area vs share of land area
sl <- now$pct_of_land_area[ord]; ss <- now$pct_of_suitable_area[ord]
xmax <- max(c(sl, ss), na.rm = TRUE) * 1.25
par(mar = c(4.2, 14.5, 2.2, 0.8))
plot.new(); plot.window(xlim = c(0, xmax), ylim = c(0.4, n_rows + 0.6), xaxs = "i", yaxs = "i")
gridlines(pretty(c(0, xmax)))
rect(0, y_pos + 0.03, sl, y_pos + 0.42, col = adjustcolor(bar_col, 0.4), border = bar_col, lwd = 0.8)
rect(0, y_pos - 0.42, ss, y_pos - 0.03, col = bar_col, border = NA)
text(sl + xmax * 0.01, y_pos + 0.225, sprintf("%.1f%%", sl), adj = 0, cex = 0.5, col = "grey35")
text(ss + xmax * 0.01, y_pos - 0.225, sprintf("%.1f%%", ss), adj = 0, cex = 0.5, col = "grey15")
axis(1, cex.axis = 0.8); axis(2, at = y_pos, labels = labels, las = 1, tick = FALSE, cex.axis = 0.7)
mtext("Share of total (%)", side = 1, line = 2.4, cex = 0.8)
legend("bottomright", legend = c("Share of land (pale)", "Share of suitable habitat (solid)"),
       fill = c(adjustcolor("grey40", 0.4), "grey40"), border = c("grey40", NA), bty = "n", cex = 0.7)
mtext("Where the suitable habitat is", side = 3, line = 0.4, adj = 0, font = 2, cex = 0.8)

# Middle: percentage of each class that is suitable
pc <- now$pct_of_class_suitable[ord]
par(mar = c(4.2, 0.8, 2.2, 0.8))
plot.new(); plot.window(xlim = c(0, 108), ylim = c(0.4, n_rows + 0.6), xaxs = "i", yaxs = "i")
gridlines(seq(0, 100, 25))
rect(0, y_pos - 0.38, pc, y_pos + 0.38, col = bar_col, border = NA)
text(pc + 1.5, y_pos, sprintf("%.0f%%", pc), adj = 0, cex = 0.55, col = "grey15")
abline(v = 100 * tot_suit / tot_land, lty = 2, col = "grey30", lwd = 1.4)
axis(1, at = seq(0, 100, 25), cex.axis = 0.8)
mtext("Class that is suitable (%)", side = 1, line = 2.4, cex = 0.8)
mtext(sprintf("Dashed: all land, %.0f%%", 100 * tot_suit / tot_land), side = 1, line = 3.3, cex = 0.6, col = "grey30")
mtext("How much of each class is suitable", side = 3, line = 0.4, adj = 0, font = 2, cex = 0.8)

# Right: spread of suitability values in each class
xr <- c(0, max(1, max(now$q90[ord], na.rm = TRUE)))
par(mar = c(4.2, 0.8, 2.2, 0.8))
plot.new(); plot.window(xlim = xr, ylim = c(0.4, n_rows + 0.6), xaxs = "i", yaxs = "i")
gridlines(pretty(xr))
segments(now$q10[ord], y_pos, now$q90[ord], y_pos, lwd = 1.4, col = "grey40")
segments(now$q25[ord], y_pos, now$q75[ord], y_pos, lwd = 6, col = bar_col, lend = 1)
points(now$median[ord], y_pos, pch = 21, bg = "white", col = "grey15", cex = 0.9)
abline(v = thr, lty = 2, col = "grey30", lwd = 1.4)
axis(1, cex.axis = 0.8)
mtext("Suitability", side = 1, line = 2.4, cex = 0.8)
mtext(sprintf("Dashed: %s threshold, %.2f", thr_label, thr), side = 1, line = 3.3, cex = 0.6, col = "grey30")
mtext("Spread of suitability in the class", side = 3, line = 0.4, adj = 0, font = 2, cex = 0.8)

mtext(sprintf("Current (%s) crowberry habitat suitability by CORINE 2018 class (%s threshold)", baseline_period, thr_label),
      outer = TRUE, side = 3, line = 1.7, font = 2, cex = 1)
mtext(paste("Right panel: thick bar = 25th-75th percentile of the suitability values in the class, thin = 10th-90th, circle = median.", foot),
      outer = TRUE, side = 3, line = 0.4, cex = 0.6, col = "grey30")
invisible(dev.off())

# ---- Scenario figures ---------------------------------------------------------------------------
mix_white <- function(col, a) {
  m <- grDevices::col2rgb(col)[, 1] / 255
  grDevices::rgb(m[1] * (1 - a) + a, m[2] * (1 - a) + a, m[3] * (1 - a) + a)
}
n_scen <- length(scenarios)
case_of <- function(s, per) cases[[paste(s, per, sep = "_")]]

# Figure 2: % of each class that is suitable
png(file.path(fig_dir, paste0("5b_suitability_overlay_", threshold_method, ".png")), width = 2800, height = max(3000, n_scen * (500 + 130 * n_rows)), res = 300)
layout(matrix(seq_len(n_scen), ncol = 1))
par(oma = c(3.2, 0, 3.6, 0))
yb <- cases[["baseline"]]$pct_of_class_suitable[match(now$code[ord], codes)]
for (i in seq_len(n_scen)) {
  s <- names(scenarios)[i]
  par(mar = c(1.2, 14.5, 1.8, 1))
  plot.new(); plot.window(xlim = c(0, 100), ylim = c(0.4, n_rows + 0.6), xaxs = "i", yaxs = "i")
  gridlines(seq(0, 100, 25))
  rect(0, y_pos - 0.4, yb, y_pos + 0.4, col = adjustcolor("grey55", 0.55), border = NA)
  fill_c <- mix_white(line_cols[[s]], c(0.6, 0.3, 0)); out_c <- mix_white(line_cols[[s]], c(0.35, 0.15, 0))
  for (j in seq_along(periods)) {
    y <- case_of(s, periods[j])$pct_of_class_suitable[match(now$code[ord], codes)]
    rect(0, y_pos - 0.4, y, y_pos + 0.4, col = adjustcolor(fill_c[j], 0.45), border = out_c[j], lwd = 1.1)
  }
  axis(2, at = y_pos, labels = labels, las = 1, tick = FALSE, cex.axis = 0.7)
  axis(1, at = seq(0, 100, 25), labels = (i == n_scen))
  legend("bottomright", legend = c(sprintf("Baseline %s", baseline_period), periods),
         fill = c(adjustcolor("grey55", 0.55), adjustcolor(fill_c, 0.45)), border = c("grey55", out_c), bty = "n", cex = 0.8)
  mtext(scenarios[[s]], side = 3, line = 0.3, adj = 0, font = 2, cex = 0.95)
}
mtext("Class that is suitable (%)", side = 1, outer = TRUE, line = 1.8, cex = 0.9)
mtext(sprintf("Share of each CORINE class that is suitable: baseline and projected (%s, %s threshold)", toupper(gcm), thr_label),
      outer = TRUE, side = 3, line = 1.8, font = 2, cex = 0.95)
if (nzchar(foot)) mtext(foot, outer = TRUE, side = 3, line = 0.5, cex = 0.6, col = "grey30")
invisible(dev.off())

# Figure 3: change in suitable area (km2) from the baseline. Only the classes that make up at least min_pct_land_change %
# of Iceland's land are shown, and the three scenarios are side by side (one panel each) on a shared x-axis scale.
ord_c <- order(-now$km2_land)
ord_c <- ord_c[now$pct_of_land_area[ord_c] >= min_pct_land_change]
if (length(ord_c) == 0) stop("No class makes up at least min_pct_land_change = ", min_pct_land_change, "% of the land.")
n_rows_c <- length(ord_c)
y_pos_c  <- rev(seq_len(n_rows_c))
labels_c <- substr(paste(now$code[ord_c], now$class_name[ord_c]), 1, 38)
message(sprintf("\nFigures 3 and 4 show the %d classes that make up at least %g%% of Iceland's land (together %.1f%% of it):\n  %s",
                n_rows_c, min_pct_land_change, sum(now$pct_of_land_area[ord_c]), paste(labels_c, collapse = "\n  ")))
if (any(now$n_points[ord_c] < 5)) message("Note: some of these classes have fewer than 5 presences (n_points in the csv files): ",
                                          paste(labels_c[now$n_points[ord_c] < 5], collapse = "; "))
foot_c <- sprintf("Only the %d classes that each make up at least %g%% of Iceland's land are shown (%.0f%% of the land together).",
                  n_rows_c, min_pct_land_change, sum(now$pct_of_land_area[ord_c]))

chg <- lapply(names(scenarios), function(s) vapply(periods, function(per) {
  case_of(s, per)$km2_suitable[match(now$code[ord_c], codes)] - cases[["baseline"]]$km2_suitable[match(now$code[ord_c], codes)]
}, numeric(n_rows_c)))
names(chg) <- names(scenarios)
xl <- range(c(0, unlist(chg))); if (diff(xl) == 0) xl <- c(-1, 1)
xl <- xl + c(-1, 1) * diff(xl) * 0.06
png(file.path(fig_dir, paste0("5b_suitable_area_change_", threshold_method, ".png")), width = 4200, height = max(1500, 800 + 170 * n_rows_c), res = 300)
layout(matrix(seq_len(n_scen), nrow = 1), widths = c(1.6, rep(1, n_scen - 1)))
par(oma = c(1.8, 0, 4, 0))
off <- c(0.27, 0, -0.27)
for (i in seq_len(n_scen)) {
  s <- names(scenarios)[i]
  par(mar = c(3.2, if (i == 1) 14.5 else 1, 1.8, 1))
  plot.new(); plot.window(xlim = xl, ylim = c(0.4, n_rows_c + 0.6), xaxs = "i", yaxs = "i")
  gridlines(pretty(xl))
  fill_c <- mix_white(line_cols[[s]], c(0.6, 0.3, 0))
  for (j in seq_along(periods)) {
    rect(0, y_pos_c + off[j] - 0.12, chg[[s]][, j], y_pos_c + off[j] + 0.12, col = fill_c[j], border = NA)
  }
  abline(v = 0, col = "grey20", lwd = 1.2)
  if (i == 1) axis(2, at = y_pos_c, labels = labels_c, las = 1, tick = FALSE, cex.axis = 0.7)
  axis(1, cex.axis = 0.8)
  legend("bottomright", legend = periods, fill = fill_c, border = NA, bty = "n", cex = 0.8)
  mtext(scenarios[[s]], side = 3, line = 0.3, adj = 0, font = 2, cex = 0.95)
}
mtext("Change in suitable area from the baseline (km2)", side = 1, outer = TRUE, line = 0.3, cex = 0.9)
mtext(sprintf("Change in suitable area by CORINE class, relative to %s (%s, %s threshold)", baseline_period, toupper(gcm), thr_label),
      outer = TRUE, side = 3, line = 2.3, font = 2, cex = 0.95)
mtext(foot_c, outer = TRUE, side = 3, line = 1.2, cex = 0.55, col = "grey30")
mtext("Classes are held at their 2018 extent: this is climate becoming suitable where the class is now, not a forecast of the class itself.",
      outer = TRUE, side = 3, line = 0.4, cex = 0.55, col = "grey30")
invisible(dev.off())

# Figure 4: change in AVERAGE suitability from the baseline. Same classes, same layout and same x-axis logic as figure 3,
# with one extra narrow panel on the left showing each class's average suitability in the baseline against the threshold.
# The average is of the continuous suitability values (no threshold), so it also moves when cells change but stay on the
# same side of the threshold.
msv_base <- cases[["baseline"]]$mean_suitability[match(now$code[ord_c], codes)]
chg_m <- lapply(names(scenarios), function(s) vapply(periods, function(per) {
  case_of(s, per)$mean_suitability[match(now$code[ord_c], codes)] - msv_base
}, numeric(n_rows_c)))
names(chg_m) <- names(scenarios)
xl_m <- range(c(0, unlist(chg_m)), na.rm = TRUE); if (diff(xl_m) == 0) xl_m <- c(-0.01, 0.01)
xl_m <- xl_m + c(-1, 1) * diff(xl_m) * 0.06
xb_m <- c(0, max(c(msv_base, thr), na.rm = TRUE) * 1.25)
bar_col_c <- unname(cls_col[now$code[ord_c]])
# Text sizes for this figure. The class labels are 50% bigger than before (0.7 -> 1.05) and the period keys (2011-2040 etc.)
# 25% bigger (0.8 -> 1.0). The bigger labels need a wider left margin, so the left margin is wider (14.5 -> 17 lines) and the
# figure and the left panel are made wider by about the same amount, which keeps the bars the size they were.
label_cex  <- 0.7 * 1.5       # class labels on the left
legend_cex <- 0.8 * 1.25      # the 2011-2040 / 2041-2070 / 2071-2100 keys in the scenario panels
label_mar  <- 17              # left margin of the first panel, in lines; if a label is cut off at the left edge, raise this
fig4_w <- 3740                # px (it was 3640)
png(file.path(fig_dir, paste0("5b_suitability_change_", threshold_method, ".png")), width = fig4_w, height = max(1500, 800 + 170 * n_rows_c), res = 300)
layout(matrix(seq_len(n_scen + 1), nrow = 1), widths = c(1.83, rep(1, n_scen)))    # the left panel's label margin is a fixed size, so it gets extra width to keep its bars visible
par(oma = c(0.6, 0, 4, 0))

# Left: average suitability of each class in the baseline, next to the threshold
par(mar = c(4.4, label_mar, 1.8, 1))
plot.new(); plot.window(xlim = xb_m, ylim = c(0.4, n_rows_c + 0.6), xaxs = "i", yaxs = "i")
gridlines(pretty(xb_m))
rect(0, y_pos_c - 0.3, msv_base, y_pos_c + 0.3, col = bar_col_c, border = NA)
text(msv_base + diff(xb_m) * 0.015, y_pos_c, sprintf("%.2f", msv_base), adj = 0, cex = 0.55, col = "grey15")
abline(v = thr, lty = 2, col = "grey30", lwd = 1.4)
axis(2, at = y_pos_c, labels = labels_c, las = 1, tick = FALSE, cex.axis = label_cex)
axis(1, cex.axis = 0.8)
mtext("Average suitability", side = 1, line = 2.4, cex = 0.8)
mtext(sprintf("Dashed: %s threshold, %.2f", thr_label, thr), side = 1, line = 3.3, cex = 0.6, col = "grey30")
mtext(sprintf("Baseline (%s)", baseline_period), side = 3, line = 0.3, adj = 0, font = 2, cex = 0.85)

# One panel per scenario: change in average suitability, three periods
for (i in seq_len(n_scen)) {
  s <- names(scenarios)[i]
  par(mar = c(4.4, 1, 1.8, 1))
  plot.new(); plot.window(xlim = xl_m, ylim = c(0.4, n_rows_c + 0.6), xaxs = "i", yaxs = "i")
  gridlines(pretty(xl_m))
  fill_c <- mix_white(line_cols[[s]], c(0.6, 0.3, 0))
  for (j in seq_along(periods)) {
    rect(0, y_pos_c + off[j] - 0.12, chg_m[[s]][, j], y_pos_c + off[j] + 0.12, col = fill_c[j], border = NA)
  }
  abline(v = 0, col = "grey20", lwd = 1.2)
  axis(1, cex.axis = 0.8)
  mtext("Change in average suitability", side = 1, line = 2.4, cex = 0.8)
  legend("bottomright", legend = periods, fill = fill_c, border = NA, bty = "n", cex = legend_cex)
  mtext(scenarios[[s]], side = 3, line = 0.3, adj = 0, font = 2, cex = 0.95)
}
mtext(sprintf("Change in average suitability by CORINE class, relative to %s (%s)", baseline_period, toupper(gcm)),
      outer = TRUE, side = 3, line = 2.3, font = 2, cex = 0.95)
mtext(paste(foot_c, "No threshold is applied: average of the suitability values over the class (cells weighted by how much of them is in the class)."),
      outer = TRUE, side = 3, line = 1.2, cex = 0.55, col = "grey30")
mtext("Classes are held at their 2018 extent: this is climate becoming more or less suitable where the class is now, not a forecast of the class itself.",
      outer = TRUE, side = 3, line = 0.4, cex = 0.55, col = "grey30")
invisible(dev.off())

message("\nSaved: ", file.path(fig_dir, paste0("5b_current_suitability_", threshold_method, ".png")))
message("Saved: ", file.path(fig_dir, paste0("5b_suitability_overlay_", threshold_method, ".png")))
message("Saved: ", file.path(fig_dir, paste0("5b_suitable_area_change_", threshold_method, ".png")))
message("Saved: ", file.path(fig_dir, paste0("5b_suitability_change_", threshold_method, ".png")))
message("Saved: ", file.path(tab_dir, paste0("5b_suitability_by_class_", threshold_method, ".csv")))
message("Saved: ", file.path(tab_dir, paste0("5b_suitability_by_scenario_", threshold_method, ".csv")))