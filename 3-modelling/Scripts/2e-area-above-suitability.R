# 2e-area-above-suitability.R
# One figure with three line plots side by side, one per scenario (SSP1-2.6, SSP3-7.0, SSP5-8.5). In each plot the
# x-axis is a suitability threshold (0 to 1) and the y-axis is the proportion of Iceland's land area with suitability
# at or above that threshold. Four lines per plot: the 1981-2010 baseline (the same line in all three plots) and the
# three future periods, 2011-2040, 2041-2070 and 2071-2100. A curve that sits above the baseline curve means more area
# at that suitability level than at baseline, below means less.
#
# The three plots are lettered (a), (b), (c), centred under each (panel_letters / letter_fmt in the settings).
#
# Shading: the area between the baseline curve and one chosen future period (default 2071-2100, set by
# `shade_period`) is filled green where that period has MORE area at or above the threshold than the baseline and
# purple where it has LESS (same colours as the change maps in 2a). Set `shade_period <- NA` for no shading.
#
# Put this file in: ISP-Crowberry-SDM/3-modelling/Scripts/   (run 2a first)
# Reads:  3-modelling/Outputs/Projection/Scenario grid/suitability_baseline_1981-2010.tif
#         3-modelling/Outputs/Projection/Scenario grid/suitability_<gcm>_<ssp>_<period>.tif   (the 9 from 2a)
# Writes: 3-modelling/Outputs/Projection/Area above suitability/
#           area_above_suitability.png               the figure
#           area_above_suitability_curves.csv        the curves: one row per scenario, period and threshold
#           area_above_suitability_summary.csv       proportion and area (km2) at the thresholds in summary_thresholds
# Inputs are only read, never changed.
#
# How it works: for each suitability raster, each land cell is weighted by its area (cells are WGS84 degrees, so they
# are not equal area), and the curve is the share of the total land area in cells whose suitability is at or above
# each threshold. The proportion is out of all cells with a suitability value, which is the same land area in every
# raster, so the curves are directly comparable. At threshold 0 every curve starts at 1.
#
# Limits: one climate model (GFDL-ESM4); suitability is a relative index, not abundance, and the curves say how much
# area reaches a suitability level, not where it is. Area above a threshold is not the same as area of suitable
# habitat unless the threshold is a meaningful presence cutoff.
#
# NOT TESTED on your data. Check the console messages and the figure before using the output.

# install.packages("terra")
library(terra)

# ---- Settings ---------------------------------------------------------------
gcm        <- "gfdl-esm4"
scenarios  <- c(ssp126 = "SSP1-2.6", ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")   # panels, left to right
periods    <- c("2011-2040", "2041-2070", "2071-2100")
step       <- 0.01                       # spacing of the suitability thresholds on the x-axis
shade_period <- "2071-2100"              # period shaded against the baseline; NA = no shading
summary_thresholds <- c(0.25, 0.5, 0.75) # thresholds reported in area_above_suitability_summary.csv
panel_letters <- letters                 # letter centred under each plot: a, b, c (LETTERS for A, B, C)
letter_fmt    <- "(%s)"                  # "(%s)" gives (a); "%s" gives a
letter_cex    <- 1.32                    # size of the panel letters (was 1.1, now 20% bigger)
axis_title_cex <- 1.584                  # x and y axis titles (was 1.32, now 20% bigger)
tick_cex       <- 1.584                  # axis numbers 0.0, 0.2, ... (was 1.32, now 20% bigger)
panel_title_cex <- 1.8876                # SSP1-2.6 etc. above each plot (was 1.573, now 20% bigger)
legend_cex     <- 1.2375                 # legend (was 1.125, now 10% bigger)
figure_title   <- "Share of area above each suitability level"
figure_title_cex <- 1.452                # overall figure title (was 1.32, now 10% bigger)

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
scen_dir <- p("3-modelling", "Outputs", "Projection", "Scenario grid")
out_dir  <- p("3-modelling", "Outputs", "Projection", "Area above suitability")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

base_file <- file.path(scen_dir, "suitability_baseline_1981-2010.tif")
fut_file  <- function(s, per) file.path(scen_dir, sprintf("suitability_%s_%s_%s.tif", gcm, s, per))
needed <- c(base_file, unlist(lapply(names(scenarios), function(s) vapply(periods, function(per) fut_file(s, per), character(1)))))
if (any(!file.exists(needed))) {
  stop(sum(!file.exists(needed)), " suitability raster(s) not found, for example:\n  ",
       paste(utils::head(needed[!file.exists(needed)], 5), collapse = "\n  "),
       "\nRun 2a-scenario-change-grid.R first.")
}

# ---- Curves -----------------------------------------------------------------
thr <- seq(0, 1, by = step)

# Area-weighted share of land area at or above each threshold.
curve_of <- function(file) {
  r  <- rast(file)
  w  <- terra::cellSize(r, unit = "km", mask = TRUE)
  v  <- terra::values(r, mat = FALSE)
  wv <- terra::values(w, mat = FALSE)
  ok <- is.finite(v) & is.finite(wv)
  v <- v[ok]; wv <- wv[ok]
  o <- order(v); v <- v[o]; wv <- wv[o]
  total <- sum(wv)
  cw    <- rev(cumsum(rev(wv)))                       # area in cells with suitability >= v[i]
  idx   <- findInterval(thr, v, left.open = TRUE) + 1   # first cell with suitability >= threshold
  area  <- ifelse(idx > length(v), 0, cw[pmin(idx, length(v))])
  list(area_km2 = area, prop = area / total, total_km2 = total)
}

message("Baseline ...")
curves <- list(baseline = curve_of(base_file))
for (s in names(scenarios)) for (per in periods) {
  message(sprintf("%s %s ...", scenarios[[s]], per))
  curves[[paste(s, per, sep = "_")]] <- curve_of(fut_file(s, per))
}
tot <- vapply(curves, function(x) x$total_km2, numeric(1))
if (diff(range(tot)) / mean(tot) > 0.001) warning("The rasters do not cover the same land area (total land area differs by more than 0.1%); the proportions are out of different totals.")
message(sprintf("Land area covered: %.0f km2 (baseline)", curves$baseline$total_km2))

# ---- Tables -----------------------------------------------------------------
curve_rows <- function(scenario, period, cv) data.frame(scenario = scenario, period = period, threshold = thr,
                                                        proportion_of_area = round(cv$prop, 5), area_km2 = round(cv$area_km2, 1),
                                                        stringsAsFactors = FALSE)
curve_tab <- rbind(
  curve_rows("All (baseline)", "1981-2010", curves$baseline),
  do.call(rbind, lapply(names(scenarios), function(s) do.call(rbind, lapply(periods, function(per)
    curve_rows(scenarios[[s]], per, curves[[paste(s, per, sep = "_")]]))))))
write.csv(curve_tab, file.path(out_dir, "area_above_suitability_curves.csv"), row.names = FALSE)

summ_tab <- curve_tab[curve_tab$threshold %in% summary_thresholds, ]
baseline_prop <- setNames(curves$baseline$prop, thr)
summ_tab$change_in_proportion_vs_baseline <- round(summ_tab$proportion_of_area - baseline_prop[as.character(summ_tab$threshold)], 5)
rownames(summ_tab) <- NULL
write.csv(summ_tab, file.path(out_dir, "area_above_suitability_summary.csv"), row.names = FALSE)
message("\n=== Area at or above selected suitability thresholds ===")
print(summ_tab, row.names = FALSE)

# ---- Figure -----------------------------------------------------------------
line_cols <- c("1981-2010 (baseline)" = "black",
               setNames(hcl.colors(5, "YlOrRd", rev = TRUE)[3:5], periods))
gain_col <- adjustcolor(hcl.colors(100, "Purple-Green")[85], alpha.f = 0.45)   # more area than baseline
loss_col <- adjustcolor(hcl.colors(100, "Purple-Green")[15], alpha.f = 0.45)   # less area than baseline

draw_figure <- function() {
  par(mfrow = c(1, 3), oma = c(0, 0, 3, 0), mar = c(6.0, 4.8, 2.2, 1.2), mgp = c(2.6, 0.7, 0))   # bottom margin leaves room for the panel letter; left/right widened for the larger text
  for (i in seq_along(scenarios)) {
    s <- names(scenarios)[i]
    plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "",
         ylab = "",
         main = scenarios[[s]], cex.main = panel_title_cex, cex.lab = axis_title_cex, cex.axis = tick_cex,
         las = 1, xaxs = "i", yaxs = "i")
    grid(col = "grey88", lty = 1)
    
    if (!is.na(shade_period)) {
      base <- curves$baseline$prop
      fut  <- curves[[paste(s, shade_period, sep = "_")]]$prop
      polygon(c(thr, rev(thr)), c(pmax(fut, base), rev(base)), col = gain_col, border = NA)
      polygon(c(thr, rev(thr)), c(pmin(fut, base), rev(base)), col = loss_col, border = NA)
    }
    for (per in periods) lines(thr, curves[[paste(s, per, sep = "_")]]$prop, col = line_cols[[per]], lwd = 2.5)
    lines(thr, curves$baseline$prop, col = line_cols[[1]], lwd = 3)
    box()
    title(xlab = "Suitability threshold (cloglog)", line = 2.4, cex.lab = axis_title_cex)   # drawn separately from the y-axis title so each can be positioned on its own
    if (s == names(scenarios)[1]) title(ylab = "Proportion of land area at or above threshold", line = 3.0, cex.lab = axis_title_cex)   # first panel only; line 3.0 keeps it clear of the larger tick numbers
    mtext(sprintf(letter_fmt, panel_letters[i]), side = 1, line = 4.25, at = 0.5, font = 2, cex = letter_cex)   # panel letter, centred under the plot, below the x-axis title
    
    if (s == names(scenarios)[1]) {
      leg <- names(line_cols); cols <- unname(line_cols); fills <- rep(NA, length(leg)); bord <- rep(NA, length(leg))
      ltys <- rep(1, length(leg)); lwds <- c(3, rep(2.5, length(periods)))
      if (!is.na(shade_period)) {
        leg   <- c(leg, sprintf("%s: more area than baseline", shade_period), sprintf("%s: less area than baseline", shade_period))
        cols  <- c(cols, NA, NA); fills <- c(fills, gain_col, loss_col); bord <- c(bord, NA, NA)
        ltys  <- c(ltys, NA, NA); lwds <- c(lwds, NA, NA)
      }
      legend("topright", legend = leg, col = cols, lty = ltys, lwd = lwds, fill = fills, border = bord,
             bty = "n", cex = legend_cex, seg.len = 1.6, x.intersp = 0.6)
    }
  }
  mtext(figure_title, outer = TRUE, side = 3, line = 0.6, font = 2, cex = figure_title_cex)
}

png(file.path(out_dir, "area_above_suitability.png"), width = 4500, height = 1800, res = 300)
draw_figure(); invisible(dev.off())

message("\nSaved: ", file.path(out_dir, "area_above_suitability.png"))
message("Saved: ", file.path(out_dir, "area_above_suitability_curves.csv"))
message("Saved: ", file.path(out_dir, "area_above_suitability_summary.csv"))