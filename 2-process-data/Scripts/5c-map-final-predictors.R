# 5c-map-final-predictors.R
# Maps the predictors kept by the stepwise removal in 5b across Iceland, as a grid of panels (n_cols columns,
# as many rows as needed), one panel per variable, each with its own colour scale and a panel letter ((a), (b), ...). Works for any number of
# variables. Meant as the "here is what the model sees" figure before the baseline MaxEnt fit.
#
# Which variables: by default the lowest-AICc model in 5b (read from stepwise_path.csv). To map a different
# set, give the names in final_vars below.
#
# Put this file in: ISP-Crowberry-SDM/2-process-data/Scripts/   (it can move to 3-modelling/Scripts/ if you prefer)
# Reads:  2-process-data/Outputs/MaxEnt tuning/Stepwise removal/stepwise_path.csv   (from 5b; only if final_vars is NULL)
#         2-process-data/Outputs/Predictor screening/selected_predictors.csv   (gives each variable's raster path)
#         2-process-data/Outputs/<historic_file column>                         (the same rasters 5a and 5b used)
#         2-process-data/Outputs/Background points/valid_cells_mask.tif          (optional, used if present, as in 5b)
#         1-download-data/Downloads/LMI-IS50V/is50v_land_polygon.gpkg            (optional coastline outline)
# Writes: 2-process-data/Outputs/Final predictors/
#           final_predictors_map.png            the figure
#           final_predictors_summary.csv        range, plotted range and share of zero cells for each variable
# Inputs are only read, never changed.
#
# Colour scales: each panel is stretched between the 2nd and 98th percentile of its own values, so a few extreme
# cells (a handful of very rough or very exposed cells, say) don't wash out the rest of the map. Values beyond
# those limits are drawn in the end colour. The summary csv records the limits and the full min and max.
# tpi, northness and eastness are centred on 0 and use a diverging palette; everything else uses a sequential one.
# Variables that aren't in the title and palette lists below get their variable name as the title and a default
# palette, so a new variable never stops the script (add it to the lists if you want a nicer label).
#
# Blank patches in a panel: the clamped raster is stored as 32-bit floats, but the colour-scale limits are doubles
# from quantile(). A clamped cell can therefore end up a hair outside the exact limits, and terra does not draw cells
# outside `range`, so every cell clamped to the end of the scale came out blank (this showed up as white holes in
# the highest-value areas of one panel). The scale is now given a tiny tolerance (range_tol) so those cells are
# drawn in the end colour. Only the figure was affected; the rasters and the model are not touched by this script.
#
# NOT TESTED on your data. Check the figure before using it. Panels for variables that are zero over much of the
# country (the summary csv has pct_zero) will be mostly the lightest colour.

# install.packages("terra")
library(terra)

# ---- Settings ---------------------------------------------------------------
# Which variables to map. NULL = the lowest-AICc model in 5b (vars_kept in stepwise_path.csv).
# Or give the names yourself, e.g. final_vars <- c("bio03", "npp", "tpi").
# The order is the order in the figure, filled row by row (left to right).
final_vars <- NULL

n_cols          <- 4                   # panels per row; the number of rows follows from the number of variables
panel_width_px  <- 1200                # width of one column of panels in the png (the png is n_cols x this wide)
panel_height_px <- 800                 # height of one row of panels in the png
panel_letters   <- letters             # a, b, c, ... ; use LETTERS for A, B, C, ...

range_tol <- 1e-6                      # share of each panel's plotted range added at both ends of the colour scale,
# so cells clamped to a limit are not dropped by float32 rounding (see header)

# Text sizes (cex multipliers; 1 = R's default size)
legend_cex <- 1.555                    # numbers on the colour bars (was 1.04, now 50% bigger)
title_cex  <- 1.254                    # title above each map (was 1.045, now 20% bigger)
main_cex   <- 1.331                    # main title of the whole figure (was 1.21, now 10% bigger)

panel_titles <- c(
  # CHELSA-BIOCLIM+ climate variables
  bio01 = "Annual mean temperature (bio01)",
  bio02 = "Mean diurnal temperature range (bio02)",
  bio03 = "Isothermality (bio03)",
  bio04 = "Temperature seasonality (bio04)",
  bio05 = "Max temperature of warmest month (bio05)",
  bio06 = "Min temperature of coldest month (bio06)",
  bio07 = "Temperature annual range (bio07)",
  bio08 = "Mean temperature of wettest quarter (bio08)",
  bio09 = "Mean temperature of driest quarter (bio09)",
  bio10 = "Mean temperature of warmest quarter (bio10)",
  bio11 = "Mean temperature of coldest quarter (bio11)",
  bio12 = "Annual precipitation (bio12)",
  bio13 = "Precipitation of wettest month (bio13)",
  bio14 = "Precipitation of driest month (bio14)",
  bio15 = "Precipitation seasonality (bio15)",
  bio16 = "Precipitation of wettest quarter (bio16)",
  bio17 = "Precipitation of driest quarter (bio17)",
  bio18 = "Precipitation of warmest quarter (bio18)",
  bio19 = "Precipitation of coldest quarter (bio19)",
  fcf   = "Frost change frequency (fcf)",
  fgd   = "First day of growing season (fgd)",
  lgd   = "Last day of growing season (lgd)",
  gsl   = "Growing season length (gsl)",
  gsp   = "Growing season precipitation (gsp)",
  gdd0  = "Growing degree days above 0 °C (gdd0)",
  gdd5  = "Growing degree days above 5 °C (gdd5)",
  ngd0  = "Number of days above 0 °C (ngd0)",
  ngd5  = "Number of days above 5 °C (ngd5)",
  ngd10 = "Number of days above 10 °C (ngd10)",
  npp   = "Net primary productivity (npp)",
  scd   = "Snow cover days (scd)",
  swe   = "Snow water equivalent (swe)",
  # EarthEnv topography
  elevation = "Elevation (elevation)",
  slope     = "Slope (slope)",
  roughness = "Terrain roughness (roughness)",
  tpi       = "Topographic position index (tpi)",
  northness = "Northness (northness)",
  eastness  = "Eastness (eastness)"
)

palettes <- c(                         # hcl.colors() palette names; variables not listed use default_palette
  bio01 = "YlOrRd", bio02 = "YlOrRd", bio03 = "YlOrRd", bio04 = "YlOrRd", bio05 = "YlOrRd",
  bio06 = "YlOrRd", bio07 = "YlOrRd", bio08 = "YlOrRd", bio09 = "YlOrRd", bio10 = "YlOrRd",
  bio11 = "YlOrRd",
  bio12 = "YlGnBu", bio13 = "YlGnBu", bio14 = "YlGnBu", bio16 = "YlGnBu", bio17 = "YlGnBu",
  bio18 = "YlGnBu", bio19 = "YlGnBu", gsp = "YlGnBu",
  bio15 = "Purples 3",
  gdd0 = "YlOrRd", gdd5 = "YlOrRd", ngd0 = "YlOrRd", ngd5 = "YlOrRd", ngd10 = "YlOrRd",
  npp = "Greens",
  roughness = "Grays", slope = "Grays", elevation = "Grays",
  scd = "Blues", swe = "Blues",
  tpi = "Purple-Green", northness = "Purple-Green", eastness = "Purple-Green"
)
default_palette <- "Viridis"
diverging <- c("tpi", "northness", "eastness")   # centred on 0 (symmetric scale, light centre)

title_for   <- function(v) if (v %in% names(panel_titles)) unname(panel_titles[v]) else v
palette_for <- function(v) if (v %in% names(palettes)) unname(palettes[v]) else default_palette

clip_q <- c(0.02, 0.98)                # colour scale limits, as quantiles of each variable's values

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
coast_file  <- p("1-download-data", "Downloads", "LMI-IS50V", "is50v_land_polygon.gpkg")
out_dir     <- file.path(outputs_dir, "Final predictors")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(sel_file)) stop("Missing input: ", sel_file)

# ---- Which variables? --------------------------------------------------------
if (is.null(final_vars)) {
  step_file <- file.path(outputs_dir, "MaxEnt tuning", "Stepwise removal", "stepwise_path.csv")
  if (!file.exists(step_file)) {
    stop("final_vars is NULL but ", step_file, " does not exist.\nRun 5b first, or set final_vars to the variables you want to map.")
  }
  steps <- read.csv(step_file, stringsAsFactors = FALSE)
  if (!all(c("AICc", "vars_kept") %in% names(steps))) stop("stepwise_path.csv needs the columns 'AICc' and 'vars_kept'.")
  best <- steps[which.min(steps$AICc), ]
  final_vars <- trimws(strsplit(best$vars_kept, ",")[[1]])
  message("Variables from the lowest-AICc model in 5b (", length(final_vars), " variables, AICc ",
          round(best$AICc, 1), "): ", paste(final_vars, collapse = ", "))
} else {
  message("Variables given in final_vars (", length(final_vars), "): ", paste(final_vars, collapse = ", "))
}
final_vars <- unique(final_vars[nzchar(final_vars)])
if (length(final_vars) == 0) stop("No variables to map.")

# ---- Predictor rasters ------------------------------------------------------
sel <- read.csv(sel_file, stringsAsFactors = FALSE)
if (!all(c("variable", "historic_file") %in% names(sel))) {
  stop("selected_predictors.csv needs the columns 'variable' and 'historic_file'.")
}
not_in_sel <- setdiff(final_vars, sel$variable)
if (length(not_in_sel) > 0) {
  stop("These variables are not in selected_predictors.csv: ", paste(not_in_sel, collapse = ", "),
       "\n(5b can only drop variables from that file, so check the spelling in final_vars.)")
}
sel <- sel[match(final_vars, sel$variable), ]
pred_files <- p(sel$historic_file)
if (any(!file.exists(pred_files))) {
  stop("Predictor raster(s) not found:\n  ", paste(pred_files[!file.exists(pred_files)], collapse = "\n  "))
}
envs <- rast(pred_files)
names(envs) <- sel$variable

# Same cells as the model sees: restrict to cells with data in every layer (as in 5b).
if (file.exists(valid_file)) {
  valid <- rast(valid_file)
  if (isTRUE(compareGeom(envs, valid, stopOnError = FALSE))) {
    envs <- mask(envs, valid)
  } else {
    warning("valid_cells_mask.tif is not on the predictors' grid; not applied.")
  }
}
message("Mapping ", nlyr(envs), " predictors: ", paste(names(envs), collapse = ", "))

# Optional coastline outline.
coast <- NULL
if (file.exists(coast_file)) {
  coast <- tryCatch(project(vect(coast_file), crs(envs)), error = function(e) NULL)
}

# ---- Colour scale limits and summary ----------------------------------------
lims <- do.call(rbind, lapply(final_vars, function(v) {
  vals <- values(envs[[v]], mat = FALSE)
  vals <- vals[is.finite(vals)]
  lo <- unname(quantile(vals, clip_q[1]))
  hi <- unname(quantile(vals, clip_q[2]))
  if (v %in% diverging) {
    m <- max(abs(c(lo, hi)))
    lo <- -m; hi <- m
  }
  if (!is.finite(lo) || !is.finite(hi) || lo == hi) {   # e.g. nearly constant: fall back to the full range
    lo <- min(vals); hi <- max(vals)
  }
  data.frame(variable = v, n_cells = length(vals),
             min = min(vals), max = max(vals), plot_lo = lo, plot_hi = hi,
             pct_zero = round(100 * mean(vals == 0), 1),
             stringsAsFactors = FALSE)
}))
write.csv(lims, file.path(out_dir, "final_predictors_summary.csv"), row.names = FALSE)
message("\n=== Predictor ranges ===")
print(lims, row.names = FALSE)

# ---- Figure -----------------------------------------------------------------
n_vars <- length(final_vars)
n_rows <- ceiling(n_vars / n_cols)
draw_figure <- function() {
  par(mfrow = c(n_rows, n_cols), oma = c(0.5, 0, 3, 0))
  for (i in seq_along(final_vars)) {
    v  <- final_vars[i]
    lo <- lims$plot_lo[lims$variable == v]
    hi <- lims$plot_hi[lims$variable == v]
    r  <- clamp(envs[[v]], lower = lo, upper = hi, values = TRUE)
    pal <- hcl.colors(100, palette_for(v), rev = !(v %in% diverging))
    # The clamped raster is float32 and lo/hi are doubles, so cells clamped to a limit can sit a hair outside
    # c(lo, hi) and would not be drawn. A tiny tolerance on the scale keeps them in the end colour.
    eps <- (hi - lo) * range_tol
    plot(r, col = pal, range = c(lo - eps, hi + eps), axes = FALSE, box = FALSE,
         main = title_for(v), cex.main = title_cex,
         mar = c(0.5, 0.5, 2, 5.5), plg = list(cex = legend_cex))
    if (!is.null(coast)) lines(coast, lwd = 0.4, col = "grey25")
    # Panel letter in the bottom-left corner of the panel's figure region (independent of the map's margins).
    op <- par(xpd = NA)
    text(grconvertX(0.02, from = "nfc", to = "user"),
         grconvertY(0.1, from = "nfc", to = "user"),
         labels = paste0("(", panel_letters[i], ")"), adj = c(0, 0), font = 2, cex = 1.6)
    par(op)
  }
  mtext("Final MaxEnt predictors across Iceland", outer = TRUE, side = 3, line = 1, font = 2, cex = main_cex)
}

png(file.path(out_dir, "final_predictors_map.png"), width = n_cols * panel_width_px,
    height = n_rows * panel_height_px + 200, res = 300)
draw_figure()
invisible(dev.off())

message("\nSaved: ", file.path(out_dir, "final_predictors_map.png"))
message("Saved: ", file.path(out_dir, "final_predictors_summary.csv"))