# 3a-suitable-habitat-by-elevation.R
# Shows where suitable crowberry habitat lies by elevation, now and under each emissions scenario, as three figures:
#   1. suitable_habitat_by_elevation_<threshold>.png   CURRENT (1981-2010) habitat, two panels:
#        top:    the share of all suitable habitat in each elevation band (bars), next to the share of ALL land in that
#                band (outline), so you can see whether suitable habitat is over- or under-represented at an elevation
#        bottom: the percentage of each elevation band that is suitable
#        inset:  a small map of elevation, with the presences on it, under the legend of the top panel
#        The panels are lettered (a) and (b), centred under each one; the inset map is part of (a). Figures 2 and 3 are
#        lettered (a), (b), (c), one per scenario panel. Set panel_letters / letter_fmt in the settings to change the style.
#        A second file, suitable_habitat_by_elevation_top_<threshold>.png, has just the top panel (with its inset map), with its
#        own x-axis numbers and title (and no panel letter unless top_only_letter is TRUE). Set top_only_file <- FALSE to skip it.
#   2. elevation_overlay_band_<threshold>.png          one bar graph per scenario (SSP1-2.6, SSP3-7.0, SSP5-8.5), each
#        bar the percentage of that elevation band's area that is suitable, for 2011-2040, 2041-2070 and 2071-2100
#        layered over a light grey baseline (1981-2010, the same in every panel). Shows how MUCH of each band is
#        suitable, so gains and losses show up.
#   3. elevation_overlay_share_<threshold>.png         (optional, off by default: see overlay_measures) the same
#        layout, but each bar is the share of the period's suitable habitat that lies in that band. Shows WHERE the
#        habitat sits.
#
# Put this file in: ISP-Crowberry-SDM/3-modelling/Scripts/   (run 2a and 2b first)
# Reads:  3-modelling/Outputs/Projection/Scenario grid/suitability_baseline_1981-2010.tif   (from 2a)
#         3-modelling/Outputs/Projection/Binary maps/Tables/thresholds.csv                         (from 2b)
#         3-modelling/Outputs/Projection/Scenario grid/suitability_<gcm>_<ssp>_<period>.tif  (9 rasters, from 2a)
#         2-process-data/Outputs/EarthEnv clipped+resampled/elevation_1KMmd_GMTEDmd_iceland_resampled.tif
#             (or the same file in "EarthEnv resampled" if the clipped one isn't there)
#         2-process-data/Outputs/GBIF thinned/crowberry_occurrences_thinned_<thin_km>km.csv  (from 1b; the presences on the inset map,
#                                                                                           optional: the map is drawn without them if it is missing)
# Writes: 3-modelling/Outputs/Elevation/
#         (Sorted into subfolders: .png in Figures/, .csv and .txt in Tables/, .tif in Rasters/.)
#           suitable_habitat_by_elevation_<threshold>.png   figure 1 (current habitat)
#           suitable_habitat_by_elevation_top_<threshold>.png   figure 1's top panel on its own, with the inset map (optional: see top_only_file)
#           elevation_overlay_band_<threshold>.png          figure 2 (% of each band suitable, by scenario)
#           elevation_overlay_share_<threshold>.png         figure 3 (optional: share of suitable area, by scenario)
#           elevation_bands_<threshold>.csv                 the numbers for each band, current habitat
#           elevation_bands_by_scenario_<threshold>.csv     the numbers for each band, baseline and all 9 projections
#           elevation_summary_by_scenario_<threshold>.csv   suitable km2 and median / 90th-percentile elevation of
#                                                           suitable habitat for the baseline and each projection
# Inputs are only read, never changed.
#
# How it works: a cell is suitable if its baseline suitability is at or above the threshold from 2b (MaxSSS by
# default; set threshold_method <- "p10" for the 10th-percentile version). Cells are grouped into elevation bands
# (bin_m metres wide, from 0 m) using the 1 km median elevation from EarthEnv, and areas are summed in km2 using
# true cell areas (cells get narrower toward the north), not cell counts. Cells slightly below 0 m go into the
# first band.
#
# Reading it:
#  - Elevation is NOT one of the model's 8 predictors (it was dropped in screening), so this is an independent look
#    at where the model puts habitat. Any elevation pattern comes indirectly from the climate and terrain variables
#    that are in the model (npp, gddlgd10, bio18, bio03, bio15, tpi, roughness, northness), which all vary with elevation.
#  - The records behind the model are concentrated in the lowlands (roads, towns). The upper bands have few or no
#    presences, so what the model says there is extrapolation from climate, not from nearby records.
#  - Elevation here is the 1 km median, which smooths steep terrain, so a cell's elevation is an average and
#    a band's true range is a bit wider than its label.
#  - Bands with fewer than min_cells land cells are left off the percent-of-band plots. min_cells is 1, so only bands with
#    no land cells at all are left off; thin bands at the top are kept (a band of a few cells is only as reliable as those cells).
#  - The scenario figures apply the SAME baseline threshold to every future map, so differences come from the changed
#    climate, not from a changed cut-off. Elevation doesn't change; only which cells are suitable does.
#  - In the scenario figures the bars are OPAQUE and, within each band, drawn tallest first with the shorter ones in
#    front. So every colour on the plot is exactly one legend colour (grey = baseline, then light to dark = 2011-2040,
#    2041-2070, 2071-2100), and a colour never depends on how many other bars happen to overlap it. At any height the
#    colour you see is the shortest bar that still reaches that height; the outlines trace each period's exact value.
#    The baseline grey is lightened toward white (base_alpha) so it looks see-through against the page and gridlines.
#  - In figure 3 each period is scaled to 100% of its own suitable area, so it shows where the habitat sits but hides how
#    much of it there is: a band can lose habitat and still gain share if the other bands lose more. Figure 2 does not
#    have that problem. In both, the legends give the median elevation of the suitable habitat for each period; a shift
#    toward higher elevation means habitat moving upslope.
#  - Habitat at the top of the elevation range in the future rests on climate values beyond what the model saw in the
#    records, so treat apparent upslope gains with caution (see the grey areas in the 2a figures).
#  - The inset map is the same elevation raster the figure is built from, on a continuous green scale (light = low, dark =
#    high; square-root stretch by default, see map_sqrt), with the thinned presences (thin_km) on top. It is sized to fit
#    the empty space under the legend, so it adjusts to the bars; if there is too little room it is left out, with a message.
#    The presences show where the model's records actually are, which is mostly the lowlands.
#
# NOT TESTED on your data. Check the console messages and the figure before using the output.

# install.packages("terra")
library(terra)

# ---- Settings ---------------------------------------------------------------
threshold_method <- "maxsss"      # "maxsss" or "p10" (as in 2b)
bin_m            <- 100           # elevation band width in metres
min_cells        <- 1             # percent-of-band plots (figure 1 bottom panel, scenario figures): leave out bands with fewer land cells than this.
# 1 = only bands with no land cells at all are left out, so the plots run to the top band (thin top bands show as 0%).
# (It was 20; a higher value hides thin bands, whose % rests on very few cells.)
col_suit <- "#009E73"             # suitable habitat
col_land <- "grey30"              # all land
gcm        <- "gfdl-esm4"         # for the scenario figures (as in the future raster file names)
scenarios  <- c(ssp126 = "SSP1-2.6", ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")      # one panel each, in this order
periods    <- c("2011-2040", "2041-2070", "2071-2100")
baseline_period <- "1981-2010"
line_cols  <- c(ssp126 = "#0072B2", ssp370 = "#E69F00", ssp585 = "#B2182B")         # each scenario's colour (light to dark by period)
col_base   <- "grey75"            # scenario figures: the baseline (1981-2010) bars, the same in every panel
base_alpha <- 0.5                 # how solid the baseline is (1 = solid grey, lower = lighter). It is mixed with WHITE, not with the colours behind it:
# so it reads as see-through against the white page and the gridlines, but the period bars never show through it
fill_white    <- c(0.70, 0.45, 0.20)   # scenario figures: how far each period's fill is blended toward white (2011-2040, 2041-2070, 2071-2100); more white = lighter
outline_white <- c(0.40, 0.20, 0.00)   # the same for each period's outline, so it is a shade darker than its fill
overlay_size       <- c(2400, 2640)   # scenario figures: size in pixels (width, height) at 300 dpi; the height was 3300 (now 20% more compact)
overlay_legend_cex <- 1.04            # scenario figures: legend text size (was 0.8; now 30% bigger)
overlay_ylab_cex   <- 1.15            # scenario figures: size of the y-axis label (was 1; now 15% bigger)
overlay_measures <- "band"        # scenario figures to make. "band" = % of each elevation band that is suitable.
# Use c("band", "share") to also make the share-of-suitable-area version.
top_only_file   <- TRUE           # figure 1: also save the top panel (a) on its own, with the inset map, as suitable_habitat_by_elevation_top_<threshold>.png
top_only_size   <- c(2400, 1900)  # that figure's size in pixels (width, height) at 300 dpi
top_only_letter <- FALSE          # put the panel letter (a) under it
inset_map    <- TRUE              # figure 1: a small map of elevation, under the legend of the top panel
inset_points <- TRUE              # draw the presences on that map
thin_km      <- 5                 # which thinned presence set to draw (as in 1b)
col_ramp     <- c("#CDE8B5", "#8FCB8A", "#4FAE72", "#1E7F55", "#0A4A33")   # map colours: low (light) to high (dark); the same as the coast-distance map in 4a
map_sqrt     <- TRUE              # TRUE = square-root colour stretch, so the lowlands (where most of the land is) aren't washed out
map_ticks    <- c(0, 250, 500, 1000, 1500, 2000)   # m: labels on the colour key (any beyond the highest land are dropped)
col_sea      <- "#eef3f8"         # map background (sea), as in 5a
panel_letters <- letters          # letter under each panel of a figure with several panels: a, b, c, ... (LETTERS for A, B, C, ...)
letter_fmt    <- "(%s)"           # "(%s)" gives (a); "%s" gives a

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
base_file <- p("3-modelling", "Outputs", "Projection", "Scenario grid", "suitability_baseline_1981-2010.tif")
thr_file  <- p("3-modelling", "Outputs", "Projection", "Binary maps", "Tables", "thresholds.csv")
elev_candidates <- c(p("2-process-data", "Outputs", "EarthEnv clipped+resampled", "elevation_1KMmd_GMTEDmd_iceland_resampled.tif"),
                     p("2-process-data", "Outputs", "EarthEnv resampled", "elevation_1KMmd_GMTEDmd_iceland_resampled.tif"))
elev_file <- elev_candidates[file.exists(elev_candidates)][1]
out_dir   <- p("3-modelling", "Outputs", "Elevation")
for (d in file.path(out_dir, c("Tables", "Figures"))) dir.create(d, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(base_file)) stop("Missing input: ", base_file, "\nRun 2a-scenario-change-grid.R first.")
if (!file.exists(thr_file))  stop("Missing input: ", thr_file, "\nRun 2b-binary-maps.R first.")
if (is.na(elev_file))        stop("Elevation raster not found. Looked for:\n  ", paste(elev_candidates, collapse = "\n  "))

thr_tab <- read.csv(thr_file, stringsAsFactors = FALSE)
if (!(threshold_method %in% thr_tab$method)) stop("No threshold \"", threshold_method, "\" in thresholds.csv.")
thr <- thr_tab$threshold[thr_tab$method == threshold_method]
message("Threshold (", threshold_method, "): ", round(thr, 4), "   elevation raster: ", basename(dirname(elev_file)), "/", basename(elev_file))

suit <- rast(base_file)
elev <- rast(elev_file)
if (!isTRUE(compareGeom(suit, elev, stopOnError = FALSE))) {
  message("The elevation raster is not on the suitability grid; resampling it (bilinear) onto that grid.")
  elev <- resample(elev, suit, method = "bilinear")
}

# ---- Per-cell values --------------------------------------------------------
w <- terra::cellSize(suit, unit = "km", mask = FALSE)       # km2 of each cell
v <- data.frame(elev = terra::values(elev, mat = FALSE),
                suit = terra::values(suit, mat = FALSE),
                w    = terra::values(w,    mat = FALSE))
keep <- is.finite(v$elev) & is.finite(v$suit)
cell_idx <- which(keep)                                    # raster cell numbers of the rows kept, to read the future rasters
v <- v[keep, ]
if (nrow(v) == 0) stop("No cells have both an elevation and a suitability value.")
n_neg <- sum(v$elev < 0)
if (n_neg > 0) message(n_neg, " cell(s) are below 0 m; they go into the first band.")
v$elev <- pmax(v$elev, 0)
v$suitable <- v$suit >= thr
message("Land cells: ", nrow(v), "   suitable: ", sum(v$suitable), " (", round(100 * mean(v$suitable), 1), "% of cells)")

# ---- Elevation bands ----------------------------------------------------------
br  <- seq(0, ceiling(max(v$elev) / bin_m) * bin_m, by = bin_m)
if (length(br) < 2) br <- c(0, bin_m)
nb  <- length(br) - 1
idx <- findInterval(v$elev, br, rightmost.closed = TRUE, all.inside = TRUE)
by_band <- function(x, fun = sum) {
  out <- tapply(x, factor(idx, levels = seq_len(nb)), fun)
  out[is.na(out)] <- 0
  as.numeric(out)
}
n_cells   <- by_band(rep(1, nrow(v)))
km2_land  <- by_band(v$w)
km2_suit  <- by_band(v$w * v$suitable)
tab <- data.frame(
  elev_from = br[-length(br)], elev_to = br[-1], n_cells = n_cells,
  km2_land = round(km2_land), km2_suitable = round(km2_suit),
  pct_of_land_area     = round(100 * km2_land / sum(km2_land), 2),
  pct_of_suitable_area = round(100 * km2_suit / sum(km2_suit), 2),
  pct_of_band_suitable = ifelse(km2_land > 0, round(100 * km2_suit / km2_land, 1), NA_real_),
  stringsAsFactors = FALSE)
write.csv(tab, file.path(out_dir, "Tables", paste0("elevation_bands_", threshold_method, ".csv")), row.names = FALSE)

# ---- Summary numbers (area-weighted) -------------------------------------------
wquant <- function(x, w, q) { o <- order(x); cw <- cumsum(w[o]) / sum(w); x[o][which(cw >= q)[1]] }
med_land <- wquant(v$elev, v$w, 0.5)
med_suit <- wquant(v$elev[v$suitable], v$w[v$suitable], 0.5)
p90_suit <- wquant(v$elev[v$suitable], v$w[v$suitable], 0.9)
tot_suit <- sum(km2_suit); tot_land <- sum(km2_land)
message(sprintf("\nSuitable area: %s km2 (%.0f%% of %s km2 of land)", format(round(tot_suit), big.mark = ","),
                100 * tot_suit / tot_land, format(round(tot_land), big.mark = ",")))
message(sprintf("Median elevation: suitable habitat %.0f m, all land %.0f m. 90%% of suitable habitat lies below %.0f m.",
                med_suit, med_land, p90_suit))
message("\n=== By elevation band ===")
print(tab[, c("elev_from", "elev_to", "n_cells", "pct_of_land_area", "pct_of_suitable_area", "pct_of_band_suitable")], row.names = FALSE)

# ---- Inset map data (figure 1) ----------------------------------------------------
# The elevation raster, shown as a small map with the presences on it. Colours run on a continuous scale (col_ramp).
inset_data <- NULL
if (inset_map) {
  el <- terra::trim(terra::mask(elev, suit))                         # land only, cropped to the land's extent
  zm <- terra::as.matrix(el, wide = TRUE)                            # rows run north to south
  zm <- t(zm)[, nrow(zm):1, drop = FALSE]                            # columns west to east, rows south to north, as image() wants
  zm[!is.na(zm) & zm < 0] <- 0                                       # cells slightly below 0 m, as in the bands
  dmax <- max(100, ceiling(max(zm, na.rm = TRUE) / 100) * 100)       # top of the colour scale: the highest land, rounded up to 100 m
  tf   <- if (map_sqrt) sqrt else identity
  zc   <- tf(pmin(zm, dmax)) / tf(dmax)                              # 0 at sea level to 1 at dmax (sea stays NA)
  xlim_m <- c(terra::xmin(el), terra::xmax(el)); ylim_m <- c(terra::ymin(el), terra::ymax(el))
  kx <- if (terra::is.lonlat(el)) cos(mean(ylim_m) * pi / 180) else 1   # a degree of longitude is shorter than a degree of latitude
  pres_xy <- NULL
  if (inset_points) {
    pres_file <- p("2-process-data", "Outputs", "GBIF thinned", sprintf("crowberry_occurrences_thinned_%dkm.csv", thin_km))
    if (file.exists(pres_file)) {
      occ <- read.csv(pres_file, stringsAsFactors = FALSE)
      lat_col <- intersect(c("decimalLatitude", "latitude", "lat"), names(occ))[1]
      lon_col <- intersect(c("decimalLongitude", "longitude", "lon", "long"), names(occ))[1]
      if (is.na(lat_col) || is.na(lon_col)) {
        message("Inset map: no lat/lon columns in ", basename(pres_file), "; drawing the map without the presences.")
      } else {
        xy <- cbind(occ[[lon_col]], occ[[lat_col]])
        xy <- xy[stats::complete.cases(xy), , drop = FALSE]
        pv <- terra::vect(xy, crs = "EPSG:4326")
        if (!terra::is.lonlat(suit)) pv <- terra::project(pv, terra::crs(suit))
        pres_xy <- terra::crds(pv)
        message("Inset map: ", nrow(pres_xy), " presences (", basename(pres_file), ")")
      }
    } else {
      message("Inset map: ", pres_file, " not found; drawing the map without the presences.")
    }
  }
  inset_data <- list(ix = terra::xFromCol(el, seq_len(ncol(el))), iy = rev(terra::yFromRow(el, seq_len(nrow(el)))), zc = zc,
                     xlim = xlim_m, ylim = ylim_m, kx = kx, asp = diff(xlim_m) * kx / diff(ylim_m),    # asp = width / height of the map
                     pres = pres_xy, cols = grDevices::colorRampPalette(col_ramp)(256), dmax = dmax, tf = tf)
}

# ---- Figure -----------------------------------------------------------------
# Panel letter, centred under the plot that is currently active (line = how far below the plot's axis, in margin lines).
panel_letter <- function(i, line) {
  mtext(sprintf(letter_fmt, panel_letters[i]), side = 1, line = line, at = mean(par("usr")[1:2]), font = 2, cex = 1)
}
step_xy <- function(h) list(x = c(br[1], rep(br[-c(1, length(br))], each = 2), br[length(br)]), y = rep(h, each = 2))
thr_label <- if (threshold_method == "maxsss") "MaxSSS" else threshold_method

share_s <- 100 * km2_suit / tot_suit; share_l <- 100 * km2_land / tot_land

# Figure 1 is drawn by a function so it can be made twice: with both panels, and with the top panel (a) on its own.
draw_current <- function(file, top_only = FALSE) {
  inset <- inset_data                                                       # a local copy: the map can be left out of one version for lack of room without affecting the other
  if (top_only) {
    png(file, width = top_only_size[1], height = top_only_size[2], res = 300)
  } else {
    png(file, width = 2400, height = 2900, res = 300)
    layout(matrix(1:2, nrow = 2), heights = c(1, 0.85))
  }
  par(oma = c(0, 0, 3, 0))
  
  # Top panel: where the suitable habitat is, compared with where the land is
  # (On its own it carries the x-axis numbers and title, which otherwise belong to the bottom panel.)
  par(mar = c(if (!top_only) 3.2 else if (top_only_letter) 6.4 else 4.6, 5, 1.5, 1))      # bottom margin leaves room for the panel letter
  plot(NA, xlim = range(br), ylim = c(0, min(100, max(share_s, share_l) * 1.15)), xaxt = "n", yaxt = "n",   # shares can't pass 100%
       xlab = if (top_only) "Elevation (m)" else "", ylab = "Share of area (%)", bty = "n")
  abline(h = pretty(c(0, max(share_s, share_l))), col = "grey92")
  rect(br[-length(br)], 0, br[-1], share_s, col = adjustcolor(col_suit, 0.75), border = "white", lwd = 0.5)
  st <- step_xy(share_l)
  lines(st$x, st$y, col = col_land, lwd = 2)
  abline(v = med_suit, lty = 2, lwd = 1.6, col = col_suit)
  abline(v = med_land, lty = 2, lwd = 1.6, col = col_land)
  axis(2, las = 1)
  axis(1, at = pretty(range(br)), labels = top_only)
  lg <- legend("topright", bty = "n", cex = 0.9,
               legend = c("Suitable habitat (share of suitable area)", "All land (share of land area)", "Dashed lines: medians"),
               fill = c(adjustcolor(col_suit, 0.75), NA, NA), border = c("white", NA, NA),
               lty = c(NA, 1, 2), lwd = c(NA, 2, 1.6), col = c(NA, col_land, "grey40"), x.intersp = c(0.5, 0.2, 0.2))
  if (!top_only) panel_letter(1, line = 1.9) else if (top_only_letter) panel_letter(1, line = 4.9)
  mtext(sprintf("Where suitable habitat lies by elevation. Median: suitable habitat %.0f m, all land %.0f m", med_suit, med_land),
        side = 3, line = 0.2, cex = 0.8, font = 2, adj = 0)
  
  # Inset map: find the empty space under the legend (right of the median lines, above the bars and the land line) and fit
  # the map and its colour key into it, right-aligned. Sizes are worked out in inches, then drawn after the bottom panel.
  if (!is.null(inset)) {
    usr   <- par("usr")
    gx    <- 0.02 * (usr[2] - usr[1]); gy <- 0.03 * (usr[4] - usr[3])
    x_lo  <- max(med_suit, med_land) + gx                               # clear of the dashed median lines
    cover <- br[-1] > x_lo                                              # bands that reach into the inset's columns
    y_lo  <- max(c(share_s[cover], share_l[cover]), 0) + gy             # above any bar or land line out there
    y_hi  <- lg$rect$top - lg$rect$h - gy                               # just under the legend
    bx <- grconvertX(c(x_lo, usr[2]), "user", "inches"); by <- grconvertY(c(y_lo, y_hi), "user", "inches")
    title_h <- 0.22; key_w <- 1.05; gap_in <- 0.15                      # inches: title line, colour key, space between map and key
    mh <- min(diff(by) - title_h, (diff(bx) - key_w - gap_in) / inset$asp)
    if (!is.finite(mh) || mh < 0.8) {
      message("Inset map: not enough room under the legend; left out.")
      inset <- NULL
    } else {
      mw <- mh * inset$asp
      x1 <- bx[2] - (mw + gap_in + key_w); y2 <- by[2]
      inset$g <- list(map = c(x1, x1 + mw, y2 - title_h - mh, y2 - title_h),
                      key = c(x1 + mw + gap_in, bx[2], y2 - title_h - mh, y2 - title_h))
      rect(grconvertX(x1 - 0.05, "inches", "user"), grconvertY(y2 - title_h - mh - 0.05, "inches", "user"),   # white backing hides the gridlines
           grconvertX(bx[2], "inches", "user"), grconvertY(y2 + 0.03, "inches", "user"), col = "white", border = NA)
      text(grconvertX(x1, "inches", "user"), grconvertY(y2, "inches", "user"), "Elevation (m)",
           adj = c(0, 1), cex = 0.75, font = 2)
    }
  }
  
  # Bottom panel: how much of each elevation band is suitable (left out of the top-panel-only version)
  if (!top_only) {
    mids <-(br[-1] + br[-length(br)]) / 2
    frac <- ifelse(n_cells >= min_cells, 100 * km2_suit / pmax(km2_land, 1e-9), NA)
    par(mar = c(6.4, 5, 1.5, 1))                                              # bottom margin: axis, axis title, then the panel letter
    plot(NA, xlim = range(br), ylim = c(0, 100), xaxt = "n", yaxt = "n", xlab = "Elevation (m)", ylab = "Band that is suitable (%)", bty = "n")
    abline(h = seq(0, 100, 25), col = "grey92")
    abline(h = 100 * tot_suit / tot_land, lty = 2, col = "grey40")
    text(br[length(br)], 100 * tot_suit / tot_land, sprintf("All of Iceland: %.0f%%", 100 * tot_suit / tot_land), adj = c(1, -0.6), cex = 0.8, col = "grey30")
    lines(mids, frac, col = col_suit, lwd = 2.5)
    points(mids, frac, pch = 16, col = col_suit, cex = 1.1)
    axis(1, at = pretty(range(br)))
    axis(2, at = seq(0, 100, 25), las = 1)
    panel_letter(2, line = 4.9)
    mtext(if (min_cells <= 1) sprintf("Share of each %d m band that is suitable", bin_m)
          else sprintf("Share of each %d m band that is suitable (bands with fewer than %d land cells left out)", bin_m, min_cells),
          side = 3, line = 0.2, cex = 0.8, font = 2, adj = 0)
  }
  
  mtext(sprintf("Current (1981-2010) suitable crowberry habitat by elevation (%s threshold, suitability >= %.2f)", thr_label, thr),
        outer = TRUE, side = 3, line = 1.2, font = 2, cex = 1.0)
  
  # Inset map, drawn last so it doesn't disturb the two-panel layout: the map first, then its colour key.
  if (!is.null(inset)) {
    g      <- inset$g
    par(oma = c(0, 0, 0, 0))                                            # the title margin would otherwise shift where fig lands
    to_fig <- function(b) c(grconvertX(b[1:2], "inches", "ndc"), grconvertY(b[3:4], "inches", "ndc"))
    par(fig = to_fig(g$map), mar = c(0, 0, 0, 0), new = TRUE)
    plot.new()
    plot.window(xlim = inset$xlim, ylim = inset$ylim, asp = 1 / inset$kx, xaxs = "i", yaxs = "i")
    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col = col_sea, border = NA)
    image(inset$ix, inset$iy, inset$zc, col = inset$cols, zlim = c(0, 1), add = TRUE, useRaster = TRUE)
    if (!is.null(inset$pres)) points(inset$pres, pch = 21, bg = "white", col = "black", cex = 0.35, lwd = 0.4)
    box(col = "grey30", lwd = 1)
    
    par(fig = to_fig(g$key), mar = c(0, 0, 0, 0), new = TRUE)
    kw <- g$key[2] - g$key[1]; kh <- g$key[4] - g$key[3]
    plot.new(); plot.window(xlim = c(0, kw), ylim = c(0, kh), xaxs = "i", yaxs = "i")    # units are inches
    y0 <- 0.34                                                                             # the bar sits above the presence key
    rasterImage(grDevices::as.raster(matrix(rev(inset$cols), ncol = 1)), 0, y0, 0.16, kh)  # lowest land at the bottom
    rect(0, y0, 0.16, kh, border = "grey40", lwd = 0.5)
    tk <- map_ticks[map_ticks <= inset$dmax]
    ty <- y0 + inset$tf(tk) / inset$tf(inset$dmax) * (kh - y0)                             # same stretch as the map
    segments(0.16, ty, 0.2, ty, col = "grey30", lwd = 0.5)
    text(0.23, ty, tk, adj = c(0, 0.5), cex = 0.6, xpd = NA)
    if (!is.null(inset$pres)) {
      points(0.08, 0.12, pch = 21, bg = "white", col = "black", cex = 0.6, lwd = 0.5)
      text(0.22, 0.12, sprintf("Presences, n = %d", nrow(inset$pres)), adj = c(0, 0.5), cex = 0.6, xpd = NA)
    }
  }
  invisible(dev.off())
}

f_cur     <- file.path(out_dir, "Figures", paste0("suitable_habitat_by_elevation_",     threshold_method, ".png"))
f_cur_top <- file.path(out_dir, "Figures", paste0("suitable_habitat_by_elevation_top_", threshold_method, ".png"))
draw_current(f_cur)
if (top_only_file) draw_current(f_cur_top, top_only = TRUE)

# ---- Scenarios: baseline and projected suitable habitat by elevation -------------------------------------------
fut_file <- function(s, per) file.path(dirname(base_file), sprintf("suitability_%s_%s_%s.tif", gcm, s, per))
fut_all  <- unlist(lapply(names(scenarios), function(s) vapply(periods, function(per) fut_file(s, per), character(1))))
if (any(!file.exists(fut_all))) {
  stop(sum(!file.exists(fut_all)), " future suitability raster(s) not found, for example:\n  ",
       paste(utils::head(fut_all[!file.exists(fut_all)], 4), collapse = "\n  "), "\nRun 2a-scenario-change-grid.R first.")
}

case_stats <- function(ssp, scenario, period, suitable) {          # suitable area per elevation band for one map
  ks <- by_band(v$w * suitable)
  data.frame(ssp = ssp, scenario = scenario, period = period,
             elev_from = br[-length(br)], elev_to = br[-1], n_cells = n_cells, km2_land = km2_land, km2_suitable = ks,
             pct_of_suitable_area = if (sum(ks) > 0) 100 * ks / sum(ks) else NA_real_,
             pct_of_band_suitable = ifelse(km2_land > 0, 100 * ks / km2_land, NA_real_),
             stringsAsFactors = FALSE)
}
case_summary <- function(ssp, scenario, period, suitable) {
  data.frame(ssp = ssp, scenario = scenario, period = period,
             km2_suitable = round(sum(v$w[suitable])),
             pct_of_land_suitable = round(100 * sum(v$w[suitable]) / tot_land, 1),
             median_elev_suitable_m = if (any(suitable)) round(wquant(v$elev[suitable], v$w[suitable], 0.5)) else NA_real_,
             p90_elev_suitable_m    = if (any(suitable)) round(wquant(v$elev[suitable], v$w[suitable], 0.9)) else NA_real_,
             stringsAsFactors = FALSE)
}

bands <- list(baseline = case_stats("baseline", "Baseline", baseline_period, v$suitable))
sums  <- list(baseline = case_summary("baseline", "Baseline", baseline_period, v$suitable))
for (s in names(scenarios)) for (per in periods) {
  r <- rast(fut_file(s, per))
  if (!isTRUE(compareGeom(suit, r, stopOnError = FALSE))) stop("Raster ", basename(fut_file(s, per)), " is not on the same grid as the baseline.")
  sf <- terra::values(r, mat = FALSE)[cell_idx]
  if (anyNA(sf)) message("Note: ", sum(is.na(sf)), " cell(s) have no value in ", basename(fut_file(s, per)), "; counted as unsuitable.")
  suitable_f <- !is.na(sf) & sf >= thr                               # same threshold as the baseline
  key <- paste(s, per, sep = "_")
  bands[[key]] <- case_stats(s, scenarios[[s]], per, suitable_f)
  sums[[key]]  <- case_summary(s, scenarios[[s]], per, suitable_f)
}
band_tab <- do.call(rbind, bands); rownames(band_tab) <- NULL
sum_tab  <- do.call(rbind, sums);  rownames(sum_tab)  <- NULL
band_out <- band_tab
band_out[, c("km2_land", "km2_suitable")] <- round(band_out[, c("km2_land", "km2_suitable")])
band_out[, c("pct_of_suitable_area", "pct_of_band_suitable")] <- round(band_out[, c("pct_of_suitable_area", "pct_of_band_suitable")], 2)
write.csv(band_out, file.path(out_dir, "Tables", paste0("elevation_bands_by_scenario_", threshold_method, ".csv")), row.names = FALSE)
write.csv(sum_tab,  file.path(out_dir, "Tables", paste0("elevation_summary_by_scenario_", threshold_method, ".csv")), row.names = FALSE)
message("\n=== Suitable habitat: area and elevation, baseline and projections ===")
print(sum_tab[, c("scenario", "period", "km2_suitable", "pct_of_land_suitable", "median_elev_suitable_m", "p90_elev_suitable_m")], row.names = FALSE)

mix_white <- function(col, a) {                                    # blend a colour toward white (a = 0 keeps it)
  m <- grDevices::col2rgb(col)[, 1] / 255
  grDevices::rgb(m[1] * (1 - a) + a, m[2] * (1 - a) + a, m[3] * (1 - a) + a)
}
# Draw one panel's bars. y_list is a list of series (the baseline first, then the periods), each one value per band
# (NA = no bar). The bars are opaque and, in each band, drawn tallest first so the shorter ones sit in front. That way
# every colour on the plot is exactly one legend colour, whatever the order of the periods is in a band. (If the bars
# themselves were see-through, the colours would add up where bars overlap, so the shade you see would depend on how
# many bars overlap and on which period happens to be behind, and the shading would be misleading.) The baseline's
# "see-through" look is done by mixing its grey with white beforehand (base_alpha), not by real transparency.
draw_layered <- function(y_list, fills) {
  Y <- do.call(cbind, y_list)                                      # bands x series
  for (k in seq_len(nrow(Y))) {
    ok <- which(!is.na(Y[k, ]) & Y[k, ] > 0)
    for (j in ok[order(Y[k, ok], decreasing = TRUE)]) {
      rect(br[k], 0, br[k + 1], Y[k, j], col = fills[j], border = NA)
    }
  }
}
n_scen <- length(scenarios)

draw_overlay <- function(measure) {                                # measure = "share" or "band"
  get_y <- function(b) if (measure == "share") b$pct_of_suitable_area else ifelse(b$n_cells >= min_cells, b$pct_of_band_suitable, NA_real_)
  # One y scale for all three panels. Both measures are percentages, so the axis never goes past 100: the
  # "band" version always spans 0-100, the "share" version stops at 100 if its tallest bar is close to it.
  ymax <- if (measure == "band") 100 else min(100, max(unlist(lapply(bands, get_y)), na.rm = TRUE) * 1.18)
  if (!is.finite(ymax) || ymax <= 0) ymax <- 100
  ylab <- if (measure == "share") "Share of suitable area (%)" else "Suitable proportion of band (%)"
  yb <- get_y(bands[["baseline"]])
  layout(matrix(seq_len(n_scen), ncol = 1))
  par(oma = c(2.6, 0, 3.6, 0))
  for (i in seq_len(n_scen)) {
    s <- names(scenarios)[i]
    last <- (i == n_scen)                                           # the last panel also carries the x-axis numbers
    par(mar = c(if (last) 3.4 else 2.4, 5, 1.8, 1))                 # bottom margin leaves room for the panel letter
    plot(NA, xlim = range(br), ylim = c(0, ymax), xaxt = "n", yaxt = "n", xlab = "", ylab = ylab, cex.lab = overlay_ylab_cex, bty = "n")
    fill_c <- mix_white(line_cols[[s]], fill_white)                 # light to dark: 2011-2040, 2041-2070, 2071-2100
    out_c  <- mix_white(line_cols[[s]], outline_white)
    ys     <- c(list(yb), lapply(periods, function(per) get_y(bands[[paste(s, per, sep = "_")]])))   # baseline, then the three periods
    base_fill <- mix_white(col_base, 1 - base_alpha)                # the baseline grey, lightened toward white
    draw_layered(ys, c(base_fill, fill_c))                          # baseline, then the three periods; all opaque
    abline(h = pretty(c(0, ymax)), col = adjustcolor("grey50", 0.25))   # gridlines, drawn over the bars so they show through them
    for (j in seq_along(periods)) {
      st <- step_xy(ys[[j + 1]]); lines(st$x, st$y, col = out_c[j], lwd = 1.6)
    }
    axis(2, las = 1); axis(1, at = pretty(range(br)), labels = last)
    panel_letter(i, line = if (last) 2.4 else 1.3)
    leg <- c(sprintf("Baseline %s (median %s m)", baseline_period, sums[["baseline"]]$median_elev_suitable_m),
             vapply(periods, function(per) sprintf("%s (median %s m)", per, sums[[paste(s, per, sep = "_")]]$median_elev_suitable_m), character(1)))
    legend("topright", legend = leg, fill = c(base_fill, fill_c),
           border = c("grey55", out_c), bty = "n", cex = overlay_legend_cex)
    mtext(scenarios[[s]], side = 3, line = 0.3, adj = 0, font = 2, cex = 0.95)
  }
  mtext("Elevation (m)", side = 1, outer = TRUE, line = 1.2, cex = 0.9)
  mtext(if (measure == "share") "Where suitable habitat lies by elevation: baseline and projected"
        else "Share of each elevation band that is suitable: baseline and projected",
        outer = TRUE, side = 3, line = 2.2, font = 2, cex = 0.95)
  mtext(sprintf("%s, %s threshold = %.4f", toupper(gcm), thr_label, thr), outer = TRUE, side = 3, line = 0.9, cex = 0.8)
}

f_share <- file.path(out_dir, "Figures", paste0("elevation_overlay_share_", threshold_method, ".png"))
f_band  <- file.path(out_dir, "Figures", paste0("elevation_overlay_band_",  threshold_method, ".png"))
if ("band" %in% overlay_measures) {
  png(f_band,  width = overlay_size[1], height = overlay_size[2], res = 300); draw_overlay("band");  invisible(dev.off())
}
if ("share" %in% overlay_measures) {
  png(f_share, width = overlay_size[1], height = overlay_size[2], res = 300); draw_overlay("share"); invisible(dev.off())
}

message("\nSaved: ", f_cur)
if (top_only_file) message("Saved: ", f_cur_top)
if ("band" %in% overlay_measures)  message("Saved: ", f_band)
if ("share" %in% overlay_measures) message("Saved: ", f_share)
message("Saved: ", file.path(out_dir, "Tables", paste0("elevation_bands_by_scenario_", threshold_method, ".csv")))
message("Saved: ", file.path(out_dir, "Tables", paste0("elevation_summary_by_scenario_", threshold_method, ".csv")))
message("Saved: ", file.path(out_dir, "Tables", paste0("elevation_bands_", threshold_method, ".csv")))