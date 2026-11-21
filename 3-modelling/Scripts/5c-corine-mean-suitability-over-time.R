# 5c-corine-mean-suitability-over-time.R
# Line plots of the mean habitat suitability of a CORINE class over time, one plot per class, for the classes that each
# make up at least min_pct_land (2%) of Iceland's land (eight of them with the default). In each plot the x-axis is time
# (the 1981-2010 baseline, then 2011-2040, 2041-2070 and 2071-2100) and the y-axis is the mean suitability of the class;
# there is one line per scenario (SSP1-2.6, SSP3-7.0, SSP5-8.5), drawn in greys (light to black, with a different marker
# for each) so they stand out against the plots, which are tinted with the colour of their class. All three lines start
# at the same baseline point. Each plot has a panel letter, (a), (b), ..., centred underneath it, in reading order.
#
# The same class colours are used on a map of those classes (the rest of the land is pale grey), which sits to the right
# of the plots as an inset (map_in_figure), is also saved under them in a second version of the figure
# (map_below_version), and is saved as its own figure (separate_map). The map is cropped to the
# land (with a small margin, map_pad_frac) and its panel is sized to the map's shape, so it shows little empty sea. The
# colours are the same as in 5a and 5b, so a class looks the same in all of them.
#
# Put this file in: ISP-Crowberry-SDM/3-modelling/Scripts/   (run 5b first)
# Reads:  3-modelling/Outputs/CORINE/Tables/5b_suitability_by_scenario_<threshold>.csv   (from 5b)
#         1-download-data/Downloads/CORINE/corine_2018_iceland.gpkg                      (CORINE download script; the map only)
# Writes: 3-modelling/Outputs/CORINE/   (all the CORINE scripts, 5a to 5e, write here; files start with the script's number)
#           Figures/5e_mean_suitability_over_time_<threshold>.png    the figure (with the map and key to the right)
#           Figures/5e_mean_suitability_over_time_<threshold>_map_below.png   the same figure with the map and key below
#           Figures/5e_class_map.png                                 the map on its own, with a key
#           Tables/5e_mean_suitability_over_time_<threshold>.csv     the numbers behind it: one row per class, scenario
#                                                                    and period
# Inputs are only read, never changed. The line plots are not recalculated from the rasters: they re-plot what 5b saved, so
# 5b's alignment of the polygons and the raster and its settings carry over. The map is drawn from the CORINE polygons
# (simplified for drawing; the data is untouched).
#
# What is plotted: the mean suitability of a class is the area-weighted average of the model's suitability values over
# the class (each suitability cell counts in proportion to how much of it lies in the class), as calculated in 5b. It
# does not depend on the suitability threshold; `threshold_method` only says which 5b file to read, and the two files
# hold the same means.
#
# Reading it:
#  - Suitability is the model's cloglog value (0 to 1), a relative index, not a probability of presence.
#  - Classes are held at their 2018 extent. A rising line means the climate where the class is now becomes more suitable,
#    not that the class will spread there. Glaciers in particular stay where they were in 2018.
#  - The time axis is the 30-year period, not a single year, and the points are joined by straight lines for readability;
#    nothing is known between periods.
#  - Different classes sit at very different suitability levels. With y_scale = "shared" (the default) every plot uses the
#    same y-axis so the levels can be compared across classes, but a small change in a low class is hard to see; use
#    y_scale = "free" to give each plot its own y-axis (then compare the shapes, not the heights).
#  - Values in climates beyond what the model saw in the records rest on extrapolation, and one climate model is used.
#  - Check n_points (presences in the class, in the csv): a class with few records is weakly supported by the model.
#
# NOT TESTED on your data. Check the console messages and the figure before using the output.

# ---- Settings ---------------------------------------------------------------
threshold_method <- "maxsss"      # "maxsss" or "p10": which of 5b's files to read (the means are the same in both)
baseline_period  <- "1981-2010"
periods          <- c("2011-2040", "2041-2070", "2071-2100")
min_pct_land     <- 2             # show only classes that make up at least this % of Iceland's land
n_col            <- 4             # plots per row (8 classes = 2 rows of 4)
y_scale          <- "shared"      # "shared" = one y-axis for all plots; "free" = each plot has its own
scenarios  <- c(ssp126 = "SSP1-2.6", ssp370 = "SSP3-7.0", ssp585 = "SSP5-8.5")
line_cols  <- c(ssp126 = "grey58", ssp370 = "grey30", ssp585 = "black")       # greys, so the lines contrast with the coloured plots
line_pch   <- c(ssp126 = 16, ssp370 = 17, ssp585 = 15)                        # marker for each scenario (so greys can be told apart)
tint_alpha <- 0.32                # how strongly each plot is tinted with its class colour (0 = white, 1 = full colour)
panel_letters <- letters          # letter under each line plot, in order: a, b, c, ... (use LETTERS for A, B, C, ...)
letter_fmt    <- "(%s)"           # "(%s)" gives (a); "%s" gives a
map_in_figure <- TRUE             # put the map to the right of the plots
separate_map  <- TRUE             # also save the map as its own figure
exclude_codes <- "523"            # classes that are not land. 523 = Sea and ocean
map_simplify_m <- 300             # polygons are simplified by this many metres for drawing the map
map_pad_frac   <- 0.01            # sea margin around the land on the map, as a fraction of the land's width and height (0 = none)
key_cex        <- 1.5             # size of the text and colour boxes in the map's key (the list of classes next to the map)
legend_cex     <- 1.8216          # size of the scenario legend along the bottom of the figure (was 1.518)
xtick_cex      <- 1.027           # size of the period labels on each plot's x-axis, e.g. 1981-2010 (was 0.893)
ytick_cex      <- 1.104           # size of the numbers on each plot's y-axis, e.g. 0.2 (was 0.96)
title_cex      <- 0.8556          # size of the title above each line plot, e.g. Moors and heathland (was 0.713)
main_title_cex <- 1.14            # size of the main title across the top of the figure (was 0.95)
ylab_cex       <- 1.683           # size of the "Mean suitability" label on the left (was 1.4025)
ylab_line      <- 2.7             # how far left of the axis that label sits, in margin lines (was 2.5; moved out so the bigger label clears the y numbers)
scale_text_cex <- 0.91            # size of the "100 km" text on the map's scale bar (was 0.7)
map_below_version <- TRUE         # also save a version of the figure with the map and its key below the plots instead of to the right
map_below_h    <- 800             # in that version: height of the map panel, in pixels
map_below_key_w <- 1300           # in that version: width of the key panel next to the map, in pixels
plot_w         <- 700             # width of each line plot's drawing area, in pixels (the figure is 300 dpi)
plot_h         <- 640             # height of each line plot's drawing area, in pixels
col_sea    <- "#eef3f8"           # map background (sea)
col_other  <- "grey85"            # map colour for land in classes that are not shown
# Class colours (by CORINE code), the same as in 5a and 5b. Any shown class not listed gets a colour from a fallback palette.
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
gcm        <- "gfdl-esm4"         # only for the title

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
out_dir <- p("3-modelling", "Outputs", "CORINE")
fig_dir <- file.path(out_dir, "Figures")
tab_dir <- file.path(out_dir, "Tables")
for (d in c(fig_dir, tab_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
in_file <- file.path(tab_dir, paste0("5b_suitability_by_scenario_", threshold_method, ".csv"))
corine_file <- p("1-download-data", "Downloads", "CORINE", "corine_2018_iceland.gpkg")
if (!file.exists(corine_file)) stop("Missing input: ", corine_file, "\nRun corine-landcover-download-iceland.R first.")
suppressPackageStartupMessages(library(sf))
if (!file.exists(in_file)) stop("Missing input: ", in_file, "\nRun 5b-corine-class-suitability.R first.")
tab <- read.csv(in_file, stringsAsFactors = FALSE, colClasses = c(code = "character"))
need <- c("ssp", "period", "code", "class_name", "km2_land", "pct_of_land_area", "n_points", "mean_suitability")
if (!all(need %in% names(tab))) stop("The 5b file is missing column(s): ", paste(setdiff(need, names(tab)), collapse = ", "), ". Rerun 5b.")

base <- tab[tab$ssp == "baseline" & tab$period == baseline_period, ]
if (nrow(base) == 0) stop("No baseline rows for ", baseline_period, " in ", basename(in_file), ".")
base <- base[base$pct_of_land_area >= min_pct_land, ]
if (nrow(base) == 0) stop("No class makes up at least ", min_pct_land, "% of the land.")
base <- base[order(-base$km2_land), ]
message(sprintf("%d class(es) with at least %g%% of Iceland's land: %s", nrow(base), min_pct_land, paste(base$code, collapse = ", ")))
if (nrow(base) > length(panel_letters)) stop("More classes than panel letters; extend panel_letters.")

# ---- Values: one row per class, scenario and period -----------------------------------------
x_lab <- c(baseline_period, periods)
get_mean <- function(code, ssp, per) {
  v <- tab$mean_suitability[tab$code == code & tab$ssp == ssp & tab$period == per]
  if (length(v) == 0) stop("No row for class ", code, ", ", ssp, ", ", per, " in ", basename(in_file), ". Rerun 5b.")
  v[1]
}
vals <- lapply(seq_len(nrow(base)), function(i) {
  b0 <- get_mean(base$code[i], "baseline", baseline_period)
  sapply(names(scenarios), function(s) c(b0, vapply(periods, function(per) get_mean(base$code[i], s, per), numeric(1))))
})                                                   # each element: 4 x 3 matrix (periods x scenarios)
out <- do.call(rbind, lapply(seq_len(nrow(base)), function(i) {
  do.call(rbind, lapply(seq_along(scenarios), function(k)
    data.frame(code = base$code[i], class_name = base$class_name[i], pct_of_land_area = round(base$pct_of_land_area[i], 2),
               n_points = base$n_points[i], scenario = unname(scenarios[k]), period = x_lab,
               mean_suitability = round(vals[[i]][, k], 4), change_from_baseline = round(vals[[i]][, k] - vals[[i]][1, k], 4),
               stringsAsFactors = FALSE)))
}))
write.csv(out, file.path(tab_dir, paste0("5e_mean_suitability_over_time_", threshold_method, ".csv")), row.names = FALSE)

# ---- Colours and map data --------------------------------------------------------------------------
cols_for <- stats::setNames(rep(NA_character_, nrow(base)), base$code)
known <- intersect(base$code, names(class_cols))
cols_for[known] <- class_cols[known]
missing <- setdiff(base$code, known)
if (length(missing) > 0) cols_for[missing] <- grDevices::hcl.colors(length(missing), "Dark 3")

poly <- st_read(corine_file, quiet = TRUE)
code_col <- names(poly)[grepl("^code_?18$", names(poly), ignore.case = TRUE)][1]
if (is.na(code_col)) code_col <- names(poly)[grepl("^code", names(poly), ignore.case = TRUE)][1]
if (is.na(code_col)) stop("Couldn't find the CORINE code column. Columns are: ", paste(names(poly), collapse = ", "))
poly$clc_code <- as.character(as.integer(as.character(poly[[code_col]])))
poly <- st_transform(poly, 3057)                                   # ISN93 / Lambert (metres)
if (any(!st_is_valid(poly))) poly <- st_make_valid(poly)
poly <- poly[!poly$clc_code %in% exclude_codes, ]
ms <- st_sf(clc_code = poly$clc_code, geometry = st_geometry(poly))
ms <- st_simplify(ms, dTolerance = map_simplify_m, preserveTopology = TRUE)
ms <- ms[!st_is_empty(ms), ]
ms_col <- unname(ifelse(ms$clc_code %in% base$code, cols_for[ms$clc_code], col_other))

# Map extent: the bounding box of the land polygons (sea is already excluded) plus a small margin.
bb0 <- st_bbox(ms)
pad_x <- map_pad_frac * as.numeric(bb0[["xmax"]] - bb0[["xmin"]])
pad_y <- map_pad_frac * as.numeric(bb0[["ymax"]] - bb0[["ymin"]])
bb <- c(xmin = as.numeric(bb0[["xmin"]]) - pad_x, xmax = as.numeric(bb0[["xmax"]]) + pad_x,
        ymin = as.numeric(bb0[["ymin"]]) - pad_y, ymax = as.numeric(bb0[["ymax"]]) + pad_y)
map_asp <- as.numeric((bb["xmax"] - bb["xmin"]) / (bb["ymax"] - bb["ymin"]))
key_lab <- c(substr(base$class_name, 1, 34), "Other land")
key_col <- c(unname(cols_for[base$code]), col_other)

draw_map <- function(scale_bar = FALSE) {
  plot.new()
  plot.window(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"]), asp = 1, xaxs = "i", yaxs = "i")
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col = col_sea, border = NA)
  plot(st_geometry(ms), col = ms_col, border = NA, add = TRUE)
  if (scale_bar) {
    x0 <- usr[1] + 0.03 * (usr[2] - usr[1]); y0 <- usr[3] + 0.05 * (usr[4] - usr[3])
    segments(x0, y0, x0 + 100000, y0, lwd = 3, lend = 1)
    text(x0 + 50000, y0, "100 km", pos = 3, cex = scale_text_cex)
  }
  box(col = "grey30", lwd = 1.2)
}
draw_key <- function(cex = key_cex) {
  plot.new()
  legend("topleft", legend = key_lab, fill = key_col, border = "grey40", bty = "n", cex = cex, y.intersp = 1.3)
}

# ---- Figure: the line plots, tinted by class, with the map -----------------------------------------------
n_cls  <- nrow(base)
n_row  <- ceiling(n_cls / n_col)
ylim_all <- c(0, max(0.05, ceiling(max(unlist(vals), na.rm = TRUE) * 20) / 20))      # 0 to the largest value, rounded up to 0.05
titles <- substr(base$class_name, 1, 36)

rows_l <- max(n_row, 2)
# Sizes are worked out in pixels so that each line plot's drawing area is plot_w x plot_h whatever the margins are.
# One margin "line" is 0.2 in * 0.66 * 300 dpi = 39.6 px (layout() with more than two rows or columns shrinks R's base text
# size to 0.66, and the margin lines shrink with it).
line_px <- 0.2 * 0.66 * 300
mar_b <- 3.3; mar_t <- 1.5; mar_r <- 0.9                  # plot margins in lines: bottom (period labels + panel letter), top (title), right
mar_l_first <- 4.2; mar_l_other <- 2.2                  # left margin: the first plot in each row also holds the "Mean suitability" label
oma_top <- 1.8                                            # outer top margin (the figure title), in lines
leg_h   <- 0.16                                           # height of the scenario legend row, relative to one row of plots
row_h   <- plot_h + (mar_b + mar_t) * line_px             # height of one row of line plots, px
col_w   <- round(plot_w + (c(mar_l_first, rep(mar_l_other, n_col - 1)) + mar_r) * line_px)    # width of each column of plots, px
# The map gets a panel the same shape as the map, so it shows little empty sea (0.3-line margins on all four sides).
map_w_px <- round(map_asp * (row_h - 0.6 * line_px) + 0.6 * line_px)
pan_ids <- matrix(0, rows_l, n_col)
for (i in seq_len(n_cls)) pan_ids[ceiling(i / n_col), ((i - 1) %% n_col) + 1] <- i

# Draws the whole figure. map_pos: "right" = map with its key to the right of the line plots, "below" = map with its key
# under the line plots (the scenario legend then sits between the two), "none" = line plots only.
# Everything else is identical between the versions.
draw_figure <- function(map_pos, file) {
  heights_px <- c(rep(row_h, rows_l), leg_h * row_h)
  if (map_pos == "right") {
    lay <- cbind(pan_ids, 0)
    lay[1, n_col + 1] <- n_cls + 1                      # map, top right
    lay[2:rows_l, n_col + 1] <- n_cls + 2               # key to the map, below it
    lay <- rbind(lay, rep(n_cls + 3, ncol(lay)))        # scenario legend along the bottom
    lay_w <- c(col_w, map_w_px)
  } else if (map_pos == "below") {
    # The map and its key sit in a row under the scenario legend, centred under the plots. The layout grid is the union of
    # the plot columns and the map/key edges, so a plot may span several grid columns.
    fig_w0 <- sum(col_w)
    mw   <- round(map_asp * (map_below_h - 0.6 * line_px) + 0.6 * line_px)
    kw   <- min(map_below_key_w, fig_w0 - mw)
    pad_px <- round((fig_w0 - mw - kw) / 2)
    b_plot <- cumsum(c(0, col_w))
    b_map  <- c(pad_px, pad_px + mw, pad_px + mw + kw)
    brk <- sort(unique(c(b_plot, b_map, fig_w0)))
    mid <- (brk[-1] + brk[-length(brk)]) / 2
    lay_w <- diff(brk)
    plot_col <- vapply(mid, function(x) which(x > b_plot[-length(b_plot)] & x < b_plot[-1])[1], numeric(1))
    top <- pan_ids[, plot_col, drop = FALSE]
    bot <- ifelse(mid > b_map[1] & mid < b_map[2], n_cls + 1, ifelse(mid > b_map[2] & mid < b_map[3], n_cls + 2, 0))
    lay <- rbind(top, rep(n_cls + 3, length(mid)), matrix(bot, nrow = 1))
    heights_px <- c(heights_px, map_below_h)
  } else {
    lay <- rbind(pan_ids, rep(n_cls + 1, n_col))
    lay_w <- col_w
  }
  fig_w <- sum(lay_w)
  fig_h <- round(sum(heights_px) + oma_top * line_px)
  png(file, width = fig_w, height = fig_h, res = 300)
  layout(lay, widths = lay_w, heights = heights_px)
  par(oma = c(0, 0, oma_top, 0), mgp = c(ylab_line, 0.7, 0))
  for (i in seq_len(n_cls)) {
    par(mar = c(mar_b, if (((i - 1) %% n_col) == 0) mar_l_first else mar_l_other, mar_t, mar_r))
    m <- vals[[i]]
    yl <- if (y_scale == "free") {
      r <- range(m, na.rm = TRUE); pad <- max(diff(r) * 0.12, 0.01); c(max(0, r[1] - pad), min(1, r[2] + pad))
    } else ylim_all
    col_i <- cols_for[[base$code[i]]]
    plot(NA, xlim = c(0.8, length(x_lab) + 0.2), ylim = yl, xaxt = "n", xlab = "", ylab = if (((i - 1) %% n_col) == 0) "Mean suitability" else "",
         las = 1, cex.axis = ytick_cex, cex.lab = ylab_cex)
    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col = adjustcolor(col_i, alpha.f = tint_alpha), border = NA)    # class colour
    grid(nx = NA, ny = NULL, col = adjustcolor("white", 0.7), lty = 1)
    abline(v = seq_along(x_lab), col = adjustcolor("white", 0.7))
    axis(1, at = seq_along(x_lab), labels = x_lab, cex.axis = xtick_cex, las = 1, tick = TRUE)
    for (k in seq_along(scenarios)) {
      lines(seq_along(x_lab), m[, k], col = line_cols[[k]], lwd = 2.4)
      points(seq_along(x_lab), m[, k], col = line_cols[[k]], pch = line_pch[[k]], cex = 0.85)
    }
    points(1, m[1, 1], pch = 21, bg = "white", col = "black", cex = 1.15, lwd = 1.4)       # the shared baseline point
    mtext(titles[i], side = 3, line = 0.5, adj = 0, font = 2, cex = title_cex)
    box(col = col_i, lwd = 3)
    mtext(sprintf(letter_fmt, panel_letters[i]), side = 1, line = 2.1, at = mean(usr[1:2]), font = 2, cex = 0.9)   # panel letter, centred under the plot
  }
  if (map_pos != "none") {
    par(mar = c(0.3, 0.3, 0.3, 0.3)); draw_map(scale_bar = TRUE)
    par(mar = c(0.3, 0.8, 0.3, 0.3)); draw_key()
  }
  mtext(sprintf("Mean habitat suitability of main CORINE classes over time (%s)", toupper(gcm)),
        outer = TRUE, side = 3, line = 0.4, font = 2, cex = main_title_cex)
  par(mar = c(0, 0, 0, 0)); plot.new()
  legend("center", legend = unname(scenarios), col = unname(line_cols), lwd = 2.4, pch = unname(line_pch), bty = "n", horiz = TRUE, cex = legend_cex)
  invisible(dev.off())
}

fig_file       <- file.path(fig_dir, paste0("5e_mean_suitability_over_time_", threshold_method, ".png"))
fig_file_below <- file.path(fig_dir, paste0("5e_mean_suitability_over_time_", threshold_method, "_map_below.png"))
draw_figure(if (map_in_figure) "right" else "none", fig_file)
if (map_in_figure && map_below_version) draw_figure("below", fig_file_below)

# ---- Figure: the map on its own ------------------------------------------------------------------------------
map_file <- file.path(fig_dir, "5e_class_map.png")
if (separate_map) {
  map_w <- 3600
  png(map_file, width = map_w, height = round(map_w * (3 / 4.4) / map_asp) + 500, res = 300)
  layout(matrix(1:2, nrow = 1), widths = c(3, 1.4))
  par(mar = c(1, 1, 4, 0.5)); draw_map(scale_bar = TRUE)
  mtext(sprintf("CORINE 2018 classes making up at least %g%% of Iceland's land", min_pct_land), side = 3, line = 1, font = 2, cex = 1)
  par(mar = c(1, 0, 4, 1)); draw_key()
  invisible(dev.off())
}

message("\n=== Mean suitability by class, baseline and ", tail(periods, 1), " ===")
last <- out[out$period == tail(periods, 1), ]
print(data.frame(class = substr(paste(last$code, last$class_name), 1, 34), scenario = last$scenario, mean = last$mean_suitability,
                 change_from_baseline = last$change_from_baseline), row.names = FALSE)
message("\nSaved: ", fig_file)
if (map_in_figure && map_below_version) message("Saved: ", fig_file_below)
if (separate_map) message("Saved: ", map_file)
message("Saved: ", file.path(tab_dir, paste0("5e_mean_suitability_over_time_", threshold_method, ".csv")))