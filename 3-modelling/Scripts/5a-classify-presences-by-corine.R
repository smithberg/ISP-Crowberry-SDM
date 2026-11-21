# 5a-classify-presences-by-corine.R
# Gives each thinned crowberry presence the CORINE Land Cover 2018 class of the polygon it falls in, and compares how
# the presences are spread across the classes with how the land area is spread across them.
#
# A point is classified in one of two ways, recorded in the output ("assignment"):
#   inside   the point lies inside a CORINE land polygon: it gets that polygon's class
#   nearest  the point lies in none (it is in the sea polygon, or in a gap between polygons, which happens close to the
#            coast because CORINE's coastline differs from the one used to filter your records): it gets the class of the
#            NEAREST land polygon, and the distance to that polygon is saved (snap_distance_m)
# "Land polygon" means every polygon except the sea (class 523, see exclude_codes). CORINE maps the sea as one big
# polygon, so a point just off the coast is "inside" the sea polygon; it is treated as outside the land and moved to the
# nearest land class instead.
#
# Put this file in: ISP-Crowberry-SDM/3-modelling/Scripts/
# Reads:  2-process-data/Outputs/GBIF thinned/crowberry_occurrences_thinned_<thin_km>km.csv   (from 1b)
#         1-download-data/Downloads/CORINE/corine_2018_iceland.gpkg                          (from the CORINE download script)
# Writes: 3-modelling/Outputs/CORINE/   (all four CORINE scripts, 5a to 5d, write here; files start with the script's number)
#           Tables/5a_presences_with_corine_<thin_km>km.csv   your presences with the new columns: corine_code,
#                                                             corine_name, corine_group, assignment, snap_distance_m
#           Tables/5a_class_comparison_<thin_km>km.csv        for every class: land area and share of land, number and share
#                                                             of presences (how many inside / nearest), and the ratio of
#                                                             the two shares
#           Figures/5a_points_vs_area_<thin_km>km.png         share of presences and share of land area by class (bars
#                                                             coloured by class, with an inset map of the classes)
#           Figures/5a_class_map_<thin_km>km.png              map of the classes, with the presences on top
#           Figures/5a_class_map_no_points.png                the same map without the presences
# Inputs are only read, never changed.
#
# Reading it:
#  - Shares are of LAND only (the sea polygon is left out of the area shares, and no presence is put in it).
#  - ratio = share of presences / share of land area. Above 1 means the class holds more presences than its area alone
#    would predict; below 1, fewer. Small classes have few presences, so their ratios are noisy: look at n_points too.
#  - 25 ha is the smallest patch CORINE maps, so a class is only as good as that: a presence in "Pastures" may sit in a
#    small patch of heath next to a field that CORINE didn't map separately.
#  - The presences are GBIF records, which are concentrated near roads and towns. A class holding more presences than its
#    area predicts may be good crowberry habitat, or just easy to reach.
#
# NOT TESTED on your data. Check the console messages and the figure before using the output.

# install.packages(c("sf", "readr"))
suppressPackageStartupMessages({
  library(sf)
  library(readr)
})

# ---- Settings ---------------------------------------------------------------
thin_km       <- 5                 # which thinned set to use (as in 1b)
exclude_codes <- "523"             # classes that are not land: points are never put in them. 523 = Sea and ocean
max_snap_km   <- 5                 # points moved further than this to reach a land polygon are listed as a warning
min_pct       <- 0.5               # figure: classes below this share of BOTH land area and presences are lumped into "Other"
# (set to 0 to show every class; the csv always has every class)
n_bottom_other <- 6                # figures: after the min_pct grouping, the this-many smallest remaining classes (by land area) are
# also lumped into "Other" (set to 0 for none). "Other" is always the bottom row of the bar figure.
map_simplify_m <- 300              # polygons are simplified by this many metres for drawing the maps (speed); the data is untouched
inset_width    <- 0.46             # width of the inset map in the bar figure, as a fraction of the figure width
inset_points   <- TRUE             # draw the presences on the inset map?
map_points     <- TRUE             # save the standalone map with the presences on top?
map_no_points  <- TRUE             # also save the same map without the presences (for the methods)?
legend_cex     <- 1.05             # standalone map: legend text size (was 0.8)
legend_width   <- 1.2              # standalone map: width of the legend panel relative to the map, which counts as 3 (was 1.4)
col_sea        <- "#eef3f8"        # map background (sea)
col_other      <- "grey80"         # colour for classes lumped into "Other"
# Class colours (by CORINE code). Any shown class not listed here gets a colour from a fallback palette.
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

# The 44 CORINE Land Cover classes (standard nomenclature), used to name the classes.
clc_names <- c(
  "111" = "Continuous urban fabric", "112" = "Discontinuous urban fabric",
  "121" = "Industrial or commercial units", "122" = "Road and rail networks and associated land",
  "123" = "Port areas", "124" = "Airports",
  "131" = "Mineral extraction sites", "132" = "Dump sites", "133" = "Construction sites",
  "141" = "Green urban areas", "142" = "Sport and leisure facilities",
  "211" = "Non-irrigated arable land", "212" = "Permanently irrigated land", "213" = "Rice fields",
  "221" = "Vineyards", "222" = "Fruit trees and berry plantations", "223" = "Olive groves",
  "231" = "Pastures",
  "241" = "Annual crops associated with permanent crops", "242" = "Complex cultivation patterns",
  "243" = "Land principally occupied by agriculture, with significant areas of natural vegetation",
  "244" = "Agro-forestry areas",
  "311" = "Broad-leaved forest", "312" = "Coniferous forest", "313" = "Mixed forest",
  "321" = "Natural grasslands", "322" = "Moors and heathland", "323" = "Sclerophyllous vegetation",
  "324" = "Transitional woodland-shrub",
  "331" = "Beaches, dunes, sands", "332" = "Bare rocks", "333" = "Sparsely vegetated areas",
  "334" = "Burnt areas", "335" = "Glaciers and perpetual snow",
  "411" = "Inland marshes", "412" = "Peat bogs", "421" = "Salt marshes", "422" = "Salines",
  "423" = "Intertidal flats",
  "511" = "Water courses", "512" = "Water bodies",
  "521" = "Coastal lagoons", "522" = "Estuaries", "523" = "Sea and ocean"
)

# Broader groups, for the corine_group column (edit freely). Classes not listed here get "Other".
class_groups <- list(
  "Heath, grass and shrub"    = c("321", "322", "324"),
  "Bare or sparsely vegetated" = c("331", "332", "333"),
  "Glacier"                   = "335",
  "Wetland"                   = c("411", "412", "421", "423"),
  "Forest"                    = c("311", "312", "313"),
  "Farmland"                  = c("211", "231", "242"),
  "Water"                     = c("511", "512", "521", "522"),
  "Built-up and artificial"   = c("112", "121", "122", "123", "124", "131", "132", "133", "141", "142")
)
group_of <- function(codes) {
  lookup <- unlist(lapply(names(class_groups), function(g) stats::setNames(rep(g, length(class_groups[[g]])), class_groups[[g]])))
  out <- unname(lookup[codes])
  out[is.na(out)] <- "Other"
  out
}

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
occ_file    <- p("2-process-data", "Outputs", "GBIF thinned", sprintf("crowberry_occurrences_thinned_%dkm.csv", thin_km))
corine_file <- p("1-download-data", "Downloads", "CORINE", "corine_2018_iceland.gpkg")
out_dir     <- p("3-modelling", "Outputs", "CORINE")
fig_dir     <- file.path(out_dir, "Figures")
tab_dir     <- file.path(out_dir, "Tables")
for (d in c(fig_dir, tab_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(occ_file))    stop("Missing input: ", occ_file, "\nRun 1b-gbif-spatial-thin.R first (thin_km = ", thin_km, ").")
if (!file.exists(corine_file)) stop("Missing input: ", corine_file, "\nRun corine-landcover-download-iceland.R first.")

# ---- CORINE polygons --------------------------------------------------------
poly <- st_read(corine_file, quiet = TRUE)
code_col <- names(poly)[grepl("^code_?18$", names(poly), ignore.case = TRUE)][1]
if (is.na(code_col)) code_col <- names(poly)[grepl("^code", names(poly), ignore.case = TRUE)][1]
if (is.na(code_col)) stop("Couldn't find the CORINE code column. Columns are: ", paste(names(poly), collapse = ", "))
name_col <- names(poly)[grepl("^code_text_en$", names(poly), ignore.case = TRUE)][1]
poly$clc_code <- as.character(as.integer(as.character(poly[[code_col]])))
message("CORINE polygons: ", format(nrow(poly), big.mark = ","), "   code column: ", code_col)

# Work in ISN93 / Lambert (EPSG:3057, metres), so distances and areas are in real units.
poly <- st_transform(poly, 3057)
if (any(!st_is_valid(poly))) {
  message(sum(!st_is_valid(poly)), " invalid polygon(s); repairing them.")
  poly <- st_make_valid(poly)
}
land <- poly[!poly$clc_code %in% exclude_codes, ]
land$area_km2 <- as.numeric(st_area(land)) / 1e6
message("Land polygons: ", format(nrow(land), big.mark = ","), "   land area: ",
        format(round(sum(land$area_km2)), big.mark = ","), " km2  (excluded classes: ", paste(exclude_codes, collapse = ", "), ")")

class_name <- function(codes) {
  nm <- unname(clc_names[codes])
  if (!is.na(name_col)) {
    from_data <- tapply(as.character(poly[[name_col]]), poly$clc_code, function(x) x[1])
    nm[is.na(nm)] <- unname(from_data[codes[is.na(nm)]])
  }
  nm[is.na(nm)] <- "(unnamed class)"
  nm
}

# ---- Presences --------------------------------------------------------------
occ <- readr::read_csv(occ_file, show_col_types = FALSE)
lat_col <- intersect(c("decimalLatitude", "latitude", "lat"), names(occ))[1]
lon_col <- intersect(c("decimalLongitude", "longitude", "lon", "long"), names(occ))[1]
if (is.na(lat_col) || is.na(lon_col)) stop("Couldn't find lat/lon columns. Columns are: ", paste(names(occ), collapse = ", "))
n_na <- sum(is.na(occ[[lat_col]]) | is.na(occ[[lon_col]]))
if (n_na > 0) {
  warning(n_na, " record(s) have no coordinates and are left out.")
  occ <- occ[!is.na(occ[[lat_col]]) & !is.na(occ[[lon_col]]), ]
}
message("Presences: ", nrow(occ), "  (", basename(occ_file), ")")

pts <- st_as_sf(data.frame(x = occ[[lon_col]], y = occ[[lat_col]]), coords = c("x", "y"), crs = 4326)
pts <- st_transform(pts, 3057)

# ---- Classify ---------------------------------------------------------------
hit   <- st_intersects(pts, land)
idx   <- vapply(hit, function(h) if (length(h) > 0) h[1] else NA_integer_, integer(1))   # first polygon if a point sits on a shared edge
how   <- ifelse(is.na(idx), "nearest", "inside")
snap  <- rep(0, nrow(pts))

need <- which(is.na(idx))
if (length(need) > 0) {
  nn <- st_nearest_feature(pts[need, ], land)
  snap[need] <- as.numeric(st_distance(pts[need, ], land[nn, ], by_element = TRUE))
  idx[need]  <- nn
}
occ$corine_code      <- land$clc_code[idx]
occ$corine_name      <- class_name(occ$corine_code)
occ$corine_group     <- group_of(occ$corine_code)
occ$assignment       <- how
occ$snap_distance_m  <- round(snap)

message("\nInside a land polygon: ", sum(how == "inside"), "   moved to the nearest land polygon: ", sum(how == "nearest"))
if (length(need) > 0) {
  message(sprintf("Distance moved (m): median %.0f, 90th percentile %.0f, maximum %.0f",
                  stats::median(snap[need]), stats::quantile(snap[need], 0.9), max(snap[need])))
  far <- snap > max_snap_km * 1000
  if (any(far)) {
    warning(sum(far), " point(s) were more than ", max_snap_km, " km from any land polygon and were assigned to the nearest one anyway. ",
            "Check them (assignment = nearest, large snap_distance_m) in the output csv.", call. = FALSE)
  }
}

# ---- Compare presences with area -----------------------------------------------
area_by <- tapply(land$area_km2, land$clc_code, sum)
codes   <- names(area_by)
n_pts   <- as.integer(table(factor(occ$corine_code, levels = codes)))
n_in    <- as.integer(table(factor(occ$corine_code[how == "inside"], levels = codes)))
pct_a   <- 100 * as.numeric(area_by) / sum(area_by)
pct_p   <- 100 * n_pts / sum(n_pts)
tab <- data.frame(code = codes, class_name = class_name(codes), group = group_of(codes),
                  area_km2 = as.numeric(area_by), pct_of_land_area = pct_a,
                  n_points = n_pts, n_inside = n_in, n_nearest = n_pts - n_in, pct_of_points = pct_p,
                  ratio = ifelse(pct_a > 0, pct_p / pct_a, NA_real_), stringsAsFactors = FALSE)
tab <- tab[order(-tab$area_km2), ]

tab_out <- tab
tab_out$area_km2         <- round(tab_out$area_km2)
tab_out$pct_of_land_area <- round(tab_out$pct_of_land_area, 2)
tab_out$pct_of_points    <- round(tab_out$pct_of_points, 2)
tab_out$ratio            <- round(tab_out$ratio, 2)

message("\n=== Presences and land area by CORINE class ===")
print(tab_out[tab_out$pct_of_land_area >= 0.1 | tab_out$n_points > 0,
              c("code", "class_name", "pct_of_land_area", "n_points", "pct_of_points", "ratio")], row.names = FALSE)

readr::write_csv(occ,     file.path(tab_dir, sprintf("5a_presences_with_corine_%dkm.csv", thin_km)))
readr::write_csv(tab_out, file.path(tab_dir, sprintf("5a_class_comparison_%dkm.csv", thin_km)))

# ---- Colours and map data -----------------------------------------------------------
# One colour per class, used for the bars AND the map. Classes with no colour set in class_cols get one from the fallback
# palette. Classes lumped into "Other" are drawn in col_other.
small    <- tab$pct_of_land_area < min_pct & tab$pct_of_points < min_pct
# tab is ordered by land area (largest first), so the last rows not yet grouped are the smallest classes
if (n_bottom_other > 0) small[tail(which(!small), min(n_bottom_other, sum(!small)))] <- TRUE
shown    <- tab$code[!small]
cols_for <- stats::setNames(rep(NA_character_, length(shown)), shown)
known    <- intersect(shown, names(class_cols))
cols_for[known] <- class_cols[known]
missing  <- setdiff(shown, known)
if (length(missing) > 0) cols_for[missing] <- grDevices::hcl.colors(length(missing), "Dark 3")

# A simplified copy of the polygons for drawing (the full polygons are slow to plot and not needed at this scale).
ms <- sf::st_sf(clc_code = land$clc_code, geometry = sf::st_geometry(land))
ms <- sf::st_simplify(ms, dTolerance = map_simplify_m, preserveTopology = TRUE)
ms <- ms[!sf::st_is_empty(ms), ]
ms_col <- ifelse(ms$clc_code %in% shown, cols_for[ms$clc_code], col_other)
ms_col <- unname(ms_col)
bb      <- sf::st_bbox(ms)
map_asp <- as.numeric((bb["xmax"] - bb["xmin"]) / (bb["ymax"] - bb["ymin"]))

draw_map <- function(show_points = FALSE, scale_bar = FALSE) {
  graphics::plot.new()
  graphics::plot.window(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"]), asp = 1, xaxs = "i", yaxs = "i")
  usr <- graphics::par("usr")
  graphics::rect(usr[1], usr[3], usr[2], usr[4], col = col_sea, border = NA)
  plot(sf::st_geometry(ms), col = ms_col, border = NA, add = TRUE)
  if (show_points) {
    plot(sf::st_geometry(pts), add = TRUE, pch = 21, bg = "white", col = "black", cex = 0.35, lwd = 0.4)
  }
  if (scale_bar) {
    x0 <- usr[1] + 0.03 * (usr[2] - usr[1]); y0 <- usr[3] + 0.04 * (usr[4] - usr[3])
    graphics::segments(x0, y0, x0 + 100000, y0, lwd = 3, lend = 1)
    graphics::text(x0 + 50000, y0, "100 km", pos = 3, cex = 0.875)
  }
  graphics::box(col = "grey30", lwd = 1.2)
}

# ---- Figure 1: share of presences vs share of land area, with inset map ------------------
fig <- data.frame(code = tab$code[!small], label = substr(tab$class_name[!small], 1, 40),
                  pa = tab$pct_of_land_area[!small], pp = tab$pct_of_points[!small], n = tab$n_points[!small],
                  col = unname(cols_for[tab$code[!small]]), stringsAsFactors = FALSE)
if (any(small)) {
  fig <- rbind(fig, data.frame(code = "other", label = sprintf("Other (%d classes)", sum(small)),
                               pa = sum(tab$pct_of_land_area[small]), pp = sum(tab$pct_of_points[small]),
                               n = sum(tab$n_points[small]), col = col_other, stringsAsFactors = FALSE))
}
fig    <- fig[order(fig$code == "other", -fig$pa), ]   # biggest class first, "Other" always last
n_rows <- nrow(fig)
xmax   <- max(c(fig$pa, fig$pp))
y_pos  <- rev(seq_len(n_rows))                  # biggest class at the top

fig_w <- 2800
fig_h <- round(0.6 * max(1800, 600 + 200 * n_rows))   # 40% shorter than before (it used to be max(1800, 600 + 200 * n_rows))
headroom <- 0                                   # extra space above the top row (the legend now sits beside the "Sparsely vegetated areas" row)
png(file.path(fig_dir, sprintf("5a_points_vs_area_%dkm.png", thin_km)), width = fig_w, height = fig_h, res = 300)
par(mar = c(4.5, 8, 3, 3))
# Left margin sized to the longest class label (so there is no blank space), plus room for the label gap. Falls back to 1.9 in.
lab_w   <- tryCatch(max(strwidth(fig$label, units = "inches", cex = 0.7)), error = function(e) NA_real_)
left_in <- if (is.finite(lab_w)) lab_w + 0.35 else 1.9
par(mai = c(par("mai")[1], left_in, par("mai")[3], par("mai")[4]))
plot_bottom <- par("plt")[3]                    # bottom edge of the bar plot, as a fraction of the figure height
plot.new()
plot.window(xlim = c(0, xmax * 1.18), ylim = c(0.4, n_rows + 0.6 + headroom), xaxs = "i", yaxs = "i")
abline(v = pretty(c(0, xmax * 1.18)), col = "grey92", lwd = 0.8)
rect(0, y_pos + 0.03, fig$pa, y_pos + 0.42, col = fig$col, border = NA)                                          # land area: solid (as on the map)
rect(0, y_pos - 0.42, fig$pp, y_pos - 0.03, col = adjustcolor(fig$col, alpha.f = 0.4), border = fig$col, lwd = 0.8)   # presences: pale, outlined
text(fig$pa + xmax * 0.01, y_pos + 0.225, labels = sprintf("%.1f%%", fig$pa), adj = 0, cex = 0.625, col = "grey35")   # cex was 0.5 (+25%)
text(fig$pp + xmax * 0.01, y_pos - 0.225, labels = sprintf("%.1f%%  (n = %d)", fig$pp, fig$n), adj = 0, cex = 0.625, col = "grey15")   # cex was 0.5 (+25%)
axis(1, cex.axis = 0.8)
axis(2, at = y_pos, labels = fig$label, las = 1, tick = FALSE, cex.axis = 0.7)
mtext("Share of total (%)", side = 1, line = 2.6, cex = 1.02)   # cex was 0.85 (+20%)
# Legend: the solid bar is the land area share (like the solid colours on the map), the pale outlined bar is the presence share.
# It is centred vertically on the "Sparsely vegetated areas" row (333; falls back to the third row if that class is not shown),
# with its right edge a little to the left of the plot's right edge. Adjust legend_x_shift (fraction of the plot width) to nudge it.
bar_legend_cex <- 0.95                          # (was 0.75; separate from legend_cex, which is for the standalone map)
legend_x_shift <- 0.03
usr_b   <- par("usr")
leg_row <- if (any(fig$code == "333")) y_pos[fig$code == "333"][1] else y_pos[min(3, n_rows)]
legend(x = usr_b[2] - legend_x_shift * (usr_b[2] - usr_b[1]), y = leg_row, xjust = 1, yjust = 0.5,
       legend = c("Share of land area", "Share of presences"),
       fill = c("grey40", adjustcolor("grey40", 0.4)), border = c(NA, "grey40"), bty = "n", cex = bar_legend_cex)
mtext(sprintf("Crowberry presences (%d km thinned, n = %d) and land area by CORINE 2018 class", thin_km, nrow(occ)),
      side = 3, line = 1.2, font = 2, cex = 0.95)

# Inset map, bottom right (the bars there are the shortest). inset_width is the fraction of the figure width.
in_w <- inset_width
in_h <- in_w * fig_w / fig_h / map_asp
in_y0 <- plot_bottom + 0.015                    # sits just above the x-axis line
par(fig = c(0.98 - in_w, 0.98, in_y0, in_y0 + in_h), mar = c(0, 0, 0, 0), new = TRUE)
draw_map(show_points = inset_points)
invisible(dev.off())

# ---- Figures 2 and 3: map of the classes, with and without the presences ---------------------
# Same layout both times; only the points, the title and the "Presence" legend entry differ.
map_w <- 3600
map_h <- round(map_w * (3 / (3 + legend_width)) / map_asp) + 500
leg_lab <- c(paste(fig$code[fig$code != "other"], tab$class_name[match(fig$code[fig$code != "other"], tab$code)]),
             if (any(small)) sprintf("Other (%d classes)", sum(small)))
leg_col <- c(fig$col[fig$code != "other"], if (any(small)) col_other)

make_class_map <- function(file, with_points) {
  png(file, width = map_w, height = map_h, res = 300)
  layout(matrix(1:2, nrow = 1), widths = c(3, legend_width))
  par(mar = c(1, 1, 4, 0.5))
  draw_map(show_points = with_points, scale_bar = TRUE)
  mtext(sprintf("CORINE 2018 land cover classes%s", if (with_points) sprintf(" and crowberry presences (%d km thinned)", thin_km) else ""),
        side = 3, line = 1, font = 2, cex = 1.25)
  par(mar = c(1, 0, 4, 1))
  plot.new()
  legend("topleft", legend = substr(leg_lab, 1, 34), fill = leg_col, border = NA, bty = "n", cex = legend_cex, y.intersp = 1.2)
  if (with_points) {
    legend("bottomleft", legend = "Presence", pch = 21, pt.bg = "white", col = "black", bty = "n", cex = legend_cex)
  }
  invisible(dev.off())
}

map_file        <- file.path(fig_dir, sprintf("5a_class_map_%dkm.png", thin_km))
map_file_nopts  <- file.path(fig_dir, "5a_class_map_no_points.png")
if (map_points)    make_class_map(map_file, TRUE)
if (map_no_points) make_class_map(map_file_nopts, FALSE)

message("\nSaved: ", file.path(tab_dir, sprintf("5a_presences_with_corine_%dkm.csv", thin_km)))
message("Saved: ", file.path(tab_dir, sprintf("5a_class_comparison_%dkm.csv", thin_km)))
message("Saved: ", file.path(fig_dir, sprintf("5a_points_vs_area_%dkm.png", thin_km)))
if (map_points)    message("Saved: ", map_file)
if (map_no_points) message("Saved: ", map_file_nopts)