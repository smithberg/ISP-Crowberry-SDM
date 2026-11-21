# 3b-screen-correlated-predictors.R
# Extracts predictor values (1981-2010) at the thinned presences and the background points, then
# screens out correlated variables so the final set has no pair with |r| above the threshold.
# Runs the whole screening once for EACH correlation method (Pearson and Spearman) and writes the
# same set of files for each, so the two can be compared directly.
#
# Put this file in: ISP-Crowberry-SDM/2-process-data/Scripts/
# Reads:  2-process-data/Outputs/CHELSA-BIOCLIM filled/**/*_1981-2010_iceland.tif   (32 expected)
#         2-process-data/Outputs/EarthEnv clipped+resampled/*.tif                   (6 expected)
#         2-process-data/Outputs/GBIF thinned/crowberry_occurrences_thinned_<thin_km>km.csv
#         2-process-data/Outputs/Background points/background_points_<n>.csv
# Writes: 2-process-data/Outputs/Predictor screening/Pearson/    and    .../Spearman/
#           predictor_values_presence_background.csv   all candidate values at every point
#           correlation_matrix_background.csv          |r| between all candidates (background points)
#           correlation_heatmap.png                    clustered picture; kept names in green
#           screening_log.csv                          every variable: kept or dropped, and why
#           selected_predictors_pearson.csv / _spearman.csv   the final list for that method
#         2-process-data/Outputs/Predictor screening/
#           selected_predictors.csv                    variables kept by BOTH methods (the main list;
#                                                      the one later scripts read)
#           method_comparison_variables.csv            kept/dropped under each method, side by side
#           method_comparison_pairs.csv                pairs above the threshold under either method
#           heatmaps_pearson_vs_spearman.png           the two heatmaps side by side, same variable order,
#                                                      plus a list of the variables kept by both methods
#                                                      (only made when both methods are in cor_methods); the two
#                                                      heatmaps are lettered (a) and (b), in the bottom-left corner of each;
#                                                      all its text is text_scale times the original size, and the figure
#                                                      is sized from the measured text so nothing is cut off
# Inputs are only read, never changed.
#
# How variables are chosen (no model is fitted here):
#   1. Constant variables are dropped.
#   2. Each variable gets a univariate score: how well it separates presences from background
#      (AUC, as |AUC - 0.5|). Stronger variables are considered first.
#   3. Going down that order, a variable is kept only if |r| <= cor_threshold with every variable
#      already kept; otherwise it is dropped and the log says which kept variable it correlated with.
#   4. Optionally, the variable with the highest VIF is removed while any VIF is > vif_max, and the
#      selection in step 3 is re-run without it (so variables dropped only because of it come back).
# This is a pre-screen. The univariate score is crude (presences are biased toward lowlands and
# roads), so after the baseline MaxEnt model you can still refine with permutation importance.
#
# NOT TESTED on your data. Check the logs and the heatmaps before using the output.

# install.packages(c("terra", "readr"))
library(terra)

# ---- Settings ---------------------------------------------------------------
thin_km        <- 5
n_background   <- 10000
cor_threshold  <- 0.75        # drop one of any pair with |r| above this
cor_methods    <- c("pearson", "spearman")   # each one gets its own output folder
vif_max        <- 10          # NULL = skip the VIF step
include_earthenv <- TRUE      # the 6 topography layers are candidates too
prefer         <- character(0)  # optional: variables to consider first, e.g. c("bio01", "bio12")
panel_letters  <- letters     # letter in the bottom-left corner of each heatmap in the side-by-side figure: a, b (LETTERS for A, B)
letter_fmt     <- "(%s)"      # "(%s)" gives (a); "%s" gives a
letter_cex     <- 2.2         # base size of the panel letters (multiplied by text_scale)
text_scale     <- 2           # side-by-side figure: every piece of text is this many times its original size (1 = original)
label_spacing  <- 1.35        # side-by-side figure: heatmap cell size as a multiple of the height of a variable name
#   (1.35 = names about one line apart, close to the original cell size; below ~1.2 they start to touch)
legend_spacing <- 0.8         # side-by-side figure: line spacing in the legend and the list (1 = R's default, smaller = closer together)
right_pad_px   <- 40          # side-by-side figure: pixels of white space kept to the right of the widest legend text

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
chelsa_dir  <- file.path(outputs_dir, "CHELSA-BIOCLIM filled")
earth_dir   <- file.path(outputs_dir, "EarthEnv clipped+resampled")
pres_file   <- file.path(outputs_dir, "GBIF thinned", paste0("crowberry_occurrences_thinned_", thin_km, "km.csv"))
bg_file     <- file.path(outputs_dir, "Background points", paste0("background_points_", n_background, ".csv"))
screen_dir  <- file.path(outputs_dir, "Predictor screening")
dir.create(screen_dir, showWarnings = FALSE, recursive = TRUE)

for (f in c(pres_file, bg_file)) if (!file.exists(f)) stop("Missing input: ", f)
if (!dir.exists(chelsa_dir)) stop("Missing folder: ", chelsa_dir, "\nRun fill-chelsa-na-zero.R first.")
if (!all(cor_methods %in% c("pearson", "spearman", "kendall"))) stop("cor_methods must be pearson, spearman or kendall.")

# ---- Find the historic layers ----------------------------------------------
all_chelsa <- list.files(chelsa_dir, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
hist_files <- all_chelsa[grepl("_1981-2010_iceland\\.tif$", basename(all_chelsa)) &
                           !grepl("^gfdl", basename(all_chelsa))]
hist_vars  <- sub("_1981-2010_iceland\\.tif$", "", basename(hist_files))
message("Historic CHELSA layers: ", length(hist_files))
if (length(hist_files) != 32) {
  warning("Expected 32 historic CHELSA layers but found ", length(hist_files), ".")
}

earth_files <- character(0); earth_vars <- character(0)
if (include_earthenv) {
  earth_files <- list.files(earth_dir, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
  earth_vars  <- sub("_.*$", "", basename(earth_files))
  message("EarthEnv layers:        ", length(earth_files))
}

files  <- c(hist_files, earth_files)
vars   <- c(hist_vars, earth_vars)
source <- c(rep("CHELSA", length(hist_files)), rep("EarthEnv", length(earth_files)))
if (anyDuplicated(vars)) stop("Duplicate variable names: ", paste(vars[duplicated(vars)], collapse = ", "))
if (length(files) == 0) stop("No predictor layers found.")

s <- rast(files)
names(s) <- vars
if (length(earth_files) > 0 && length(hist_files) > 0) {
  if (!isTRUE(compareGeom(s[[1]], s[[length(hist_files) + 1]], stopOnError = FALSE))) {
    stop("CHELSA and EarthEnv layers are not on the same grid. Re-run clip-rasters-to-land.R.")
  }
}

# ---- Read points ------------------------------------------------------------
pres <- readr::read_csv(pres_file, show_col_types = FALSE)
plat <- intersect(c("decimalLatitude", "latitude", "lat"), names(pres))[1]
plon <- intersect(c("decimalLongitude", "longitude", "lon", "long"), names(pres))[1]
if (is.na(plat) || is.na(plon)) stop("Couldn't find lat/lon columns in ", pres_file)
bg <- readr::read_csv(bg_file, show_col_types = FALSE)
message("Presences: ", nrow(pres), "   background: ", nrow(bg))

# ---- Extract values (shared by both methods) --------------------------------
xy   <- rbind(as.matrix(pres[, c(plon, plat)]), as.matrix(bg[, c("longitude", "latitude")]))
vals <- terra::extract(s, xy)
vals$ID <- NULL
tab <- data.frame(type = c(rep("presence", nrow(pres)), rep("background", nrow(bg))),
                  longitude = xy[, 1], latitude = xy[, 2], vals, check.names = FALSE)

bad <- !complete.cases(tab[, vars])
if (any(bad)) {
  warning(sum(bad), " point(s) have NA in at least one predictor and are dropped (",
          sum(bad & tab$type == "presence"), " presences, ", sum(bad & tab$type == "background"), " background).")
  tab <- tab[!bad, ]
}
message("Extracted predictor values at ", nrow(tab), " points.")

is_pres <- tab$type == "presence"
bgv     <- tab[!is_pres, vars, drop = FALSE]

# ---- Steps 1 and 2 (shared): constant variables, univariate score -----------
sdv    <- vapply(bgv, sd, numeric(1))
const  <- names(sdv)[is.na(sdv) | sdv == 0]

uni_auc <- function(x, pres) {
  r  <- rank(x); n1 <- sum(pres); n0 <- sum(!pres)
  (sum(r[pres]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
auc   <- vapply(vars, function(v) uni_auc(tab[[v]], is_pres), numeric(1))
score <- abs(auc - 0.5)

cand <- setdiff(vars, const)
pref <- intersect(prefer, cand)
ord  <- c(pref, setdiff(cand[order(-score[cand])], pref))

# ---- Screening for one correlation method -----------------------------------
screen_one <- function(method) {
  out_dir <- file.path(screen_dir, tools::toTitleCase(method))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  message("\n---------- ", tools::toTitleCase(method), " ----------")
  
  readr::write_csv(tab, file.path(out_dir, "predictor_values_presence_background.csv"))
  
  # Correlation matrix (background points)
  R <- cor(bgv, method = method, use = "pairwise.complete.obs")
  write.csv(round(abs(R), 4), file.path(out_dir, "correlation_matrix_background.csv"))
  
  # Steps 3 and 4: greedy selection, then VIF (repeated until stable)
  greedy <- function(order_vec) {
    kept    <- character(0)
    because <- setNames(rep(NA_character_, length(vars)), vars)
    r_with  <- setNames(rep(NA_real_, length(vars)), vars)
    for (v in order_vec) {
      if (length(kept) == 0) { kept <- v; next }
      rr <- abs(R[v, kept])
      if (all(rr <= cor_threshold, na.rm = TRUE)) {
        kept <- c(kept, v)
      } else {
        j <- which.max(rr)
        because[v] <- kept[j]
        r_with[v]  <- round(R[v, kept[j]], 3)
      }
    }
    list(kept = kept, because = because, r_with = r_with)
  }
  
  # If the VIF check removes a variable, the selection is re-run without it. That way variables that
  # were dropped only because of the removed one get another chance instead of being lost.
  vif_vals    <- setNames(rep(NA_real_, length(vars)), vars)
  vif_dropped <- character(0)
  vif_note    <- setNames(rep(NA_character_, length(vars)), vars)
  repeat {
    g    <- greedy(setdiff(ord, vif_dropped))
    kept <- g$kept
    if (is.null(vif_max) || length(kept) <= 2) break
    Rk  <- cor(bgv[, kept, drop = FALSE], method = method)
    inv <- tryCatch(solve(Rk), error = function(e) NULL)
    if (is.null(inv)) { warning("Correlation matrix is singular; VIF step stopped."); break }
    vf <- setNames(diag(inv), kept)
    if (max(vf) <= vif_max) { vif_vals[kept] <- vf; break }
    worst <- names(which.max(vf))
    vif_dropped <- c(vif_dropped, worst)
    vif_note[worst] <- paste0("VIF ", round(max(vf), 1))
    message("VIF step: removing ", worst, " (VIF ", round(max(vf), 1),
            "); re-running the selection without it")
  }
  
  status  <- setNames(rep("dropped: correlated", length(vars)), vars)
  because <- g$because
  r_with  <- g$r_with
  status[const]       <- "dropped: constant"
  status[vif_dropped] <- "dropped: VIF"
  because[vif_dropped] <- vif_note[vif_dropped]
  r_with[vif_dropped]  <- NA_real_
  status[kept]        <- "kept"
  
  # Log and outputs
  scr_log <- data.frame(
    variable = vars, source = source,
    univariate_auc = round(auc, 3), score = round(score, 3),
    status = status, dropped_because_of = because, r_with_that_variable = r_with,
    vif_final = round(vif_vals, 2), stringsAsFactors = FALSE
  )
  scr_log <- scr_log[order(scr_log$status != "kept", -scr_log$score), ]
  write.csv(scr_log, file.path(out_dir, "screening_log.csv"), row.names = FALSE)
  
  sel <- scr_log[scr_log$status == "kept", ]
  rel_file <- sub(paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", proj_root), "/?"), "",
                  files[match(sel$variable, vars)])
  selected <- data.frame(variable = sel$variable, source = sel$source,
                         univariate_auc = sel$univariate_auc, historic_file = rel_file,
                         stringsAsFactors = FALSE)
  write.csv(selected, file.path(out_dir, paste0("selected_predictors_", method, ".csv")), row.names = FALSE)
  
  # Check that the selected CHELSA variables exist for every future scenario
  fut_pat <- function(v) paste0("(^|_)", v, "_(2011-2040|2041-2070|2071-2100)_iceland\\.tif$")
  for (v in selected$variable[selected$source == "CHELSA"]) {
    n_fut <- sum(grepl(fut_pat(v), basename(all_chelsa)) & grepl("^gfdl", basename(all_chelsa)))
    if (n_fut != 9) warning("Variable ", v, " has ", n_fut, " future layers in the filled folder (expected 9).")
  }
  
  # Heatmap
  cand_all <- setdiff(vars, const)
  hc  <- hclust(as.dist(1 - abs(R[cand_all, cand_all])), method = "average")
  o   <- cand_all[hc$order]
  M   <- abs(R[o, o])
  n   <- length(o)
  
  # Colours: three light-to-orange shades up to the threshold and one dark red above it, so the
  # cutoff shows as an abrupt colour change. The diagonal (a variable with itself) is drawn black.
  brks    <- c(seq(0, cor_threshold, length.out = 4), 1)
  cols    <- c("#FFF7BC", "#FEC44F", "#EC7014", "#7F0000")
  lab_txt <- c(sprintf("%.2f to %.2f", brks[1:3], brks[2:4]), sprintf("above %.2f", cor_threshold))
  is_kept <- o %in% kept
  lab_col <- ifelse(is_kept, "#1B7F37", "grey55")
  lab_fnt <- ifelse(is_kept, 2, 1)
  Mp <- M
  diag(Mp) <- NA
  
  png(file.path(out_dir, "correlation_heatmap.png"), width = 2100, height = 1800, res = 150)
  layout(matrix(1:2, nrow = 1), widths = c(8, 2.2))
  par(mar = c(10, 10, 6, 1))
  image(1:n, 1:n, Mp[, n:1], axes = FALSE, xlab = "", ylab = "", col = cols, breaks = brks)
  for (i in seq_len(n)) {   # black diagonal
    rect(i - 0.5, n + 1 - i - 0.5, i + 0.5, n + 1 - i + 0.5, col = "black", border = NA)
  }
  for (i in seq_len(n)) {
    axis(1, at = i, labels = o[i], las = 2, cex.axis = 0.85, col.axis = lab_col[i],
         font.axis = lab_fnt[i], tick = FALSE)
    axis(2, at = n + 1 - i, labels = o[i], las = 2, cex.axis = 0.85, col.axis = lab_col[i],
         font.axis = lab_fnt[i], tick = FALSE)
  }
  box()
  title(main = sprintf("%s |r| between predictors at background points\nTwo variables with |r| above %.2f cannot both be kept  (%d kept, %d dropped)",
                       tools::toTitleCase(method), cor_threshold, sum(is_kept), sum(!is_kept)),
        cex.main = 1.1, line = 2)
  
  par(mar = c(10, 0, 6, 0), xpd = NA)
  plot.new()
  legend("topleft", legend = c(lab_txt, "same variable"), fill = c(cols, "black"),
         border = "grey40", title = "|r| (absolute correlation)", bty = "n", cex = 1.1)
  legend("left", legend = c("kept", "dropped"), text.col = c("#1B7F37", "grey55"),
         text.font = c(2, 1), title = "Label colour", bty = "n", cex = 1.1)
  invisible(dev.off())
  
  # Summary
  message("=== ", tools::toTitleCase(method), " screening summary (|r| threshold ", cor_threshold, ") ===")
  message("Candidates:           ", length(vars))
  message("  dropped constant:   ", sum(status == "dropped: constant"))
  message("  dropped correlated: ", sum(status == "dropped: correlated"))
  message("  dropped VIF:        ", sum(status == "dropped: VIF"))
  message("Kept:                 ", length(kept))
  print(selected[, c("variable", "source", "univariate_auc")], row.names = FALSE)
  message("Saved to: ", out_dir)
  
  list(R = R, kept = kept, status = status, out_dir = out_dir, selected = selected)
}

results <- lapply(cor_methods, screen_one)
names(results) <- cor_methods

# ---- Main list: variables kept by every method ------------------------------
# A subset of a set with no pair above the threshold also has no pair above it, so this list passes
# the correlation check under both Pearson and Spearman.
kept_all  <- Reduce(intersect, lapply(results, function(r) r$kept))
final_sel <- results[[1]]$selected[results[[1]]$selected$variable %in% kept_all, ]
final_sel <- final_sel[order(-abs(final_sel$univariate_auc - 0.5)), ]
write.csv(final_sel, file.path(screen_dir, "selected_predictors.csv"), row.names = FALSE)
message("\nMain list (kept by ", paste(cor_methods, collapse = " and "), "): ", nrow(final_sel), " variables")
print(final_sel[, c("variable", "source", "univariate_auc")], row.names = FALSE)

# ---- Compare the methods ----------------------------------------------------
if (all(c("pearson", "spearman") %in% cor_methods)) {
  Rp <- results$pearson$R; Rs <- results$spearman$R
  
  var_cmp <- data.frame(
    variable = vars, source = source,
    pearson  = results$pearson$status[vars],
    spearman = results$spearman$status[vars],
    stringsAsFactors = FALSE, row.names = NULL
  )
  var_cmp$same_decision <- (var_cmp$pearson == "kept") == (var_cmp$spearman == "kept")
  var_cmp <- var_cmp[order(var_cmp$same_decision, var_cmp$variable), ]
  write.csv(var_cmp, file.path(screen_dir, "method_comparison_variables.csv"), row.names = FALSE)
  
  cv <- setdiff(vars, const)
  ij <- which(upper.tri(Rp[cv, cv]), arr.ind = TRUE)
  pairs <- data.frame(
    variable_1 = cv[ij[, 1]], variable_2 = cv[ij[, 2]],
    pearson_r  = round(Rp[cv, cv][ij], 3), spearman_r = round(Rs[cv, cv][ij], 3),
    stringsAsFactors = FALSE
  )
  pairs$abs_gap <- round(abs(abs(pairs$spearman_r) - abs(pairs$pearson_r)), 3)
  pairs$flagged_by <- ifelse(abs(pairs$pearson_r) > cor_threshold & abs(pairs$spearman_r) > cor_threshold, "both",
                             ifelse(abs(pairs$pearson_r) > cor_threshold, "pearson only",
                                    ifelse(abs(pairs$spearman_r) > cor_threshold, "spearman only", "neither")))
  pairs <- pairs[pairs$flagged_by != "neither", ]
  pairs <- pairs[order(pairs$flagged_by == "both", -pairs$abs_gap), ]
  write.csv(pairs, file.path(screen_dir, "method_comparison_pairs.csv"), row.names = FALSE)
  
  kp <- results$pearson$kept; ks <- results$spearman$kept
  message("\n========== Pearson vs Spearman ==========")
  message("Pairs above ", cor_threshold, ":  both ", sum(pairs$flagged_by == "both"),
          ",  Pearson only ", sum(pairs$flagged_by == "pearson only"),
          ",  Spearman only ", sum(pairs$flagged_by == "spearman only"))
  message("Variables kept:  Pearson ", length(kp), ",  Spearman ", length(ks),
          ",  in both ", length(intersect(kp, ks)))
  if (!setequal(kp, ks)) {
    message("  only Pearson keeps:  ", paste(setdiff(kp, ks), collapse = ", "))
    message("  only Spearman keeps: ", paste(setdiff(ks, kp), collapse = ", "))
  } else {
    message("  The two methods keep the same variables.")
  }
  message("Saved: ", file.path(screen_dir, "method_comparison_variables.csv"))
  message("Saved: ", file.path(screen_dir, "method_comparison_pairs.csv"))
  
  # ---- Side-by-side heatmaps --------------------------------------------------
  # Both panels use the SAME variable order (clustered on the average of the Pearson and Spearman
  # |r|), so a given cell is in the same place in both and the two methods can be compared by eye.
  # Label colour shows kept (green, bold) or dropped (grey) under that panel's own method.
  cand_all <- setdiff(vars, const)
  n  <- length(cand_all)
  Mavg <- (abs(Rp[cand_all, cand_all]) + abs(Rs[cand_all, cand_all])) / 2
  o  <- cand_all[hclust(as.dist(1 - Mavg), method = "average")$order]
  
  brks    <- c(seq(0, cor_threshold, length.out = 4), 1)
  cols    <- c("#FFF7BC", "#FEC44F", "#EC7014", "#7F0000")
  lab_txt <- c(sprintf("%.2f to %.2f", brks[1:3], brks[2:4]), sprintf("above %.2f", cor_threshold))
  
  # ---- Sizes in the side-by-side figure ---------------------------------------
  # Every piece of text is text_scale times its original size. At that size the variable names, titles,
  # legend and list need much more room than before, so the figure is sized from the text itself: each
  # piece is measured on a scratch png (same device and resolution as the real figure), and the margins,
  # the heatmap cell size and the right-hand column are made big enough that nothing is cut off or overlaps.
  fig_res     <- 150
  # In a layout() with three columns R draws all text at 0.66 of the size asked for, so the sizes written in the
  # original code were really 0.66 times smaller. That shrinking is switched off below (par(cex = 1)) so that the
  # measurements and the drawing agree, and layout_cex puts the 0.66 back into the sizes: the text ends up exactly
  # text_scale times the size it had in the original figure.
  layout_cex  <- 0.66
  tsz         <- text_scale * layout_cex
  cex_title   <- 1.82 * tsz            # panel titles
  cex_axis    <- 1.10 * tsz            # variable names along the axes
  cex_letter  <- letter_cex * tsz      # (a) and (b)
  cex_leg     <- 1.65 * tsz            # colour legend
  cex_key     <- 1.43 * tsz            # "Label colour" key
  cex_head    <- 1.90 * tsz            # "Kept by both methods" heading
  nk          <- nrow(final_sel)
  cex_list    <- 1.5 * tsz * (if (nk > 24) 24 / nk else 1)   # the list shrinks only for very long lists
  list_txt    <- if (nk > 0) final_sel$variable else "(none)"
  head_txt    <- sprintf("Kept by both\nmethods (%d)", nk)
  panel_lab   <- sprintf(letter_fmt, panel_letters[1:2])
  panel_title <- function(method) {
    k <- sum(o %in% results[[method]]$kept)
    sprintf("%s  (%d kept, %d dropped)", tools::toTitleCase(method), k, n - k)
  }
  
  # An empty plot whose user coordinates are inches, so sizes can be read off and used directly.
  inch_canvas <- function() {
    plot.new()
    pin <- par("pin")
    plot.window(xlim = c(0, pin[1]), ylim = c(0, pin[2]), xaxs = "i", yaxs = "i")
  }
  # The three legends of the right-hand column (x, y = top-left corner, in inches; plot = FALSE only measures).
  legend_colour <- function(x, y, ...) legend(x = x, y = y, legend = c(lab_txt, "same variable"), fill = c(cols, "black"),
                                              border = "grey40", title = "|r| (absolute correlation)", bty = "n",
                                              cex = cex_leg, y.intersp = legend_spacing, ...)
  legend_key    <- function(x, y, ...) legend(x = x, y = y, legend = c("kept", "dropped"), text.col = c("#1B7F37", "grey55"),
                                              text.font = c(2, 1), title = "Label colour", bty = "n", cex = cex_key,
                                              y.intersp = legend_spacing, ...)
  legend_list   <- function(x, y, ...) legend(x = x, y = y, legend = list_txt, pch = if (nk > 0) 16 else NA,
                                              pt.cex = 0.9 * cex_list, col = "black", text.col = "black",
                                              text.font = 2, bty = "n", cex = cex_list, y.intersp = legend_spacing, ...)
  
  # Measure everything (sizes in inches)
  png(tempfile(fileext = ".png"), width = 4000, height = 4000, res = fig_res)
  par(mai = c(0, 0, 0, 0))
  inch_canvas()
  sw <- function(s, cex, font = 1) strwidth(s,  units = "inches", cex = cex, font = font)
  sh <- function(s, cex, font = 1) strheight(s, units = "inches", cex = cex, font = font)
  top0     <- par("pin")[2]
  lab_w    <- max(sw(o, cex_axis, 2))               # widest variable name (bold, the wider of the two styles)
  lab_h    <- sh("M", cex_axis, 2)                  # height of a variable name
  ttl_txt  <- c(panel_title("pearson"), panel_title("spearman"))
  ttl_w    <- max(sw(ttl_txt, cex_title, 2))
  ttl_h    <- max(sh(ttl_txt, cex_title, 2))
  pl_w     <- max(sw(panel_lab, cex_letter, 2))
  pl_h     <- max(sh(panel_lab, cex_letter, 2))
  head_w   <- sw(head_txt, cex_head, 2)
  head_h   <- sh(head_txt, cex_head, 2)
  box1     <- legend_colour(0, top0, plot = FALSE)$rect
  box2     <- legend_list(0, top0, plot = FALSE)$rect
  box3     <- legend_key(0, top0, plot = FALSE)$rect
  head_off <- cex_list * par("cin")[1]              # the heading lines up with the text of the list, not the bullets
  line_in  <- par("cin")[2]                         # one margin line, in inches
  invisible(dev.off())
  
  # Layout (inches)
  axis_line  <- 0.7    # gap between the heatmap and the variable names, in margin lines
  title_line <- 1.3    # gap between the heatmap and the panel title, in margin lines
  slack      <- 0.15   # spare room beyond the widest name, so nothing touches the edge of the png
  head_gap   <- 0.1    # between the heading and the list
  min_gap    <- 0.25   # clear space between the three blocks of the right-hand column
  cell_in   <- label_spacing * lab_h
  plot_w    <- n * cell_in
  plot_h    <- max(n * cell_in, box1$h + box3$h + head_h + head_gap + box2$h + 2 * min_gap)   # tall enough for the right-hand column
  over      <- max(0, (ttl_w - plot_w) / 2)          # how far a title is wider than its heatmap, on each side
  left_in   <- max(axis_line * line_in + lab_w, 0.08 + pl_w, over) + slack
  right_in  <- max(0.3, over + 0.1)
  top_in    <- title_line * line_in + ttl_h + 0.2
  bottom_in <- max(axis_line * line_in + lab_w, 0.08 + pl_h + 0.2) + slack
  col_w     <- max(box1$w, box2$w, box3$w, head_off + head_w) + right_pad_px / fig_res
  panel_w   <- left_in + plot_w + right_in
  fig_w     <- 2 * panel_w + col_w
  fig_h     <- top_in + plot_h + bottom_in
  message("Side-by-side figure: ", round(fig_w * fig_res), " x ", round(fig_h * fig_res), " px")
  
  draw_panel <- function(method, letter) {
    kept    <- results[[method]]$kept
    Mp      <- abs(results[[method]]$R[o, o])
    diag(Mp) <- NA
    is_kept <- o %in% kept
    lab_col <- ifelse(is_kept, "#1B7F37", "grey55")
    lab_fnt <- ifelse(is_kept, 2, 1)
    
    par(mai = c(bottom_in, left_in, top_in, right_in), mgp = c(3, axis_line, 0))
    image(1:n, 1:n, Mp[, n:1], axes = FALSE, xlab = "", ylab = "", col = cols, breaks = brks)
    for (i in seq_len(n)) {   # black diagonal
      rect(i - 0.5, n + 1 - i - 0.5, i + 0.5, n + 1 - i + 0.5, col = "black", border = NA)
    }
    for (i in seq_len(n)) {
      axis(1, at = i, labels = o[i], las = 2, cex.axis = cex_axis, col.axis = lab_col[i],
           font.axis = lab_fnt[i], tick = FALSE)
      axis(2, at = n + 1 - i, labels = o[i], las = 2, cex.axis = cex_axis, col.axis = lab_col[i],
           font.axis = lab_fnt[i], tick = FALSE)
    }
    box()
    title(main = panel_title(method), cex.main = cex_title, line = title_line)
    # Panel letter in the bottom-left corner, in the empty space outside the heatmap below the last row label (bio15, say)
    # and to the left of the first column label (eastness, say): its top right corner sits just off the heatmap's corner.
    usr <- par("usr")
    op  <- par(xpd = NA)
    text(usr[1] - xinch(0.08), usr[3] - yinch(0.08), labels = letter, adj = c(1, 1), font = 2, cex = cex_letter)
    par(op)
  }
  
  side_file <- file.path(screen_dir, "heatmaps_pearson_vs_spearman.png")
  # Two heatmap panels and the right-hand column, sized from the measurements above.
  png(side_file, width = round(fig_w * fig_res), height = round(fig_h * fig_res), res = fig_res)
  layout(matrix(1:3, nrow = 1), widths = c(panel_w, panel_w, col_w))
  par(cex = 1)    # switch off R's automatic 0.66 text shrinking in a 3-panel layout (see layout_cex above)
  draw_panel("pearson",  panel_lab[1])
  draw_panel("spearman", panel_lab[2])
  par(mai = c(bottom_in, 0, top_in, 0), xpd = NA)
  inch_canvas()
  
  # Right-hand column, top to bottom: colour legend (top), the main list (kept under every method, same
  # order as selected_predictors.csv, strongest univariate score first) centred in the middle, and the
  # label-colour key in the bottom corner (level with the last heatmap rows).
  leg1   <- legend_colour(0, par("pin")[2])    # top-left corner of the column
  leg3   <- legend_key(0, box3$h)               # bottom edge level with the bottom of the heatmaps
  x0     <- 0
  list_h <- box2$h
  
  # centre the heading + list between the colour legend and the label-colour key
  space_top <- leg1$rect$top - leg1$rect$h
  space_bot <- leg3$rect$top
  y_top     <- (space_top + space_bot) / 2 + (head_h + head_gap + list_h) / 2
  
  text(x0 + head_off, y_top, head_txt, adj = c(0, 1), font = 2, cex = cex_head, col = "black")
  legend_list(x0, y_top - head_h - head_gap)
  invisible(dev.off())
  message("Saved: ", side_file)
}

message("\nMain list (the file later scripts read): ", file.path(screen_dir, "selected_predictors.csv"))
message("Per-method lists:")
for (m in cor_methods) message("  ", file.path(results[[m]]$out_dir, paste0("selected_predictors_", m, ".csv")))