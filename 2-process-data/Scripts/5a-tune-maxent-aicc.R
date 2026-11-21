# 5a-tune-maxent-aicc.R
# Tests every combination of MaxEnt feature classes (L, Q, H, P, T: 31 non-empty combinations) at
# several regularization multipliers (RM, 0.5 to 5 in steps of 0.5), fits one model per combination
# (310 in total) and compares them by AICc. Lower AICc = better balance of fit and complexity.
#
# Put this file in: ISP-Crowberry-SDM/2-process-data/Scripts/
# Reads:  2-process-data/Outputs/Predictor screening/selected_predictors.csv     (which layers to use)
#         2-process-data/Outputs/<historic_file column>                           (those rasters)
#         2-process-data/Outputs/Spatial blocks/occurrences_thinned_<thin_km>km_folds.csv
#         2-process-data/Outputs/Spatial blocks/background_points_<n>_folds.csv
#         2-process-data/Outputs/Background points/valid_cells_mask.tif           (optional, used if present)
# Writes: 2-process-data/Outputs/MaxEnt tuning/
#           tuning_results_all.csv       all 310 models, sorted by AICc (fc, rm, ncoef, AICc, delta.AICc, ...)
#           tuning_top_models.csv        the top_n models by AICc
#           aicc_by_rm.png               AICc against RM, one line per feature-class combination
#           tuning_failed_models.csv     models that could not be fitted (fc, rm, reason); also in
#                                        tuning_results_all.csv as NA with a fit_status column
#           run_info.txt                 R, ENMeval, maxnet and terra versions and the settings of the run
# Inputs are only read, never changed.
#
# How it works: ENMeval (algorithm = "maxnet") fits each model on the thinned presences and the
# background points. AICc is calculated from the model's predictions over the whole study area, which
# is why the predictor RASTERS are passed in, not just values at the points. k in AICc is the number of
# non-zero coefficients (ncoef), so it changes with both the feature classes and the RM.
# Presence-only data has no absences, so AICc is calculated on the presences. Compare models only within this run.
# If ncoef is not smaller than the number of presences minus 1, AICc is undefined (NA) for that model.
#
# Known issue: with maxnet 0.1.4, some models that include hinge features (H) fail with "non-conformable
# arguments" at certain RM values. The fitted model contains two coefficients with the same hinge-feature
# name (e.g. hinge(x):min:max twice), and maxnet's prediction step then rebuilds one column too few. It
# reproduces with plain maxnet, so it is deterministic for the same data and versions. Such models are
# recorded as NA with fit_status "failed: ...", listed in tuning_failed_models.csv, left out of the ranking
# and shown as gaps / red crosses in the plot. If a batch errors, it is refitted one model at a time
# (slower) to find out which models failed.
#
# Progress: the models are fitted one RM value at a time (31 models per batch). After each batch the
# console shows how many of the 310 models are done and a rough time remaining. Each finished batch is
# saved in "MaxEnt tuning/checkpoints/", so if the run is interrupted, running the script again resumes
# from the last finished batch.
#
# Re-running: if tuning_results_all.csv already exists, the models are NOT refitted (overwrite <- FALSE).
# The saved results are loaded and only the summary and plot are redone. The saved file doesn't record
# which predictors, presence set or settings produced it, so set overwrite <- TRUE (or delete the file)
# after changing any of those. A different fc/RM grid is detected and stops the script.
#
# Set run_block_cv <- TRUE to also fit each model with the 4 spatial blocks from 4a (about 5 times the
# fitting work) and get cross-validated metrics (AUC, CBI) next to the AICc.
#
# NOT TESTED on your data. Check the console messages and the plot before using the output.

# install.packages(c("terra", "readr", "ENMeval", "maxnet"))
library(terra)

# ---- Settings ---------------------------------------------------------------
thin_km      <- 5                          # which thinned presence set (must have a *_folds.csv from 4a)
n_background <- 10000                      # must match the background_points_<n>_folds.csv file
fc_letters   <- c("L", "Q", "H", "P", "T") # linear, quadratic, hinge, product, threshold
rm_values    <- seq(0.5, 5, by = 0.5)      # regularization multipliers to test
default_fc   <- "LQHPT"                    # the setting to compare against (MaxEnt default with 80+ presences)
default_rm   <- 1
top_n        <- 10                         # how many feature-class combinations get a colour in the plot
run_block_cv <- FALSE                      # TRUE = also use the 4a spatial blocks (slower)
n_cores      <- max(1, parallel::detectCores() - 1)
overwrite    <- FALSE                      # FALSE = if tuning_results_all.csv already exists, skip the fitting
# and reuse it (just redraws the plot). TRUE = refit everything.
plot_label_band <- 0.20                    # plot: empty band under the lines (fraction of the AICc range) that holds the
# "Best" / "Default" labels and the red crosses, so they don't sit on top of the lines

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

# ---- Packages ---------------------------------------------------------------
for (pkg in c("ENMeval", "maxnet", "readr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Package '", pkg, "' is needed. install.packages(\"", pkg, "\")")
}
if (utils::packageVersion("ENMeval") < "2.0.0") stop("ENMeval 2.0.0 or newer is needed (ENMevaluate with tune.args).")

# ---- Paths ------------------------------------------------------------------
outputs_dir <- p("2-process-data", "Outputs")
sel_file    <- file.path(outputs_dir, "Predictor screening", "selected_predictors.csv")
assign_dir  <- file.path(outputs_dir, "Spatial blocks")
pres_file   <- file.path(assign_dir, paste0("occurrences_thinned_", thin_km, "km_folds.csv"))
bg_file     <- file.path(assign_dir, paste0("background_points_", n_background, "_folds.csv"))
valid_file  <- file.path(outputs_dir, "Background points", "valid_cells_mask.tif")
out_dir     <- file.path(outputs_dir, "MaxEnt tuning")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
all_file    <- file.path(out_dir, "tuning_results_all.csv")
top_file    <- file.path(out_dir, "tuning_top_models.csv")
failed_file <- file.path(out_dir, "tuning_failed_models.csv")

for (f in c(sel_file, pres_file, bg_file)) if (!file.exists(f)) stop("Missing input: ", f)

# ---- The grid: 31 feature-class combinations x RM values --------------------
fc_values <- unlist(lapply(seq_along(fc_letters), function(k) {
  apply(utils::combn(fc_letters, k), 2, paste, collapse = "")
}))
if (length(fc_values) != 2^length(fc_letters) - 1) stop("Feature-class grid has the wrong size.")
message("Feature-class combinations: ", length(fc_values), "   RM values: ", length(rm_values),
        " (", paste(rm_values, collapse = ", "), ")   models to fit: ", length(fc_values) * length(rm_values))
if (!(default_fc %in% fc_values)) warning("default_fc '", default_fc, "' is not in the grid; it won't be marked.")

# ---- Predictor rasters ------------------------------------------------------
sel <- readr::read_csv(sel_file, show_col_types = FALSE)
if (!all(c("variable", "historic_file") %in% names(sel))) {
  stop("selected_predictors.csv needs the columns 'variable' and 'historic_file'.")
}
pred_files <- p(sel$historic_file)
if (any(!file.exists(pred_files))) {
  stop("Predictor raster(s) not found:\n  ", paste(pred_files[!file.exists(pred_files)], collapse = "\n  "))
}
envs <- rast(pred_files)
names(envs) <- sel$variable
message("Predictors (", nlyr(envs), "): ", paste(names(envs), collapse = ", "))

# Restrict to cells with data in every layer, the same cells the background was drawn from.
if (file.exists(valid_file)) {
  valid <- rast(valid_file)
  if (isTRUE(compareGeom(envs, valid, stopOnError = FALSE))) {
    envs <- mask(envs, valid)
  } else {
    warning("valid_cells_mask.tif is not on the predictors' grid; not applied.")
  }
}

# ---- Points -----------------------------------------------------------------
pres <- readr::read_csv(pres_file, show_col_types = FALSE)
bg   <- readr::read_csv(bg_file,   show_col_types = FALSE)
get_xy <- function(df, what) {
  lat <- intersect(c("decimalLatitude", "latitude", "lat"), names(df))[1]
  lon <- intersect(c("decimalLongitude", "longitude", "lon", "long"), names(df))[1]
  if (is.na(lat) || is.na(lon)) stop("Couldn't find lat/lon columns in the ", what, " file.")
  out <- data.frame(longitude = df[[lon]], latitude = df[[lat]])
  out
}
pxy <- get_xy(pres, "presence")
bxy <- get_xy(bg, "background")

# Drop points with no predictor data (done here, not inside ENMeval, so the fold columns stay aligned).
pv <- terra::extract(envs, as.matrix(pxy))[, names(envs), drop = FALSE]
bv <- terra::extract(envs, as.matrix(bxy))[, names(envs), drop = FALSE]
ok_p <- stats::complete.cases(pv) & !is.na(pxy$longitude) & !is.na(pxy$latitude)
ok_b <- stats::complete.cases(bv)
if (any(!ok_p) || any(!ok_b)) {
  message("Dropping points with a missing predictor value: ", sum(!ok_p), " presences, ", sum(!ok_b), " background")
}
pres <- pres[ok_p, ]; pxy <- pxy[ok_p, ]
bg   <- bg[ok_b, ];   bxy <- bxy[ok_b, ]
rownames(pxy) <- NULL; rownames(bxy) <- NULL
message("Presences: ", nrow(pxy), "   background: ", nrow(bxy))

# ---- Fit all models (skipped if results already exist) ----------------------
if (file.exists(all_file) && !overwrite) {
  message("Found existing results, so the models are NOT refitted:\n  ", all_file,
          "\nSet overwrite <- TRUE (or delete that file) to refit, e.g. after changing predictors or settings.")
  res <- read.csv(all_file, stringsAsFactors = FALSE)
  if (!all(c("fc", "rm", "AICc") %in% names(res))) stop("The existing results file lacks fc, rm or AICc columns. Set overwrite <- TRUE.")
  res$fc <- as.character(res$fc)
  res$rm <- as.numeric(res$rm)
  key_old <- paste(res$fc, res$rm)
  key_new <- as.vector(outer(fc_values, rm_values, paste))
  if (!setequal(key_old, key_new)) {
    stop("The existing results were made with a different feature-class / RM grid than the current settings. ",
         "Set overwrite <- TRUE to refit.")
  }
  if (!("ncoef" %in% names(res))) res$ncoef <- NA_integer_
  res <- res[order(res$AICc), ]
  res_out <- res
  rownames(res_out) <- NULL
} else {
  if (file.exists(all_file)) message("overwrite = TRUE: refitting and replacing ", all_file)
  args <- list(
    occs = pxy, envs = envs, bg = bxy,
    tune.args = list(fc = fc_values, rm = rm_values),
    algorithm = "maxnet",
    partitions = if (run_block_cv) "user" else "none",
    parallel = n_cores > 1
  )
  if (n_cores > 1) args$numCores <- n_cores
  if (run_block_cv) {
    if (!("fold" %in% names(pres)) || !("fold" %in% names(bg))) stop("No 'fold' column found. Run 4a-assign-spatial-blocks.R.")
    args$user.grp <- list(occs.grp = pres$fold, bg.grp = bg$fold)
    message("Using the 4 spatial blocks from 4a for cross-validation.")
  }
  # Fit one RM value at a time (31 models per batch, run in parallel) so progress can be reported,
  # and save each finished batch to checkpoints/. If the run is stopped, running the script again
  # picks up from the batches already saved. overwrite <- TRUE deletes the checkpoints.
  ckpt_dir <- file.path(out_dir, "checkpoints")
  dir.create(ckpt_dir, showWarnings = FALSE, recursive = TRUE)
  if (overwrite) unlink(list.files(ckpt_dir, full.names = TRUE))
  ckpt_file <- function(rmv) file.path(ckpt_dir, paste0("rm_", rmv, ".csv"))
  n_to_fit  <- sum(!file.exists(vapply(rm_values, ckpt_file, character(1))))
  n_models  <- length(fc_values) * length(rm_values)
  message("Fitting ", n_models, " models in ", length(rm_values), " batches (one per RM value, ",
          length(fc_values), " models each) on ", n_cores, " core(s). ",
          if (n_to_fit < length(rm_values)) paste0(length(rm_values) - n_to_fit, " batch(es) already saved; skipping those. ") else "",
          "You'll get an update after each batch.")
  t0 <- Sys.time(); n_fitted <- 0; batches <- list()
  for (i in seq_along(rm_values)) {
    rmv <- rm_values[i]
    if (file.exists(ckpt_file(rmv))) {
      b <- read.csv(ckpt_file(rmv), stringsAsFactors = FALSE)
      message(sprintf("Batch %d of %d (RM %s): loaded from checkpoint", i, length(rm_values), rmv))
    } else {
      a <- args
      a$tune.args <- list(fc = fc_values, rm = rmv)
      b <- tryCatch(ENMeval::eval.results(do.call(ENMeval::ENMevaluate, a)),
                    error = function(err) {
                      message("Batch RM ", rmv, " failed in parallel (", conditionMessage(err),
                              "). Refitting one feature class at a time to find the failing model(s).")
                      NULL
                    })
      if (is.null(b)) {
        # Slower fallback: one model at a time, so a single failing model (e.g. "non-conformable
        # arguments", seen for H and HT at RM 2) is recorded as NA instead of stopping the run.
        a$parallel <- FALSE
        a$numCores <- NULL
        fail_msgs <- list()
        pieces <- lapply(fc_values, function(f) {
          a2 <- a
          a2$tune.args <- list(fc = f, rm = rmv)
          tryCatch(ENMeval::eval.results(do.call(ENMeval::ENMevaluate, a2)),
                   error = function(err) {
                     message("  Model fc = ", f, ", RM = ", rmv, " failed: ", conditionMessage(err), " (recorded as NA)")
                     fail_msgs[[f]] <<- conditionMessage(err)
                     NULL
                   })
        })
        pieces <- Filter(Negate(is.null), pieces)
        if (length(pieces) == 0) stop("Every model failed at RM ", rmv, ".")
        b <- do.call(rbind, pieces)
        b$fc <- as.character(b$fc)
        b$fit_status <- ifelse(is.na(b$AICc), "AICc undefined (no result)", "ok")
        failed <- setdiff(fc_values, b$fc)
        if (length(failed) > 0) {
          na_rows <- b[rep(1, length(failed)), , drop = FALSE]
          na_rows[, setdiff(names(na_rows), c("fc", "rm"))] <- NA
          na_rows$fc <- failed
          na_rows$rm <- rmv
          na_rows$fit_status <- paste0("failed: ", unlist(fail_msgs[failed]))
          b <- rbind(b, na_rows)
        }
      }
      b$fc <- as.character(b$fc)
      b$rm <- as.numeric(as.character(b$rm))
      b$tune.args <- NULL
      write.csv(b, ckpt_file(rmv), row.names = FALSE)
      n_fitted <- n_fitted + 1
      mins <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
      eta  <- if (n_fitted < n_to_fit) sprintf("about %.0f min left (rough: the low-RM batches are the slowest)",
                                               mins / n_fitted * (n_to_fit - n_fitted)) else "all batches done"
      message(sprintf("[%s] Batch %d of %d done (RM %s): %d of %d models complete. %.1f min elapsed, %s.",
                      format(Sys.time(), "%H:%M"), i, length(rm_values), rmv, i * length(fc_values), n_models, mins, eta))
    }
    # Every batch (new or loaded from an older checkpoint) gets a fit_status column.
    if (!("fit_status" %in% names(b))) b$fit_status <- NA_character_
    miss <- is.na(b$fit_status)
    b$fit_status[miss] <- ifelse(is.na(b$AICc[miss]), "AICc undefined (no result)", "ok")
    batches[[i]] <- b
  }
  message("Done fitting.")
  
  res <- do.call(rbind, batches)
  if (!("AICc" %in% names(res))) stop("ENMeval returned no AICc column. Columns: ", paste(names(res), collapse = ", "))
  # ENMeval's delta.AICc and w.AIC were computed within each RM batch; redo them across all 310 models.
  res$delta.AICc <- res$AICc - min(res$AICc, na.rm = TRUE)
  w <- exp(-0.5 * res$delta.AICc)
  res$w.AIC <- w / sum(w, na.rm = TRUE)
  if (!("ncoef" %in% names(res))) res$ncoef <- NA_integer_
  
  res <- res[order(res$AICc), ]
  res_out <- res[, setdiff(names(res), "tune.args"), drop = FALSE]
  rownames(res_out) <- NULL
  write.csv(res_out, all_file, row.names = FALSE)   # saved straight away, so a later error can't lose the fits
  writeLines(c(
    paste("Run finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("R:", R.version.string),
    paste("ENMeval:", as.character(utils::packageVersion("ENMeval"))),
    paste("maxnet:", as.character(utils::packageVersion("maxnet"))),
    paste("terra:", as.character(utils::packageVersion("terra"))),
    paste0("Presences: ", nrow(pxy), "   Background: ", nrow(bxy), "   thin_km: ", thin_km),
    paste("Predictors:", paste(names(envs), collapse = ", ")),
    paste0("Feature classes: ", paste(fc_letters, collapse = ""), "   RM values: ", paste(rm_values, collapse = ", ")),
    "Note: batches loaded from checkpoints were fitted in an earlier run; the versions above are those of the run that completed this file."
  ), file.path(out_dir, "run_info.txt"))
}

# ---- Results ----------------------------------------------------------------
if (!("fit_status" %in% names(res_out))) {
  res_out$fit_status <- ifelse(is.na(res_out$AICc), "AICc undefined (no result)", "ok")
}
failed_rows <- res_out[grepl("^failed", res_out$fit_status), c("fc", "rm", "fit_status"), drop = FALSE]
write.csv(failed_rows, failed_file, row.names = FALSE)
if (nrow(failed_rows) > 0) {
  message("\n", nrow(failed_rows), " model(s) could not be fitted and are recorded as NA ",
          "(not ranked, left as gaps in the plot):")
  print(failed_rows, row.names = FALSE)
}
write.csv(utils::head(res_out, top_n), top_file, row.names = FALSE)

n_na <- sum(is.na(res$AICc))
if (n_na > 0) {
  warning(n_na, " model(s) have no AICc (more non-zero coefficients than the presences allow, or a failed fit). ",
          "They are left out of the ranking and the plot.")
}
best <- res_out[1, ]
def  <- res_out[res_out$fc == default_fc & res_out$rm == default_rm, ]

message("\n=== Best models by AICc ===")
show_cols <- intersect(c("fc", "rm", "ncoef", "AICc", "delta.AICc"), names(res_out))
print(utils::head(res_out[, show_cols], top_n), row.names = FALSE)
message("\nBest: fc = ", best$fc, ", RM = ", best$rm, ", AICc = ", round(best$AICc, 1), ", ncoef = ", best$ncoef)
if (nrow(def) == 1 && !is.na(def$AICc)) {
  message("Default (", default_fc, ", RM ", default_rm, "): AICc = ", round(def$AICc, 1),
          ", ", round(def$AICc - best$AICc, 1), " higher than the best")
}
message("Models within 2 AICc units of the best: ", sum(res_out$AICc - best$AICc <= 2, na.rm = TRUE),
        "  (differences of about 2 or less are essentially ties)")
if (best$rm == min(rm_values)) {
  message("NOTE: the best RM is the lowest value tested. Try lower values (e.g. rm_values <- c(0.25, 0.5, 1, 1.5, 2)) ",
          "to see whether AICc turns back up.")
}
if (best$rm == max(rm_values)) {
  message("NOTE: the best RM is the highest value tested. Try higher values.")
}

nb <- failed_rows[failed_rows$fc == best$fc, ]
if (nrow(nb) > 0) {
  message("NOTE: the best feature class (", best$fc, ") failed to fit at RM ", paste(nb$rm, collapse = ", "),
          ", so its AICc curve has gaps. Treat the comparison around those RM values with care.")
}

# ---- Plot: AICc against RM, one line per feature-class combination ----------
ok <- res[!is.na(res$AICc), ]
fc_best <- tapply(ok$AICc, ok$fc, min)
top_fc  <- names(sort(fc_best))[seq_len(min(top_n, length(fc_best)))]
pal     <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
             "#D55E00", "#CC79A7", "#000000", "#999999", "#8C510A")
cols    <- setNames(pal[seq_along(top_fc)], top_fc)
yd      <- range(ok$AICc)                         # range of the AICc values
band    <- plot_label_band * diff(yd)             # empty band under the lowest line, for the labels and the red crosses
yl      <- c(yd[1] - band, yd[2])
yt      <- pretty(yd); yt <- yt[yt >= yd[1] & yt <= yd[2]]   # axis ticks and grid lines only where there is data
xl      <- range(rm_values) + c(-0.15, 0.15)

png(file.path(out_dir, "aicc_by_rm.png"), width = 1700, height = 1200, res = 150)
par(mar = c(5.6, 6.4, 4, 9), xpd = FALSE)
plot(NA, xlim = xl, ylim = yl, xlab = "Regularization multiplier", ylab = "", xaxt = "n", yaxt = "n",
     main = sprintf("MaxEnt tuning: %d feature-class combinations x %d RM values\n(%d presences, %d background points)",
                    length(fc_values), length(rm_values), nrow(pxy), nrow(bxy)))
axis(1, at = rm_values)
axis(2, at = yt, las = 1)
mtext("AICc", side = 2, line = 4.8)               # further out than the tick labels, which are horizontal and 5 digits wide
abline(h = yt, v = rm_values, col = "grey92")
draw_line <- function(f, col, lwd, cex) {
  d <- data.frame(rm = rm_values)
  d$AICc <- ok$AICc[match(paste(f, rm_values), paste(ok$fc, ok$rm))]   # NA where a model is missing, so the line has a gap
  lines(d$rm, d$AICc, col = col, lwd = lwd)
  points(d$rm, d$AICc, col = col, pch = 16, cex = cex)
}
for (f in setdiff(unique(ok$fc), top_fc)) draw_line(f, "grey80", 1, 0.5)
for (f in top_fc) draw_line(f, cols[[f]], 2, 1.1)

# "Best" and "Default" labels: circled on the line, labelled in the empty band under the lines (two-line white box, thin
# leader line). The left label is right-aligned on its point and the right label left-aligned, so neighbouring RM values
# (e.g. 1 and 1.5) don't make the boxes overlap; if they still would, the right box is pushed right.
lab_y <- yd[1] - band * 0.55
labs <- list(list(x = best$rm, y = best$AICc, txt = paste0("Best:\n", best$fc, ", RM ", best$rm)))
if (nrow(def) == 1 && !is.na(def$AICc)) {
  labs[[2]] <- list(x = def$rm, y = def$AICc, txt = paste0("Default:\n", default_fc, ", RM ", default_rm))
}
labs <- labs[order(vapply(labs, function(l) l$x, numeric(1)))]
prev_right <- -Inf
for (i in seq_along(labs)) {
  l <- labs[[i]]
  w <- strwidth(l$txt, cex = 0.85) + 0.06
  h <- strheight(l$txt, cex = 0.85) / 2 * 1.25 + 0.008 * diff(yl)
  left <- if (length(labs) == 1) l$x - w / 2 else if (i == 1) l$x + 0.06 - w else l$x - 0.06
  left <- max(left, xl[1] + 0.02, prev_right + 0.06)
  left <- min(left, xl[2] - 0.02 - w)
  right <- left + w
  prev_right <- right
  points(l$x, l$y, pch = 1, cex = 3, lwd = 3)
  xe <- min(max(l$x, left), right)
  segments(l$x, l$y - 0.045 * diff(yd), xe, lab_y + h, col = "grey30", lwd = 1)
  rect(left, lab_y - h, right, lab_y + h, col = "white", border = "grey30")
  text((left + right) / 2, lab_y, l$txt, cex = 0.85)
}
if (nrow(failed_rows) > 0) {
  points(failed_rows$rm, rep(yd[1] - band * 0.2, nrow(failed_rows)), pch = 4, col = "red", lwd = 2)
  mtext("x = model could not be fitted (see tuning_failed_models.csv)", side = 1, line = 4.3,
        adj = 0, cex = 0.7, col = "red")
}
box()
par(xpd = NA)
usr <- par("usr")
legend(usr[2] + diff(usr[1:2]) * 0.02, usr[4], legend = top_fc, col = cols, lwd = 2, pch = 16,
       title = paste0("Top ", length(top_fc), " feature classes"), bty = "n", xjust = 0, yjust = 1, cex = 0.9)
invisible(dev.off())

message("\nSaved: ", all_file)
message("Saved: ", top_file)
message("Saved: ", file.path(out_dir, "aicc_by_rm.png"))