# 5b-stepwise-variable-removal.R
# Backward stepwise variable removal for the MaxEnt model, using the feature classes and regularization
# multiplier chosen in 5a (QHP, RM 1.5). Start with all the
# candidate variables, then repeatedly remove the one variable whose removal gives the lowest AICc, until
# only min_vars variables are left. The model with the lowest AICc along the way is the best variable set.
#
# Put this file in: ISP-Crowberry-SDM/2-process-data/Scripts/
# Reads:  2-process-data/Outputs/Predictor screening/selected_predictors.csv     (the candidate variables)
#         2-process-data/Outputs/<historic_file column>                           (their rasters)
#         2-process-data/Outputs/Spatial blocks/occurrences_thinned_<thin_km>km_folds.csv
#         2-process-data/Outputs/Spatial blocks/background_points_<n>_folds.csv
#         2-process-data/Outputs/Background points/valid_cells_mask.tif           (optional, used if present)
# Writes: 2-process-data/Outputs/MaxEnt tuning/Stepwise removal/
#           stepwise_path.csv          the chosen path: variables left, variable removed, AICc, delta.AICc, ncoef
#           stepwise_candidates.csv    every model fitted (each variable dropped at each step), incl. failures
#           stepwise_aicc.png          AICc against number of variables
#           run_info.txt               R, ENMeval, maxnet and terra versions and the settings of the run
# Inputs are only read, never changed.
#
# How it works: the model with all variables is fitted first. At each step, one model is fitted for every
# variable that could be dropped (n models with n-1 variables each), and the drop that gives the lowest AICc
# is kept. The path is greedy: it is not a search of every possible subset of variables.
# AICc is calculated the same way as in 5a (ENMeval, maxnet, from the model's predictions over the whole
# study area at the presence points), so only compare AICc values within this run and against 5a.
#
# Known issue (see 5a): with maxnet 0.1.4 some models that include hinge features can fail with
# "non-conformable arguments" because of a duplicated hinge-feature name. QHP contains hinge features, so
# a candidate model can fail here too. A failed candidate is recorded (status "failed: ...") and cannot be
# chosen at that step. If every candidate fails at a step, the run stops there.
#
# Progress: each step is saved in "Stepwise removal/checkpoints/". If the run is interrupted, running the
# script again resumes from the last finished step. The checkpoints don't record which presence set or
# rasters were used, so set overwrite <- TRUE (or delete the checkpoints folder) after changing any of those.
# A checkpoint made with a different variable set, feature classes or RM is detected and refitted.
#
# NOT TESTED on your data. Check the console messages and the plot before using the output.

# install.packages(c("terra", "readr", "ENMeval", "maxnet"))
library(terra)

# ---- Settings ---------------------------------------------------------------
thin_km      <- 5                          # which thinned presence set (must have a *_folds.csv from 4a)
n_background <- 10000                      # must match the background_points_<n>_folds.csv file
fc           <- "QHPT"                      # feature classes from the 5a tuning (Q quadratic, H hinge, P product, T threshold)
rm_value     <- 1.5                        # regularization multiplier from the 5a tuning
min_vars     <- 4                          # stop when this many variables are left
overwrite    <- TRUE                      # FALSE = resume from saved checkpoints. TRUE = refit everything.

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
out_dir     <- file.path(outputs_dir, "MaxEnt tuning", "Stepwise removal")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
path_file   <- file.path(out_dir, "stepwise_path.csv")
cand_file   <- file.path(out_dir, "stepwise_candidates.csv")

for (f in c(sel_file, pres_file, bg_file)) if (!file.exists(f)) stop("Missing input: ", f)

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

# ---- Helper: fit one model with a given set of variables and return its AICc --
fit_model <- function(vars) {
  tryCatch({
    e <- NULL
    utils::capture.output(
      e <- suppressMessages(ENMeval::ENMevaluate(
        occs = pxy, envs = envs[[vars]], bg = bxy,
        tune.args = list(fc = fc, rm = rm_value),
        algorithm = "maxnet", partitions = "none", parallel = FALSE))
    )
    r <- ENMeval::eval.results(e)
    data.frame(AICc  = r$AICc[1],
               ncoef = if ("ncoef" %in% names(r)) r$ncoef[1] else NA_integer_,
               status = if (is.na(r$AICc[1])) "AICc undefined (no result)" else "ok",
               stringsAsFactors = FALSE)
  }, error = function(err) {
    data.frame(AICc = NA_real_, ncoef = NA_integer_,
               status = paste0("failed: ", conditionMessage(err)), stringsAsFactors = FALSE)
  })
}

# ---- Helper: use a saved checkpoint if it matches, otherwise fit and save ---
get_or_fit <- function(file, key, fit_fun) {
  if (file.exists(file)) {
    d <- read.csv(file, stringsAsFactors = FALSE)
    same <- all(c("from_vars", "fc", "rm") %in% names(d)) &&
      all(d$from_vars == key) && all(d$fc == fc) && all(as.numeric(d$rm) == rm_value)
    if (same) return(list(d = d, loaded = TRUE))
    message("  Checkpoint ", basename(file), " was made with different settings or variables; refitting.")
  }
  d <- fit_fun()
  d$from_vars <- key
  write.csv(d, file, row.names = FALSE)
  list(d = d, loaded = FALSE)
}

# ---- Stepwise removal -------------------------------------------------------
ckpt_dir <- file.path(out_dir, "checkpoints")
dir.create(ckpt_dir, showWarnings = FALSE, recursive = TRUE)
if (overwrite) unlink(list.files(ckpt_dir, full.names = TRUE))

all_vars <- names(envs)
if (length(all_vars) <= min_vars) stop("Only ", length(all_vars), " variables, so nothing to remove with min_vars = ", min_vars, ".")
n_steps <- length(all_vars) - min_vars
n_fits  <- 1 + sum(seq(length(all_vars), min_vars + 1))
message("Stepwise removal: fc = ", fc, ", RM = ", rm_value, ", ", length(all_vars), " variables down to ", min_vars,
        " (", n_steps, " steps, ", n_fits, " model fits).")
t0 <- Sys.time()
elapsed <- function() sprintf("%.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))

# Step 0: the model with all variables
full <- get_or_fit(file.path(ckpt_dir, "step_00_full_model.csv"),
                   paste(sort(all_vars), collapse = "|"),
                   function() {
                     message("Fitting the model with all ", length(all_vars), " variables ...")
                     f <- fit_model(all_vars)
                     data.frame(fc = fc, rm = rm_value, n_vars = length(all_vars), removed = NA_character_,
                                vars_kept = paste(all_vars, collapse = ", "), f, stringsAsFactors = FALSE)
                   })$d
if (is.na(full$AICc[1])) stop("The model with all variables could not be fitted: ", full$status[1])
message(sprintf("[%s] All %d variables: AICc = %.1f", format(Sys.time(), "%H:%M"), length(all_vars), full$AICc[1]))

path <- list(full)
cands_all <- list()
current <- all_vars
while (length(current) > min_vars) {
  n <- length(current)
  step <- get_or_fit(file.path(ckpt_dir, sprintf("step_%02d_to_%02d_vars.csv", n, n - 1)),
                     paste(sort(current), collapse = "|"),
                     function() {
                       rows <- lapply(current, function(drop_v) {
                         keep <- setdiff(current, drop_v)
                         f <- fit_model(keep)
                         data.frame(fc = fc, rm = rm_value, n_vars = n - 1, removed = drop_v,
                                    vars_kept = paste(keep, collapse = ", "), f, stringsAsFactors = FALSE)
                       })
                       do.call(rbind, rows)
                     })
  cand <- step$d
  cands_all[[length(cands_all) + 1]] <- cand
  okc <- cand[!is.na(cand$AICc), ]
  n_failed <- sum(grepl("^failed", cand$status))
  if (nrow(okc) == 0) {
    message("Every candidate failed at the step from ", n, " to ", n - 1, " variables. Stopping here.")
    break
  }
  chosen <- okc[which.min(okc$AICc), ]
  message(sprintf("[%s] %d -> %d variables: removed %s (AICc %.1f)%s   %s elapsed%s",
                  format(Sys.time(), "%H:%M"), n, n - 1, chosen$removed, chosen$AICc,
                  if (n_failed > 0) paste0("   [", n_failed, " candidate(s) failed]") else "",
                  elapsed(), if (step$loaded) "   (from checkpoint)" else ""))
  path[[length(path) + 1]] <- chosen
  current <- setdiff(current, chosen$removed)
}

# ---- Results ----------------------------------------------------------------
steps <- do.call(rbind, path)
steps$from_vars <- NULL
steps$delta.AICc <- steps$AICc - min(steps$AICc, na.rm = TRUE)
rownames(steps) <- NULL
cands <- do.call(rbind, cands_all)
cands$from_vars <- NULL
write.csv(steps, path_file, row.names = FALSE)
write.csv(cands, cand_file, row.names = FALSE)

failed_c <- cands[grepl("^failed", cands$status), c("n_vars", "removed", "status"), drop = FALSE]
if (nrow(failed_c) > 0) {
  message("\n", nrow(failed_c), " candidate model(s) could not be fitted (recorded as NA, never chosen):")
  print(failed_c, row.names = FALSE)
}

message("\n=== Stepwise path (one row per number of variables) ===")
print(steps[, c("n_vars", "removed", "AICc", "delta.AICc", "ncoef")], row.names = FALSE)
best <- steps[which.min(steps$AICc), ]
message("\nLowest AICc: ", best$n_vars, " variables, AICc = ", round(best$AICc, 1))
message("Variables kept: ", best$vars_kept)
if (best$n_vars == length(all_vars)) {
  message("NOTE: no removal improved AICc, so the model with all ", length(all_vars), " variables is the best one.")
}
within2 <- steps[steps$delta.AICc <= 2, ]
small <- within2[which.min(within2$n_vars), ]
message("Fewest variables within 2 AICc units of the best: ", small$n_vars, " (AICc ", round(small$AICc, 1),
        ", delta ", round(small$delta.AICc, 1), "). Differences of about 2 or less are essentially ties.")
if (nrow(steps) < n_steps + 1) {
  message("NOTE: the path stopped early (", nrow(steps), " models instead of ", n_steps + 1, ") because every candidate failed.")
}

# ---- Plot: AICc against number of variables --------------------------------
ok <- steps[!is.na(steps$AICc), ]
yr <- range(ok$AICc)
yl <- yr + c(-0.12, 0.15) * max(diff(yr), 1)
xl <- c(max(ok$n_vars), min(ok$n_vars)) + c(0.4, -0.4)   # reversed: most variables on the left, fewest on the right
png(file.path(out_dir, "stepwise_aicc.png"), width = 1500, height = 1100, res = 150)
par(mar = c(4.6, 6.4, 4.5, 2))
plot(ok$n_vars, ok$AICc, type = "b", pch = 16, lwd = 2, cex = 1.2, ylim = yl, xlim = xl,
     xlab = "Number of variables", ylab = "", xaxt = "n", las = 1,
     main = sprintf("Stepwise variable removal: AICc vs number of variables\n(%s, RM %s; %d presences, %d background points)",
                    fc, rm_value, nrow(pxy), nrow(bxy)))
axis(1, at = ok$n_vars)
mtext("AICc", side = 2, line = 4.6)
abline(h = pretty(yl), v = ok$n_vars, col = "grey92")
abline(h = best$AICc, col = "red", lty = 2, lwd = 2)
lines(ok$n_vars, ok$AICc, lwd = 2)
points(ok$n_vars, ok$AICc, pch = 16, cex = 1.2)
points(best$n_vars, best$AICc, pch = 21, bg = "red", cex = 2)
lab <- ok[!is.na(ok$removed), ]
if (nrow(lab) > 0) text(lab$n_vars, lab$AICc, labels = paste0("-", lab$removed), pos = 3, cex = 0.7, col = "grey30")
text(best$n_vars, best$AICc, labels = paste0("Best model: ", best$n_vars, " variables"),
     col = "red", font = 2, pos = 1, offset = 1.4, cex = 0.9)   # below the point, clear of the dashed line and the label above
mtext("Labels show the variable removed to reach each model.", side = 1, line = 3.6, adj = 0, cex = 0.7, col = "grey30")
invisible(dev.off())

writeLines(c(
  paste("Run finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste("R:", R.version.string),
  paste("ENMeval:", as.character(utils::packageVersion("ENMeval"))),
  paste("maxnet:", as.character(utils::packageVersion("maxnet"))),
  paste("terra:", as.character(utils::packageVersion("terra"))),
  paste0("Feature classes: ", fc, "   RM: ", rm_value, "   min_vars: ", min_vars),
  paste0("Presences: ", nrow(pxy), "   Background: ", nrow(bxy), "   thin_km: ", thin_km),
  paste("Candidate variables:", paste(all_vars, collapse = ", ")),
  "Note: steps loaded from checkpoints were fitted in an earlier run; the versions above are those of the run that completed this file."
), file.path(out_dir, "run_info.txt"))

message("\nSaved: ", path_file)
message("Saved: ", cand_file)
message("Saved: ", file.path(out_dir, "stepwise_aicc.png"))
message("Saved: ", file.path(out_dir, "run_info.txt"))
