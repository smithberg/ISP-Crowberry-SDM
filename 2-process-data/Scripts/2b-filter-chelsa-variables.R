# 2b-filter-chelsa-variables.R
# Decides which CHELSA variables are left out of the predictor set, and records the decision in one
# small file that the later scripts read (2c clips only the variables that are not listed).
#
# Currently excluded: the Julian-day "growing degree day" layers
#   gddlgd0, gddlgd5, gddlgd10   last day of the year with a daily mean above 0 / 5 / 10 degrees C
#   gdgfgd5, gdgfgd10            first day of the year with a daily mean above 5 / 10 degrees C
# These are day numbers, not sums. Where the threshold is never reached the source files hold 0, which is
# not a real day number: it sits far from every real value and makes a large share of the country look
# like one artificial value. Filling or leaving it as 0 would put a false step into the model.
# Also excluded, by choice:
#   gdd10                        heat sum above 10 degrees C; NA on about two thirds of the land, and the
#                                share of NA cells changes sharply between the historic and future layers
#
# Put this file in: ISP-Crowberry-SDM/2-process-data/Scripts/   (run it before 2c-clip-rasters-to-land.R)
# Reads:  1-download-data/Downloads/CHELSA-BIOCLIM/**/*.tif        (file names only, to count the layers)
# Writes: 2-process-data/Outputs/Predictor filtering/
#           excluded_variables.csv     one row per excluded variable: variable, reason, files found
#           excluded_files.txt         every raw file that belongs to an excluded variable
# The downloaded files are never changed or deleted.
#
# Clean-up: if an earlier run of 2c (clip) or 2d (fill) already produced files for these variables,
# those copies are removed from "CHELSA-BIOCLIM clipped" and "CHELSA-BIOCLIM filled" (set
# remove_old_outputs <- FALSE to keep them). They are outputs and can be rebuilt, so nothing is lost,
# but without this step the old files would still be picked up by the screening in 3b.
#
# To change the exclusion, edit exclude_vars below and run 2b again, then re-run 2c onwards.
#
# NOT TESTED on your data. Check the console messages and excluded_variables.csv before using the output.

# ---- Settings ---------------------------------------------------------------
exclude_vars <- c("gddlgd0", "gddlgd5", "gddlgd10", "gdgfgd5", "gdgfgd10", "gdd10")
julian_reason <- "Julian-day layer: 0 stands in for 'threshold never reached', which is not a real day number"
reasons <- c(
  gddlgd0  = julian_reason, gddlgd5 = julian_reason, gddlgd10 = julian_reason,
  gdgfgd5  = julian_reason, gdgfgd10 = julian_reason,
  gdd10    = "left out by choice: NA on about two thirds of the land and the NA share changes sharply between historic and future"
)
if (!all(exclude_vars %in% names(reasons))) {
  stop("Add a reason for: ", paste(setdiff(exclude_vars, names(reasons)), collapse = ", "))
}
files_per_variable <- 10        # 1 historic layer + 3 scenarios x 3 periods
remove_old_outputs <- TRUE      # remove existing clipped/filled copies of the excluded variables

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
chelsa_in   <- p("1-download-data", "Downloads", "CHELSA-BIOCLIM")
clipped_dir <- p("2-process-data", "Outputs", "CHELSA-BIOCLIM clipped")
filled_dir  <- p("2-process-data", "Outputs", "CHELSA-BIOCLIM filled")
out_dir     <- p("2-process-data", "Outputs", "Predictor filtering")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

if (!dir.exists(chelsa_in)) stop("Missing folder: ", chelsa_in)
if (length(exclude_vars) == 0) stop("exclude_vars is empty. Nothing to filter.")
if (anyDuplicated(exclude_vars)) stop("exclude_vars lists a variable twice.")

# ---- Match layers to variables ----------------------------------------------
# The variable name sits between underscores (or at the start of the name), e.g.
# "gddlgd10_1981-2010_iceland.tif" or "gfdl-esm4_ssp126_gddlgd10_2011-2040_iceland.tif".
# The underscores keep "gdd10" from matching "gddlgd10".
var_pat <- function(v) paste0("(^|_)", v, "_")
is_excluded <- function(files) {
  Reduce(`|`, lapply(exclude_vars, function(v) grepl(var_pat(v), basename(files))))
}

raw_files <- list.files(chelsa_in, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
if (length(raw_files) == 0) stop("No .tif files in ", chelsa_in)
hit <- is_excluded(raw_files)

summ <- data.frame(
  variable = exclude_vars,
  reason   = unname(reasons[exclude_vars]),
  files_found = vapply(exclude_vars, function(v) sum(grepl(var_pat(v), basename(raw_files))), numeric(1)),
  stringsAsFactors = FALSE, row.names = NULL
)
write.csv(summ, file.path(out_dir, "excluded_variables.csv"), row.names = FALSE)
writeLines(basename(raw_files[hit]), file.path(out_dir, "excluded_files.txt"))

message("CHELSA layers found:        ", length(raw_files))
message("Variables to exclude (", length(exclude_vars), "): ", paste(exclude_vars, collapse = ", "))
message("Layers that belong to them: ", sum(hit), "  (expected ", length(exclude_vars) * files_per_variable, ")")
message("Layers that stay:           ", sum(!hit))
print(summ[, c("variable", "files_found")], row.names = FALSE)

if (any(summ$files_found == 0)) {
  warning("No files found for: ", paste(summ$variable[summ$files_found == 0], collapse = ", "),
          ". Check the spelling in exclude_vars (the layer names use e.g. 'gdgfgd10').")
}
if (any(summ$files_found != 0 & summ$files_found != files_per_variable)) {
  warning("Expected ", files_per_variable, " layers per variable but found a different number for: ",
          paste(summ$variable[summ$files_found != 0 & summ$files_found != files_per_variable], collapse = ", "))
}

# ---- Remove old outputs of the excluded variables ---------------------------
if (remove_old_outputs) {
  for (d in c(clipped_dir, filled_dir)) {
    if (!dir.exists(d)) next
    old <- list.files(d, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
    old <- old[is_excluded(old)]
    if (length(old) > 0) {
      unlink(old)
      message("Removed ", length(old), " old file(s) of excluded variables from: ", d)
    }
  }
}

message("\nSaved: ", file.path(out_dir, "excluded_variables.csv"))
message("Saved: ", file.path(out_dir, "excluded_files.txt"))
message("Next: run 2c-clip-rasters-to-land.R (it skips these variables), then 2d-fill-chelsa-na-zero.R.")