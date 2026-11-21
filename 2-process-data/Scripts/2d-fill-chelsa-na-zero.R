# 2d-fill-chelsa-na-zero.R
# Builds the final CHELSA predictor set in "CHELSA-BIOCLIM filled":
#   - gdd0, gdd5 (all scenarios/periods): NA cells inside the land mask are set to 0
#   - swe (all scenarios/periods): NA cells inside the land mask are filled from their NEIGHBOURS
#     (see below), whatever the size of the gap; only cells the fill cannot reach are set to 0
#   - gst (all scenarios/periods): EXCLUDED (gst is undefined where there is no growing season,
#     so 0 would be wrong)
#   - every other layer: copied unchanged
# Ocean/outside-mask cells stay NA. Nothing in "CHELSA-BIOCLIM clipped" is changed.
#
# gdd10 and the Julian-day layers (gddlgd*, gdgfgd*) are not handled here: 2b-filter-chelsa-variables.R
# lists them and 2c-clip-rasters-to-land.R never clips them, so they do not reach this script.
#
# Put this file in: ISP-Crowberry-SDM/2-process-data/Scripts/
# Reads:  2-process-data/Outputs/CHELSA-BIOCLIM clipped/**/*.tif   (from 2c-clip-rasters-to-land.R)
#         2-process-data/Outputs/Raster clip/land_mask_touches.tif (or Outputs/land_mask_touches.tif)
# Writes: 2-process-data/Outputs/CHELSA-BIOCLIM filled/            (the filled + copied layers)
#         2-process-data/Outputs/CHELSA-BIOCLIM filled/excluded_layers.txt   (the gst files left out)
#         2-process-data/Outputs/CHELSA-BIOCLIM filled/chelsa_fill_report.csv
#
# Why two methods:
#   gdd0, gdd5   NA means "no degree-days above the threshold", so 0 is the true value at the cold end.
#                The report prints a sanity check: filled cells should be the COLD places.
#   swe          NA is a mixture. In warm lowlands it can mean "no snow" (a true 0), but CHELSA also has NA
#                gaps inside the highest-snow areas (glaciers), where 0 is the opposite of the truth and makes
#                the snowiest cells look snow-free. So swe is filled from the neighbouring cells instead, using
#                only the swe values themselves (no other layer is involved).
#
# How the swe neighbour fill works, per layer:
#   1. Every NA cell inside the land mask is a cell to fill (the report also counts the patches they form).
#   2. The cells are filled from the outside in: every pass gives the NA cells that touch data the mean of
#      their data neighbours (nbr_window x nbr_window), and passes repeat until no cell is left, or no cell
#      could be filled in the last pass, or max_passes is reached. There is NO limit on the size of a gap.
#   3. Limitation: in a big gap the fill spreads the edge values inwards, so it gives a smooth, flat-ish
#      surface and will UNDERESTIMATE a peak in the middle of the gap (e.g. the centre of an ice cap).
#      Cells that could not be reached at all (no data anywhere near them) are set to 0.
# The report lists, for every filled layer, how many cells were filled from neighbours and how many were set
# to 0, so you can see how much of the NA each rule covered.
#
# Re-running: files that already exist are skipped (overwrite <- FALSE), EXCEPT the neighbour-filled layers,
# which are rebuilt every time while redo_neighbour <- TRUE. That is needed once after switching from the old
# 0-fill, because the old swe files would otherwise be kept. The report is merged with the existing one, so
# the gdd0 and gdd5 rows are kept when only swe is rebuilt.
#
# After this script: rerun 3b (screening) and compare selected_predictors.csv with the previous list before
# rerunning anything later, then the tuning (5a/5b) and the model, since swe values change.
#
# NOT TESTED on your data. Check the report and look at a swe layer (e.g. in QGIS) before using the output.

# install.packages("terra")
library(terra)
terraOptions(progress = 0)

# ---- Settings ---------------------------------------------------------------
zero_fill_vars <- c("gdd0", "gdd5")                 # NA -> 0 inside the land mask
nbr_fill_vars  <- c("swe")                          # NA -> filled from neighbouring cells
exclude_vars   <- c("gst")                          # left out of the filled folder
copy_unchanged <- TRUE    # TRUE = the filled folder holds the complete usable CHELSA set
overwrite      <- FALSE   # FALSE = skip files that already exist (safe to re-run)
redo_neighbour <- TRUE    # TRUE = always rebuild the neighbour-filled layers, even if the file exists
nbr_window     <- 3       # swe: size of the window (cells, odd number) whose mean fills an NA cell
max_passes     <- 500     # swe: safety cap on the number of passes per layer (a gap closes by about one cell per pass)

fill_vars <- c(zero_fill_vars, nbr_fill_vars)
if (anyDuplicated(fill_vars)) stop("A variable is in both zero_fill_vars and nbr_fill_vars.")

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
in_dir    <- p("2-process-data", "Outputs", "CHELSA-BIOCLIM clipped")
out_dir   <- p("2-process-data", "Outputs", "CHELSA-BIOCLIM filled")
# 2c writes the mask into "Raster clip"; an older layout kept it directly in Outputs. Use whichever exists.
mask_candidates <- c(p("2-process-data", "Outputs", "Raster clip", "land_mask_touches.tif"),
                     p("2-process-data", "Outputs", "land_mask_touches.tif"))
mask_file <- mask_candidates[file.exists(mask_candidates)][1]
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
rep_file  <- file.path(out_dir, "chelsa_fill_report.csv")

if (is.na(mask_file)) stop("Run 2c-clip-rasters-to-land.R first; no land mask found in:\n  ",
                           paste(mask_candidates, collapse = "\n  "))
files <- list.files(in_dir, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
if (length(files) == 0) stop("No .tif files in ", in_dir, "\nRun 2c-clip-rasters-to-land.R first.")
rel <- substring(files, nchar(in_dir) + 2)    # path relative to the clipped folder
message("Land mask: ", mask_file)

land <- rast(mask_file)
land_ok <- !is.na(land)
n_land <- as.numeric(global(land_ok, "sum", na.rm = TRUE)[1, 1])
message("Land cells in mask: ", format(n_land, big.mark = ","))

# ---- Which layers are which? ------------------------------------------------
# Variable name sits between underscores (or at the start): "gdd10_1981-2010_iceland.tif",
# "gfdl-esm4_ssp126_gdd10_2011-2040_iceland.tif". Underscore boundaries keep "gdd10" from
# matching other names such as "gddlgd10".
var_pat <- function(v) paste0("(^|_)", v, "_")
is_var  <- function(v) Reduce(`|`, lapply(v, function(x) grepl(var_pat(x), basename(files))))
is_zero    <- is_var(zero_fill_vars)
is_nbr     <- is_var(nbr_fill_vars)
is_fill    <- is_zero | is_nbr
is_exclude <- is_var(exclude_vars)
if (any(is_fill & is_exclude)) stop("A layer matches both a fill variable and exclude_vars.")

message("Layers found:   ", length(files))
message("  0-fill:       ", sum(is_zero), "  (", paste(zero_fill_vars, collapse = ", "), ")")
message("  neighbour fill: ", sum(is_nbr), "  (", paste(nbr_fill_vars, collapse = ", "), ")")
message("  to exclude:   ", sum(is_exclude), "  (", paste(exclude_vars, collapse = ", "), ")")
message("  unchanged:    ", sum(!is_fill & !is_exclude))
if (sum(is_fill) != 30 || sum(is_exclude) != 10) {
  warning("Expected 30 layers to fill and 10 to exclude but found ", sum(is_fill), " and ",
          sum(is_exclude), ". Check the file names in ", in_dir)
}

# ---- Excluded layers: write the list ----------------------------------------
excl_file <- file.path(out_dir, "excluded_layers.txt")
writeLines(c(
  paste0("Excluded from the CHELSA predictor set (variables: ", paste(exclude_vars, collapse = ", "), ")."),
  "Reasons:",
  "  gst (growing season temperature) is undefined where there is no growing season, so its NA",
  "  cells are not zeros and cannot be filled. It also largely overlaps the warm-season bioclim variables.",
  "",
  "gdd10 and the Julian-day layers (gddlgd*, gdgfgd*) are excluded earlier, by 2b-filter-chelsa-variables.R.",
  "",
  rel[is_exclude]
), excl_file)
message("Saved: ", excl_file)

# Remove stale outputs of excluded layers from an earlier run, if any
stale <- file.path(out_dir, rel[is_exclude])
unlink(stale[file.exists(stale)])

# ---- Optional sanity check helper -------------------------------------------
# Mean of a reference layer over the cells that were NA vs. the cells that had data.
ref_means <- function(var, na_cells, ok_cells) {
  ref_name <- if (var == "swe") "bio11_1981-2010" else "bio10_1981-2010"   # coldest / warmest quarter
  rf <- files[grepl(paste0("^", ref_name), basename(files))]
  if (length(rf) == 0) return(c(NA_real_, NA_real_))
  v <- values(rast(rf[1]), mat = FALSE)
  c(mean(v[na_cells], na.rm = TRUE), mean(v[ok_cells], na.rm = TRUE))
}

# ---- Neighbour fill ---------------------------------------------------------
# Returns the filled layer (every NA cell inside the land mask has a value) and the counts for the report.
fill_from_neighbours <- function(r) {
  na_land <- is.na(r) & land_ok                                   # NA cells inside the land mask
  n_na    <- as.numeric(global(na_land, "sum", na.rm = TRUE)[1, 1])
  res <- list(r = r, n_na = n_na, n_nbr = 0, n_zero = n_na, n_patches = 0, largest = 0, mean_nbr = NA_real_)
  if (n_na == 0) return(res)
  
  pt <- terra::patches(terra::ifel(na_land, 1, NA), directions = 8)   # one id per group of touching NA cells
  fq <- terra::freq(pt)
  fq <- fq[!is.na(fq$value), , drop = FALSE]
  res$n_patches <- nrow(fq)
  res$largest   <- max(fq$count)
  
  cur <- r
  n_prev <- Inf
  for (i in seq_len(max_passes)) {
    todo   <- is.na(cur) & land_ok                                # land cells still empty
    n_todo <- as.numeric(global(todo, "sum", na.rm = TRUE)[1, 1])
    if (n_todo == 0 || n_todo >= n_prev) break                    # done, or the last pass filled nothing
    n_prev <- n_todo
    f   <- terra::focal(cur, w = nbr_window, fun = "mean", na.policy = "only", na.rm = TRUE)
    cur <- terra::ifel(todo, f, cur)                              # only land cells take the new values
  }
  filled <- !is.na(cur) & na_land
  res$n_nbr <- as.numeric(global(filled, "sum", na.rm = TRUE)[1, 1])
  if (res$n_nbr > 0) {
    res$mean_nbr <- as.numeric(global(terra::ifel(filled, cur, NA), "mean", na.rm = TRUE)[1, 1])
  }
  res$n_zero <- n_na - res$n_nbr
  res$r <- terra::ifel(is.na(cur) & land_ok, 0, cur)               # cells the passes could not reach: 0
  res
}

# ---- Process ----------------------------------------------------------------
keep_idx <- which(!is_exclude & (is_fill | copy_unchanged))
rows <- list()
unlink(list.files(out_dir, pattern = "\\.part$", recursive = TRUE, full.names = TRUE))

for (j in seq_along(keep_idx)) {
  i   <- keep_idx[j]
  out <- file.path(out_dir, rel[i])
  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  
  rebuild <- is_nbr[i] && redo_neighbour
  if (file.exists(out) && !overwrite && !rebuild) next   # already there; leaves the existing report untouched
  
  if (!is_fill[i]) {                       # copy unchanged, byte for byte
    ok <- file.copy(files[i], out, overwrite = TRUE)
    if (!ok) warning("Could not copy ", basename(files[i]))
    next
  }
  
  r <- rast(files[i])
  if (!isTRUE(compareGeom(land, r, stopOnError = FALSE))) {
    warning("Skipping (grid differs from the land mask): ", basename(files[i]))
    next
  }
  v_before  <- values(r, mat = FALSE)
  na_inland <- is.na(v_before) & values(land_ok, mat = FALSE)
  n_na      <- sum(na_inland)
  
  if (is_nbr[i]) {
    fr <- fill_from_neighbours(r)
    r2 <- fr$r
    method <- "neighbour"; n_zero <- fr$n_zero; n_nbr <- fr$n_nbr
    n_patches <- fr$n_patches; largest <- fr$largest; mean_nbr <- fr$mean_nbr
  } else {
    r2 <- ifel(is.na(r) & land_ok, 0, r)
    method <- "zero"; n_zero <- n_na; n_nbr <- 0
    n_patches <- NA_integer_; largest <- NA_integer_; mean_nbr <- NA_real_
  }
  names(r2) <- names(r)
  tmp <- paste0(out, ".part")
  writeRaster(r2, tmp, filetype = "GTiff", overwrite = TRUE, gdal = c("COMPRESS=LZW"))
  file.rename(tmp, out)
  
  var <- fill_vars[vapply(fill_vars, function(x) grepl(var_pat(x), basename(files[i])), logical(1))][1]
  hist_layer <- grepl("1981-2010", basename(files[i]))
  chk <- if (hist_layer) ref_means(var, which(na_inland), which(!is.na(v_before))) else c(NA_real_, NA_real_)
  
  rows[[length(rows) + 1]] <- data.frame(
    file = basename(files[i]), variable = var, action = "filled", fill_method = method,
    land_cells = n_land, na_cells_filled = n_na,
    fraction_filled = round(n_na / n_land, 4),
    na_cells_zero = n_zero, na_cells_neighbour = n_nbr,
    na_patches = n_patches, largest_na_patch_cells = largest,
    mean_value_in_neighbour_filled = round(mean_nbr, 1),
    ref_layer_mean_in_filled_cells = round(chk[1], 2),
    ref_layer_mean_in_cells_with_data = round(chk[2], 2),
    stringsAsFactors = FALSE
  )
  if (is_nbr[i] && n_zero > 0) {
    message("  note: ", basename(files[i]), ": ", n_zero, " NA cell(s) could not be reached by the neighbour fill and were set to 0.")
  }
  if (j %% 25 == 0 || j == length(keep_idx)) message(sprintf("[%d/%d] %s", j, length(keep_idx), basename(files[i])))
}

# ---- Report -----------------------------------------------------------------
if (length(rows) == 0) {
  message("\nNo layers were filled in this run (all outputs already exist), so the existing ",
          "chelsa_fill_report.csv was left as it is. Set overwrite <- TRUE to redo everything.")
} else {
  report <- do.call(rbind, rows)
  # Keep the rows of layers that were not rebuilt in this run (e.g. gdd0 and gdd5 when only swe is redone)
  if (file.exists(rep_file)) {
    old <- read.csv(rep_file, stringsAsFactors = FALSE)
    old <- old[!old$file %in% report$file, , drop = FALSE]
    if (!("fill_method" %in% names(old))) old$fill_method <- "zero"
    for (cc in setdiff(names(report), names(old))) old[[cc]] <- NA
    z <- which(old$fill_method == "zero" & is.na(old$na_cells_zero))
    old$na_cells_zero[z]      <- old$na_cells_filled[z]
    old$na_cells_neighbour[z] <- 0
    old <- old[, names(report), drop = FALSE]
    report <- rbind(old, report)
  }
  report <- report[order(report$variable, report$file), ]
  write.csv(report, rep_file, row.names = FALSE)
  
  message("\n=== Fill summary (historic layers) ===")
  h <- report[grepl("1981-2010", report$file),
              c("variable", "fill_method", "na_cells_filled", "na_cells_zero", "na_cells_neighbour",
                "largest_na_patch_cells", "mean_value_in_neighbour_filled",
                "ref_layer_mean_in_filled_cells", "ref_layer_mean_in_cells_with_data")]
  print(h, row.names = FALSE)
  message("\nSanity check: for gdd layers the reference is bio10 (warmest-quarter temperature); filled cells")
  message("should be COLDER than cells with data. For swe the reference is bio11 (coldest-quarter")
  message("temperature): a mix of warm lowland cells (true zeros) and cold glacier cells (neighbour-filled).")
  message("For swe, mean_value_in_neighbour_filled should be high (the gaps sit in high-snow areas) and")
  message("na_cells_zero should be 0 or tiny. Big gaps (largest_na_patch_cells) get a smooth surface from")
  message("their edges, which underestimates a peak in the middle: look at them before using the layer.")
  message("Warm lowland NA patches that were true zeros are filled from neighbours too, so a high")
  message("na_cells_neighbour that is not in glacier areas is worth a look.")
}

n_out <- length(list.files(out_dir, pattern = "\\.tif$", recursive = TRUE))
message("\nFiles now in the filled folder: ", n_out)
message("Folder: ", out_dir)