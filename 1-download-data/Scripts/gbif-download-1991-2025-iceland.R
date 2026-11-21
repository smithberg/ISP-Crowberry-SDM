# =============================================================================
# Download crowberry (Empetrum nigrum) occurrence records from GBIF
#
# What it does
#   1. Asks GBIF for a download of all crowberry records in Iceland, 1991-2025
#      (needs a free GBIF account, see below), and saves the download KEY
#   2. Waits until GBIF has prepared it, then saves the zip in "Downloads"
#   3. Cleans the records (coordinate uncertainty, location, duplicates), printing
#      how many records each step removes
#   4. Saves the cleaned records as CSV files, plus the citation (DOI) to use
#
# Re-running is safe and cheap
#   - If the key file exists, the script reuses that download instead of asking GBIF
#     for a new one, so everybody who runs it gets the SAME data (commit the key
#     file "Input data/gbif_download_key.txt" with your project; no login needed)
#   - If the zip is already downloaded, it is not downloaded again
#   - To make a brand new download, delete the key file (or set force_new_download <- TRUE)
#
# Before the first run
#   - Make a free account at https://www.gbif.org (top right, "Login")
#   - Keep this script in ISP-Crowberry-SDM/1-download-data/Scripts. It finds the project
#     folder from its own location (or from R's working directory), so files always go into
#     that project; no RStudio Project (.Rproj) is needed. The first lines of output print
#     the folder it chose.
#   - The script asks for your GBIF username, password and email the first time it
#     needs to make a request. They are used for that request only and never saved.
#     (To avoid typing them: usethis::edit_r_environ() and add the lines
#      GBIF_USER=..., GBIF_PWD=..., GBIF_EMAIL=...)
# =============================================================================

# --- Packages ----------------------------------------------------------------
if (!requireNamespace("rgbif", quietly = TRUE)) install.packages("rgbif")
library(rgbif)

# --- Locate project root ------------------------------------------------------
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

# --- Settings (edit these if needed) -----------------------------------------
species_names <- c("Empetrum nigrum")   # add "Empetrum hermaphroditum" here to download it too
country_code  <- "IS"                   # Iceland
year_from     <- 1991
year_to       <- 2025

# Records of these types are excluded (fossils and cultivated plants are not wild occurrences)
exclude_basis <- c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN")

# Cleaning rules (applied after download, so you can change them without a new download)
max_uncertainty_m        <- 1000   # drop records whose coordinates are less precise than this (metres)
keep_missing_uncertainty <- TRUE   # many records have no uncertainty value; TRUE keeps them, FALSE drops them
bbox <- c(xmin = -25.0, xmax = -12.5, ymin = 63.0, ymax = 67.0)  # same window as the CHELSA clip

force_new_download <- FALSE

# --- Folders and file names --------------------------------------------------
input_dir  <- file.path(proj_root, "1-download-data", "Input data")
output_dir <- file.path(proj_root, "1-download-data", "Downloads", "GBIF")
dir.create(input_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

key_file <- file.path(input_dir, "gbif_download_key.txt")
tag      <- sprintf("%d-%d", year_from, year_to)

# --- Helpers -----------------------------------------------------------------
read_key <- function(f) {
  x <- trimws(readLines(f, warn = FALSE))
  x <- x[nzchar(x) & !startsWith(x, "#")]
  if (length(x) > 0) x[1] else NA_character_
}

ask <- function(label, secret = FALSE) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    out <- if (secret) rstudioapi::askForPassword(label) else rstudioapi::showPrompt("GBIF login", label)
  } else {
    out <- readline(paste0(label, ": "))
  }
  if (is.null(out) || !nzchar(out)) stop("No ", label, " given - stopping.", call. = FALSE)
  out
}

# one value -> pred(), several values -> pred_in()
pred_any <- function(field, values) {
  if (length(values) == 1) pred(field, values) else pred_in(field, values)
}

# =============================================================================
# STEP 1: get a GBIF download key (ask GBIF for the download, or reuse the saved key)
# =============================================================================
key <- if (file.exists(key_file) && !force_new_download) read_key(key_file) else NA_character_

if (!is.na(key)) {
  message("Reusing the saved GBIF download: ", key,
          "\n  (delete ", basename(key_file), " if you want a brand new download)\n")
} else {
  # Look up the GBIF taxon key of each species name
  taxon_keys <- character(0)
  for (nm in species_names) {
    bb <- name_backbone(name = nm, rank = "species")
    if (is.null(bb$usageKey)) stop("GBIF found no match for '", nm, "'", call. = FALSE)
    message("Species '", nm, "' -> GBIF: ", bb$scientificName, " (", bb$status,
            ", match: ", bb$matchType, ", taxon key ", bb$usageKey, ")")
    taxon_keys <- c(taxon_keys, as.character(bb$usageKey))
  }
  
  # The request: what GBIF should include
  preds <- list(
    pred_any("taxonKey", taxon_keys),
    pred("country", country_code),
    pred("hasCoordinate", TRUE),
    pred("hasGeospatialIssue", FALSE),
    pred("occurrenceStatus", "PRESENT"),
    pred_gte("year", year_from),
    pred_lte("year", year_to)
  )
  if (length(exclude_basis) > 0) {
    preds <- c(preds, list(pred_not(pred_any("basisOfRecord", exclude_basis))))
  }
  
  # GBIF login (environment variables if set, otherwise you are asked)
  gbif_user  <- Sys.getenv("GBIF_USER");  if (!nzchar(gbif_user))  gbif_user  <- ask("GBIF username")
  gbif_pwd   <- Sys.getenv("GBIF_PWD");   if (!nzchar(gbif_pwd))   gbif_pwd   <- ask("GBIF password", secret = TRUE)
  gbif_email <- Sys.getenv("GBIF_EMAIL"); if (!nzchar(gbif_email)) gbif_email <- ask("GBIF email")
  
  message("\nSending the download request to GBIF...")
  d <- do.call(occ_download,
               c(preds, list(format = "SIMPLE_CSV",
                             user = gbif_user, pwd = gbif_pwd, email = gbif_email)))
  key <- as.character(unclass(d))[1]
  
  # Save the key right away: if anything goes wrong from here on, re-running picks it up
  writeLines(c(
    "# GBIF download key (created by download_gbif.R). Keep this file with your project:",
    "# anyone who has it can re-download exactly the same data.",
    paste0("# Created: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
    paste0("# Species: ", paste(species_names, collapse = ", "), " (taxon keys: ", paste(taxon_keys, collapse = ", "), ")"),
    paste0("# Filters: country ", country_code, ", years ", year_from, "-", year_to,
           ", present, with coordinates, no geospatial issues, excluding ", paste(exclude_basis, collapse = "/")),
    key
  ), key_file)
  message("Download key: ", key, "  (saved to ", basename(key_file), ")\n")
}

# =============================================================================
# STEP 2: download the zip from GBIF (skipped if you already have it)
# =============================================================================
zip_path <- file.path(output_dir, paste0(key, ".zip"))
zip_ok <- function(p) file.exists(p) && !inherits(try(unzip(p, list = TRUE), silent = TRUE), "try-error")

if (zip_ok(zip_path)) {
  message("Zip already downloaded: ", basename(zip_path), "\n")
} else {
  message("Waiting for GBIF to prepare the download (a few minutes; you can leave R running)...")
  occ_download_wait(key)
  occ_download_get(key, path = output_dir, overwrite = TRUE)
  if (!zip_ok(zip_path)) stop("The zip did not download properly: ", zip_path, call. = FALSE)
  message("Saved: ", zip_path, "\n")
}

# =============================================================================
# STEP 3: read the records
# =============================================================================
inner <- unzip(zip_path, list = TRUE)$Name
inner <- inner[grepl("\\.csv$", inner, ignore.case = TRUE)][1]
if (is.na(inner)) stop("No .csv found inside ", zip_path, call. = FALSE)

tmp <- tempfile("gbif_")
dir.create(tmp)
unzip(zip_path, files = inner, exdir = tmp)
occ <- read.delim(file.path(tmp, inner), quote = "", comment.char = "", na.strings = c("", "NA"),
                  check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
unlink(tmp, recursive = TRUE)

num <- function(x) suppressWarnings(as.numeric(x))
occ$decimalLatitude  <- num(occ$decimalLatitude)
occ$decimalLongitude <- num(occ$decimalLongitude)
occ$coordinateUncertaintyInMeters <- num(occ$coordinateUncertaintyInMeters)

# =============================================================================
# STEP 4: clean the records (the log shows how many each step removes)
# =============================================================================
log_steps <- data.frame(step = "Downloaded from GBIF", removed = NA_integer_, remaining = nrow(occ))
cleaning_step <- function(df, label, keep) {
  keep[is.na(keep)] <- FALSE
  out <- df[keep, , drop = FALSE]
  log_steps[nrow(log_steps) + 1, ] <<- list(label, nrow(df) - nrow(out), nrow(out))
  out
}

occ <- cleaning_step(occ, "Has valid coordinates", !is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude))

occ <- cleaning_step(occ, "Inside the Iceland window",
                     occ$decimalLongitude >= bbox[["xmin"]] & occ$decimalLongitude <= bbox[["xmax"]] &
                       occ$decimalLatitude  >= bbox[["ymin"]] & occ$decimalLatitude  <= bbox[["ymax"]])

unc <- occ$coordinateUncertaintyInMeters
n_missing_unc <- sum(is.na(unc))
unc_ok <- (!is.na(unc) & unc <= max_uncertainty_m) | (is.na(unc) & keep_missing_uncertainty)
occ <- cleaning_step(occ, paste0("Coordinate uncertainty <= ", max_uncertainty_m, " m",
                                 if (keep_missing_uncertainty) " (missing kept)" else " (missing dropped)"), unc_ok)

dup <- duplicated(occ[, c("decimalLatitude", "decimalLongitude", "eventDate")])
occ <- cleaning_step(occ, "Exact duplicates (same coordinates and date)", !dup)

cat("\n=== Cleaning summary ===\n")
print(log_steps, row.names = FALSE)
cat("\nRecords with no uncertainty value (before the uncertainty step):", n_missing_unc, "\n")
cat("\nRecords per species:\n"); print(table(occ$species, useNA = "ifany"))
cat("\nYears:", paste(range(occ$year, na.rm = TRUE), collapse = " - "), "\n\n")

if (nrow(occ) == 0) stop("No records left after cleaning - check the settings.", call. = FALSE)

# =============================================================================
# STEP 5: save the results
# =============================================================================
sp <- ifelse(is.na(occ$species), occ$scientificName, occ$species)

clean <- data.frame(
  gbifID    = occ$gbifID,
  species   = sp,
  scientificName = occ$scientificName,
  taxonKey  = occ$taxonKey,
  longitude = occ$decimalLongitude,
  latitude  = occ$decimalLatitude,
  coordinateUncertaintyInMeters = occ$coordinateUncertaintyInMeters,
  eventDate = occ$eventDate,
  year = occ$year, month = occ$month, day = occ$day,
  basisOfRecord = occ$basisOfRecord,
  datasetKey = occ$datasetKey,
  institutionCode = occ$institutionCode,
  stringsAsFactors = FALSE
)
clean_file <- file.path(output_dir, sprintf("crowberry_occurrences_clean_%s.csv", tag))
write.csv(clean, clean_file, row.names = FALSE, na = "", fileEncoding = "UTF-8")

summary_file <- file.path(output_dir, sprintf("gbif_cleaning_summary_%s.csv", tag))
write.csv(log_steps, summary_file, row.names = FALSE)

# Citation (GBIF asks you to cite the download DOI)
meta <- try(occ_download_meta(key), silent = TRUE)
if (!inherits(meta, "try-error") && !is.null(meta$doi)) {
  made <- tryCatch(format(as.Date(substr(meta$created, 1, 10)), "%d %B %Y"), error = function(e) format(Sys.Date(), "%d %B %Y"))
  cite <- sprintf("GBIF.org (%s) GBIF Occurrence Download https://doi.org/%s", made, meta$doi)
} else {
  cite <- paste0("DOI could not be retrieved just now. Look up download ", key,
                 " at https://www.gbif.org/occurrence/download/", key, " and cite its DOI.")
}
writeLines(c(cite, "", paste("Download key:", key)), file.path(output_dir, "gbif_download_citation.txt"))

message("Done. Saved in ", output_dir, ":")
message("  ", basename(clean_file),  "   (", nrow(clean), " records, with details)")
message("  ", basename(summary_file))
message("  gbif_download_citation.txt")
message("\nCite as: ", cite)