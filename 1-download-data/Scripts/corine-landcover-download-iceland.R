# corine-landcover-download-iceland.R
# Downloads CORINE Land Cover 2018 for Iceland (vector polygons, 44-class CORINE nomenclature, 25 ha minimum
# mapping unit) from the National Land Survey of Iceland's public WFS service (layer CORINE:clc18_is, the same
# server as the IS 50V coastline), in pages, and combines the pages into one GeoPackage. The layer only covers
# Iceland, so no coordinate box is needed.
#
# Put this file in: ISP-Crowberry-SDM/1-download-data/Scripts/
# Writes to:        ISP-Crowberry-SDM/1-download-data/Downloads/CORINE/
#   raw/page_<n>.geojson                 the service's answer, <page_size> features per file, exactly as downloaded
#   corine_2018_iceland.gpkg             all polygons combined, WGS84 (EPSG:4326)
#   corine_2018_class_summary.csv        each CORINE class present: number of polygons, area in km2, share of the total
#   corine_2018_attribute_preview.csv    the first 50 rows of the attribute table, to see what the columns hold
#   corine_2018_preview.png              quick picture to check the result by eye
#   ATTRIBUTION.txt                      credit line required by the Copernicus data policy
#
# How it works:
#   1. Asks the service how many features the layer has (the dataset record says 17,633).
#   2. Works out a way of asking for features that the service accepts (it tries a few request styles on the first
#      feature, and if none works it prints what the server said).
#   3. Downloads the features page by page. Each page is checked as soon as it arrives and retried if it is bad.
#      Pages already downloaded and readable are skipped on a re-run, so you can stop it and start it again.
#   4. Combines the pages into the GeoPackage and summarises the classes.
#
# The class summary needs the column that holds the CORINE code (normally "Code_18"). It is found by name; if it
# can't be found the script says so and skips the summary, and the preview csv shows you the columns.
#
# Licence (from the dataset record): free, full and open access under the Copernicus data policy, Regulation (EU)
# No 1159/2013. When you share or publish anything made with it you must credit the source, not imply that the
# EU endorses your work, say if you changed the data, and acknowledge that the data were produced "with funding by
# the European Union". ATTRIBUTION.txt has the wording.
#
# NOT TESTED against the live service. If the script stops, read the message it prints.

# install.packages(c("sf", "curl"))
if (!requireNamespace("curl", quietly = TRUE)) install.packages("curl")
if (!requireNamespace("xml2", quietly = TRUE)) install.packages("xml2")
suppressPackageStartupMessages({
  library(sf)
  library(curl)
  library(xml2)
})

# ---- Settings ---------------------------------------------------------------
wfs_url   <- "https://gis.lmi.is/geoserver/wfs"
layer     <- "CORINE:clc18_is"
page_size <- 2000                # features per request; lower this if pages keep failing
# Column(s) to sort by when paging. NULL = work it out from the layer: an ID-type column if there is one, otherwise the
# first few columns. (The layer has no primary key, and the server refuses to hand out pages unless it is told how to
# sort them. Set this to a column name, e.g. sort_by <- "ID", if the automatic choice is wrong.)
sort_by   <- NULL
overwrite <- FALSE               # TRUE re-downloads even if the pages already exist and are readable
options(timeout = 900)           # seconds

# The 44 CORINE Land Cover classes (standard nomenclature), used to name the classes in the summary.
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

out_dir <- file.path(proj_root, "1-download-data", "Downloads", "CORINE")
raw_dir <- file.path(out_dir, "raw")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
message("Output dir:   ", out_dir)

gpkg_file <- file.path(out_dir, "corine_2018_iceland.gpkg")
sum_file  <- file.path(out_dir, "corine_2018_class_summary.csv")
prev_file <- file.path(out_dir, "corine_2018_attribute_preview.csv")
png_file  <- file.path(out_dir, "corine_2018_preview.png")

# ---- Helpers ----------------------------------------------------------------
peek <- function(f, n = 250) {                 # start and end of a file on one line, for error messages
  sz <- file.size(f)
  con <- file(f, "rb"); on.exit(close(con))
  first <- readChar(con, min(n, sz), useBytes = TRUE)
  seek(con, max(0, sz - n))
  last <- readChar(con, min(n, sz), useBytes = TRUE)
  squash <- function(s) substr(gsub("\\s+", " ", iconv(s, "", "UTF-8", sub = "?")), 1, n)
  paste0("size ", format(sz, big.mark = ","), " bytes.\n  Starts with: ", squash(first), "\n  Ends with:   ", squash(last))
}

# NULL if the file is a readable layer with at least one feature, otherwise a short description of the problem.
file_problem <- function(f) {
  if (!file.exists(f) || file.size(f) == 0) return("the file is empty")
  start <- trimws(readChar(f, 300, useBytes = TRUE))
  if (!startsWith(start, "{")) return(paste0("the service did not send JSON data. ", peek(f)))
  r <- tryCatch(st_read(f, quiet = TRUE), error = function(e) e)
  if (inherits(r, "error")) return(paste0("the file can't be read (probably cut off in transit): ", conditionMessage(r), "\n  ", peek(f)))
  if (nrow(r) == 0) return("the file has no features")
  NULL
}

# Downloads url to dest. A refusal (400, 404, ...) comes back together with the message the server sent, which usually
# says what it didn't like. Returns NULL if the file arrived, otherwise a description of the problem.
http_get <- function(url, dest) {
  if (file.exists(dest)) file.remove(dest)
  res <- tryCatch(curl::curl_fetch_disk(url, dest, handle = curl::new_handle(timeout = 900, connecttimeout = 60)),
                  error = function(e) e)
  if (inherits(res, "error")) return(paste0("connection problem: ", conditionMessage(res)))
  if (res$status_code != 200) {
    body <- tryCatch(paste(readLines(dest, n = 20, warn = FALSE), collapse = " "), error = function(e) "")
    body <- substr(gsub("\\s+", " ", iconv(body, "", "UTF-8", sub = "?")), 1, 500)
    return(paste0("HTTP ", res$status_code, ". The server said: ", if (nzchar(body)) body else "(no message)"))
  }
  NULL
}

# Download url to dest, checking the result and retrying if it is bad.
fetch_checked <- function(url, dest, tries = 3, wait = 10) {
  tmp <- paste0(dest, ".part")
  problem <- "no attempt made"
  for (i in seq_len(tries)) {
    problem <- http_get(url, tmp)
    if (is.null(problem)) problem <- file_problem(tmp)
    if (is.null(problem)) { file.rename(tmp, dest); return(invisible(TRUE)) }
    message("  attempt ", i, " failed: ", problem)
    if (i < tries) Sys.sleep(wait)
  }
  stop("Could not get a good file from the service after ", tries, " tries.\n  Last problem: ", problem,
       "\n  URL: ", url, call. = FALSE)
}

# ---- Step 1: how many features? -----------------------------------------------
message("\nLayer: ", layer, "   service: ", wfs_url)
type_param <- utils::URLencode(layer, reserved = TRUE)
hits_file  <- tempfile(fileext = ".xml")
hits_prob  <- http_get(paste0(wfs_url, "?service=WFS&version=2.0.0&request=GetFeature&typeNames=", type_param,
                              "&resultType=hits"), hits_file)
if (!is.null(hits_prob)) {
  stop("Could not ask the service for the feature count: ", hits_prob,
       "\nIf the layer name is the problem, open https://gis.lmi.is/geoserver/wfs?request=GetCapabilities&service=WFS ",
       "in a browser, find the CORINE layer, and set layer at the top of this script.", call. = FALSE)
}
hits_txt <- paste(readLines(hits_file, warn = FALSE), collapse = " ")
if (!grepl("numberMatched=\"[0-9]+\"", hits_txt)) {
  stop("The service did not report a feature count. It said:\n  ", substr(hits_txt, 1, 400), call. = FALSE)
}
n_match <- as.numeric(sub(".*numberMatched=\"([0-9]+)\".*", "\\1", hits_txt))
n_pages <- ceiling(n_match / page_size)
message("The layer has ", format(n_match, big.mark = ","), " features: ", n_pages, " page(s) of up to ",
        format(page_size, big.mark = ","), ".  (The dataset record says 17,633.)")

# ---- Step 1b: choose a sort order for paging -------------------------------------------
# The server said "Cannot do natural order without a primary key": the layer has no ID column the server can page by,
# so every page request has to say how to sort. A column that is unique per polygon (an ID) keeps the pages from
# overlapping or leaving gaps.
sort_attrs <- sort_by
if (is.null(sort_attrs)) {
  desc_file <- tempfile(fileext = ".xml")
  prob <- http_get(paste0(wfs_url, "?service=WFS&version=2.0.0&request=DescribeFeatureType&typeNames=", type_param), desc_file)
  if (!is.null(prob)) {
    stop("Could not ask the service for the layer's columns: ", prob,
         "\nSet sort_by at the top of this script to a column name.", call. = FALSE)
  }
  attr_names <- character(0)
  desc <- tryCatch(xml2::read_xml(desc_file), error = function(e) NULL)
  if (!is.null(desc)) {
    xml2::xml_ns_strip(desc)
    # Columns are the <element name="..."> entries inside the feature's <sequence>. Columns with a length or range limit
    # have no type attribute (the type is written inside the element), so don't require one. The geometry column is
    # the one whose type is a GML type.
    el        <- xml2::xml_find_all(desc, "//sequence/element[@name]")
    col_names <- xml2::xml_attr(el, "name")
    col_types <- xml2::xml_attr(el, "type")
    is_geom   <- !is.na(col_types) & grepl("^gml:", col_types)
    attr_names <- col_names[!is_geom]
  }
  if (length(attr_names) == 0) {
    # Second way of finding the column names: ask for one feature (no paging, so no sort order is needed) and read its columns.
    message("Could not read the column list from the service's schema; reading the columns from one feature instead.")
    one_file <- tempfile(fileext = ".geojson")
    prob <- http_get(paste0(wfs_url, "?service=WFS&version=2.0.0&request=GetFeature&typeNames=", type_param,
                            "&count=1&outputFormat=application%2Fjson"), one_file)
    if (is.null(prob)) prob <- file_problem(one_file)
    if (!is.null(prob)) {
      stop("Could not find the layer's column names: ", prob,
           "\nSet sort_by at the top of this script to a column name (open the layer in QGIS to see the columns).", call. = FALSE)
    }
    one <- st_read(one_file, quiet = TRUE)
    attr_names <- setdiff(names(one), attr(one, "sf_column"))
  }
  if (length(attr_names) == 0) {
    stop("Could not read the layer's column names from the service. Set sort_by at the top of this script to a column name.", call. = FALSE)
  }
  message("Layer columns: ", paste(attr_names, collapse = ", "))
  id_like    <- attr_names[tolower(attr_names) %in% c("id", "objectid", "fid", "gid", "oid")]
  sort_attrs <- if (length(id_like) > 0) id_like[1] else utils::head(attr_names, 4)
}
message("Sorting pages by: ", paste(sort_attrs, collapse = ", "))
sort_param <- utils::URLencode(paste(sort_attrs, collapse = ","), reserved = TRUE)

# ---- Step 2: find a way of asking for features that the service accepts ---------------
page_url_for <- function(v, start, n) {
  paste0(wfs_url, "?service=WFS&version=", v$version, "&request=GetFeature",
         if (v$version == "2.0.0") "&typeNames=" else "&typeName=", type_param,
         if (v$version == "2.0.0") "&count=" else "&maxFeatures=", n,
         "&startIndex=", start,
         "&sortBy=", sort_param,
         "&outputFormat=", v$format,
         if (!is.null(v$srs)) paste0("&srsName=", v$srs) else "")
}
variants <- list(
  list(name = "WFS 2.0.0, application/json, EPSG:4326",    version = "2.0.0", format = "application%2Fjson", srs = "EPSG:4326"),
  list(name = "WFS 2.0.0, application/json, native CRS",   version = "2.0.0", format = "application%2Fjson", srs = NULL),
  list(name = "WFS 2.0.0, json, EPSG:4326",                version = "2.0.0", format = "json",               srs = "EPSG:4326"),
  list(name = "WFS 1.1.0, application/json, EPSG:4326",    version = "1.1.0", format = "application%2Fjson", srs = "EPSG:4326"),
  list(name = "WFS 1.0.0, application/json, EPSG:4326",    version = "1.0.0", format = "application%2Fjson", srs = "EPSG:4326"),
  list(name = "WFS 1.0.0, application/json, native CRS",   version = "1.0.0", format = "application%2Fjson", srs = NULL)
)
variant <- NULL
probe_msgs <- character(0)
probe_file <- tempfile(fileext = ".geojson")
for (v in variants) {
  prob <- http_get(page_url_for(v, 0, 1), probe_file)
  if (is.null(prob)) prob <- file_problem(probe_file)
  if (is.null(prob)) { variant <- v; break }
  probe_msgs <- c(probe_msgs, paste0("  - ", v$name, ": ", prob))
}
if (is.null(variant)) {
  stop("The service rejected every way of asking for features that I tried:\n", paste(probe_msgs, collapse = "\n"),
       "\nPaste this whole message to me.", call. = FALSE)
}
message("Request style the service accepts: ", variant$name)

# ---- Step 3: download the pages ---------------------------------------------
page_files <- file.path(raw_dir, sprintf("page_%04d.geojson", seq_len(n_pages)))
for (i in seq_len(n_pages)) {
  label <- sprintf("[%d/%d] %s", i, n_pages, basename(page_files[i]))
  if (file.exists(page_files[i]) && !overwrite && is.null(file_problem(page_files[i]))) {
    message(label, "  -> already downloaded, skipping")
    next
  }
  fetch_checked(page_url_for(variant, (i - 1) * page_size, page_size), page_files[i])
  message(label, "  -> saved (", format(round(file.size(page_files[i]) / 1e6, 1), nsmall = 1), " MB)")
}

# ---- Step 4: combine into one GeoPackage and summarise the classes --------------------
message("\nCombining the pages into one GeoPackage ...")
sf_use_s2(FALSE)   # planar geometry operations in lon/lat; fine at this scale
if (file.exists(gpkg_file)) file.remove(gpkg_file)
n_total    <- 0
n_repaired <- 0
code_col   <- NULL
pieces    <- vector("list", n_pages)
for (i in seq_len(n_pages)) {
  pg <- st_read(page_files[i], quiet = TRUE)
  if (is.na(st_crs(pg))) {
    if (is.null(variant$srs)) stop("The pages have no coordinate system and were not requested in WGS84, so I can't tell what they are in. Tell me and I will add a fix.", call. = FALSE)
    pg <- st_set_crs(pg, 4326)
  }
  pg <- st_transform(pg, 4326)
  
  # Repair invalid polygons. A few polygons (26 of the 17,633 in the version checked) cross over their own edges
  # ("bow-ties"), and QGIS tools such as Select by Location then complain about invalid geometry. st_make_valid() leaves
  # valid polygons alone and redraws the bad ones without the crossing; where that splits a polygon into pieces they are
  # kept as parts of one multipolygon, so the feature count and the attributes don't change.
  g   <- st_geometry(pg)
  bad <- !st_is_valid(g)
  if (any(bad)) {
    n_repaired <- n_repaired + sum(bad)
    g  <- st_make_valid(g)
    gc <- as.character(st_geometry_type(g)) == "GEOMETRYCOLLECTION"
    if (any(gc)) g[gc] <- st_collection_extract(g[gc], "POLYGON")
  }
  st_geometry(pg) <- st_cast(g, "MULTIPOLYGON")
  
  if (i == 1) {
    bb <- st_bbox(pg)
    if (bb["xmin"] > 60) stop("The coordinates look like lat/lon instead of lon/lat (axis order swapped). Tell me and I will add a fix.", call. = FALSE)
    write.csv(utils::head(sf::st_drop_geometry(pg), 50), prev_file, row.names = FALSE)
    message("Columns: ", paste(names(pg), collapse = ", "))
    cand <- names(pg)[grepl("^code_?18$|^clc_?code|^code", names(pg), ignore.case = TRUE)]
    if (length(cand) > 0) {
      code_col <- cand[1]
      message("CORINE code column: ", code_col)
    } else {
      message("No CORINE code column found by name; the class summary and the coloured preview will be skipped. ",
              "Look at corine_2018_attribute_preview.csv to see which column holds the class.")
    }
  }
  st_write(pg, gpkg_file, layer = "corine_2018", append = (i > 1), quiet = TRUE)
  area_km2 <- as.numeric(st_area(st_make_valid(st_transform(pg, 3057)))) / 1e6   # ISN93 / Lambert, km2
  pieces[[i]] <- data.frame(code = if (!is.null(code_col)) as.character(as.integer(as.character(pg[[code_col]]))) else NA_character_,
                            area_km2 = area_km2, stringsAsFactors = FALSE)
  n_total <- n_total + nrow(pg)
}
message("Saved: ", gpkg_file, "   (", format(n_total, big.mark = ","), " features; ",
        n_repaired, " invalid polygon(s) repaired)")
if (n_total != n_match) {
  warning("Expected ", n_match, " features but saved ", n_total, ". Some pages may overlap or be missing; ",
          "the sort column (", paste(sort_attrs, collapse = ", "), ") may not be unique. Tell me and I will change it.")
}
message("Saved: ", prev_file)

all_pieces <- do.call(rbind, pieces)
tot_km2 <- sum(all_pieces$area_km2)
message("Total mapped area: ", format(round(tot_km2), big.mark = ","), " km2  (Iceland is about 103,000 km2)")

if (!is.null(code_col)) {
  agg <- stats::aggregate(area_km2 ~ code, data = all_pieces, FUN = sum)
  cnt <- as.data.frame(table(code = all_pieces$code), stringsAsFactors = FALSE)
  summ <- merge(agg, cnt, by = "code")
  summ$class_name <- unname(clc_names[summ$code])
  summ$pct_of_area <- round(100 * summ$area_km2 / tot_km2, 2)
  summ$area_km2 <- round(summ$area_km2)
  summ <- summ[order(-summ$area_km2), c("code", "class_name", "Freq", "area_km2", "pct_of_area")]
  names(summ)[names(summ) == "Freq"] <- "n_polygons"
  write.csv(summ, sum_file, row.names = FALSE)
  message("\n=== CORINE classes in Iceland ===")
  print(summ, row.names = FALSE)
  message("Saved: ", sum_file)
}

# ---- Step 5: preview picture and attribution ----------------------------------------
all_sf <- st_read(gpkg_file, quiet = TRUE)
png(png_file, width = 1500, height = 1000, res = 130)
if (!is.null(code_col)) {
  codes <- as.character(as.integer(as.character(all_sf[[code_col]])))
  lev   <- sort(unique(codes))
  cols  <- stats::setNames(grDevices::hcl.colors(length(lev), "Dynamic"), lev)
  plot(st_geometry(all_sf), col = cols[codes], border = NA, axes = TRUE, main = "CORINE Land Cover 2018, Iceland")
  lab <- ifelse(is.na(clc_names[lev]), lev, paste0(lev, " ", clc_names[lev]))
  legend("bottomright", legend = lab, fill = cols, bty = "n", cex = 0.6, ncol = if (length(lev) > 14) 2 else 1)
} else {
  plot(st_geometry(all_sf), col = "grey80", border = NA, axes = TRUE, main = "CORINE Land Cover 2018, Iceland")
}
invisible(dev.off())
message("Saved: ", png_file)

writeLines(c(
  "CORINE Land Cover 2018, Iceland (layer CORINE:clc18_is).",
  "Custodian: Natural Science Institute of Iceland (Natturufraedistofnun Islands). Owner: European Commission (Copernicus Land Monitoring Service).",
  paste0("Retrieved ", format(Sys.Date(), "%d %B %Y"), " via ", wfs_url, "."),
  "Changes made here: downloaded in pages and reprojected to WGS84 (EPSG:4326); the data was not otherwise altered.",
  "",
  "Terms (Copernicus data and information policy, Regulation (EU) No 1159/2013):",
  "  - When you share or publish anything made with these data, state the source.",
  "  - Do not give the impression that the European Union endorses your work.",
  "  - If you change or adapt the data, say so clearly.",
  "  - Acknowledge that the data were produced \"with funding by the European Union\"."
), file.path(out_dir, "ATTRIBUTION.txt"))

message("\nDone. All files are in: ", out_dir)