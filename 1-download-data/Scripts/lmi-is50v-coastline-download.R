# lmi-is50v-coastline-download.R
# Downloads the IS 50V coastline (Strandlina) from Landmaelingar Islands (National Land
# Survey of Iceland, open data, CC BY 4.0) through their public WFS service, and builds a
# land polygon you can use to mask the rasters.
#
# Put this file in: ISP-Crowberry-SDM/1-download-data/Scripts/
# Writes to:        ISP-Crowberry-SDM/1-download-data/Downloads/LMI-IS50V/
#   is50v_coastline_raw.geojson   raw layer exactly as downloaded
#   is50v_land_polygon.gpkg       land polygon(s), WGS84 (EPSG:4326)
#   is50v_land_preview.png        quick picture to check the result by eye
#   wfs_layers.txt                every layer the service offers (for troubleshooting)
#   ATTRIBUTION.txt               credit line required by the CC BY 4.0 licence
#
# NOT TESTED against the live service. The layer name is auto-detected, because it
# could not be confirmed in advance. If the script stops, read the message it prints.

# install.packages(c("sf", "xml2"))
suppressPackageStartupMessages({
  library(sf)
  library(xml2)
})

# ---- Settings ---------------------------------------------------------------
wfs_url        <- "https://gis.lmi.is/geoserver/wfs"
layer_pattern  <- "strandl|coast"   # matched (case-insensitive) against layer names/titles
layer_override <- "IS_50V:strandlina_flakar"
overwrite      <- FALSE             # TRUE re-downloads even if the raw file already exists
options(timeout = 900)              # seconds; the coastline is a large layer

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

out_dir <- file.path(proj_root, "1-download-data", "Downloads", "LMI-IS50V")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
message("Output dir:   ", out_dir)

raw_file  <- file.path(out_dir, "is50v_coastline_raw.geojson")
land_file <- file.path(out_dir, "is50v_land_polygon.gpkg")

# ---- Small helper: download with retries ------------------------------------
download_retry <- function(url, dest, tries = 3, wait = 10) {
  for (i in seq_len(tries)) {
    ok <- tryCatch({
      download.file(url, dest, mode = "wb", quiet = TRUE)
      file.exists(dest) && file.size(dest) > 0
    }, error = function(e) {
      message("  attempt ", i, " failed: ", conditionMessage(e))
      FALSE
    }, warning = function(w) {
      message("  attempt ", i, " warning: ", conditionMessage(w))
      FALSE
    })
    if (isTRUE(ok)) return(invisible(TRUE))
    if (i < tries) Sys.sleep(wait)
  }
  stop("Download failed after ", tries, " tries: ", url)
}

# ---- Step 1: find the coastline layer ---------------------------------------
if (is.null(layer_override)) {
  message("\nAsking the service which layers it offers ...")
  caps_file <- tempfile(fileext = ".xml")
  download_retry(paste0(wfs_url, "?service=WFS&version=1.1.0&request=GetCapabilities"), caps_file)

  caps <- read_xml(caps_file)
  xml_ns_strip(caps)
  ft     <- xml_find_all(caps, "//FeatureType")
  l_name <- xml_text(xml_find_first(ft, "./Name"))
  l_title <- xml_text(xml_find_first(ft, "./Title"))

  writeLines(paste0(l_name, "   |   ", l_title), file.path(out_dir, "wfs_layers.txt"))
  message("Service offers ", length(l_name), " layers (list saved to wfs_layers.txt)")

  hit <- grepl(layer_pattern, paste(l_name, l_title), ignore.case = TRUE)
  if (sum(hit) == 0) {
    stop("No layer name/title matched '", layer_pattern, "'. Open wfs_layers.txt, find the ",
         "IS 50V coastline layer, and set layer_override at the top of this script.")
  }
  if (sum(hit) > 1) {
    message("\nSeveral layers matched:")
    print(data.frame(name = l_name[hit], title = l_title[hit]), row.names = FALSE)
    stop("Pick one and set layer_override at the top of this script.")
  }
  layer <- l_name[hit]
} else {
  layer <- layer_override
}
message("Using layer: ", layer)

# ---- Step 2: download it ----------------------------------------------------
if (file.exists(raw_file) && !overwrite) {
  message("\nRaw file already exists, skipping download (set overwrite <- TRUE to redo): ", raw_file)
} else {
  message("\nDownloading (this can take a few minutes) ...")
  get_url <- paste0(wfs_url,
                    "?service=WFS&version=1.0.0&request=GetFeature",
                    "&typeName=", utils::URLencode(layer, reserved = TRUE),
                    "&outputFormat=application%2Fjson",
                    "&srsName=EPSG:4326")
  tmp <- paste0(raw_file, ".part")
  download_retry(get_url, tmp)
  file.rename(tmp, raw_file)
  message("Saved: ", raw_file)
}

# ---- Step 3: read and sanity-check ------------------------------------------
x <- st_read(raw_file, quiet = TRUE)
if (is.na(st_crs(x))) x <- st_set_crs(x, 4326)
x <- st_transform(x, 4326)

bb <- st_bbox(x)
message("\nFeatures: ", nrow(x), "   geometry: ", paste(unique(as.character(st_geometry_type(x))), collapse = ", "))
message("Bounding box (lon/lat): ",
        paste(round(bb[c("xmin", "ymin", "xmax", "ymax")], 3), collapse = ", "))
if (bb["xmin"] > 60) {
  stop("The bounding box looks like lat/lon instead of lon/lat (axis order swapped). ",
       "Tell me and I will add a fix.")
}
if (bb["xmin"] < -26 || bb["xmax"] > -11 || bb["ymin"] < 62 || bb["ymax"] > 68) {
  warning("The layer extends beyond Iceland's usual bounding box; check the preview picture.")
}

# ---- Step 4: make a land polygon --------------------------------------------
geom_types <- unique(as.character(st_geometry_type(x)))
sf_use_s2(FALSE)   # planar geometry operations in lon/lat; fine at this scale

if (any(grepl("POLYGON", geom_types))) {
  message("\nLayer is already polygons; dissolving into one land layer.")
  geom <- st_make_valid(st_geometry(x))
  u    <- st_union(geom)
  u    <- st_collection_extract(u, "POLYGON")   # keep only polygons
  land <- st_sf(geometry = st_cast(u, "MULTIPOLYGON"))
} else {
  message("\nLayer is lines; closing the coastline rings into polygons.")
  lines <- st_union(st_geometry(x))               # merge and node all line pieces
  polys <- st_polygonize(lines)
  polys <- st_collection_extract(polys, "POLYGON")
  if (length(polys) == 0) {
    stop("The coastline lines did not close into any polygons (gaps in the lines?).")
  }
  land <- st_sf(geometry = st_cast(st_union(polys), "MULTIPOLYGON"))
}
st_crs(land) <- 4326

area_km2 <- as.numeric(sum(st_area(st_transform(land, 3057)))) / 1e6   # ISN93 / Lambert
n_parts  <- length(st_cast(st_geometry(land), "POLYGON"))
message("Land polygon: ", n_parts, " parts, total area ", format(round(area_km2), big.mark = ","), " km2",
        "  (Iceland is about 103,000 km2)")
if (area_km2 < 95000 || area_km2 > 110000) {
  warning("Total area is far from Iceland's ~103,000 km2. The polygon may be wrong ",
          "(gaps in the coastline, or the layer is sea instead of land). Check the preview picture.")
}

# ---- Step 5: save -----------------------------------------------------------
st_write(land, land_file, layer = "land", delete_dsn = TRUE, quiet = TRUE)
message("Saved: ", land_file)

png(file.path(out_dir, "is50v_land_preview.png"), width = 1400, height = 900, res = 130)
plot(st_geometry(land), col = "grey85", border = "grey20", lwd = 0.3, axes = TRUE,
     main = paste0("IS 50V land polygon (", format(round(area_km2), big.mark = ","), " km2)"))
invisible(dev.off())
message("Saved: ", file.path(out_dir, "is50v_land_preview.png"))

writeLines(c(
  "Contains data from the IS 50V database of the National Land Survey of Iceland (Landmaelingar Islands),",
  paste0("retrieved ", format(Sys.Date(), "%d %B %Y"), " via ", wfs_url, " (layer: ", layer, ")."),
  "Licence: Creative Commons Attribution 4.0 International (CC BY 4.0).",
  "",
  paste0("Inniheldur gogn fra IS 50V gagnagrunni Landmaelinga Islands fra ", format(Sys.Date(), "%m/%Y"), ".")
), file.path(out_dir, "ATTRIBUTION.txt"))

message("\nDone. All files are in: ", out_dir)
