
# Vector operations ----


#' Write Shapefile to a New Directory and Create a Zipped Version
#'
#' This function writes an `sf` object to a shapefile in a new, file-specific directory and optionally creates a zipped version of the shapefile.
#' It also allows for the removal of the original unzipped files and handles overwriting existing files.
#'
#' @param shp An `sf` object to write as a shapefile.
#' @param location A character string specifying the path of the directory to create the new file-specific subdirectory in.
#' @param filename A character string specifying the name of the file without the `.shp` extension.
#' @param zip_only A logical value indicating whether the original (unzipped) files should be removed after zipping. Defaults to `FALSE`.
#' @param overwrite A logical value indicating whether existing files should be overwritten. Defaults to `FALSE`.
#' @return No return value. The function writes a shapefile to a specified directory, optionally zips the files, and manages file cleanup based on user input.
#' @examples
#' \dontrun{
#' # Example usage
#' st_write_shp(shp = prepped_for_parks_etal,
#'              location = here::here("data/derived"),
#'              filename = "career_lba_for_parks_v1",
#'              zip_only = TRUE,
#'              overwrite = TRUE)
#' }
#' @importFrom sf st_write
#' @importFrom zip zip
#' @export
st_write_shp <- function(shp, location, filename, zip_only = FALSE, overwrite = FALSE) {
  
  # Define paths
  out_dir <- file.path(location, filename)
  zip_file <- file.path(out_dir, paste0(filename, ".zip"))
  zip_file_dest <- file.path(location, paste0(filename, ".zip"))
  
  # Manage overwriting and directory creation
  if (dir.exists(out_dir)) {
    if (overwrite) {
      unlink(out_dir, recursive = TRUE)
    } else {
      stop("Directory '", out_dir, "' already exists and overwrite is set to FALSE.")
    }
  }
  
  if (file.exists(zip_file_dest) && zip_only) {
    if (overwrite) {
      unlink(zip_file_dest)
    } else {
      stop("Zip file '", zip_file_dest, "' already exists and overwrite is set to FALSE.")
    }
  }
  
  # Create the directory if not there
  dir_ensure(out_dir)
  
  # Write the shapefile
  shapefile_path <- file.path(out_dir, paste0(filename, ".shp"))
  sf::st_write(shp, shapefile_path, append = FALSE)
  
  # Get all shapefile components
  all_shp_files <- list.files(out_dir, pattern = paste0(filename, ".*"), full.names = TRUE)
  
  # Create zip file
  zip::zip(zipfile = zip_file, files = all_shp_files, mode = "cherry-pick")
  
  # Remove raw files if zip_only is TRUE
  if (zip_only) {
    file.copy(zip_file, zip_file_dest)
    unlink(out_dir, recursive = TRUE)
  }
}


#' Convert Bounding Box to String Format
#'
#' This function takes a spatial object (`sf` or `sfc` class) and returns a string representation of its bounding box
#' coordinates in the format `xmin,ymin,xmax,ymax`. This format is useful for spatial queries in APIs and other geospatial operations.
#'
#' @param shp An `sf` (simple features) or `sfc` (simple feature geometry) object. This object should represent a spatial feature 
#' or a collection of features from which the bounding box will be extracted.
#'
#' @return A character string in the format `xmin,ymin,xmax,ymax`, where `xmin`, `ymin`, `xmax`, and `ymax` are the bounding 
#' box coordinates of the input spatial object.
#'
#' @importFrom sf st_bbox
#' @examples
#' \dontrun{
#' library(sf)
#' # Create a simple sf object (example polygon)
#' polygon <- st_sfc(st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))), crs = 4326)
#' # Convert the bounding box to string format
#' bbox_string <- st_bbox_str(polygon)
#' print(bbox_string)
#' }
#' 
#' @importFrom sf st_bbox
#' @export
st_bbox_str <- function(shp) {
  bbox <- sf::st_bbox(shp)
  bbox_str <- sprintf("%f,%f,%f,%f", bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
  return(bbox_str)
}


# This function takes in a set of polygons and returns the same set of polygons
# with the area of the polygon added as a column called "st_area", in whatever units the polygon CRS is in
# PARAMETERS
# polys : a set of polygons as an sf object
st_area_to_poly <- function(polys) {
  
  out <- polys |>
    sf::st_area() |> #get area from sf package
    units::drop_units() %>%
    cbind(polys, .) |> #join to polygons
    dplyr::rename(st_area = `.`) #rename
  
  return(out)
  
}


#' Clip a raster to multiple polygons using st_crop_careful_universal
#'
#' This function clips a raster to a set of polygons (one clip per polygon) and returns a named list of clipped rasters.
#' It supports parallel execution if a future plan is set, and uses `st_crop_careful_universal()` for each clipping.
#'
#' @param raster A SpatRaster, PackedSpatRaster, RasterLayer, RasterStack, or RasterBrick object.
#' @param vectors An sf object containing multiple polygons.
#' @param namefield A character string indicating the field used to name the output list.
#' @param mask Logical. Whether to mask the raster using the polygons.
#'
#' @return A named list of clipped raster objects.
#' @export
careful_clip_set <- function(raster, vectors, namefield, mask = TRUE) {
  if (!inherits(vectors, "sf")) {
    stop("`vectors` must be an sf object.")
  }
  if (!namefield %in% colnames(vectors)) {
    stop("`namefield` not found in the sf object.")
  }
  
  # Split sf object by namefield into a list of sf polygons
  if (nrow(vectors) == 1) {
    splitVec <- list(vectors)
    names(splitVec) <- as.character(vectors[[namefield]])
  } else {
    splitVec <- split(vectors, f = vectors[[namefield]])
  }
  
  # Sequential execution
  if (inherits(future::plan(), "sequential")) {
    message("Performing clip set in sequence")
    out <- purrr::imap(splitVec, ~ st_crop_careful_universal(raster, .x, mask = mask))
  } else {
    # Parallel execution using furrr
    message("Performing clip set in parallel")
    r <- terra::wrap(raster, proxy = TRUE)
    v <- purrr::map(splitVec, terra::vect) |> purrr::map(terra::wrap)
    out <- furrr::future_map(v, st_crop_careful_universal, raster = r, mask = mask)
    out <- purrr::map(out, terra::unwrap)
    names(out) <- names(splitVec)
  }
  
  return(out)
}



#' Split an `sf` object into a list of polygons by a field
#'
#' This function takes an `sf` object with one or more polygon features and splits it into a list
#' of `sf` objects by a given field (column). This is useful when individual polygon-level operations are needed.
#'
#' @param vectors An `sf` object with one or more polygon features (rows).
#' @param name_field A string indicating the column name used to split the polygons.
#'
#' @return A named list of `sf` polygon subsets, each corresponding to a unique value in `name_field`.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#' polygons <- sf_to_polygon_list(nc, name_field = "NAME")
#' }
#'
#' @export
sf_to_polygon_list <- function(vectors, name_field) {
  if (!inherits(vectors, "sf")) {
    stop("`vectors` must be an sf object.")
  }
  if (!name_field %in% names(vectors)) {
    stop("The specified `name_field` is not a column in the sf object.")
  }
  
  if (nrow(vectors) == 1) {
    split_vec <- list(vectors)
    names(split_vec) <- as.character(vectors[[name_field]])
  } else {
    split_vec <- split(vectors, f = vectors[[name_field]])
  }
  
  return(split_vec)
}




#' Test whether an object can be serialized and unserialized
#'
#' This function checks whether a given R object can be safely serialized and unserialized in memory.
#' It prints diagnostic messages based on success or failure of both operations.
#'
#' @param obj An arbitrary R object to be tested for serialization.
#'
#' @return This function does not return a value; it prints messages to the console.
#'
#' @examples
#' test_serialization(mtcars)
#' test_serialization(function(x) x^2)
#'
#' @export
test_serialization <- function(obj) {
  serialized_obj <- serialize(obj, NULL)
  
  tryCatch({
    serialize(obj, NULL)
    print("Serialized successfully")
  }, error = function(e) {
    cat("Can't serialize: ", e$message, "\n")
  })
  
  tryCatch({
    unserialize(serialized_obj)
    print("Unserialized successfully")
  }, error = function(e) {
    cat("Can't unserialize: ", e$message, "\n")
  })
}


#' Find NAD83 UTM EPSG Code
#'
#' This helper function calculates the appropriate NAD83 UTM EPSG code for a given polygon based 
#' on its centroid's longitude. The polygon is first transformed to WGS84 (EPSG:4326) for accurate 
#' calculation.
#'
#' @param polygon An sf object representing a polygon. The coordinate reference system (CRS) is assumed to be set.
#'
#' @return An integer representing the EPSG code for the corresponding NAD83 UTM zone.
#'
#' @importFrom sf st_transform st_centroid st_coordinates
#' 
#' @export
find_nad83_utm_epsg <- function(polygon) {
  # Ensure the polygon is in WGS84 (EPSG:4326) for accurate longitude calculation
  polygonWgs84 <- sf::st_transform(polygon, crs = 4326)
  
  # Calculate the centroid
  centroid <- sf::st_centroid(polygonWgs84)
  centroidCoords <- sf::st_coordinates(centroid)
  
  # Get the longitude of the centroid
  longitude <- centroidCoords[1, "X"]
  
  # Calculate the UTM zone
  utmZone <- (floor((longitude + 180) / 6) %% 60) + 1
  
  # Determine the EPSG code for NAD83 UTM zone
  epsgCode <- 26900 + utmZone
  
  return(epsgCode)
}





#' Calculate Approximate Diameter of Maximum Inscribed Circle
#'
#' This function calculates the approximate diameter of the maximum inscribed circle 
#' within irregular polygons in an `sf` object. The result is added as a new column 
#' named `diam` to the input `sf` object.
#'
#' @param polys An `sf` object representing the polygons.
#' @param tolerance A numeric value specifying the threshold for considering circles 
#' to be touching a boundary.
#'
#' @return An `sf` object containing the original polygons with an additional column `diam`, 
#' which represents the approximate diameter of the maximum inscribed circle for each polygon.
#'
#' @details
#' This function uses the `geos::geos_maximum_inscribed_crc()` function to calculate 
#' the largest inscribed circle for each polygon. The diameter is then estimated as 
#' the maximum distance between boundary points of the circle. The result is returned 
#' as an `sf` object with the `diam` column appended.
#'
#' @importFrom geos geos_maximum_inscribed_crc as_geos_geometry
#' @importFrom sf st_as_sf st_crs st_transform st_cast st_distance
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' # Example with sample polygons
#' library(sf)
#' library(geos)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#'
#' # Calculate diameters with a specified tolerance
#' nc_with_diam <- calculate_approx_diameter_maximum_inscribed_circle(nc, tolerance = 0.1)
#' head(nc_with_diam)
#' }
#'
#' @export
calculate_approx_diameter_maximum_inscribed_circle <- function(polys, tolerance) {
  max_insc_crcs <- geos::geos_maximum_inscribed_crc(polys |>
                                                      geos::as_geos_geometry(), tolerance = tolerance) |>
    sf::st_as_sf() |>
    sf::st_transform(sf::st_crs(polys))
  diam <- c()
  for (i in 1:nrow(max_insc_crcs)) {
    p <- max_insc_crcs[i,]
    d <- p |>
      sf::st_cast('MULTIPOINT') %>%
      sf::st_cast('POINT') %>%
      sf::st_distance(which = "Euclidean") |>
      max()
    diam <- diam |> append(d)
  }
  return(cbind(polys, diam))
}

#' Buffer Polygons to Half-Diameter
#'
#' This function buffers polygons inwards based on half the approximate diameter of 
#' their maximum inscribed circle. The resulting polygons are "half-diameter polygons".
#'
#' @param poly An `sf` object representing the polygons.
#' @param tolerance A numeric value specifying the threshold for considering circles 
#' to be touching a boundary.
#'
#' @return An `sf` object with the buffered polygons and an additional column 
#' `old_diam` that contains the original diameter values.
#'
#' @details
#' This function first calculates the approximate diameter of the maximum inscribed 
#' circle for each polygon using the 
#' \code{\link{calculate_approx_diameter_maximum_inscribed_circle}} function. It then 
#' buffers the polygons inward by a distance equal to half the diameter.
#'
#' The resulting polygons are added to a new column `geometry`, while the original 
#' diameters are stored in a column `old_diam`.
#'
#' @importFrom dplyr filter mutate rename
#' @importFrom sf st_buffer
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' # Example with sample polygons
#' library(sf)
#' library(geos)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#'
#' # Create half-diameter polygons
#' nc_half_diam <- buffer_to_half_diam(nc, tolerance = 0.1)
#' head(nc_half_diam)
#' }
#'
#' @export
buffer_to_half_diam <- function(poly, tolerance) {
  pWithD <- calculate_approx_diameter_maximum_inscribed_circle(poly, tolerance) |>
    dplyr::filter(is.finite(diam))
  newPolys <- pWithD |>
    dplyr::mutate(geometry = sf::st_buffer(geometry, dist = (diam / 4) * -1)) |>
    dplyr::rename(old_diam = diam)
  return(newPolys)
}








# Raster operations ----


#' Clip a raster to a vector with careful handling of projections
#'
#' This function clips a raster to the extent of a vector, ensuring that the
#' raster and vector are in the same projection system. It supports raster and
#' vector objects from both the `terra` and `raster` packages. If the input raster
#' or vector is packed (for parallelized workflows), it will be unpacked before
#' processing and optionally re-packed afterward. 
#'
#' @param raster A SpatRaster, PackedSpatRaster, RasterLayer, RasterStack, or RasterBrick object. 
#'        The raster to be clipped.
#' @param vector A SpatVector, PackedSpatVector, or sf object. The vector defining the clipping boundary.
#' @param mask Logical. Should the raster be masked to the vector? Defaults to `FALSE`.
#' @param verbose Logical. If `TRUE`, provides detailed output of the steps being performed.
#'
#' @return A raster object clipped to the vector's extent, returned in the same format as the input raster.
#' @examples
#' \dontrun{
#' raster_obj <- terra::rast(system.file("ex/logo.tif", package = "terra"))
#' vector_obj <- terra::vect(system.file("ex/logo.shp", package = "terra"))
#' cropped_raster <- crop_careful_universal(raster_obj, vector_obj, mask = TRUE, verbose = TRUE)
#' }
#' @export
#' @importFrom terra unwrap wrap crop mask crs same.crs
#' @importFrom raster crs crop mask
#' @importFrom sf st_as_sf st_crs st_transform

crop_careful_universal <- function(raster, vector, mask = FALSE, verbose = FALSE) {
  pack <- FALSE
  
  # Unpack if parallelized inputs (PackedSpatRaster)
  if (inherits(raster, "PackedSpatRaster")) {
    if (verbose) print("Unpacking raster...")
    raster <- terra::unwrap(raster)
    pack <- TRUE
  }
  if (inherits(vector, "PackedSpatVector")) {
    if (verbose) print("Unpacking vector...")
    vector <- sf::st_as_sf(terra::unwrap(vector))
  }
  
  # Handle SpatVector by converting to sf if necessary
  if (inherits(vector, "SpatVector")) {
    vector <- sf::st_as_sf(vector)
  }
  
  # Process for raster package objects (RasterLayer, RasterStack, RasterBrick)
  if (inherits(raster, c("RasterLayer", "RasterStack", "RasterBrick"))) {
    
    # Check if CRS is different and reproject vector if needed
    if (!raster::crs(vector) == raster::crs(raster)) {
      if (verbose) print("Reprojecting vector to raster CRS...")
      vector <- sf::st_transform(vector, raster::crs(raster))
    } else {
      if (verbose) print("Vector already in raster CRS")
    }
    
    # Perform crop and optional masking
    if (verbose) print("Clipping raster using vector...")
    r <- raster::crop(raster, vector)
    if (mask) {
      if (verbose) print("Applying mask to raster...")
      r <- raster::mask(r, vector)
    }
    return(r)
    
  } else {  # Process for terra package objects
    
    # Check if CRS is different and reproject vector if needed
    if (!terra::same.crs(vector, raster)) {
      if (verbose) print("Reprojecting vector to raster CRS...")
      vector <- sf::st_transform(vector, terra::crs(raster))
    } else {
      if (verbose) print("Vector already in raster CRS")
    }
    
    # Perform crop and optional masking
    if (verbose) print("Clipping raster using vector...")
    r <- terra::crop(raster, vector, mask = mask)
    
    # Repack if the input was packed
    if (pack) {
      if (verbose) print("Repacking raster...")
      r <- terra::wrap(r)
    }
    return(r)
  }
}


#' Merge a List of Raster Files and Optionally Write to Disk
#'
#' Merges a list of raster files into a single raster object. The merged raster can either
#' be saved to a specified file path or returned as an in-memory object.
#'
#' @param file_list Character vector. A list of file paths to the raster files to be merged.
#' @param file_final_path Character. The file path where the merged raster will be saved if `write = TRUE`.
#' @param datatype Character. The data type of the output raster. Defaults to `"INT2U"`.
#' @param compress Logical. If `TRUE`, compresses the output file with DEFLATE compression when writing to disk. Defaults to `TRUE`.
#' @param write Logical. If `TRUE`, writes the merged raster to `file_final_path`. If `FALSE`, returns the merged raster in memory. Defaults to `TRUE`.
#'
#' @return If `write = TRUE`, returns invisible `NULL` after writing to disk. If `write = FALSE`, returns the merged raster object.
#'
#' @details This function reads a list of raster files, merges them, and either writes the merged raster to a specified path
#' or returns it in memory. Compression is available when writing to disk to reduce file size.
#'
#' @importFrom purrr map
#' @importFrom terra rast sprc merge writeRaster
#' @export
#'
#' @examples
#' \dontrun{
#' file_paths <- c("path/to/raster1.tif", "path/to/raster2.tif")
#' # To write to disk
#' merge_list_of_rasters(file_paths, "path/to/final_raster.tif", datatype = "FLT4S", compress = TRUE, write = TRUE)
#' # To return in memory
#' merged_raster <- merge_list_of_rasters(file_paths, write = FALSE)
#' }
merge_list_of_rasters <- function(file_list, file_final_path = NULL, datatype = "INT2U", compress = TRUE, write = TRUE) {
  # Validate inputs
  if (!is.character(file_list) || length(file_list) < 1) stop("`file_list` must be a non-empty character vector.")
  if (write && (is.null(file_final_path) || !is.character(file_final_path) || length(file_final_path) != 1)) {
    stop("When `write = TRUE`, `file_final_path` must be a single, non-null character string.")
  }
  if (!is.logical(compress) || length(compress) != 1) stop("`compress` must be a single logical value.")
  if (!is.logical(write) || length(write) != 1) stop("`write` must be a single logical value.")
  
  # Load and merge the rasters
  combined_rasters <- file_list |>
    purrr::map(~ terra::rast(.x)) |>
    terra::sprc() |>
    terra::merge()
  
  # Write or return the merged raster
  if (write) {
    if (compress) {
      terra::writeRaster(combined_rasters,
                         file_final_path,
                         datatype = datatype,
                         gdal = c("COMPRESS=DEFLATE"))
    } else {
      terra::writeRaster(combined_rasters,
                         file_final_path,
                         datatype = datatype)
    }
    invisible(NULL)  # Return NULL after writing to disk
  } else {
    return(combined_rasters)  # Return the merged raster in memory
  }
}

# Specialized functions ----



#' Calculate landscape metrics and attach raster layer names
#'
#' This function wraps `landscapemetrics::calculate_lsm()` and appends the corresponding
#' layer names from a multi-layer SpatRaster as a new column called `layer_name`.
#'
#' @param land A `SpatRaster` object (from the `terra` package), with one or more layers.
#' @param ... Additional arguments passed to `landscapemetrics::calculate_lsm()`.
#'
#' @return A `data.frame` of landscape metrics with an additional column `layer_name` indicating the raster band.
#'
#' @examples
#' \dontrun{
#' library(landscapemetrics)
#' land <- terra::rast(system.file("ex/logo.tif", package = "terra"))
#' result <- calculate_lsm_with_names(land, level = "patch", what = "lsm_p_area")
#' }
#'
#' @export
calculate_lsm_with_names <- function(land, ...) {
  if (!inherits(land, "SpatRaster")) {
    stop("`land` must be a SpatRaster object.")
  }
  
  out <- land |>
    landscapemetrics::calculate_lsm(...) |>
    dplyr::mutate(layer_name = names(land)[layer])
  
  return(out)
}



#' Extract Topographic Features for an sf Object
#'
#' This function extracts topographic features, including elevation, slope, and aspect, 
#' for a given `sf` object. The function allows extracting values at the centroid of each 
#' feature or aggregating values over the entire feature using `exactextractr`.
#'
#' @param sf_set An `sf` object representing spatial features.
#' @param centroid Logical. If `TRUE`, extracts topographic values at the centroid of 
#'   each feature; otherwise, values are aggregated over the entire feature using `exact_extract`. 
#'   Default is `TRUE`.
#' @param z Integer. The zoom level for the elevation raster retrieved using `elevatr::get_elev_raster()`. 
#'   Higher values provide higher resolution. Default is `12` (~30m resolution).
#' @param ... Additional arguments passed to `exact_extract()` when `centroid = FALSE`.
#'
#' @return An `sf` object with added columns:
#'   - `elevation`: Elevation values (meters).
#'   - `slope`: Slope values (degrees).
#'   - `aspect`: Aspect values (degrees, where 0° = North).
#'
#' @details
#' - When `centroid = TRUE`, the function computes the centroid of each feature and extracts 
#'   the elevation, slope, and aspect values at that point.
#' - When `centroid = FALSE`, `exact_extract()` is used to aggregate values over each feature.
#' - The function internally converts the elevation raster to a `terra` raster for slope 
#'   and aspect calculations.
#'
#' @importFrom sf st_bbox st_crs st_centroid
#' @importFrom elevatr get_elev_raster
#' @importFrom terra rast terrain extract
#' @importFrom exactextractr exact_extract
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(elevatr)
#' library(terra)
#' library(exactextractr)
#' library(dplyr)
#' 
#' # Example sf object (polygon)
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' 
#' # Extract topographic features at centroids
#' nc_topo <- extract_topo(nc, centroid = TRUE)
#' 
#' # Extract topographic features using exact extraction
#' nc_topo2 <- extract_topo(nc, centroid = FALSE, fun = mean)
#' }
#'
#' @export
extract_topo <- function(sf_set,
                         centroid = TRUE,
                         z = 12, #~30m
                         ...) {
  
  # Get bounding box and fetch elevation raster
  bbox <- sf::st_bbox(sf_set)
  elevation_raster <- elevatr::get_elev_raster(locations = bbox,
                                               z = z,
                                               prj = sf::st_crs(sf_set)) #~30m res
  
  # Convert to terra raster for processing
  elevation_rast <- terra::rast(elevation_raster)
  
  # Calculate slope and aspect
  slope <- terra::terrain(elevation_rast, v = "slope", unit = "degrees")
  aspect <- terra::terrain(elevation_rast, v = "aspect", unit = "degrees")
  
  if(centroid) {
    
    # Compute centroids and extract values
    centroids <- sf::st_centroid(sf_set)
    extracted_vals <- cbind(
      terra::extract(elevation_rast, centroids, ID = FALSE),
      terra::extract(slope, centroids, ID = FALSE),
      terra::extract(aspect, centroids, ID = FALSE)
    )
    
    # Add extracted values to sf object
    sf_set_plus <- sf_set %>%
      mutate(elevation = extracted_vals[,1],
             slope = extracted_vals[,2],
             aspect = extracted_vals[,3])
    
  } else {
    
    # Extract values from polygons using exactextractr
    sf_set_plus <- sf_set |>
      dplyr::mutate(
        elevation = exactextractr::exact_extract(elevation_rast, sf_set, ...),
        slope = exactextractr::exact_extract(slope, sf_set, ...),
        aspect = exactextractr::exact_extract(aspect, sf_set, ...)
      )
    
  }
  
  return(sf_set_plus)
}




