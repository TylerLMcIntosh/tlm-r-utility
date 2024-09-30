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
  tlmr::dir_ensure(out_dir)
  
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
#' cropped_raster <- st_crop_careful_universal(raster_obj, vector_obj, mask = TRUE, verbose = TRUE)
#' }
#' @export
#' @importFrom terra unwrap wrap crop mask crs same.crs
#' @importFrom raster crs crop mask
#' @importFrom sf st_as_sf st_crs st_transform

st_crop_careful_universal <- function(raster, vector, mask = FALSE, verbose = FALSE) {
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

