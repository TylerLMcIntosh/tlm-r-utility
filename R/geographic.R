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

