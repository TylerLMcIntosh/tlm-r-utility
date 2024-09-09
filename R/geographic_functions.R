#' Write Shapefile to a New Directory and Create a Zipped Version
#'
#' This function writes an `sf` object to a shapefile in a new, file-specific directory and optionally creates a zipped version of the shapefile. It also allows for removal of the original unzipped files and handles overwriting existing files.
#'
#' @param shp An `sf` object to write as a shapefile.
#' @param location A character string specifying the path of the directory to create the new file-specific subdirectory in.
#' @param filename A character string specifying the name of the file without the `.shp` extension.
#' @param zipOnly A logical value indicating whether the original (unzipped) files should be removed after zipping. Defaults to `FALSE`.
#' @param overwrite A logical value indicating whether existing files should be overwritten. Defaults to `FALSE`.
#' @return No return value. The function writes a shapefile to a specified directory, optionally zips the files, and manages file cleanup based on user input.
#' @examples
#' \dontrun{
#' # Example usage
#' st_write_shp(shp = prepped_for_parks_etal,
#'              location = here::here("data/derived"),
#'              filename = "career_lba_for_parks_v1",
#'              zipOnly = TRUE,
#'              overwrite = TRUE)
#' }
#'
#' @export
st_write_shp <- function(shp, location, filename, zipOnly, overwrite) {
  
  # Check for required packages and install if not installed, then load
  list.of.packages <- c("zip", "sf", "here")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if (length(new.packages)) install.packages(new.packages)
  
  # Create subdirectory & manage overwriting
  zipOnlyFile <- here::here(location, glue::glue("{filename}.zip"))
  outDir <- here::here(location, filename)
  
  if (!zipOnly & dir.exists(outDir) & overwrite) {
    unlink(outDir, recursive = TRUE)
  } else if (!zipOnly & dir.exists(outDir) & !overwrite) {
    stop("Directory already exists")
  }
  
  if (zipOnly & file.exists(zipOnlyFile) & overwrite) {
    unlink(zipOnlyFile)
  } else if (zipOnly & file.exists(zipOnlyFile) & !overwrite) {
    stop("Zip file already exists")
  }
  
  if (!dir.exists(outDir)){
    dir.create(outDir)
  }
  
  # Write file
  sf::st_write(shp,
               here::here(outDir, paste(filename, ".shp", sep = "")),
               append = FALSE) # overwrite
  
  # Get all shapefile components
  allShpNms <- list.files(here::here(outDir),
                          pattern = paste(filename, "*", sep = ""),
                          full.names = TRUE)
  
  # Zip together
  zipfile <- here::here(outDir, paste(filename, ".zip", sep=""))
  zip::zip(zipfile = zipfile,
           files = allShpNms,
           mode = "cherry-pick")
  
  # Remove raw files if desired
  if (zipOnly == TRUE) {
    file.copy(zipfile, zipOnlyFile)
    unlink(here::here(outDir), recursive = TRUE)          
  }
}
