
#' Fetch Data from an ArcGIS REST API Endpoint with Pagination
#'
#' This function retrieves geojson data from an ArcGIS REST API endpoint using pagination. It supports fetching a specified
#' number of entries or all available entries from the API endpoint. Written with ChatGPT 4o assistance.
#'
#' @param base_url A character string. The base URL of the ArcGIS REST API endpoint.
#' @param query_params A list. Query parameters to be used in the API request. The list should contain the necessary
#' parameters required by the API, such as `where`, `outFields`, and `f`.
#' @param max_record An integer. The maximum number of records that can be fetched in a single API request. This value is
#' usually defined by the ArcGIS REST API server limitations.
#' @param n An integer or character. Specifies the total number of entries to fetch. If `"all"`, the function fetches
#' all available records from the API. If an integer, it specifies the exact number of records to fetch.
#' @param timeout An integer. The time in seconds to wait before timing out the request.
#'
#' @return An `sf` object. A Simple Features (sf) object containing the fetched data.
#' @import httr sf
#' @examples
#' \dontrun{
#' base_url <- "https://example.com/arcgis/rest/services/your_service/FeatureServer/0/query"
#' query_params <- list(where = "1=1", outFields = "*", f = "geojson")
#' max_record <- 100
#' n <- 500  # Can also be "all"
#' result <- get.x.from.arcgis.rest.api(base_url, query_params, max_record, n)
#' print(result)
#' }
#' @importFrom httr GET status_code content timeout
#' @importFrom sf st_read
#' @export
access_data_get_x_from_arcgis_rest_api_geojson <- function(base_url, query_params, max_record, n, timeout) {
  # Input validation
  if (!is.character(base_url) || length(base_url) != 1) {
    stop("Parameter 'base_url' must be a single character string.")
  }
  if (!is.list(query_params)) {
    stop("Parameter 'query_params' must be a list.")
  }
  if (!is.numeric(max_record) || length(max_record) != 1 || max_record <= 0) {
    stop("Parameter 'max_record' must be a positive integer.")
  }
  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    stop("Parameter 'timeout' must be a positive integer.")
  }
  
  
  # Initialize variables
  total_features <- list()
  offset <- 0
  total_fetched <- 0  # Keep track of the total number of records fetched
  
  # Determine the limit for fetching records
  fetch_all <- FALSE
  if (n == "all") {
    fetch_all <- TRUE
  } else if (!is.numeric(n) || n <= 0) {
    stop("Parameter 'n' must be a positive integer or 'all'.")
  }
  
  repeat {
    # Update the resultOffset parameter in query_params
    query_params$resultOffset <- offset
    
    # Make the GET request using the base URL and query parameters
    response <- httr::GET(url = base_url, query = query_params, httr::timeout(timeout))
    
    # Check if the request was successful
    if (httr::status_code(response) == 200) {
      # Read the GeoJSON data directly into an sf object
      data <- sf::st_read(httr::content(response, as = "text"), quiet = TRUE)
      
      # Append the data to the list of features
      total_features <- append(total_features, list(data))
      
      # Update the total number of fetched records
      total_fetched <- total_fetched + nrow(data)
      
      # Provide user feedback for long-running processes
      cat(sprintf("Fetched %d records so far...\n", total_fetched))
      
      # Determine if we should stop fetching
      if ((nrow(data) < max_record) || (!fetch_all && total_fetched >= n)) {
        break
      }
      
      # Increment the offset by the maximum number of records for the next page
      offset <- offset + max_record
    } else {
      # Handle errors and provide meaningful messages
      error_message <- httr::content(response, "text", encoding = "UTF-8")
      stop("Failed to fetch data: ", httr::status_code(response), " - ", error_message)
    }
  }
  
  # Combine all pages into one sf object
  all_data_sf <- do.call(rbind, total_features)
  
  # If n is not "all", limit the output to the first n records
  if (!fetch_all) {
    all_data_sf <- all_data_sf[1:min(n, nrow(all_data_sf)), ]
  }
  
  return(all_data_sf)
}




#' Access MTBS CONUS Polygons
#'
#' This function accesses the MTBS (Monitoring Trends in Burn Severity) CONUS (Continental United States) polygons by downloading and reading the MTBS perimeter shapefile directly from the USGS website. The shapefile is accessed via a URL and read into an `sf` object.
#'
#' @return An `sf` object containing the MTBS CONUS polygons.
#' @examples
#' \dontrun{
#' mtbs_data <- access_data_mtbs_conus()
#' print(mtbs_data)
#' }
#' 
#' @importFrom sf st_read 
#' @export
access_data_mtbs_conus <- function() {
  mtbs <- paste0(
    "/vsizip/vsicurl/",
    "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip",
    "/mtbs_perims_DD.shp"
  ) |>
    sf::st_read()
  
  return(mtbs)
}


#' Access Data from the Welty-Jeffries GIS Server
#'
#' This function retrieves geospatial data from the Welty-Jeffries GIS server using the specified bounding box and EPSG code. It constructs a URL query and sends a request to the server to fetch data in GeoJSON format.
#'
#' @param bbox_str A character string representing the bounding box in the format required by the GIS server. The format is usually "xmin,ymin,xmax,ymax".
#' @param epsg_n An integer specifying the EPSG code for the spatial reference system to be used for the input and output data.
#' @param where_param A character string representing the SQL WHERE clause to filter the data. Default is "1=1", which retrieves all records.
#' @param timeout An integer specifying the timeout (in seconds) for the server request. Default is 600 seconds.
#' @return A `sf` (simple features) object containing the geospatial data retrieved from the Welty-Jeffries GIS server.
#' @examples
#' \dontrun{
#' # Example usage:
#' bbox <- "-109,36,-102,41"
#' epsg <- 4326
#' data <- access_data_welty_jeffries(bbox_str = bbox, epsg_n = epsg)
#' }
#' 
#' @export
access_data_welty_jeffries <- function(bbox_str, epsg_n, where_param = "1=1", timeout = 600) {
  # Write out the URL query
  base_url <- "https://gis.usgs.gov/sciencebase3/rest/services/Catalog/61aa537dd34eb622f699df81/MapServer/0/query"
  query_params <- list(f = "geojson",
                       where = where_param,
                       outFields = "*",
                       returnGeometry = "true",
                       geometryType = "esriGeometryEnvelope",
                       geometry = bbox_str,
                       spatialRel = "esriSpatialRelIntersects",
                       inSR = epsg_n,
                       outSR = epsg_n
  )
  
  # Request data
  welty <- tlmr::access_data_get_x_from_arcgis_rest_api_geojson(
    base_url = base_url, 
    query_params = query_params, 
    max_record = 1000, 
    n = "all", 
    timeout = timeout
  )
  
  return(welty)
}

#' Read CSV from Google Drive Path
#'
#' This function reads a CSV file directly from a specified Google Drive path using the `googledrive` package. It first retrieves the file using the provided path and then reads the content into a data frame.
#'
#' @param path A character string specifying the Google Drive path to the CSV file. The path can be a file ID, URL, or a full path to the file.
#' @return A data frame containing the contents of the CSV file.
#' @details The function uses the `googledrive` package to access Google Drive files. Ensure that you have authenticated with Google Drive using `googledrive::drive_auth()` before using this function.
#' @examples
#' \dontrun{
#' # Example usage:
#' csv_data <- access_data_read_csv_from_gdrive("your-file-id-or-url")
#' head(csv_data)
#' }
#' @importFrom googledrive drive_get drive_read_string
#' @export
read_csv_from_gdrive <- function(path) {
  # Retrieve the file metadata from Google Drive
  f <- googledrive::drive_get(path)
  
  # Read the content of the file as a string and convert it to a data frame
  csv <- f |>
    googledrive::drive_read_string() %>%
    read.csv(text = .)
  
  return(csv)
}

#' Download a file from Google Drive to a local directory
#'
#' This function downloads a file from a Google Drive path to a specified local path.
#'
#' @param gDrivePath A character string. The path or name of the file on Google Drive.
#' @param localPath A character string. The local path where the file will be saved.
#' @param overwrite A logical value indicating whether to overwrite the file if it already exists at the local path. Defaults to `TRUE`.
#'
#' @details This function retrieves a file's ID from Google Drive using the provided `gDrivePath` and downloads it to the local directory specified by `localPath`. The file will be overwritten if `overwrite` is set to `TRUE` (default).
#' 
#' @return The downloaded file will be saved to the specified `localPath`.
#' 
#' @note You must be authenticated with Google Drive via the `googledrive` package for this function to work.
#' 
#' @importFrom googledrive drive_get drive_download as_id
#' 
#' @examples
#' \dontrun{
#' # Example usage:
#' download_data_from_gdrive("path/to/file/on/drive", "path/to/local/file.csv")
#' }
#' 
#' @export
download_data_from_gdrive <- function(gDrivePath, localPath) {
  # Validate inputs
  if (missing(gDrivePath) || missing(localPath)) {
    stop("Both 'gDrivePath' and 'localPath' must be provided.")
  }
  if (!is.character(gDrivePath) || !nzchar(gDrivePath)) {
    stop("'gDrivePath' must be a non-empty string.")
  }
  if (!is.character(localPath) || !nzchar(localPath)) {
    stop("'localPath' must be a non-empty string.")
  }
  
  # Retrieve file ID from GDrive
  f <- googledrive::drive_get(gDrivePath)
  id <- f$id
  nm <- f$name
  
  googledrive::drive_download(googledrive::as_id(id), path = localPath, overwrite = TRUE)
}



