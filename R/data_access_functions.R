
#' Fetch Data from an ArcGIS REST API Endpoint with Pagination
#'
#' This function retrieves geojson data from an ArcGIS REST API endpoint using pagination. It supports fetching a specified
#' number of entries or all available entries from the API endpoint. Written with ChatGPT 4o assistance.
#'
#' @param baseUrl A character string. The base URL of the ArcGIS REST API endpoint.
#' @param queryParams A list. Query parameters to be used in the API request. The list should contain the necessary
#' parameters required by the API, such as `where`, `outFields`, and `f`.
#' @param maxRecord An integer. The maximum number of records that can be fetched in a single API request. This value is
#' usually defined by the ArcGIS REST API server limitations.
#' @param n An integer or character. Specifies the total number of entries to fetch. If `"all"`, the function fetches
#' all available records from the API. If an integer, it specifies the exact number of records to fetch.
#' @param timeout An integer. The time in seconds to wait before timing out the request.
#'
#' @return An `sf` object. A Simple Features (sf) object containing the fetched data.
#' @import httr sf
#' @examples
#' \dontrun{
#' baseUrl <- "https://example.com/arcgis/rest/services/your_service/FeatureServer/0/query"
#' queryParams <- list(where = "1=1", outFields = "*", f = "geojson")
#' maxRecord <- 100
#' n <- 500  # Can also be "all"
#' result <- get.x.from.arcgis.rest.api(baseUrl, queryParams, maxRecord, n)
#' print(result)
#' }
#' @export
access.data.get.x.from.arcgis.rest.api.geojson <- function(baseUrl, queryParams, maxRecord, n, timeout) {
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
    # Update the resultOffset parameter in queryParams
    queryParams$resultOffset <- offset
    
    # Make the GET request using the base URL and query parameters
    response <- httr::GET(url = baseUrl, query = queryParams, httr::timeout(timeout))
    
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
      if ((nrow(data) < maxRecord) || (!fetch_all && total_fetched >= n)) {
        break
      }
      
      # Increment the offset by the maximum number of records for the next page
      offset <- offset + maxRecord
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


