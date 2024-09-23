# ACCESS DATA FUNCTIONS ----


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



#' Access EPA Level I Ecoregions Data via VSI
#'
#' This function retrieves the U.S. EPA Level I ecoregions shapefile from a remote server via VSI (Virtual Spatial Infrastructure).
#' The shapefile is stored in a ZIP file, and the function accesses it without downloading the file locally.
#'
#' @return A `sf` (simple features) object containing the EPA Level I ecoregions shapefile data.
#' 
#' @details
#' The function accesses the EPA Level I ecoregions shapefile directly from the EPA's data commons, utilizing the `/vsizip/` 
#' and `/vsicurl/` mechanisms to stream the shapefile from the zipped file. The file is accessed via a URL without the need to 
#' download it locally. This method allows efficient access to the shapefile data using the `sf` package.
#'
#' @source
#' U.S. EPA Ecoregions Data: \url{https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/}
#'
#' @references
#' U.S. EPA Ecoregions Information: \url{https://www.epa.gov/eco-research/ecoregions-north-america}
#'
#' @importFrom sf st_read
#' @export
#' @examples
#' # Example usage
#' epa_ecoregions <- access_data_epa_l1_ecoregions_vsi()
#'
access_data_epa_l1_ecoregions_vsi <- function() {
  epa_l1 <- paste0(
    "/vsizip/vsicurl/",
    "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l1.zip",
    "/NA_CEC_Eco_Level1.shp"
  ) |>
    sf::st_read()
  
  return(epa_l1)
}



#' Access EPA Level II Ecoregions Data via VSI
#'
#' This function retrieves the U.S. EPA Level II ecoregions shapefile from a remote server via VSI (Virtual Spatial Infrastructure).
#' The shapefile is stored in a ZIP file, and the function accesses it without downloading the file locally.
#'
#' @return A `sf` (simple features) object containing the EPA Level II ecoregions shapefile data.
#' 
#' @details
#' The function accesses the EPA Level II ecoregions shapefile directly from the EPA's data commons, utilizing the `/vsizip/` 
#' and `/vsicurl/` mechanisms to stream the shapefile from the zipped file. The file is accessed via a URL without the need to 
#' download it locally. This method allows efficient access to the shapefile data using the `sf` package.
#'
#' @source
#' U.S. EPA Ecoregions Data: \url{https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/}
#'
#' @references
#' U.S. EPA Ecoregions Information: \url{https://www.epa.gov/eco-research/ecoregions-north-america}
#'
#' @importFrom sf st_read
#' @export
#' @examples
#' # Example usage
#' epa_ecoregions <- access_data_epa_l2_ecoregions_vsi()
#'
access_data_epa_l2_ecoregions_vsi <- function() {
  epa_l2 <- paste0(
    "/vsizip/vsicurl/",
    "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l2.zip",
    "/NA_CEC_Eco_Level2.shp"
  ) |>
    sf::st_read()
  
  return(epa_l2)
}


#' Access EPA Level III Ecoregions Data via VSI
#'
#' This function retrieves the U.S. EPA Level III ecoregions shapefile from a remote server via VSI (Virtual Spatial Infrastructure).
#' The shapefile is stored in a ZIP file, and the function accesses it without downloading the file locally.
#'
#' @return A `sf` (simple features) object containing the EPA Level III ecoregions shapefile data.
#' 
#' @details
#' The function accesses the EPA Level III ecoregions shapefile directly from the EPA's data commons, utilizing the `/vsizip/` 
#' and `/vsicurl/` mechanisms to stream the shapefile from the zipped file. The file is accessed via a URL without the need to 
#' download it locally. This method allows efficient access to the shapefile data using the `sf` package.
#'
#' @source
#' U.S. EPA Ecoregions Data: \url{https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/}
#' 
#' @references
#' U.S. EPA Ecoregions Information: \url{https://www.epa.gov/eco-research/ecoregions-north-america}
#'
#' @importFrom sf st_read
#' @export
#' @examples
#' # Example usage
#' epa_ecoregions <- access_data_epa_l3_ecoregions_vsi()
#'
access_data_epa_l3_ecoregions_vsi <- function() {
  epa_l3 <- paste0(
    "/vsizip/vsicurl/",
    "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip",
    "/us_eco_l3.shp"
  ) |>
    sf::st_read()
  
  return(epa_l3)
}


#' Access EPA Level IV Ecoregions Data via VSI
#'
#' This function retrieves the U.S. EPA Level IV ecoregions shapefile from a remote server via VSI (Virtual Spatial Infrastructure).
#' The shapefile is stored in a ZIP file, and the function accesses it without downloading the file locally.
#'
#' @return A `sf` (simple features) object containing the EPA Level IV ecoregions shapefile data.
#' 
#' @details
#' The function accesses the EPA Level IV ecoregions shapefile directly from the EPA's data commons, utilizing the `/vsizip/` 
#' and `/vsicurl/` mechanisms to stream the shapefile from the zipped file. The file is accessed via a URL without the need to 
#' download it locally. This method allows efficient access to the shapefile data using the `sf` package.
#'
#' @source
#' U.S. EPA Ecoregions Data: \url{https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/}
#' 
#' @references
#' U.S. EPA Ecoregions Information: \url{https://www.epa.gov/eco-research/ecoregions-north-america}
#'
#' @importFrom sf st_read
#' @export
#' @examples
#' # Example usage
#' epa_ecoregions <- access_data_epa_l4_ecoregions_vsi()
#'
access_data_epa_l4_ecoregions_vsi <- function() {
  epa_l4 <- paste0(
    "/vsizip/vsicurl/",
    "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4.zip",
    "/us_eco_l4_no_st.shp"
  ) |>
    sf::st_read()
  
  return(epa_l4)
}




#' Access LCMS CONUS v2023.9 Data via VSI
#'
#' This function allows you to access LCMS (Land Cover Monitoring System) CONUS v2023.9 annual land cover data
#' for a specified year via the VSI (Virtual Spatial Infrastructure) system. The function retrieves the
#' raster file from a remote USDA data server.
#'
#' @param year Integer or character. The year for which LCMS data is being accessed (e.g., 2020).
#'
#' @return A `SpatRaster` object from the `terra` package containing the land cover data for the specified year.
#' 
#' @details
#' This function accesses the LCMS dataset stored remotely and retrieves the appropriate raster file for
#' the given year. The file is accessed via the VSI system using the `/vsizip/` and `/vsicurl/` protocols to
#' read the zipped GeoTIFF directly from the USDA website without downloading it locally.
#'
#' @source
#' USDA Forest Service LCMS Data: \url{https://data.fs.usda.gov/geodata/rastergateway/LCMS/}
#'
#' @importFrom terra rast
#' @export
#' @examples
#' # Example usage
#' lcms_raster <- access_lcms_conus_v20239_via_vsi(2020)
#' 
#' # Access data for the year 2019
#' lcms_raster_2019 <- access_data_lcms_conus_v20239_via_vsi("2019")
access_data_lcms_conus_v20239_vsi <- function(year) {
  
  # Allow both character and numeric values for 'year'
  if (is.numeric(year)) {
    year <- as.character(year)
  }
  
  # Ensure that the year is valid
  yearNum <- as.numeric(year)
  if (yearNum < 1985 || yearNum > 2023) {
    stop("'year' must be from 1985-2023")
  }
  
  #Access LCMS data via VSI from this data page: https://data.fs.usda.gov/geodata/rastergateway/LCMS/
  lcms_layer <- paste0(
    "/vsizip/vsicurl/",
    "https://data.fs.usda.gov/geodata/LCMS/LCMS_CONUS_v2023-9_Land_Cover_Annual_", year, ".zip",
    "/LCMS_CONUS_v2023-9_Land_Cover_", year, ".tif") |>
    terra::rast()
  
  names(lcms_layer) <- paste0("lcms_", year)
  
  return(lcms_layer)
  
}


#' Access LCMS CONUS v2023.9 Data via VSI
#'
#' This function allows you to access LCMS (Land Cover Monitoring System) CONUS v2023.9 annual land cover data
#' for a specified year range via the VSI (Virtual Spatial Infrastructure) system. The function retrieves the
#' raster files from a remote USDA data server.
#'
#' @param earliest_year Integer or character. The earliest year for which LCMS data is being accessed (e.g., 2020).
#' @param earliest_year Integer or character. The latest year for which LCMS data is being accessed (e.g., 2022). Must be greater than or equal to earliest_year
#'
#' @return A `SpatRaster` object from the `terra` package containing the land cover data for the specified year.
#' 
#' @details
#' This function accesses the LCMS dataset stored remotely and retrieves the appropriate raster file for
#' the given year. The file is accessed via the VSI system using the `/vsizip/` and `/vsicurl/` protocols to
#' read the zipped GeoTIFF directly from the USDA website without downloading it locally.
#'
#' @source
#' USDA Forest Service LCMS Data: \url{https://data.fs.usda.gov/geodata/rastergateway/LCMS/}
#'
#' @importFrom terra rast
#' @export
#' @examples
#' # Example usage
#' lcms_raster <- access_lcms_conus_v20239_via_vsi(2020)
#' 
#' # Access data for the year 2019
#' lcms_raster_2019 <- access_data_lcms_conus_v20239_via_vsi("2019")
access_data_lcms_conus_v20239_vsi_year_range <- function(earliest_year, latest_year) {
  # Validate year inputs
  if (!is.numeric(earliest_year) && !is.character(earliest_year)) {
    stop("'earliest_year' must be numeric or a character string representing a year.")
  }
  if (!is.numeric(latest_year) && !is.character(latest_year)) {
    stop("'latest_year' must be numeric or a character string representing a year.")
  }
  
  # Convert character strings to numeric years 
  if (is.character(earliest_year)) {
    earliest_year <- as.numeric(earliest_year)
  }
  if (is.character(latest_year)) {
    latest_year <- as.numeric(latest_year)
  }
  
  # Ensure earliest_year <= latest_year
  if (earliest_year > latest_year) {
    stop("'earliest_year' must be less than or equal to 'latest_year'.")
  }
  
  # Ensure that the years are valid
  if (earliest_year < 1985 || latest_year > 2021) {
    stop("The year range requested must be from 1985-2021")
  }
  
  years <- seq(earliest_year, latest_year)
  
  dats <- years |>
    purrr::map(~ access_data_lcms_conus_v20239_vsi(.x)) |>
    terra::rast()
  
  return(dats)
  
}




#' Access LCMAP v13 Data via STAC
#'
#' This function retrieves LCMAP (Land Change Monitoring, Assessment, and Projection) data for a specified year
#' and area of interest (AOI) using the STAC API. It downloads the LCMAP raster data and returns it as a `SpatRaster` object.
#'
#' @param year Integer or character representing the year of interest.
#' @param aoi An `sf` object representing the area of interest (AOI). Must have a valid CRS, which will be transformed to EPSG:4326.
#'
#' @return A `SpatRaster` object representing the LCMAP data for the specified years and AOI with each year in a layer named "lcmap_[YEAR]"
#' 
#' @details
#' The function queries the Microsoft Planetary Computer's STAC API for the USGS LCMAP dataset. It constructs the correct 
#' vsicurl URL to stream the data and processes it using GDAL's `warp` function. The returned raster is cropped to the 
#' specified AOI.
#' 
#' @source
#' STAC API: \url{https://planetarycomputer.microsoft.com/api/stac/v1}
#' 
#' @references
#' Code adapted from: \url{https://stacspec.org/en/tutorials/1-download-data-using-r/}
#'
#' @importFrom rstac stac stac_search get_request assets_url
#' @importFrom sf st_transform st_bbox st_crs gdal_utils
#' @importFrom terra rast
#' @export
#' @examples
#' \dontrun{
#' aoi <- sf::st_as_sfc(sf::st_bbox(c(xmin = -120, xmax = -119, ymin = 34, ymax = 35), crs = 4326))
#' lcmap_raster <- access_data_lcmap_v13_stac_single_year(2000, aoi)
#' }
access_data_lcmap_v13_stac_single_year <- function(year, aoi) {
  
  # Convert numeric year to character string
  if (is.numeric(year)) {
    year <- as.character(year)
  }
  
  # Ensure that the year is valid
  yearNum <- as.numeric(year)
  if (yearNum < 1985 || yearNum > 2021) {
    stop("'year' must be from 1985-2021")
  }
  
  # Ensure the AOI is a valid sf object and transform to EPSG:4326
  if (!inherits(aoi, "sf")) {
    stop("'aoi' must be a valid 'sf' object.")
  }
  aoi <- sf::st_transform(aoi, "EPSG:4326")
  bbox4326 <- sf::st_bbox(aoi)
  
  # Perform STAC query
  stac_query <- tryCatch({
    rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1") |>
      rstac::stac_search(
        collections = "usgs-lcmap-conus-v13",
        bbox = bbox4326,
        datetime = paste0(year, "-01-01/", year, "-12-31")
      ) |>
      rstac::get_request()
  }, error = function(e) {
    stop("Failed to query the STAC API: ", e$message)
  })
  
  # Ensure query returned results
  if (length(stac_query$features) == 0) {
    stop("No data found for the given date range and AOI.")
  }
  
  
  # Helper function to create a vsicurl URL
  make_lcmap_vsicurl_url <- function(base_url) {
    paste0(
      "/vsicurl", 
      "?pc_url_signing=yes",
      "&pc_collection=usgs-lcmap-conus-v13",
      "&url=",
      base_url
    )
  }
  
  # Extract the LCMAP primary raster URL (lcpri)
  lcpri_url <- tryCatch({
    make_lcmap_vsicurl_url(rstac::assets_url(stac_query, "lcpri"))
  }, error = function(e) {
    stop("Failed to retrieve 'lcpri' asset URL: ", e$message)
  })
  
  
  # Prepare output file
  out_file <- tempfile(fileext = ".tif")
  
  # Use GDAL to download and process the raster data
  tryCatch({
    sf::gdal_utils(
      "warp",
      source = lcpri_url,
      destination = out_file,
      options = c(
        "-t_srs", sf::st_crs(aoi)$wkt,
        "-te", sf::st_bbox(aoi)
      )
    )
  }, error = function(e) {
    stop("GDAL warp process failed: ", e$message)
  })
  
  # Load the processed raster and return
  tryCatch({
    raster_output <- terra::rast(out_file)
  }, error = function(e) {
    stop("Failed to create raster from the downloaded file: ", e$message)
  })
  
  # Set layer names in the format "lcmap_year"
  names(raster_output) <- paste0("lcmap_", year)
  
  return(raster_output)
}


#' Access LCMAP v13 Data via STAC
#'
#' This function retrieves LCMAP (Land Change Monitoring, Assessment, and Projection) data for a specified range of years
#' and area of interest (AOI) using the STAC API. It downloads the LCMAP raster data and returns it as a `SpatRaster` object.
#'
#' @param earliest_year Integer or character representing the earliest year of interest.
#' @param latest_year Integer or character representing the latest year of interest. Must be equal to or greater than the earliest year.
#' @param aoi An `sf` object representing the area of interest (AOI). Must have a valid CRS, which will be transformed to EPSG:4326.
#'
#' @return A `SpatRaster` object representing the LCMAP data for the specified years and AOI, with each year in a layer named "lcmap_[YEAR]"
#' 
#' @details
#' The function queries the Microsoft Planetary Computer's STAC API for the USGS LCMAP dataset. It constructs the correct 
#' vsicurl URL to stream the data and processes it using GDAL's `warp` function. The returned raster is cropped to the 
#' specified AOI.
#' 
#' @source
#' STAC API: \url{https://planetarycomputer.microsoft.com/api/stac/v1}
#' 
#' @references
#' Code adapted from: \url{https://stacspec.org/en/tutorials/1-download-data-using-r/}
#'
#' @importFrom terra rast
#' @importFrom purrr map
#' @export
#' @examples
#' \dontrun{
#' aoi <- sf::st_as_sfc(sf::st_bbox(c(xmin = -120, xmax = -119, ymin = 34, ymax = 35), crs = 4326))
#' lcmap_raster <- access_data_lcmap_v13_stac_year_range(2000, 2003, aoi)
#' }
access_data_lcmap_v13_stac_year_range <- function(earliest_year, latest_year, aoi) {
  # Validate year inputs
  if (!is.numeric(earliest_year) && !is.character(earliest_year)) {
    stop("'earliest_year' must be numeric or a character string representing a year.")
  }
  if (!is.numeric(latest_year) && !is.character(latest_year)) {
    stop("'latest_year' must be numeric or a character string representing a year.")
  }
  
  # Convert character strings to numeric years 
  if (is.character(earliest_year)) {
    earliest_year <- as.numeric(earliest_year)
  }
  if (is.character(latest_year)) {
    latest_year <- as.numeric(latest_year)
  }
  
  # Ensure earliest_year <= latest_year
  if (earliest_year > latest_year) {
    stop("'earliest_year' must be less than or equal to 'latest_year'.")
  }
  
  # Ensure that the years are valid
  if (earliest_year < 1985 || latest_year > 2021) {
    stop("The year range requested must be from 1985-2021")
  }
  
  # Ensure the AOI is a valid sf object
  if (!inherits(aoi, "sf")) {
    stop("'aoi' must be a valid 'sf' object.")
  }
  
  years <- seq(earliest_year, latest_year)
  
  
  dats <- years |>
    purrr::map(~ access_data_lcmap_v13_stac_single_year(.x, aoi)) |>
    terra::rast()
  
  return(dats)
  
}



# DATA MANAGEMENT FUNCTIONS ---- 

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

