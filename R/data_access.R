# ACCESS DATA FUNCTIONS ----

# Data management ---- 

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

#' Get an ArcGIS Online Token
#'
#' This function generates a token for accessing ArcGIS Online resources. 
#' It prompts the user for their ArcGIS username and password and uses these 
#' credentials to obtain a token via the ArcGIS REST API.
#'
#' @return A character string containing the ArcGIS Online token.
#'
#' @details 
#' The function uses the ArcGIS REST API endpoint 
#' (\url{https://www.arcgis.com/sharing/rest/generateToken}) to authenticate the 
#' user and generate a token. The token can be used for subsequent API requests 
#' to access ArcGIS Online resources. The function makes use of the `httr` 
#' package for the HTTP POST request and the `askpass` package to securely 
#' request the user's password.
#'
#' @importFrom httr POST content
#' @importFrom askpass askpass
#'
#' @examples
#' \dontrun{
#' # Generate an ArcGIS Online token
#' token <- get_arcgis_online_token()
#' print(token)
#' }
#'
#' @export
get_arcgis_online_token <- function() {
  username <- readline(prompt = "Enter your ArcGIS username: ")
  password <- askpass::askpass(prompt = "Enter your ArcGIS password: ")
  
  response <- httr::POST(
    url = "https://www.arcgis.com/sharing/rest/generateToken",
    body = list(
      username = username,
      password = password,
      referer = "https://www.arcgis.com",
      f = "json"
    )
  )
  
  token <- httr::content(response)$token
  return(token)
}


#' Fetch Data from an ArcGIS REST API Endpoint with Pagination
#'
#' This function retrieves GeoJSON data from an ArcGIS REST API endpoint using pagination.
#' It checks for valid content and stops if the server returns an error page.
#'
#' @param base_url A character string. The base URL of the ArcGIS REST API endpoint.
#' @param query_params A list. Query parameters required by the API, such as `where`, `outFields`, and `f`.
#' @param max_record An integer. Max records to fetch in a single request.
#' @param n An integer or "all". Total records to fetch.
#' @param timeout An integer. Request timeout in seconds.
#'
#' @return An `sf` object of combined fetched features.
#' @export
access_data_get_x_from_arcgis_rest_api_geojson <- function(base_url, query_params, max_record, n, timeout) {
  if (!is.character(base_url) || length(base_url) != 1) stop("Parameter 'base_url' must be a single character string.")
  if (!is.list(query_params)) stop("Parameter 'query_params' must be a list.")
  if (!is.numeric(max_record) || max_record <= 0) stop("Parameter 'max_record' must be a positive integer.")
  if (!is.numeric(timeout) || timeout <= 0) stop("Parameter 'timeout' must be a positive integer.")
  
  total_features <- list()
  offset <- 0
  total_fetched <- 0
  fetch_all <- identical(n, "all")
  
  if (!fetch_all && (!is.numeric(n) || n <= 0)) {
    stop("Parameter 'n' must be a positive integer or 'all'.")
  }
  
  repeat {
    query_params$resultOffset <- offset
    query_params$resultRecordCount <- max_record
    
    response <- httr::GET(url = base_url, query = query_params, httr::timeout(timeout))
    
    # Check for valid content type
    resp_type <- httr::headers(response)[["content-type"]]
    if (!grepl("geo\\+json|application/json", resp_type)) {
      error_message <- httr::content(response, "text", encoding = "UTF-8")
      stop("Received non-GeoJSON content. Likely an error page:\n", substr(error_message, 1, 500))
    }
    
    # Attempt to read GeoJSON as sf
    data <- tryCatch({
      sf::st_read(httr::content(response, as = "text", encoding = "UTF-8"), quiet = TRUE)
    }, error = function(e) {
      stop("Failed to parse GeoJSON at offset ", offset, ": ", e$message)
    })
    
    # Append and track
    total_features <- append(total_features, list(data))
    fetched_now <- nrow(data)
    total_fetched <- total_fetched + fetched_now
    cat(sprintf("Fetched %d records so far...\n", total_fetched))
    
    # Stop if fewer than max or we hit the user-defined limit
    if (fetched_now < max_record || (!fetch_all && total_fetched >= n)) break
    
    offset <- offset + max_record
  }
  
  all_data_sf <- do.call(rbind, total_features)
  if (!fetch_all) {
    all_data_sf <- all_data_sf[1:min(n, nrow(all_data_sf)), ]
  }
  
  return(all_data_sf)
}



# Fire datasets ----

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

# Social data ----

#A function to access road data from OSM
# PARAMETERS
# aoi :: an area of interest as an sf object - roads will be accessed within the area plus a 1km buffer
# Adapt function as necessary for filtering
access_osm_roads <- function(aoi) {
  roadsData <- osmdata::opq(bbox = sf::st_bbox(sf::st_transform(sf::st_buffer(aoi, 2000), 4326))) |>
    osmdata::add_osm_feature(key = "highway",
                             key_exact = FALSE,
                             value_exact = FALSE,
                             match_case = FALSE) |>
    osmdata::osmdata_sf()
  desiredColumns <- c("USFS", "highway", "access", "maintained", "motor_vehicle", "service", "smoothness", "surface", "tracktype")
  roads <- roadsData$osm_lines |>
    dplyr::select(dplyr::any_of(desiredColumns)) |>
    dplyr::filter(highway != "path" | is.na(highway)) |> 
    dplyr::filter(tracktype != "grade5" | is.na(tracktype)) |>
    dplyr::filter(access != "private" | is.na(access)) |>
    dplyr::mutate(group = 1) |>
    group_by(group) |>
    summarise(geometry = st_union(geometry)) |>
    ungroup() |>
    sf::st_transform(epsg) |>
    sf::st_intersection(sf::st_buffer(aoi, 1000)) #clip to district of interest + 1km
  
  return(roads)
}


access_ynp_bear_management_areas <- function() {
  # Write out the URL query
  base_url <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/YELL_BEAR_MANAGEMENT_AREAS_public_viewview/FeatureServer/0/query"
  query_params <- list(f = "json",
                       where = "1=1",
                       outFields = "*",
                       returnGeometry = "true")
  
  # Request data
  bma <- access_data_get_x_from_arcgis_rest_api_geojson(
    base_url = base_url, 
    query_params = query_params, 
    max_record = 1000, 
    n = "all", 
    timeout = 600
  )
  
  return(bma)
  
}

# The ranger districts file is quite small, so it is accessed via VSI
access_usfs_ranger_districts <- function() {
  usfs_rds <- paste0(
    "/vsizip/vsicurl/", #magic remote connection
    "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.RangerDistrict.zip", #copied link to download location
    "/S_USA.RangerDistrict.shp") |> #path inside zip file
    sf::st_read()
  return(usfs_rds)
}


# A function to access the US federal surface management agency polygon dataset
# The function downloads and unzips a geodatabase rather than accessing via VSI since this layer is 
# useful for visualization and field planning, as well as it being accessed multiple times
access_us_sma <- function(dir_path, layer) {
  
  loc <- here::here(dir_path, "SMA_WM.gdb")
  if(file.exists(loc)) {
    sma <- sf::st_read(loc, layer = layer)
  } else {
    download_unzip_file(url = "https://blm-egis.maps.arcgis.com/sharing/rest/content/items/6bf2e737c59d4111be92420ee5ab0b46/data",
                        extract_to = dir_path,
                        keep_zip = FALSE)
    sma <-  sf::st_read(loc, layer = layer)
  }
  return(sma)
}

access_us_sma_helper_show_layers <- function(dir_path) {
  sf::st_layers(here::here(dir_path, "SMA_WM.gdb"))
}

access_us_wilderness <- function(dest_path = NA) {
  if(is.na(dest_path)) {
    wild <- paste0(
      "/vsizip/vsicurl/", #magic remote connection
      "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.Wilderness.zip", #copied link to download location
      "/S_USA.Wilderness.shp") |> #path inside zip file
      sf::st_read()
  } else {
    if(file.exists(dest_path)) {
      wild <- sf::st_read(dest_path)
    } else {
      wild <- paste0(
        "/vsizip/vsicurl/", #magic remote connection
        "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.Wilderness.zip", #copied link to download location
        "/S_USA.Wilderness.shp") |> #path inside zip file
        sf::st_read()
      sf::st_write(wild, dest_path)
    }
  }
  return(wild)
}


# Queries the PADUS REST service for WSAs
# PADUS interactive online: https://usgs.maps.arcgis.com/home/item.html?id=98fce3fb0c8241ce8847e9f7d0d212e9
access_us_wilderness_study_areas <- function(dest_path = NA) {
  if(is.na(dest_path)) {
    query_params <- list(where = "DesTp_Desc='Wilderness Study Area'",
                         outFields = "*",
                         f = "json")
    base_url = "https://services.arcgis.com/v01gqwM5QqNysAAi/ArcGIS/rest/services/PADUS_Protection_Status_by_GAP_Status_Code/FeatureServer/0/QUERY"
    wsa <- access_data_get_x_from_arcgis_rest_api_geojson(base_url = base_url,
                                                          query_params = query_params,
                                                          max_record = 2000,
                                                          n = "all",
                                                          timeout = 500)
  } else {
    if(file.exists(dest_path)) {
      wsa <- sf::st_read(dest_path)
    } else {
      query_params <- list(where = "DesTp_Desc='Wilderness Study Area'",
                           outFields = "*",
                           f = "json")
      base_url = "https://services.arcgis.com/v01gqwM5QqNysAAi/ArcGIS/rest/services/PADUS_Protection_Status_by_GAP_Status_Code/FeatureServer/0/QUERY"
      wsa <- access_data_get_x_from_arcgis_rest_api_geojson(base_url = base_url,
                                                            query_params = query_params,
                                                            max_record = 2000,
                                                            n = "all",
                                                            timeout = 500)
      sf::st_write(wsa, dest_path)
    }
  }
  return(wsa)
}


# EPA Ecoregions ----


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
    "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/us/us_eco_l3.zip",
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


# Landcover Data ----


access_treemap <- function() {
  treemap <- glue::glue(
    "/vsizip/vsicurl/", #magic remote connection 
    "https://s3-us-west-2.amazonaws.com/fs.usda.rds/RDS-2021-0074/RDS-2021-0074_Data.zip", #copied link to download location
    "/Data/TreeMap2016.tif") |> #path inside zip file
    terra::rast() 
  return(treemap)
}



#' Access LANDFIRE EVT Raster for CONUS (2023)
#'
#' This function remotely accesses and reads the 2023 LANDFIRE Existing Vegetation Type (EVT) raster data for the contiguous United States (CONUS). The data is accessed directly from a zipped online source using GDAL's VSI (Virtual File System) protocol.
#'
#' @details
#' The function utilizes GDAL's virtual file system (`/vsizip/vsicurl/`) to remotely access the LANDFIRE EVT raster file without needing to download or unzip it manually. The raster data is read into a `terra` raster object, suitable for geospatial analysis in R.
#'
#' @return
#' A `terra` raster object containing the 2023 LANDFIRE EVT data for CONUS.
#'
#' @examples
#' \dontrun{
#' lf_evt <- access_landfire_evt_conus_2023()
#' plot(lf_evt)
#' }
#' 
#' @importFrom terra rast
#' @export
access_landfire_evt_conus_2023 <- function() {
  lf_evt <- paste0(
    "/vsizip/vsicurl/", #magic remote connection
    "https://landfire.gov/data-downloads/US_240/LF2023_EVT_240_CONUS.zip", #copied link to download location
    "/LF2023_EVT_240_CONUS/Tif/LC23_EVT_240.tif") |> #path inside zip file
    terra::rast()
  return(lf_evt)
}

#' Access LANDFIRE EVT CSV for CONUS (2023)
#'
#' This function remotely accesses and reads the 2023 LANDFIRE Existing Vegetation Type (EVT) CSV data for the contiguous United States (CONUS). The data is accessed directly from a zipped online source using GDAL's VSI (Virtual File System) protocol.
#'
#' @details
#' This function uses GDAL's virtual file system (`/vsizip/vsicurl/`) to remotely access the LANDFIRE EVT CSV data without manual download or extraction. The CSV is read into an `sf` object using `sf::st_read()`, as GDAL's CSV handling is supported by spatial data functions. This method is necessary since standard R CSV readers do not natively support remote access via VSI.
#'
#' @return
#' An `sf` object containing the CSV data from the 2023 LANDFIRE EVT for CONUS.
#'
#' @examples
#' \dontrun{
#' lf_evt_csv <- access_landfire_evt_conus_2023_csv()
#' head(lf_evt_csv)
#' }
#' 
#' @importFrom sf st_read
#' @export
access_landfire_evt_conus_2023_csv <- function() {
  lf_evt_csv <- paste0(
    "/vsizip/vsicurl/", #magic remote connection
    "https://landfire.gov/data-downloads/US_240/LF2023_EVT_240_CONUS.zip", #copied link to download location
    "/LF2023_EVT_240_CONUS/CSV_Data/LF23_EVT_240.csv") |> #path inside zip file
    sf::st_read() #note that read_csv and other csv drivers in R don't talk to GDAL. Instead use st_read or terra::vect() to access CSV data in zip files
  return(lf_evt_csv)
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


#' Download individual FIA data files by state and table
#'
#' This function downloads selected Forest Inventory and Analysis (FIA) CSV datasets
#' for specified states and file suffixes (Oracle table names), placing them into
#' a subdirectory named `fia_individual_data_files` within a target directory.
#'
#' Consider increasing the download timeout with `options(timeout = 300)` for large requests.
#'
#' @param state_abbreviations A character vector of state abbreviations (e.g., `c("CO", "WY")`).
#' @param file_suffixes A character vector of FIA table names (e.g., `c("DWM_VISIT", "COUNTY")`).
#' @param directory A string indicating the directory in which to store downloaded files.
#'
#' @return A character vector of full file paths for the downloaded files.
#'
#' @examples
#' \dontrun{
#' files <- fia_download_individual_data_files(
#'   state_abbreviations = c("CO"),
#'   file_suffixes = c("DWM_VISIT", "COUNTY"),
#'   directory = "~/fia_data"
#' )
#' data_list <- lapply(files, readr::read_csv)
#' names(data_list) <- basename(files)
#' }
#'
#' @export
fia_download_individual_data_files <- function(state_abbreviations, file_suffixes, directory) {
  # Ensure the base directory exists
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  base_url <- "https://apps.fs.usda.gov/fia/datamart/CSV/"
  subdirectory_path <- file.path(directory, "fia_individual_data_files")
  
  if (!dir.exists(subdirectory_path)) {
    dir.create(subdirectory_path, recursive = TRUE)
  }
  
  downloaded_files <- character()
  
  for (state in state_abbreviations) {
    for (suffix in file_suffixes) {
      url_suffix <- gsub("_", " ", suffix)
      url_suffix <- gsub(" ", "_", toupper(url_suffix))  # Ensure uppercase with underscores
      
      file_url <- paste0(base_url, state, "_", url_suffix, ".csv")
      file_path <- file.path(subdirectory_path, paste0(state, "_", suffix, ".csv"))
      
      tryCatch({
        download.file(file_url, destfile = file_path, mode = "wb")
        downloaded_files <- c(downloaded_files, file_path)
        message("Downloaded: ", file_path)
      }, error = function(e) {
        message("Failed to download ", file_url, ": ", e$message)
      })
    }
  }
  
  return(downloaded_files)
}

#' Download FIA bulk data sets by type and state
#'
#' This function bulk-downloads FIA datasets grouped by functional categories (e.g., "down woody material", "tree level").
#' Files are downloaded into subdirectories within a main folder named `fia_bulk_data_files` inside the specified directory.
#'
#' Each returned list element corresponds to a bulk data type, containing a character vector of downloaded file paths.
#'
#' For large downloads, consider increasing the timeout: `options(timeout = 300)`
#'
#' @param state A character vector of state abbreviations (e.g., `c("CO", "WY")`).
#' @param directory A string indicating the root directory for all downloads.
#' @param bulk_data_types A character vector of FIA bulk data categories.
#'        Supported types include: "location level", "tree level", "invasives and understory vegetation",
#'        "down woody material", "tree regeneration", "ground cover", "soils", "population", "plot", "reference".
#'
#' @return A named list of character vectors. Each element contains file paths downloaded for one bulk data type.
#'
#' @examples
#' \dontrun{
#' downloaded <- fia_bulk_download_data_files(
#'   state = c("CO"),
#'   directory = "~/fia_data",
#'   bulk_data_types = c("down woody material", "plot")
#' )
#' read_list <- lapply(downloaded$`down woody material`, readr::read_csv)
#' names(read_list) <- basename(downloaded$`down woody material`)
#' }
#'
#' @export
fia_bulk_download_data_files <- function(state, directory, bulk_data_types) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  bulk_data_mappings <- list(
    "down woody material" = c(
      "DWM_VISIT", "DWM_COARSE_WOODY_DEBRIS", "DWM_DUFF_LITTER_FUEL",
      "DWM_FINE_WOODY_DEBRIS", "DWM_MICROPLOT_FUEL", "DWM_RESIDUAL_PILE",
      "DWM_TRANSECT_SEGMENT", "COND_DWM_CALC"
    ),
    "location level" = c("SURVEY", "PROJECT", "COUNTY", "PLOT", "COND", "SUBPLOT", "SUBP_COND", "SUBP_COND_CHNG_MTRX"),
    "tree level" = c("TREE", "WOODLAND_STEMS", "GRM_COMPONENT", "GRM_THRESHOLD", "GRM_MIDPT", "GRM_BEGIN", "GRM_ESTN", "BEGINEND", "SEEDLING", "SITETREE"),
    "invasives and understory vegetation" = c("INVASIVE_SUBPLOT_SPP", "P2VEG_SUBPLOT_SPP", "P2VEG_SUBP_STRUCTURE"),
    "tree regeneration" = c("PLOT_REGEN", "SUBPLOT_REGEN", "SEEDLING_REGEN"),
    "ground cover" = c("GRND_CVR", "GRND_LYR_FNCTL_GRP", "GRND_LYR_MICROQUAD"),
    "soils" = c("SUBP_SOIL_SAMPLE_LOC", "SUBP_SOIL_SAMPLE_LAYER"),
    "population" = c(
      "POP_ESTN_UNIT", "POP_EVAL", "POP_EVAL_ATTRIBUTE", "POP_EVAL_GRP", "POP_EVAL_TYP",
      "POP_PLOT_STRATUM_ASSGN", "POP_STRATUM"
    ),
    "plot" = c("PLOTGEOM", "PLOTSNAP"),
    "reference" = c(
      "REF_POP_ATTRIBUTE", "REF_POP_EVAL_TYP_DESCR", "REF_FOREST_TYPE", "REF_FOREST_TYPE_GROUP",
      "REF_SPECIES", "REF_PLANT_DICTIONARY", "REF_SPECIES_GROUP", "REF_INVASIVE_SPECIES",
      "REF_HABTYP_DESCRIPTION", "REF_HABTYP_PUBLICATION", "REF_CITATION", "REF_FIADB_VERSION",
      "REF_STATE_ELEV", "REF_UNIT", "REF_RESEARCH_STATION", "REF_NVCS_HIERARCHY_STRICT",
      "REF_NVCS_LEVEL_1_CODES", "REF_NVCS_LEVEL_2_CODES", "REF_NVCS_LEVEL_3_CODES",
      "REF_NVCS_LEVEL_4_CODES", "REF_NVCS_LEVEL_5_CODES", "REF_NVCS_LEVEL_6_CODES",
      "REF_NVCS_LEVEL_7_CODES", "REF_NVCS_LEVEL_8_CODES", "REF_AGENT", "REF_DAMAGE_AGENT",
      "REF_DAMAGE_AGENT_GROUP", "REF_FVS_VAR_NAME", "REF_FVS_LOC_NAME", "REF_OWNGRP_CD",
      "REF_DIFFERENCE_TEST_PER_ACRE", "REF_DIFFERENCE_TEST_TOTALS", "REF_EQUATION_TABLE",
      "REF_SEQN", "REF_GRM_TYPE", "REF_INTL_TO_DOYLE_FACTOR", "REF_TREE_CARBON_RATIO_DEAD",
      "REF_TREE_DECAY_PROP", "REF_TREE_STAND_DEAD_CR_PROP", "REF_GRND_LYR"
    )
  )
  
  all_downloaded_files <- setNames(vector("list", length(bulk_data_types)), bulk_data_types)
  main_bulk_dir <- file.path(directory, "fia_bulk_data_files")
  
  if (!dir.exists(main_bulk_dir)) {
    dir.create(main_bulk_dir, recursive = TRUE)
  }
  
  for (bulk_data_type in bulk_data_types) {
    if (!bulk_data_type %in% names(bulk_data_mappings)) {
      stop("Unknown bulk data type: ", bulk_data_type)
    }
    
    subdirectory <- gsub(" ", "_", bulk_data_type)
    subdirectory_path <- file.path(main_bulk_dir, subdirectory)
    
    if (!dir.exists(subdirectory_path)) {
      dir.create(subdirectory_path, recursive = TRUE)
    }
    
    file_suffixes <- bulk_data_mappings[[bulk_data_type]]
    
    downloaded_files <- fia_download_individual_data_files(
      state_abbreviations = state,
      file_suffixes = file_suffixes,
      directory = subdirectory_path
    )
    
    all_downloaded_files[[bulk_data_type]] <- downloaded_files
  }
  
  return(all_downloaded_files)
}



# NEON ----

#' Access AOP Flight Box Data
#'
#' Retrieves the flight box shapefile data for all NEON AOP sites by downloading and reading
#' it from the specified remote location.
#'
#' @return An sf object containing the flight box data for all NEON AOP sites.
#'
#' @importFrom sf st_read
#' 
#' @export
access_neon_aop_flight_box_data <- function() {
  aop_all <- paste0(
    "/vsizip/vsicurl/", # Magic remote connection
    "https://www.neonscience.org/sites/default/files/AOP_flightBoxes_0.zip", # Copied link to download location
    "/AOP_flightBoxes/AOP_flightboxesAllSites.shp") |> # Path inside zip file
    sf::st_read()
  return(aop_all)
}

#' Access NEON Plot Shapefiles
#'
#' This function remotely accesses and reads the NEON plot shapefiles directly from a zipped online source using GDAL's VSI (Virtual File System) protocol.
#' It downloads the shapefile for all NEON TOS (Tower Observation System) plots.
#'
#' @details
#' This function uses a magic remote connection through GDAL's virtual file system (`/vsizip/vsicurl/`) to access the shapefile directly from the zipped NEON data repository. The function does not require the user to download or unzip the file manually. The shapefile is read into an `sf` object for further spatial analysis in R.
#'
#' @return
#' An `sf` object containing the NEON TOS plot polygons.
#'
#' @examples
#' \dontrun{
#' neon_plots <- access_neon_plots_shp()
#' plot(neon_plots)
#' }
#' 
#' @importFrom sf st_read
#' @export
access_neon_plots_shp <- function() {
  neon_plots <- paste0(
    "/vsizip/vsicurl/", #magic remote connection
    "https://www.neonscience.org/sites/default/files/All_NEON_TOS_Plots_V10.zip", #copied link to download location
    "/All_NEON_TOS_Plots_V10/All_NEON_TOS_Plot_Polygons_V10.shp") |> #path inside zip file
    sf::st_read()
  return(neon_plots)
}

#' Access NEON Domain Shapefiles
#'
#' This function remotely accesses and reads the NEON domain shapefiles directly from a zipped online source using GDAL's VSI (Virtual File System) protocol.
#' It downloads the shapefile for NEON's defined geographic domains.
#'
#' @details
#' Similar to the `access_neon_plots_shp()` function, this function uses the GDAL virtual file system (`/vsizip/vsicurl/`) to access and read NEON domain shapefiles directly from a zipped source. The shapefile contains geographic domain boundaries for the NEON project, which is read into an `sf` object.
#'
#' @return
#' An `sf` object containing the NEON domain polygons.
#'
#' @examples
#' \dontrun{
#' neon_domains <- access_neon_domains_shp()
#' plot(neon_domains)
#' }
#' 
#' @importFrom sf st_read
#' @export
access_neon_domains_shp <- function() {
  neon_domains <- paste0(
    "/vsizip/vsicurl/", #magic remote connection
    "https://www.neonscience.org/sites/default/files/NEONDomains_0.zip", #copied link to download location
    "/NEON_Domains.shp") |> #path inside zip file
    sf::st_read()
  return(neon_domains)
}


