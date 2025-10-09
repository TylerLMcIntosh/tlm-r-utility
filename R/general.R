
# Basic Utility ----

#' Generate a Timestamp
#'
#' This function generates a timestamp in various formats representing the current date and time along with the time zone.
#'
#' @param type Either 'human_read' or 'for_file'
#' @return A character string representing the current date and time in the selected
#' @examples
#' # Generate a timestamp
#' timestamp()
#'
#' @export
timestamp <- function(type = 'human_read'){
  if(type == 'human_read') {
    t <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")    
  }
  if(type == 'for_file') {
    t <- format(Sys.time(), "%Y%m%d%H%M%S")
  }

  
  return(t)
}



#' Ensure Directories Exist
#'
#' This function checks if one or more directories exist at the specified paths,
#' and creates any that do not exist.
#'
#' @param path A character string or a vector of strings specifying directory paths.
#' @return A character vector of all directory paths that were checked/created.
#' @examples
#' # Ensure a single directory
#' dir_ensure("data")
#'
#' # Ensure multiple directories
#' dir_ensure(c("data", "output", "logs"))
#'
#' @export
dir_ensure <- function(path) {
  if (!is.character(path)) {
    stop("`path` must be a character string or a vector of character strings.")
  }
  
  created_paths <- character()
  
  for (p in path) {
    if (!dir.exists(p)) {
      tryCatch({
        dir.create(p, recursive = TRUE)
        message("Directory created: ", p)
        created_paths <- c(created_paths, p)
      }, error = function(e) {
        warning("Failed to create directory: ", p, " — ", conditionMessage(e))
      })
    } else {
      message("Directory already exists: ", p)
    }
  }
  
  return(invisible(path))
}


#' Substring from the Right
#'
#' This function extracts a substring from the right side of a given string, retaining a specified number of characters.
#' It will work for either single strings or a vectorized input (e.g. when used on a data frame)
#'
#' @param str A character string from which to extract the substring.
#' @param n An integer specifying the number of characters to keep from the right of the string.
#' @return A character string containing the rightmost \code{n} characters of the input string.
#' @examples
#' # Extract the last 3 characters from a string
#' substr_right("Hello, World!", 3)
#'
#' @export
substr_right <- function(str, n) {
  # Check if input is a vector, and apply the function element-wise using sapply
  if (length(str) > 1) {
    return(sapply(str, function(x) {
      if (n > nchar(x)) {
        warning("n is greater than the length of the string. Returning the full string.")
        return(x)
      }
      return(substr(x, nchar(x) - n + 1, nchar(x)))
    }))
  }
  
  # If input is a single string, apply the logic directly
  if (n > nchar(str)) {
    warning("n is greater than the length of the string. Returning the full string.")
    return(str)
  }
  
  return(substr(str, nchar(str) - n + 1, nchar(str)))
}

#' Convert R Color to Hexadecimal
#'
#' This function converts a standard R color name (e.g., 'red', 'steelblue') to its corresponding hexadecimal color code.
#'
#' @param color A character string specifying a standard R color name.
#' @return A character string representing the hexadecimal color code of the specified R color.
#' @examples
#' # Convert the color 'red' to its hexadecimal equivalent
#' col2hex("red")
#'
#' # Convert the color 'steelblue' to its hexadecimal equivalent
#' col2hex("steelblue")
#'
#' @export
col2hex <- function(color) {
  rgb_values <- col2rgb(color)
  hex_color <- rgb(rgb_values[1], rgb_values[2], rgb_values[3], maxColorValue=255)
  return(hex_color)
}


#' Convert Hexadecimal Color to RGB
#'
#' This function converts a 6-character hexadecimal color code into its
#' corresponding RGB values.
#'
#' @param hex A string representing a 6-character hex color code prefixed with `#`
#'   (e.g., `"#FF5733"`).
#'
#' @return A named numeric vector with RGB values (`R`, `G`, `B`).
#'
#' @examples
#' hex_to_rgb("#FF5733")
#' hex_to_rgb("#00AABB")
#'
#' @export
hex_to_rgb <- function(hex) {
  # Ensure the input is a valid hex code with a leading "#"
  if (!grepl("^#([A-Fa-f0-9]{6})$", hex)) {
    stop("Invalid hex color format. Use a 6-character hex code, e.g., '#FF5733'.")
  }
  
  # Extract RGB components
  r <- strtoi(substr(hex, 2, 3), base = 16)
  g <- strtoi(substr(hex, 4, 5), base = 16)
  b <- strtoi(substr(hex, 6, 7), base = 16)
  
  # Check for NA values
  if (any(is.na(c(r, g, b)))) stop("Failed to convert hex to RGB.")
  
  return(c(R = r, G = g, B = b))
}










#' Safely Extract a ZIP or TAR Archive
#'
#' Handles both .zip and .tar(.gz) files. Supports skipping if files/folders exist,
#' recursive extraction of nested archives, and optional cleanup.
#'
#' @param archive_path Character. Path to a .zip, .tar, or .tar.gz file.
#' @param extract_to Character. Directory for extraction. Defaults to archive's directory.
#' @param recursive Logical. Recursively extract nested archives? Defaults to FALSE.
#' @param keep_archive Logical. Keep original and nested archives after extraction? Defaults to TRUE.
#' @param full_contents_check Logical. If TRUE, skip extraction only if all files exist.
#' @param return_all_paths Logical. If TRUE, return all extracted file paths;
#'                          if FALSE, return all top-level files and directories.
#'
#' @return Character vector of extracted paths.
#' @export
safe_extract <- function(archive_path,
                         extract_to = dirname(archive_path),
                         recursive = FALSE,
                         keep_archive = TRUE,
                         full_contents_check = FALSE,
                         return_all_paths = FALSE) {
  # --- Validate inputs ---
  if (!file.exists(archive_path)) stop("Archive does not exist: ", archive_path)
  if (!dir.exists(extract_to)) dir.create(extract_to, recursive = TRUE)
  
  ext <- tolower(tools::file_ext(archive_path))
  is_zip <- ext == "zip"
  is_tar <- ext %in% c("tar", "gz", "tgz", "tar.gz")
  
  if (!is_zip && !is_tar) stop("Unsupported archive type: ", ext)
  
  # --- List archive contents ---
  contents <- if (is_zip) {
    utils::unzip(archive_path, list = TRUE)$Name
  } else {
    utils::untar(archive_path, list = TRUE)
  }
  
  # Determine top-level items
  top_level_items <- unique(sub("^([^/]+).*", "\\1", contents))
  top_level_paths <- file.path(extract_to, top_level_items)
  
  # --- Skip logic ---
  skip_extract <- if (full_contents_check) {
    all(file.exists(file.path(extract_to, contents)))
  } else {
    all(file.exists(top_level_paths))
  }
  
  if (!skip_extract) {
    tryCatch({
      if (is_zip) {
        unzip(archive_path, exdir = extract_to)
      } else {
        utils::untar(archive_path, exdir = extract_to)
      }
    }, error = function(e) stop("Extraction failed: ", e$message))
    
    # --- Recursive extraction ---
    if (recursive) {
      nested_archives <- list.files(extract_to, pattern = "\\.(zip|tar|gz|tgz)$", recursive = TRUE, full.names = TRUE)
      nested_archives <- setdiff(nested_archives, archive_path)
      for (na in nested_archives) {
        safe_extract(na, dirname(na), recursive = recursive, keep_archive = keep_archive,
                     full_contents_check = FALSE, return_all_paths = FALSE)
        if (!keep_archive) unlink(na)
      }
    }
    
    if (!keep_archive) unlink(archive_path)
  } else {
    message("Skipping extract: Targets already exist in ", extract_to)
  }
  
  # --- Return paths ---
  if (return_all_paths) {
    # Get full paths of extracted files
    extracted_paths <- file.path(extract_to, contents)
    extracted_files <- extracted_paths[file.exists(extracted_paths) & !file.info(extracted_paths)$isdir]
    return(invisible(normalizePath(extracted_files, winslash = "/", mustWork = FALSE)))
  } else {
    paths <- file.path(extract_to, top_level_items)
    return(invisible(normalizePath(paths[file.exists(paths)], winslash = "/", mustWork = FALSE)))
  }
}



#' Safely Download a File to a Directory
#'
#' Downloads a file from a URL to a specified directory, only if it doesn't already exist there.
#'
#' @param url Character. The URL to download from.
#' @param dest_dir Character. The directory where the file should be saved.
#' @param mode Character. Mode passed to `download.file()`. Default is "wb" (write binary).
#' @param timeout Integer. Optional timeout in seconds. Will be reset afterward.
#'
#' @return A character string with the full path to the downloaded file.
#'
#' @importFrom utils download.file
#' @export
#'
#' @examples
#' \dontrun{
#' path <- safe_download("https://example.com/data.zip", "data/")
#' }
safe_download <- function(url,
                          dest_dir,
                          mode = "wb",
                          timeout = NA) {
  # Validate input
  if (!is.character(url) || length(url) != 1) stop("`url` must be a single character string.")
  if (!is.character(dest_dir) || length(dest_dir) != 1) stop("`dest_dir` must be a single character string.")
  
  # Ensure destination directory exists
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  
  # Derive destination file path from URL and directory
  filename <- basename(url)
  destfile <- file.path(dest_dir, filename)
  
  # Skip download if file already exists
  if (file.exists(destfile)) {
    message("Skipping download: File already exists at ", destfile)
    return(normalizePath(destfile, winslash = "/", mustWork = FALSE))
  }
  
  # Handle optional timeout
  original_timeout <- getOption("timeout")
  if (!is.na(timeout) && timeout > original_timeout) {
    options(timeout = timeout)
    on.exit(options(timeout = original_timeout), add = TRUE)
  }
  
  # Attempt to download
  tryCatch({
    download.file(url, destfile, mode = mode)
    message("Downloaded: ", destfile)
  }, error = function(e) {
    stop("Failed to download file from URL: ", e$message)
  })
  
  return(normalizePath(destfile, winslash = "/", mustWork = FALSE))
}



#' Recursively List Full Directory Contents
#'
#' This function lists all files and subdirectories within a specified directory recursively, displaying the structure with indentation for subdirectories and files.
#'
#' @param dir_path A character string specifying the path to the directory whose contents should be listed.
#' @param indent A character string used for indentation. This is mainly for internal recursive use and should not be manually set by the user.
#' 
#' @return No return value. The function prints the directory structure to the console, showing files and folders with indented formatting.
#'
#' @details 
#' - The function prints each file and directory at the top level of `dir_path`. 
#' - If a directory is encountered, it recursively lists the contents of the directory, applying additional indentation for nested levels.
#' - Files are listed without a trailing slash, while directories are listed with a trailing `/` for clarity.
#'
#' @examples
#' \dontrun{
#' # List all contents of a directory
#' list_full_directory_contents("path/to/directory")
#' }
list_full_directory_contents <- function(dir_path, indent = "") {
  # Get all files and directories in the current directory
  items <- list.files(dir_path, full.names = TRUE)
  
  for (item in items) {
    # Check if the item is a directory
    if (dir.exists(item)) {
      # Print the directory with indentation
      cat(indent, "- ", basename(item), "/\n", sep = "")
      # Recursively list the contents of the directory
      list_full_directory_contents(item, paste0(indent, "  "))
    } else {
      # Print the file with indentation
      cat(indent, "- ", basename(item), "\n", sep = "")
    }
  }
}




write_session_info <- function(path) {
  writeLines(capture.output(sessionInfo()), path)
}



install_load_packages <- function(pkgs, date = NULL, groundhog = FALSE, pak_quiet = TRUE) {
  
  # --- Helper: quiet install with base R ---
  safe_install <- function(pkg, repos = "https://cloud.r-project.org") {
    tryCatch(
      suppressWarnings(install.packages(pkg, repos = repos, dependencies = TRUE)),
      error = function(e) message("Could not install ", pkg, ": ", e$message)
    )
  }
  
  # --- Check which packages are missing ---
  not_installed <- vapply(pkgs, function(p) !requireNamespace(p, quietly = TRUE), logical(1))
  missing_pkgs <- pkgs[not_installed]
  
  if (length(missing_pkgs) == 0) {
    message("All requested packages are already installed.")
  }
  
  # --- Only ensure pak if actually needed ---
  has_pak <- requireNamespace("pak", quietly = TRUE)
  if (length(missing_pkgs) > 0 && !has_pak) {
    message("Some packages are missing; installing 'pak'...")
    
    pak_install_success <- FALSE
    try({
      suppressWarnings(
        install.packages("pak", repos = "https://cloud.r-project.org", dependencies = TRUE)
      )
      pak_install_success <- requireNamespace("pak", quietly = TRUE)
    }, silent = TRUE)
    
    if (!pak_install_success) {
      message("Standard install failed; trying pak bootstrap installer...")
      try({
        source("https://pak.r-lib.org/install.R")
        pak_install_success <- requireNamespace("pak", quietly = TRUE)
      }, silent = TRUE)
    }
    
    if (!pak_install_success) {
      warning("Failed to install 'pak' by any method; will fall back to base installers only.")
    }
    
    has_pak <- requireNamespace("pak", quietly = TRUE)
  }
  
  # --- Optionally ensure groundhog ---
  if (groundhog) {
    if (!requireNamespace("groundhog", quietly = TRUE)) {
      message("Installing 'groundhog'...")
      safe_install("groundhog")
    }
    if (is.null(date)) {
      stop("groundhog = TRUE requires a non-null 'date' argument (YYYY-MM-DD).")
    }
  }
  
  # --- Repository selection ---
  repo <- if (!is.null(date)) {
    sprintf("https://packagemanager.posit.co/cran/%s", date)
  } else {
    "https://cloud.r-project.org"
  }
  message("Using CRAN repository: ", repo)
  options(repos = c(CRAN = repo))
  
  # --- Install missing packages ---
  installed_or_updated <- FALSE
  
  if (length(missing_pkgs) > 0) {
    message("Missing packages detected: ", paste(missing_pkgs, collapse = ", "))
    
    if (has_pak) {
      tryCatch({
        if (pak_quiet) {
          message("Attempting install with pak (quietly)...")
          suppressMessages(suppressWarnings(
            pak::pkg_install(missing_pkgs, ask = FALSE, upgrade = FALSE)
          ))
        } else {
          message("Attempting install with pak...")
          pak::pkg_install(missing_pkgs, ask = FALSE, upgrade = FALSE)
        }
        installed_or_updated <<- TRUE
      }, error = function(e) {
        message("pak installation failed: ", e$message)
        message("Falling back to install.packages()...")
        for (p in missing_pkgs) safe_install(p, repos = repo)
        installed_or_updated <<- TRUE
      })
    } else {
      message("pak unavailable; installing missing packages with install.packages()...")
      for (p in missing_pkgs) safe_install(p, repos = repo)
      installed_or_updated <- TRUE
    }
  }
  
  # --- Load packages ---
  failed_to_load <- character()
  
  if (groundhog) {
    message("Loading packages with groundhog (date = ", date, ")...")
    tryCatch({
      groundhog::groundhog.library(pkgs, date = date)
    }, error = function(e) {
      message("groundhog loading error: ", e$message)
      failed_to_load <<- pkgs
    })
  } else {
    message("Loading packages...")
    for (p in pkgs) {
      ok <- tryCatch({
        library(p, character.only = TRUE, quietly = TRUE)
        TRUE
      }, error = function(e) {
        message("Failed to load ", p, ": ", e$message)
        FALSE
      })
      if (!ok) failed_to_load <- c(failed_to_load, p)
    }
  }
  
  # --- Restart message if needed ---
  if (installed_or_updated || length(failed_to_load) > 0) {
    message("\nSome packages were newly installed, updated, or failed to load.\n",
            "This may be due to updated dependencies already loaded in memory.\n",
            "Please restart R and re-run this function to ensure all packages load correctly.\n")
  }
  
  # --- Report loaded versions ---
  loaded_versions <- sapply(pkgs, function(p) {
    if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else NA_character_
  })
  message("Packages loaded:\n",
          paste(names(loaded_versions), loaded_versions, collapse = "\n"))
  invisible(loaded_versions)
}


# Specialized functions ----

#' Package Existing Data File(s) with Metadata into ZIP
#'
#' Copies one or more existing data files, creates a Markdown metadata file using provided column names and descriptions, and packages all into a ZIP archive.
#'
#' @param data_file_path Character string or vector of file paths (e.g., CSVs).
#' @param column_names Character vector of column names.
#' @param column_descriptions Character vector of column descriptions (same length as column_names).
#' @param overall_description Overall dataset description.
#' @param author Author name.
#' @param github_repo GitHub repo URL.
#' @param out_dir Output directory path.
#' @param data_name_full Full dataset name for metadata.
#' @param data_name_file Base filename for output (no extension).
#'
#' @return NULL. Writes metadata and zip file to disk.
#'
#' @importFrom zip zip
package_with_metadata <- function(data_file_path, column_names, column_descriptions,
                                  overall_description, author, github_repo,
                                  out_dir, data_name_full, data_name_file) {
  # Ensure data_file_path is a character vector
  if (!is.character(data_file_path)) {
    stop("data_file_path must be a character string or a character vector.")
  }
  
  # Check that all specified files exist
  missing_files <- data_file_path[!file.exists(data_file_path)]
  if (length(missing_files) > 0) {
    stop("The following files do not exist:\n", paste(missing_files, collapse = "\n"))
  }
  
  stopifnot(length(column_names) == length(column_descriptions))
  
  # Create metadata table
  df_metadata <- cbind(column_names, column_descriptions)
  
  # Create metadata markdown
  meta_path <- file.path(out_dir, "metadata.md")
  stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  
  sink(meta_path)
  cat("# Metadata for the ", data_name_full, " dataset\n")
  cat(overall_description, "\n\n")
  cat("## Information\n")
  cat("Author: ", author, "\n")
  cat("Date generated: ", stamp, "\n")
  cat("[GitHub repo with code for reproduction](", github_repo, ")\n\n")
  cat("## Metadata\n")
  cat("column_names :: column_descriptions\n")
  cat(apply(df_metadata, 1, paste, collapse = " :: "), sep = "\n")
  sink()
  
  # Zip files
  zip_path <- file.path(out_dir, paste0(data_name_file, ".zip"))
  zip::zip(zipfile = zip_path,
           files = c(data_file_path, meta_path),
           mode = "cherry-pick")
  
  # Clean up temporary metadata file
  file.remove(meta_path)
}

#' Export Data and Metadata to CSV and Markdown, then Zip
#'
#' Exports a data frame and its metadata to a CSV and Markdown file, then packages them in a ZIP archive.
#'
#' @param df A data frame to export.
#' @param description Character vector of descriptions corresponding to each column (must match length of `columns`).
#' @param overall_description Overall dataset description (string).
#' @param author Author name (string).
#' @param github_repo GitHub repo URL (string).
#' @param out_dir Full path to output directory (string, e.g., from `here()`).
#' @param df_name_full Dataset name to write in metadata.
#' @param df_name_file Base name for output files (no extension).
#'
#' @return NULL. Writes files to disk and zips them.
#'
#' @importFrom zip zip
#' @importFrom utils write.csv
#'
#' @note Be sure to include `zip` and `utils` in the `Imports` section of your DESCRIPTION file.
export_df_with_metadata <- function(df, description,
                                    overall_description, author, github_repo,
                                    out_dir, df_name_full, df_name_file) {
  
  columns <- colnames(df)
  stopifnot(length(columns) == length(description))
  
  # Create metadata table
  df_metadata <- cbind(columns, description)
  
  # Write data CSV
  dfFile <- file.path(out_dir, paste0(df_name_file, ".csv"))
  write.csv(df, dfFile, row.names = FALSE)
  
  # Generate timestamp
  stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  
  # Write metadata Markdown file
  metaFile <- file.path(out_dir, "metadata.md")
  sink(metaFile)
  cat("# Metadata for the ", df_name_full, " dataset\n")
  cat(overall_description, "\n\n")
  cat("## Information\n")
  cat("Author: ", author, "\n")
  cat("Date generated: ", stamp, "\n")
  cat("[GitHub repo with code for reproduction](", github_repo, ")\n\n")
  cat("## Metadata\n")
  cat(paste0(colnames(df_metadata), collapse = ' :: '), "\n")
  cat(apply(df_metadata, 1, paste, collapse = " :: "), sep = "\n")
  sink()
  
  # Zip files
  zipfile <- file.path(out_dir, paste0(df_name_file, ".zip"))
  zip::zip(zipfile = zipfile,
           files = c(dfFile, metaFile),
           mode = "cherry-pick")
  
  # Clean up
  file.remove(dfFile)
  file.remove(metaFile)
}


# Function Description:
#   The function create.qgis.style.for.paletted.raster.from.csv generates a QGIS style file (.qml) for raster layers, specifically formatted for paletted rasters. It uses styling information provided in a data frame (styleData), allowing users to define colors and labels for different raster values. The function supports hexadecimal (hex) and RGB color schemes.
# 
# Parameters:
#   styleData: A data frame containing the styling information. The data frame should include the columns specified by valueColumn and labelColumn. For the hex color scheme, a color column is required. For RGB, columns R, G, and B are necessary.
# outputQmlPath: A string specifying the file path where the generated QGIS style file (.qml) will be saved.
# valueColumn: The name of the column in styleData that contains the raster values.
# labelColumn: The name of the column in styleData that contains the labels for each raster value.
# colorScheme: A string indicating the color scheme used in styleData. It can be "hex" for hexadecimal colors or "RGB" for separate red, green, and blue values. The default is "hex".
# Functionality:
#   The function iterates through each row of styleData, extracting the value, label, and color information to create palette entries in the QML file. For RGB color schemes, it converts the RGB values to hex using the rgb function. The function ensures proper XML formatting by escaping special characters in labels. After constructing the QML content, it is written to the specified output path.
# 
# Usage Example:
# # Assuming styleData is pre-defined with the appropriate columns
# create_qgis_style_for_paletted_raster_from_csv(styleData, "path/to/output.qml", "value", "label", "hex")
# Citation:
#   Function authored by R Code Stylist, GPT-4, OpenAI, in collaboration with the user.
create_qgis_style_for_paletted_raster_from_csv <- function(styleData, outputQmlPath, valueColumn, labelColumn, colorScheme = "hex") {
  
  # Check for the necessary columns in the CSV based on the color scheme
  if (!labelColumn %in% colnames(styleData)) {
    stop("CSV file must contain the specified label column.")
  }
  
  if (colorScheme == "hex" && !("color" %in% colnames(styleData))) {
    stop("CSV file must contain a 'color' column for hex color scheme.")
  } else if (colorScheme == "RGB" && !all(c("R", "G", "B") %in% colnames(styleData))) {
    stop("CSV file must contain 'R', 'G', 'B' columns for RGB color scheme.")
  }
  
  # Start creating the QML content
  qmlContent <- '<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE qgis PUBLIC \'http://mrcc.com/qgis.dtd\' \'SYSTEM\'>
<qgis hasScaleBasedVisibilityFlag="0" maxScale="0" version="3.22.12-Białowieża" styleCategories="AllStyleCategories" minScale="1e+08">
   <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
    <Private>0</Private>
  </flags>
  <temporal enabled="0" fetchMode="0" mode="0">
    <fixedRange>
      <start></start>
      <end></end>
    </fixedRange>
  </temporal>
  <customproperties>
    <Option type="Map">
      <Option value="false" type="bool" name="WMSBackgroundLayer"/>
      <Option value="false" type="bool" name="WMSPublishDataSourceUrl"/>
      <Option value="0" type="int" name="embeddedWidgets/count"/>
      <Option value="Value" type="QString" name="identify/format"/>
    </Option>
  </customproperties>
  <pipe-data-defined-properties>
    <Option type="Map">
      <Option value="" type="QString" name="name"/>
      <Option name="properties"/>
      <Option value="collection" type="QString" name="type"/>
    </Option>
  </pipe-data-defined-properties>
  <pipe>
    <provider>
      <resampling enabled="false" zoomedInResamplingMethod="nearestNeighbour" zoomedOutResamplingMethod="nearestNeighbour" maxOversampling="2"/>
    </provider>
    <rasterrenderer opacity="1" nodataColor="" type="paletted" band="1" alphaBand="-1">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>None</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
  <colorPalette>'  
  # Append palette entries from the CSV data
  for (i in 1:nrow(styleData)) {
    # Determine the color based on the scheme
    if (colorScheme == "hex") {
      color <- styleData$color[i]
    } else {
      color <- rgb(red = styleData$R[i], green = styleData$G[i], blue = styleData$B[i], maxColorValue = 255)
    }
    
    label <- styleData[[labelColumn]][i]
    label <- gsub("&", "and", label)
    label <- gsub('\\"', '', label)
    value <- styleData[[valueColumn]][i]
    
    
    qmlContent <- glue::glue('{qmlContent}
             <paletteEntry value="{value}" label="{label}" alpha="255" color="{color}"/>'
    )
  }
  
  # Finalize the QML content with closing tags
  qmlContent <- paste0(qmlContent, '\n      </colorPalette>
          <colorramp type="randomcolors" name="[source]">
        <Option/>
      </colorramp>
    </rasterrenderer>
    <brightnesscontrast gamma="1" brightness="0" contrast="0"/>
    <huesaturation grayscaleMode="0" colorizeOn="0" colorizeGreen="128" saturation="0" colorizeBlue="128" colorizeRed="255" colorizeStrength="100" invertColors="0"/>
    <rasterresampler maxOversampling="2"/>
    <resamplingStage>resamplingFilter</resamplingStage>
  </pipe>
  <blendMode>0</blendMode>
</qgis>')
  
  # Write the QML content to a file
  writeLines(qmlContent, outputQmlPath)
  
  print(qmlContent)
  
  return(paste("QGIS style file created at:", outputQmlPath))
}














# DEPRECATED ----
#' 
#' # REPLACED BY safe_extract
#' #' Safe Unzip a File (with Optional Recursive Unzipping and ZIP Cleanup)
#' #'
#' #' Safely unzips a ZIP file to a specified directory. Supports skipping extraction if files or top-level folder already exist, recursive unzipping of nested ZIPs, and optional deletion of ZIP files.
#' #'
#' #' @param zip_path Character. Path to the local ZIP file.
#' #' @param extract_to Character. Directory where the contents should be extracted. Defaults to the ZIP's directory.
#' #' @param recursive Logical. If TRUE, recursively unzip nested ZIP files. Defaults to FALSE.
#' #' @param keep_zip Logical. If FALSE, deletes the original ZIP and any nested ZIPs after unzipping. Defaults to TRUE.
#' #' @param full_contents_check Logical. If TRUE, skip unzip only if all expected files exist. If FALSE (default), skip unzip if the top-level directory exists.
#' #' @param return_all_paths Logical. If TRUE, returns full paths to all extracted files. If FALSE (default), returns only the top-level directory path.
#' #'
#' #' @return A character vector of extracted file paths (if \code{return_all_paths = TRUE}) or a single path to the top-level extracted directory (if \code{return_all_paths = FALSE}).
#' #'
#' #' @importFrom utils unzip
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Recursively unzip and delete all ZIPs, return full paths
#' #' files <- safe_unzip("data/archive.zip", recursive = TRUE, keep_zip = FALSE, return_all_paths = TRUE)
#' #'
#' #' # Unzip only if top folder doesn't exist, return folder path
#' #' folder <- safe_unzip("data/archive.zip", full_contents_check = FALSE, return_all_paths = FALSE)
#' #' }
#' safe_unzip <- function(zip_path,
#'                        extract_to = dirname(zip_path),
#'                        recursive = FALSE,
#'                        keep_zip = TRUE,
#'                        full_contents_check = FALSE,
#'                        return_all_paths = FALSE) {
#'   # Validate inputs
#'   if (!file.exists(zip_path)) stop("ZIP file does not exist: ", zip_path)
#'   if (!is.character(extract_to) || length(extract_to) != 1) stop("`extract_to` must be a single character string.")
#'   if (!is.logical(recursive) || length(recursive) != 1) stop("`recursive` must be a single logical value.")
#'   if (!is.logical(keep_zip) || length(keep_zip) != 1) stop("`keep_zip` must be a single logical value.")
#'   if (!is.logical(full_contents_check) || length(full_contents_check) != 1) stop("`full_contents_check` must be logical.")
#'   if (!is.logical(return_all_paths) || length(return_all_paths) != 1) stop("`return_all_paths` must be logical.")
#'   
#'   # Get ZIP listing and top-level directory
#'   zip_listing <- unzip(zip_path, list = TRUE)
#'   top_level_dirs <- unique(sub("/.*", "", zip_listing$Name))
#'   top_dir_path <- file.path(extract_to, top_level_dirs[1])
#'   
#'   # Determine whether to skip unzip
#'   skip_unzip <- FALSE
#'   if (full_contents_check) {
#'     expected_paths <- file.path(extract_to, zip_listing$Name)
#'     skip_unzip <- all(file.exists(expected_paths))
#'   } else {
#'     skip_unzip <- dir.exists(top_dir_path)
#'   }
#'   
#'   if (!skip_unzip) {
#'     if (!dir.exists(extract_to)) dir.create(extract_to, recursive = TRUE)
#'     tryCatch({
#'       unzip(zip_path, exdir = extract_to)
#'     }, error = function(e) {
#'       stop("Failed to unzip: ", e$message)
#'     })
#'     
#'     if (recursive) {
#'       nested_zips <- list.files(extract_to, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE)
#'       for (nz in nested_zips) {
#'         unzip(nz, exdir = dirname(nz))
#'         if (!keep_zip) unlink(nz)
#'       }
#'     }
#'     
#'     if (!keep_zip) unlink(zip_path)
#'   } else {
#'     message("Skipping unzip: Extraction target(s) already exist in ", extract_to)
#'   }
#'   
#'   if (return_all_paths) {
#'     all_files <- list.files(extract_to, recursive = TRUE, full.names = TRUE)
#'     file_paths <- all_files[file.info(all_files)$isdir == FALSE]
#'     return(invisible(normalizePath(file_paths, winslash = "/", mustWork = FALSE)))
#'   } else {
#'     return(invisible(normalizePath(top_dir_path, winslash = "/", mustWork = FALSE)))
#'   }
#' }
#' 
#' #' Unzip Files or All Zip Files in a Directory
#' #'
#' #' This function checks if the input is a zip file or a directory. If it's a specific zip file, it will unzip the file into a folder with the same name (excluding the `.zip` extension) if the folder does not already exist. If the input is a directory, it will locate all `.zip` files in that directory and unzip them into their respective folders, creating the folder if necessary.
#' #'
#' #' @param zip_location A character string representing either a path to a specific zip file or a directory containing zip files.
#' #' 
#' #' @return No return value. The function unzips files as needed and prints messages indicating whether files were unzipped or if the target folders already existed.
#' #'
#' #' @details 
#' #' - If `zip_location` points to a zip file and the corresponding folder doesn't exist, the function will unzip the file into a new folder located in the same directory as the zip file.
#' #' - If `zip_location` points to a directory, the function will iterate over all zip files in the directory, unzipping each into a folder named after the zip file (without the `.zip` extension).
#' #' - If a folder with the same name as the zip file already exists, the function will skip unzipping that file.
#' #' 
#' #' @examples
#' #' \dontrun{
#' #' # Unzipping a specific file
#' #' unzip_if_zipped("path/to/file.zip")
#' #'
#' #' # Unzipping all zip files in a directory
#' #' unzip_if_zipped("path/to/directory")
#' #' }
#' #'
#' #' @importFrom utils unzip
#' unzip_if_zipped <- function(zip_location) {
#'   # Check if the input is a specific zip file
#'   if (file.exists(zip_location) && grepl("\\.zip$", zip_location)) {
#'     # It's a specific zip file
#'     folder_name <- sub("\\.zip$", "", basename(zip_location))
#'     destination_path <- file.path(dirname(zip_location), folder_name)
#'     
#'     # Check if the corresponding folder already exists
#'     if (!dir.exists(destination_path)) {
#'       # Unzip the file into the new folder
#'       unzip(zip_location, exdir = destination_path)
#'       cat("Unzipped:", zip_location, "to", destination_path, "\n")
#'     } else {
#'       cat("Folder already exists:", destination_path, "\n")
#'     }
#'   } else if (dir.exists(zip_location)) {
#'     # It's a directory, process all zip files in the directory
#'     zip_files <- list.files(zip_location, pattern = "\\.zip$", full.names = TRUE)
#'     
#'     if (length(zip_files) == 0) {
#'       cat("No zip files found in the directory:", zip_location, "\n")
#'     } else {
#'       for (zip_file in zip_files) {
#'         folder_name <- sub("\\.zip$", "", basename(zip_file))
#'         destination_path <- file.path(zip_location, folder_name)
#'         
#'         # Check if the corresponding folder already exists
#'         if (!dir.exists(destination_path)) {
#'           # Unzip the file into the new folder
#'           unzip(zip_file, exdir = destination_path)
#'           cat("Unzipped:", zip_file, "to", destination_path, "\n")
#'         } else {
#'           cat("Folder already exists:", destination_path, "\n")
#'         }
#'       }
#'     }
#'   } else {
#'     cat("The provided path is neither a valid zip file nor a directory.\n")
#'   }
#' }
#' 
#' 
#' 
#' #' Install and Load Required Packages Using pak
#' #'
#' #' This function checks if the specified packages (both CRAN and GitHub) are installed and loads them. 
#' #' If any packages are missing, it offers to install them automatically or asks for user permission.
#' #' It uses the `pak` package for faster and more efficient package installation.
#' #'
#' #' @param package_list A list of package names to check and install (non-string, e.g., `c(dplyr, here)`).
#' #' GitHub packages should be specified as `username/repo` in strings.
#' #' @param auto_install A character ("y" or "n", default is "n"). If "y", installs all required packages 
#' #' without asking for user permission. If "n", asks for permission from the user.
#' #' @return No return value. Installs and loads the specified packages as needed.
#' #' @examples
#' #' \dontrun{
#' #' install_and_load_packages(c(dplyr, here, "username/repo"))
#' #' }
#' #' @importFrom pak pkg_install
#' #' @export
#' install_and_load_packages <- function(package_list, auto_install = "n") {
#'   # Convert non-string package names to strings
#'   package_list <- lapply(package_list, function(pkg) {
#'     if (is.symbol(pkg)) {
#'       deparse(substitute(pkg))
#'     } else {
#'       pkg
#'     }
#'   })
#'   
#'   # Check if pak is installed; install if not
#'   if (!requireNamespace("pak", quietly = TRUE)) {
#'     cat("The 'pak' package is required for fast installation of packages.\n")
#'     response <- if (auto_install == "y") "y" else readline(prompt = "\nDo you want to install the 'pak' package? (y/n): ")
#'     if (tolower(response) == "y") {
#'       install.packages("pak")
#'     } else {
#'       stop("Installation cannot proceed without 'pak'. Please install it manually and rerun.")
#'     }
#'   }
#'   
#'   # Initialize lists to store missing CRAN and GitHub packages
#'   missing_cran_packages <- c()
#'   missing_github_packages <- c()
#'   
#'   # Helper function to get user input
#'   get_user_permission <- function(prompt_msg) {
#'     if (auto_install == "y") {
#'       return("y")
#'     } else {
#'       return(tolower(readline(prompt = prompt_msg)))
#'     }
#'   }
#'   
#'   # Check for missing packages
#'   for (pkg in package_list) {
#'     if (grepl("/", pkg)) { # GitHub package
#'       package_name <- unlist(strsplit(pkg, "/"))[2]
#'       package_loaded <- require(package_name, character.only = TRUE, quietly = TRUE)
#'     } else { # CRAN package
#'       package_loaded <- require(pkg, character.only = TRUE, quietly = TRUE)
#'     }
#'     if (!package_loaded) {
#'       if (grepl("/", pkg)) {
#'         missing_github_packages <- c(missing_github_packages, pkg)
#'       } else {
#'         missing_cran_packages <- c(missing_cran_packages, pkg)
#'       }
#'     }
#'   }
#'   
#'   # Install missing CRAN packages using pak::pkg_install
#'   if (length(missing_cran_packages) > 0) {
#'     cat("The following CRAN packages are missing: ", paste(missing_cran_packages, collapse = ", "), "\n")
#'     response <- get_user_permission("\nDo you want to install the missing CRAN packages? (y/n): ")
#'     if (response == "y") {
#'       pak::pkg_install(missing_cran_packages, upgrade = TRUE)
#'     } else {
#'       cat("Skipping installation of missing CRAN packages.\n")
#'     }
#'   }
#'   
#'   # Install missing GitHub packages using pak::pkg_install
#'   if (length(missing_github_packages) > 0) {
#'     cat("The following GitHub packages are missing: ", paste(missing_github_packages, collapse = ", "), "\n")
#'     response <- get_user_permission("\nDo you want to install the missing GitHub packages? (y/n): ")
#'     if (response == "y") {
#'       pak::pkg_install(missing_github_packages, upgrade = TRUE)
#'     } else {
#'       cat("Skipping installation of missing GitHub packages.\n")
#'     }
#'   }
#'   
#'   # Load all packages after checking for installation
#'   for (pkg in package_list) {
#'     if (grepl("/", pkg)) { # GitHub package
#'       package_name <- unlist(strsplit(pkg, "/"))[2]
#'       if (!require(package_name, character.only = TRUE)) {
#'         cat("Failed to load GitHub package:", package_name, "\n")
#'       }
#'     } else { # CRAN package
#'       if (!require(pkg, character.only = TRUE)) {
#'         cat("Failed to load CRAN package:", pkg, "\n")
#'       }
#'     }
#'   }
#'   
#'   cat("All specified packages installed and loaded.\n")
#' }
#' 
#' 
#' 
#' #' Install and Load Required Packages Using pak
#' #'
#' #' This function checks if the specified packages (both CRAN and GitHub) are installed and loads them. 
#' #' If any packages are missing, it installs them automatically.
#' #' It uses the `pak` package for faster and more efficient package installation.
#' #'
#' #' @param package_list A list of package names to check and install (non-string, e.g., `c(dplyr, here)`).
#' #' GitHub packages should be specified as `username/repo` in strings.
#' #' @param auto_install A character ("y" or "n", default is "n"). If "y", installs all required packages 
#' #' without asking for user permission. If "n", asks for permission from the user.
#' #' @return No return value. Installs and loads the specified packages as needed.
#' #' @examples
#' #' \dontrun{
#' #' install_and_load_packages(c(dplyr, here, "username/repo"))
#' #' }
#' #' @importFrom pak pkg_install
#' #' @export
#' install_and_load_packages <- function(package_list, auto_install = "n") {
#'   # Convert non-string package names to strings
#'   package_list <- lapply(package_list, function(pkg) {
#'     if (is.symbol(pkg)) {
#'       deparse(substitute(pkg))
#'     } else {
#'       pkg
#'     }
#'   })
#'   
#'   # # Check if 'renv' is installed; if not, skip the 'renv' check
#'   # if (requireNamespace("renv", quietly = TRUE) && renv::is_active()) {
#'   #   cat("renv is active. Only loading packages...\n")
#'   #   for (pkg in package_list) {
#'   #     package_name <- if (grepl("/", pkg)) unlist(strsplit(pkg, "/"))[2] else pkg
#'   #     if (!require(package_name, character.only = TRUE)) {
#'   #       cat("Failed to load package:", package_name, "\n")
#'   #     }
#'   #   }
#'   #   return(invisible())
#'   # }
#'   
#'   # Check if pak is installed; install if not
#'   if (!requireNamespace("pak", quietly = TRUE)) {
#'     cat("The 'pak' package is required for fast installation of packages, installing now.\n")
#'     install.packages("pak")
#'   }
#'   
#'   # Initialize lists to store missing CRAN and GitHub packages
#'   missing_cran_packages <- c()
#'   missing_github_packages <- c()
#'   
#'   # # Helper function to get user input
#'   # get_user_permission <- function(prompt_msg) {
#'   #   if (auto_install == "y") {
#'   #     return("y")
#'   #   } else {
#'   #     return(tolower(readline(prompt = prompt_msg)))
#'   #   }
#'   # }
#'   
#'   # Check for missing packages
#'   for (pkg in package_list) {
#'     if (grepl("/", pkg)) { # GitHub package
#'       package_name <- unlist(strsplit(pkg, "/"))[2]
#'       package_loaded <- require(package_name, character.only = TRUE, quietly = TRUE)
#'     } else { # CRAN package
#'       package_loaded <- require(pkg, character.only = TRUE, quietly = TRUE)
#'     }
#'     if (!package_loaded) {
#'       if (grepl("/", pkg)) {
#'         missing_github_packages <- c(missing_github_packages, pkg)
#'       } else {
#'         missing_cran_packages <- c(missing_cran_packages, pkg)
#'       }
#'     }
#'   }
#'   
#'   # Install missing CRAN packages using pak::pkg_install
#'   if (length(missing_cran_packages) > 0) {
#'     # cat("The following CRAN packages are missing: ", paste(missing_cran_packages, collapse = ", "), "\n")
#'     # response <- get_user_permission("\nDo you want to install the missing CRAN packages? (y/n): ")
#'     # if (response == "y") {
#'     pak::pkg_install(missing_cran_packages, upgrade = TRUE)
#'     # } else {
#'     #   cat("Skipping installation of missing CRAN packages.\n")
#'     # }
#'   }
#'   
#'   # Install missing GitHub packages using pak::pkg_install
#'   if (length(missing_github_packages) > 0) {
#'     # cat("The following GitHub packages are missing: ", paste(missing_github_packages, collapse = ", "), "\n")
#'     # response <- get_user_permission("\nDo you want to install the missing GitHub packages? (y/n): ")
#'     # if (response == "y") {
#'     pak::pkg_install(missing_github_packages, upgrade = TRUE)
#'     # } else {
#'     #   cat("Skipping installation of missing GitHub packages.\n")
#'     # }
#'   }
#'   
#'   # Load all packages after checking for installation
#'   for (pkg in package_list) {
#'     if (grepl("/", pkg)) { # GitHub package
#'       package_name <- unlist(strsplit(pkg, "/"))[2]
#'       if (!require(package_name, character.only = TRUE)) {
#'         cat("Failed to load GitHub package:", package_name, "\n")
#'       }
#'     } else { # CRAN package
#'       if (!require(pkg, character.only = TRUE)) {
#'         cat("Failed to load CRAN package:", pkg, "\n")
#'       }
#'     }
#'   }
#'   
#'   cat("All specified packages installed and loaded.\n")
#' }
#' 
#' 
#' install_and_load_packages <- function(package_list, auto_install = "n") {
#'   # Ensure pak is available
#'   if (!requireNamespace("pak", quietly = TRUE)) {
#'     cat("The 'pak' package is required for fast installation of packages, installing now.\n")
#'     install.packages("pak")
#'   }
#'   
#'   # Helper: Extract base name of a package for require()
#'   parse_pkg_name <- function(pkg) {
#'     if (grepl("/", pkg)) {
#'       sub("^.+/(.+?)(@.+)?$", "\\1", pkg)  # GitHub: extract repo name
#'     } else {
#'       sub("@.*$", "", pkg)  # CRAN: remove @version if present
#'     }
#'   }
#'   
#'   # Classify and separate packages
#'   missing_pkgs <- c()
#'   for (pkg in package_list) {
#'     pkg_name <- parse_pkg_name(pkg)
#'     if (!requireNamespace(pkg_name, quietly = TRUE)) {
#'       missing_pkgs <- c(missing_pkgs, pkg)
#'     }
#'   }
#'   
#'   # Install missing ones (CRAN or GitHub), with version support
#'   if (length(missing_pkgs) > 0) {
#'     pak::pkg_install(missing_pkgs, upgrade = TRUE)
#'   }
#'   
#'   # Load all packages
#'   for (pkg in package_list) {
#'     pkg_name <- parse_pkg_name(pkg)
#'     success <- require(pkg_name, character.only = TRUE, quietly = TRUE)
#'     if (!success) cat("Failed to load package:", pkg_name, "\n")
#'   }
#'   
#'   cat("All specified packages installed and loaded.\n")
#' }
#' 
#' 
#' install_and_load_packages <- function(package_list) {
#'   # Ensure pak is available
#'   if (!requireNamespace("pak", quietly = TRUE)) {
#'     cat("The 'pak' package is required for fast installation of packages, installing now.\n")
#'     install.packages("pak")
#'   }
#'   
#'   # Helper: Extract base name of a package for require()
#'   parse_pkg_name <- function(pkg) {
#'     if (grepl("/", pkg)) {
#'       sub("^.+/(.+?)(@.+)?$", "\\1", pkg)  # GitHub: extract repo name
#'     } else {
#'       sub("@.*$", "", pkg)  # CRAN: remove @version if present
#'     }
#'   }
#'   
#'   # Classify and separate packages
#'   missing_pkgs <- c()
#'   for (pkg in package_list) {
#'     pkg_name <- parse_pkg_name(pkg)
#'     if (!requireNamespace(pkg_name, quietly = TRUE)) {
#'       missing_pkgs <- c(missing_pkgs, pkg)
#'     }
#'   }
#'   
#'   # Install missing ones (CRAN or GitHub), with version support
#'   if (length(missing_pkgs) > 0) {
#'     pak::pkg_install(missing_pkgs, upgrade = TRUE, ask = FALSE)
#'   }
#'   
#'   # Load all packages
#'   for (pkg in package_list) {
#'     pkg_name <- parse_pkg_name(pkg)
#'     success <- require(pkg_name, character.only = TRUE, quietly = TRUE)
#'     if (!success) cat("Failed to load package:", pkg_name, "\n")
#'   }
#'   
#'   cat("All specified packages installed and loaded.\n")
#' }
#' 
#' 
#' 
#' #' Install and Load Required Packages Using pak
#' #'
#' #' This function ensures that the specified packages (from CRAN or GitHub) are installed and loaded.
#' #' It uses the `pak` package for fast and reliable package installation, supporting versioned and GitHub installs.
#' #' If any packages are missing, they are automatically installed without prompting the user.
#' #'
#' #' @param package_list A character vector of package specifications to check, install, and load.
#' #' For CRAN packages, use names like `"dplyr"` or `"dplyr@1.1.4"`. For GitHub packages, use the
#' #' `"username/repo"` format, optionally with a version or ref (e.g., `"hadley/ggplot2@main"`).
#' #'
#' #' @return No return value. The specified packages are installed (if missing) and loaded into the session.
#' #'
#' #' @details
#' #' This function automatically installs the `pak` package if it is not available.
#' #' It distinguishes between CRAN and GitHub packages based on the presence of a "/" in the string.
#' #' It loads each package by extracting its base name from the specification.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' install_and_load_packages(c("dplyr", "hadley/ggplot2", "data.table@1.14.2"))
#' #' }
#' #'
#' #' @importFrom pak pkg_install
#' #' @export
#' install_and_load_packages <- function(package_list) {
#'   # Ensure pak is available
#'   if (!requireNamespace("pak", quietly = TRUE)) {
#'     cat("The 'pak' package is required for fast installation of packages, installing now.\n")
#'     install.packages("pak")
#'   }
#'   
#'   # Helper: Extract base name of a package for require()
#'   parse_pkg_name <- function(pkg) {
#'     if (grepl("/", pkg)) {
#'       sub("^.+/(.+?)(@.+)?$", "\\1", pkg)  # GitHub: extract repo name
#'     } else {
#'       sub("@.*$", "", pkg)  # CRAN: remove @version if present
#'     }
#'   }
#'   
#'   # Classify and separate packages
#'   missing_pkgs <- c()
#'   for (pkg in package_list) {
#'     pkg_name <- parse_pkg_name(pkg)
#'     if (!requireNamespace(pkg_name, quietly = TRUE)) {
#'       missing_pkgs <- c(missing_pkgs, pkg)
#'     }
#'   }
#'   
#'   # Install missing ones (CRAN or GitHub), with version support
#'   if (length(missing_pkgs) > 0) {
#'     pak::pkg_install(missing_pkgs, upgrade = TRUE, ask = FALSE)
#'   }
#'   
#'   # Load all packages
#'   for (pkg in package_list) {
#'     pkg_name <- parse_pkg_name(pkg)
#'     success <- require(pkg_name, character.only = TRUE, quietly = TRUE)
#'     if (!success) cat("Failed to load package:", pkg_name, "\n")
#'   }
#'   
#'   cat("All specified packages installed and loaded.\n")
#' }
