#' Generate a Timestamp
#'
#' This function generates a timestamp in the format "YYYY-MM-DD HH:MM:SS TZ", representing the current date and time along with the time zone.
#'
#' @return A character string representing the current date and time in the format "YYYY-MM-DD HH:MM:SS TZ".
#' @examples
#' # Generate a timestamp
#' timestamp()
#'
#' @export
timestamp <- function(){
  return(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
}


#' Ensure Directory Exists
#'
#' This function checks if a directory exists at the specified path, and if not, creates a new directory.
#'
#' @param path A character string specifying the path to the new directory.
#' @return The function does not return any value. It creates a directory if it does not already exist.
#' @examples
#' # Ensure a directory named "data" exists
#' dir_ensure("data")
#'
#' @export
dir_ensure <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
    message("Directory created: ", path)
  } else {
    message("Directory already exists: ", path)
  }
}

#' Substring from the Right
#'
#' This function extracts a substring from the right side of a given string, retaining a specified number of characters.
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



#' Save Kable Output as PNG with Workaround
#'
#' This function provides a workaround for an issue (as of 3/20/24) with `kableExtra::save_kable`, which fails to export tables as `.png` files. It first saves the table as an HTML file and then converts it to a PNG using `webshot2`.
#'
#' @param k An output object from the `kable` function.
#' @param file_path A character string specifying the full desired file path (e.g., 'myDir/figs/myTable.png') for the output PNG file.
#' @return No return value. The function saves the PNG file to the specified location.
#' @examples
#' # Save a kable output as a PNG file
#' \dontrun{
#' k <- knitr::kable(head(mtcars))
#' save_kable_workaround(k, "myDir/figs/myTable.png")
#' }
#'
#' @importFrom webshot2 webshot
#' @importFrom kableExtra save_kable
#' @export
save_kable_workaround <- function(k, file_path) {
  html_path <- paste0(tools::file_path_sans_ext(file_path), ".html")
  kableExtra::save_kable(x = k, file = html_path)
  webshot2::webshot(html_path, file = file_path)
  file.remove(html_path)
}

#' Install and Load Required Packages Using pak
#'
#' This function checks if the specified packages (both CRAN and GitHub) are installed and loads them. 
#' If any packages are missing, it offers to install them automatically or asks for user permission.
#' It uses the `pak` package for faster and more efficient package installation.
#'
#' @param package_list A character vector of package names to check and install. 
#' GitHub packages should be specified as "username/repo".
#' @param auto_install A character ("y" or "n", default is "n"). If "y", installs all required packages 
#' without asking for user permission. If "n", asks for permission from the user.
#' @return No return value. Installs and loads the specified packages as needed.
#' @examples
#' \dontrun{
#' install_and_load_packages(c("here", "dplyr", "tidyverse", "username/repo"))
#' }
#' @importFrom pak pkg_install
#' @importFrom utils install.packages
#' @export
install_and_load_packages <- function(package_list, auto_install = "n") {
  # Check if pak is installed; install if not
  if (!requireNamespace("pak", quietly = TRUE)) {
    cat("The 'pak' package is required for fast installation of packages.\n")
    response <- if (auto_install == "y") "y" else readline(prompt = "\nDo you want to install the 'pak' package? (y/n): ")
    if (tolower(response) == "y") {
      utils::install.packages("pak")
    } else {
      stop("Installation cannot proceed without 'pak'. Please install it manually and rerun.")
    }
  }
  
  # Initialize lists to store missing CRAN and GitHub packages
  missing_cran_packages <- c()
  missing_github_packages <- c()
  
  # Helper function to get user input
  get_user_permission <- function(prompt_msg) {
    if (auto_install == "y") {
      return("y")
    } else {
      return(tolower(readline(prompt = prompt_msg)))
    }
  }
  
  # Check for missing packages
  for (pkg in package_list) {
    if (grepl("/", pkg)) { # GitHub package
      package_name <- unlist(strsplit(pkg, "/"))[2]
      package_loaded <- require(package_name, character.only = TRUE, quietly = TRUE)
    } else { # CRAN package
      package_loaded <- require(pkg, character.only = TRUE, quietly = TRUE)
    }
    if (!package_loaded) {
      if (grepl("/", pkg)) {
        missing_github_packages <- c(missing_github_packages, pkg)
      } else {
        missing_cran_packages <- c(missing_cran_packages, pkg)
      }
    }
  }
  
  # Install missing CRAN packages using pak::pkg_install
  if (length(missing_cran_packages) > 0) {
    cat("The following CRAN packages are missing: ", paste(missing_cran_packages, collapse = ", "), "\n")
    response <- get_user_permission("\nDo you want to install the missing CRAN packages? (y/n): ")
    if (response == "y") {
      pak::pkg_install(missing_cran_packages, upgrade = TRUE)
    } else {
      cat("Skipping installation of missing CRAN packages.\n")
    }
  }
  
  # Install missing GitHub packages using pak::pkg_install
  if (length(missing_github_packages) > 0) {
    cat("The following GitHub packages are missing: ", paste(missing_github_packages, collapse = ", "), "\n")
    response <- get_user_permission("\nDo you want to install the missing GitHub packages? (y/n): ")
    if (response == "y") {
      pak::pkg_install(missing_github_packages, upgrade = TRUE)
    } else {
      cat("Skipping installation of missing GitHub packages.\n")
    }
  }
  
  # Load all packages after checking for installation
  for (pkg in package_list) {
    if (grepl("/", pkg)) { # GitHub package
      package_name <- unlist(strsplit(pkg, "/"))[2]
      if (!require(package_name, character.only = TRUE)) {
        cat("Failed to load GitHub package:", package_name, "\n")
      }
    } else { # CRAN package
      if (!require(pkg, character.only = TRUE)) {
        cat("Failed to load CRAN package:", pkg, "\n")
      }
    }
  }
  
  cat("All specified packages installed and loaded.\n")
}


