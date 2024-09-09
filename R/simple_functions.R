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
#' dir.ensure("data")
#'
#' @export
dir.ensure <- function(path) {
  if (!dir.exists(path)){
    dir.create(path)
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
#' substrRight("Hello, World!", 3)
#'
#' @export
substrRight <- function(str, n) {
  return(substr(str, nchar(str)-n+1, nchar(str)))
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
#' @param filePath A character string specifying the full desired file path (e.g., 'myDir/figs/myTable.png') for the output PNG file.
#' @return No return value. The function saves the PNG file to the specified location.
#' @examples
#' # Save a kable output as a PNG file
#' \dontrun{
#' k <- knitr::kable(head(mtcars))
#' save_kable_workaround(k, "myDir/figs/myTable.png")
#' }
#'
#' @export
save_kable_workaround <- function(k, filePath) {
  htmlPath <- paste0(tools::file_path_sans_ext(filePath), ".html")
  kableExtra::save_kable(x = k, file = htmlPath)
  webshot2::webshot(htmlPath, file = filePath)
  file.remove(htmlPath)
}


