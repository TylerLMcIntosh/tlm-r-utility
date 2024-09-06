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

#' Convert Image File to CMYK Format Using ImageMagick
#'
#' This function converts an image file to CMYK format using ImageMagick. ImageMagick must be installed and accessible via the command line for this function to work.
#'
#' @param file A character string specifying the path to the file to convert to CMYK format.
#' @param outDir A character string specifying the directory to output the converted file. If left empty, the converted file will be saved in the same directory as the original file.
#' @return No return value. The function outputs the converted file to the specified directory.
#' @examples
#' # Convert all PNG files in a directory to CMYK format
#' pngs <- list.files(here::here('figs'), pattern = "\\.png$", full.names = TRUE, ignore.case = TRUE)
#' pngs |> purrr::walk(~ convert.to.cmyk(file = .x))
#'
#' # Convert a single file to CMYK format and save it in the same directory
#' convert.to.cmyk(file = "example.png")
#'
#' @export
convert.to.cmyk <- function(file, outDir = NA) {
  
  # Check if the file exists
  if (!file.exists(file)) {
    warning(paste("File does not exist:", file))
    next
  }
  
  # Set outdir
  if (is.na(outDir)) {
    directory <- dirname(file)
  } else {
    directory <- outDir
  }
  
  # Construct the new filename by appending '_cmyk' before the file extension
  filePath <- normalizePath(file, mustWork = FALSE)
  fileNm <- tools::file_path_sans_ext(basename(filePath))
  fileExt <- tools::file_ext(filePath)
  newFileNm <- paste0(fileNm, "_cmyk.", fileExt)
  newPath <- file.path(directory, newFileNm)
  
  # Form the command to convert the file to CMYK
  command <- sprintf('magick "%s" -colorspace CMYK "%s"', filePath, newPath)
  
  # Execute the command using system()
  system(command, intern = FALSE)
  
  # Print the status message
  cat("Converted:", filePath, "to CMYK and saved as:", newFileNm, "\n")
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

#' Convert PDF to PNG
#'
#' This function converts a PDF file to a PNG image with a specified DPI (dots per inch). It allows the output to be saved in a specified directory or in the same directory as the original PDF file.
#'
#' @param pdfFile A character string specifying the path to the PDF file to be converted to PNG format.
#' @param dpi An integer specifying the required DPI (dots per inch) of the converted PNG.
#' @param outDir A character string specifying the directory to output the converted file. If left empty, the converted file will be saved in the same directory as the original file.
#' @return No return value. The function saves the PNG file to the specified location.
#' @examples
#' # Convert all PDF files in a directory to PNG format with 300 DPI
#' pdfs <- list.files(here::here('figs'), pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
#' pdfs |> purrr::walk(~ convert.pdf.to.png(pdfFile = .x, dpi = 300))
#'
#' # Convert a single PDF file to PNG format with 300 DPI and save it in a specific directory
#' convert.pdf.to.png(pdfFile = "example.pdf", dpi = 300, outDir = "output_directory")
#'
#' @export
convert.pdf.to.png <- function(pdfFile, dpi, outDir = NA) {
  
  # Set outdir
  if (is.na(outDir)) {
    directory <- dirname(pdfFile)
  } else {
    directory <- outDir
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
  }
  
  # Create new path
  fileNm <- tools::file_path_sans_ext(basename(pdfFile))
  newFileNm <- paste0(fileNm, ".png")
  newPath <- here::here(directory, newFileNm)
  
  # image_write does not have an overwrite option! Do this manually
  if (file.exists(newPath)) {
    print(paste0("Overwriting file: ", newPath))
    file.remove(newPath)
  }
  
  # Convert & write out
  pngImage <- pdftools::pdf_render_page(pdfFile, page = 1, dpi = dpi)
  writeImage <- magick::image_read(pngImage)
  magick::image_write(image = writeImage, path = newPath, format = "png")
}

