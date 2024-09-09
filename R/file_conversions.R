
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