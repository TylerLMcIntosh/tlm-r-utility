#' Convert PDF to PNG
#'
#' This function converts a PDF file to a PNG image with a specified DPI (dots per inch). It allows the output to be saved in a specified directory or in the same directory as the original PDF file.
#'
#' @param pdf_file A character string specifying the path to the PDF file to be converted to PNG format.
#' @param dpi An integer specifying the required DPI (dots per inch) of the converted PNG.
#' @param out_dir A character string specifying the directory to output the converted file. If left empty, the converted file will be saved in the same directory as the original file.
#' @return No return value. The function saves the PNG file to the specified location.
#' @examples
#' # Convert all PDF files in a directory to PNG format with 300 DPI
#' pdfs <- list.files(here::here('figs'), pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
#' pdfs |> purrr::walk(~ convert_pdf_to_png(pdf_file = .x, dpi = 300))
#'
#' # Convert a single PDF file to PNG format with 300 DPI and save it in a specific directory
#' convert_pdf_to_png(pdf_file = "example.pdf", dpi = 300, out_dir = "output_directory")
#'
#' @importFrom magick image_read image_write
#' @importFrom here here
#' @importFrom pdftools pdf_render_page
#' @export
convert_pdf_to_png <- function(pdf_file, dpi, out_dir = NA) {
  
  # Set output directory
  if (is.na(out_dir)) {
    directory <- dirname(pdf_file)
  } else {
    directory <- out_dir
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
  }
  
  # Create new path
  file_nm <- tools::file_path_sans_ext(basename(pdf_file))
  new_file_nm <- paste0(file_nm, ".png")
  new_path <- here::here(directory, new_file_nm)
  
  # image_write does not have an overwrite option! Do this manually
  if (file.exists(new_path)) {
    print(paste0("Overwriting file: ", new_path))
    file.remove(new_path)
  }
  
  # Convert & write out
  png_image <- pdftools::pdf_render_page(pdf_file, page = 1, dpi = dpi)
  write_image <- magick::image_read(png_image)
  magick::image_write(image = write_image, path = new_path, format = "png")
}


#' Convert Image File to CMYK Format Using ImageMagick
#'
#' This function converts an image file to CMYK format using ImageMagick. ImageMagick must be installed and accessible via the command line for this function to work.
#'
#' @param file A character string specifying the path to the file to convert to CMYK format.
#' @param out_dir A character string specifying the directory to output the converted file. If left empty, the converted file will be saved in the same directory as the original file.
#' @return No return value. The function outputs the converted file to the specified directory.
#' @examples
#' # Convert all PNG files in a directory to CMYK format
#' pngs <- list.files(here::here('figs'), pattern = "\\.png$", full.names = TRUE, ignore.case = TRUE)
#' pngs |> purrr::walk(~ convert_to_cmyk(file = .x))
#'
#' # Convert a single file to CMYK format and save it in the same directory
#' convert_to_cmyk(file = "example.png")
#'
#' @export
convert_to_cmyk <- function(file, out_dir = NA) {
  
  # Check if the file exists
  if (!file.exists(file)) {
    warning(paste("File does not exist:", file))
    return()
  }
  
  # Set output directory
  if (is.na(out_dir)) {
    directory <- dirname(file)
  } else {
    directory <- out_dir
  }
  
  # Construct the new filename by appending '_cmyk' before the file extension
  file_path <- normalizePath(file, mustWork = FALSE)
  file_nm <- tools::file_path_sans_ext(basename(file_path))
  file_ext <- tools::file_ext(file_path)
  new_file_nm <- paste0(file_nm, "_cmyk.", file_ext)
  new_path <- file.path(directory, new_file_nm)
  
  # Form the command to convert the file to CMYK
  command <- sprintf('magick "%s" -colorspace CMYK "%s"', file_path, new_path)
  
  # Execute the command using system()
  system(command, intern = FALSE)
  
  # Print the status message
  cat("Converted:", file_path, "to CMYK and saved as:", new_file_nm, "\n")
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

#' Create a Bivariate Palette Using a Color Vector
#'
#' Generates a biscale-compatible palette from a set of nine hex color codes.
#'
#' @param p_vec A character vector of exactly 9 hex color codes.
#' @param flip A logical value. If `TRUE`, the palette is reversed.
#'
#' @return A list containing:
#'   - `biscale_pal`: A named character vector of hex colors mapped to bivariate classes.
#'   - `rgb_df`: A data frame with class identifiers and corresponding RGB values.
#'
#' @details The function creates a bivariate color palette compatible with biscale mapping.
#'   The `pals` package provides various palettes that can be used to define `p_vec`.
#'   See available palettes here: \url{https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html}.
#'
#' @examples
#' p_vec <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b")
#' pals_bi_3(p_vec)
#' pals_bi_3(p_vec, flip = TRUE)
#'
#' @export
pals_bi_3 <- function(p_vec, flip = FALSE) {
  if (length(p_vec) != 9) {
    stop("p_vec must contain exactly 9 hex color codes.")
  }
  
  if (flip) {
    p_vec <- rev(p_vec)
  }
  
  # Create biscale-compatible palette
  biscale_pal <- setNames(p_vec, c("1-1", "2-1", "3-1", "1-2", "2-2", "3-2", "1-3", "2-3", "3-3"))
  
  # Create dataframe
  class <- c(11, 12, 13, 21, 22, 23, 31, 32, 33)
  rgb_list <- lapply(p_vec, hex_to_rgb)
  rgb_df <- data.frame(class, do.call(rbind, rgb_list), stringsAsFactors = FALSE)
  
  return(list(biscale_pal = biscale_pal, rgb_df = rgb_df))
}
