% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geographic.R
\name{st_write_shp}
\alias{st_write_shp}
\title{Write Shapefile to a New Directory and Create a Zipped Version}
\usage{
st_write_shp(shp, location, filename, zip_only = FALSE, overwrite = FALSE)
}
\arguments{
\item{shp}{An `sf` object to write as a shapefile.}

\item{location}{A character string specifying the path of the directory to create the new file-specific subdirectory in.}

\item{filename}{A character string specifying the name of the file without the `.shp` extension.}

\item{zip_only}{A logical value indicating whether the original (unzipped) files should be removed after zipping. Defaults to `FALSE`.}

\item{overwrite}{A logical value indicating whether existing files should be overwritten. Defaults to `FALSE`.}
}
\value{
No return value. The function writes a shapefile to a specified directory, optionally zips the files, and manages file cleanup based on user input.
}
\description{
This function writes an `sf` object to a shapefile in a new, file-specific directory and optionally creates a zipped version of the shapefile.
It also allows for the removal of the original unzipped files and handles overwriting existing files.
}
\examples{
\dontrun{
# Example usage
st_write_shp(shp = prepped_for_parks_etal,
             location = here::here("data/derived"),
             filename = "career_lba_for_parks_v1",
             zip_only = TRUE,
             overwrite = TRUE)
}
}
