% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_access.R
\name{access_data_lcmap_v13_stac_year_range}
\alias{access_data_lcmap_v13_stac_year_range}
\title{Access LCMAP v13 Data via STAC}
\source{
STAC API: \url{https://planetarycomputer.microsoft.com/api/stac/v1}
}
\usage{
access_data_lcmap_v13_stac_year_range(earliest_year, latest_year, aoi)
}
\arguments{
\item{earliest_year}{Integer or character representing the earliest year of interest.}

\item{latest_year}{Integer or character representing the latest year of interest. Must be equal to or greater than the earliest year.}

\item{aoi}{An `sf` object representing the area of interest (AOI). Must have a valid CRS, which will be transformed to EPSG:4326.}
}
\value{
A `SpatRaster` object representing the LCMAP data for the specified years and AOI, with each year in a layer named "lcmap_[YEAR]"
}
\description{
This function retrieves LCMAP (Land Change Monitoring, Assessment, and Projection) data for a specified range of years
and area of interest (AOI) using the STAC API. It downloads the LCMAP raster data and returns it as a `SpatRaster` object.
}
\details{
The function queries the Microsoft Planetary Computer's STAC API for the USGS LCMAP dataset. It constructs the correct 
vsicurl URL to stream the data and processes it using GDAL's `warp` function. The returned raster is cropped to the 
specified AOI.
}
\examples{
\dontrun{
aoi <- sf::st_as_sfc(sf::st_bbox(c(xmin = -120, xmax = -119, ymin = 34, ymax = 35), crs = 4326))
lcmap_raster <- access_data_lcmap_v13_stac_year_range(2000, 2003, aoi)
}
}
\references{
Code adapted from: \url{https://stacspec.org/en/tutorials/1-download-data-using-r/}
}
