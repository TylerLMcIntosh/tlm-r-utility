
#' Fit MBLM (Median-Based Linear Model) Estimator and Visualize
#'
#' This function fits a Theil-Sen or Siegel estimator using median-based linear 
#' modeling (MBLM) and visualizes the results with a scatter plot and fitted 
#' regression line. The type of estimator (Theil-Sen or Siegel) can be 
#' controlled via the `repeated` argument.
#'
#' @param dats A data frame containing the data.
#' @param x The predictor variable (unquoted column name) from the data frame.
#' @param y The response variable (unquoted column name) from the data frame.
#' @param repeated Logical, if `TRUE`, uses the Siegel estimator which allows for 
#' repeated medians; if `FALSE`, uses the Theil-Sen estimator (default: `FALSE`).
#'
#' @return A `ggplot` object showing the scatter plot of `x` vs `y` with the 
#' fitted regression line overlaid.
#'
#' @details
#' The function uses the `mblm` package to fit the linear model using either 
#' the Theil-Sen or Siegel estimator based on the value of the `repeated` argument.
#' It also visualizes the fit using `ggplot2` by plotting the data points and 
#' adding a dashed regression line based on the model's coefficients.
#'
#' Non-standard evaluation (NSE) is used to allow unquoted column names for `x` and `y`. 
#' The `rlang::enquo()` function captures the column names, which are converted to 
#' strings for constructing the model formula and labeling the plot.
#'
#' @examples
#' # Example with Theil-Sen estimator
#' fit_mblm_estimator_and_visualize(mtcars, mpg, disp, repeated = FALSE)
#' 
#' # Example with Siegel estimator
#' fit_mblm_estimator_and_visualize(mtcars, mpg, disp, repeated = TRUE)
#'
#' @importFrom rlang enquo as_name
#' @importFrom ggplot2 ggplot aes geom_point geom_abline labs
#' @importFrom mblm mblm
#' @export
fit_mblm_estimator_and_visualize <- function(dats, x, y, repeated = FALSE) {
  
  if(repeated) {
    estimator <- "siegel estimator"
  } else {
    estimator <- "thiel-sen estimator"
  }
  
  # Capture unquoted column names
  x <- enquo(x)
  y <- enquo(y)
  
  # Convert to strings for formula
  formula <- as.formula(paste(rlang::as_name(y), "~", rlang::as_name(x)))
  
  # Fit the mblm model
  fit <- mblm::mblm(formula, data = dats, repeated = repeated)
  
  # Create the plot
  p <- ggplot(dats, aes(x = !!x, y = !!y)) +
        geom_point() +  # Use points for continuous data
        geom_abline(intercept = fit$coefficients["(Intercept)"],
                    slope = fit$coefficients[rlang::as_name(x)],  # Access slope using the variable name
                    linetype = "dashed",
                    linewidth = 0.8) +
        labs(title = paste("MBLM Fit for", rlang::as_name(y), "vs", rlang::as_name(x)),
             x = rlang::as_name(x),
             y = rlang::as_name(y),
             caption = paste0("Coefficient: ", fit$coefficients[rlang::as_name(x)], "\nUsing ", estimator))
  
  return(p)
}