
# MBLM ----

#' Fit MBLM (Median-Based Linear Model) Estimator and Visualize
#'
#' This function fits a Theil-Sen or Siegel estimator using median-based linear 
#' modeling (MBLM) and visualizes the results with a scatter plot and fitted 
#' regression line. The type of estimator (Theil-Sen or Siegel) can be controlled 
#' via the `repeated` argument.
#'
#' @param dats A data frame containing the data.
#' @param x The predictor variable (unquoted column name) from the data frame.
#' @param y The response variable (unquoted column name) from the data frame.
#' @param repeated Logical, if `TRUE`, uses the Siegel estimator which allows 
#' for repeated medians; if `FALSE`, uses the Theil-Sen estimator (default: `FALSE`).
#'
#' @return A `ggplot` object showing the scatter plot of `x` vs `y` with the 
#' fitted regression line overlaid.
#'
#' @details
#' The function uses the `mblm` package to fit the linear model using either 
#' the Theil-Sen or Siegel estimator based on the value of the `repeated` argument.
#' It visualizes the fit using `ggplot2` by plotting the data points and 
#' adding a dashed regression line based on the model's coefficients.
#'
#' Non-standard evaluation (NSE) is used to allow unquoted column names for `x` 
#' and `y`. The variables are converted to strings using `deparse(substitute())`, 
#' which allows them to be used in the formula for model fitting and in the plot labels.
#'
#' @examples
#' # Example with Theil-Sen estimator
#' mblm_fit_estimator_and_visualize(mtcars, mpg, disp, repeated = FALSE)
#' 
#' # Example with Siegel estimator
#' mblm_fit_estimator_and_visualize(mtcars, mpg, disp, repeated = TRUE)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_abline labs
#' @importFrom mblm mblm
#' @export
mblm_fit_estimator_and_visualize <- function(dats, x, y, repeated = FALSE) {
  
  # Determine which estimator to use
  estimator <- ifelse(repeated, "siegel estimator", "thiel-sen estimator")
  
  # Convert x and y to string names for formula creation
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  
  # Create the formula dynamically
  formula <- as.formula(paste(y_name, "~", x_name))
  
  # Fit the MBLM model
  fit <- mblm::mblm(formula, data = dats, repeated = repeated)
  
  # Create the plot with ggplot2
  p <- ggplot2::ggplot(dats, ggplot2::aes(x = {{x}}, y = {{y}})) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = fit$coefficients["(Intercept)"],
                         slope = fit$coefficients[x_name],  # Access slope using variable name
                         linetype = "dashed",
                         linewidth = 0.8) +
    ggplot2::labs(
      title = paste("MBLM Fit for", y_name, "vs", x_name),
      x = x_name,
      y = y_name,
      caption = paste0("Coefficient: ", fit$coefficients[x_name], "\nUsing ", estimator)
    )
  
  return(p)
}


#' Get MBLM Coefficients by Group
#'
#' This function estimates the slope coefficients of a linear relationship between two variables (`x` and `y`)
#' for each group in a dataset, using either the Siegel or Theil-Sen estimator (from the `mblm` package).
#'
#' @param dats A data frame containing the variables.
#' @param x The independent variable.
#' @param y The dependent variable.
#' @param group The grouping variable. The function will estimate coefficients for each unique value of this group.
#' @param repeated Logical, if `TRUE`, the Siegel estimator is used, otherwise the Theil-Sen estimator is applied. Defaults to `FALSE`.
#'
#' @return A tibble with three columns: `group` (unique group values), `coefficient` (estimated slope coefficients), 
#' and `estimator` (the name of the estimator used).
#'
#' @importFrom dplyr pull filter tibble group_split
#' @importFrom purrr map
#' @importFrom mblm mblm
#'
#' @examples
#' # Example usage
#' df <- data.frame(group = rep(c("A", "B"), each = 10), x = rnorm(20), y = rnorm(20))
#' mblm_get_coefficients_by_group(df, x, y, group, repeated = FALSE)
#'
#' @export
mblm_get_coefficients_by_group <- function(dats, x, y, group, repeated = FALSE) {
  
  # Determine which estimator to use
  estimator <- ifelse(repeated, "siegel estimator", "thiel-sen estimator")
  
  # Convert x and y to string names for formula creation
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  
  # Get unique values for the group variable
  unique_groups <- dats |> dplyr::pull({{group}}) |> unique()
  
  # Create a list of data frames, one for each group
  subset_list <- dats |> dplyr::group_split({{group}})
  
  # Estimate coefficients for each group
  estimators <- subset_list |> purrr::map(function(subset_data) {
    formula <- as.formula(paste(y_name, "~", x_name))
    fit <- mblm::mblm(formula, data = subset_data, repeated = repeated)
    coef <- fit$coefficients[x_name]
    return(coef)
  })
  
  # Combine unique groups and estimators into a data frame
  results <- dplyr::tibble(
    group = unique_groups,
    coefficient = unlist(estimators),
    estimator = estimator
  )
  
  return(results)
}
