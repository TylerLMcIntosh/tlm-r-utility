# General analysis ----


#' Normalize Values to a [0,1] Scale
#'
#' This function rescales numeric values to a 0-to-1 range.
#'
#' @param x A numeric vector or raster object to be normalized.
#'
#' @return A numeric vector or raster with values normalized between 0 and 1.
#'
#' @examples
#' normalize(c(10, 20, 30, 40))
#' normalize(c(5, 15, NA, 25, 35))
#'
#' @export
normalize <- function(x) {
  return(x - min(x[], na.rm = TRUE)) / (max(x[], na.rm = TRUE) - min(x[], na.rm = TRUE))
}

#' Scatterplot with Linear Model and Summary Statistics using ggplot2
#'
#' Generates a scatterplot of two numeric variables with a fitted linear regression line.
#' Displays the R-squared and p-value in the plot caption.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param x A character string naming the predictor (x-axis) variable. Must be numeric.
#' @param y A character string naming the response (y-axis) variable. Must be numeric.
#' @param filename Optional. A file name to which the plot will be saved. If \code{NULL}, the plot is not saved. Defaults to \code{NULL}. 
#'
#' @details
#' This function checks that the provided variables exist in the data frame and are numeric.
#' It fits a linear model of \code{y ~ x} using \code{lm()}, extracts the R-squared and p-value 
#' from the model summary, and includes them as a caption in the resulting ggplot2-based scatterplot.
#'
#' The function uses \code{ggplot2::geom_smooth()} to plot the linear regression line with 
#' a 95% confidence band.
#'
#' @return A \code{ggplot} object representing the scatterplot with the fitted linear model.
#'
#' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth labs theme_minimal
#' @importFrom stats lm as.formula
#' @export
plot_lm_ggplot <- function(data, x, y, filename = NULL) {
  # Error checking
  if (!x %in% names(data)) stop("Variable 'x' not found in data.")
  if (!y %in% names(data)) stop("Variable 'y' not found in data.")
  if (!is.numeric(data[[x]]) || !is.numeric(data[[y]])) {
    stop("Both x and y must be numeric variables.")
  }
  
  # Fit linear model
  formula <- as.formula(paste(y, "~", x))
  model <- lm(formula, data = data)
  model_summary <- summary(model)
  
  # Extract R-squared and p-value
  r_squared <- round(model_summary$r.squared, 4)
  p_value <- round(coef(model_summary)[2, 4], 4)
  caption_text <- paste0("R² = ", r_squared, ", p-value = ", p_value)
  
  # Generate plot
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_point(color = "steelblue", alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, fill = "purple", color = "darkblue") +
    labs(
      title = paste("Linear Model:", y, "vs", x),
      x = x,
      y = y,
      caption = caption_text
    ) +
    theme_minimal()
  
  if(!is.null(filename)) {
    ggsave(filename = filename,
           plot = p)
    
  }
  return(p)
}



#' Improved Correlation Plot with Significance Testing
#'
#' Creates a correlation matrix plot using Pearson's r, displaying only statistically 
#' significant correlations. Supports saving to file and custom graphical parameters.
#'
#' @param df A data frame containing the variables to be correlated.
#' @param variables A character vector of column names in \code{df} to include in the correlation plot.
#' @param title A character string for the main title of the plot. Defaults to \code{"Correlations"}.
#' @param filename Optional. A file name to which the plot will be saved as a PNG. If \code{NULL}, the plot is not saved. Defaults to \code{NULL}.
#' @param units A character string specifying the units for the image dimensions if saving to file. Defaults to \code{"px"}.
#' @param width Width of the image in pixels (or other units). Defaults to \code{900}.
#' @param height Height of the image in pixels (or other units). Defaults to \code{900}.
#' @param ... Additional arguments passed to \code{corrplot::corrplot()}.
#'
#' @details
#' This function computes pairwise Pearson correlations and corresponding p-values using 
#' \code{Hmisc::rcorr}. It then plots the upper triangle of the correlation matrix using 
#' \code{corrplot::corrplot}, displaying only statistically significant correlations 
#' (p <= 0.05) and blanking out non-significant ones.
#'
#' If a filename is provided, the plot is saved as a PNG file using the specified dimensions.
#'
#' @return A correlation plot is displayed or saved. The function does not return an R object.
#'
#' @importFrom Hmisc rcorr
#' @importFrom corrplot corrplot
#' @export
corrplot_improved <- function(df, variables, title = "Correlations", 
                              filename = NULL, units = "px", 
                              width = 900, height = 900, ...) {
  # Load required packages
  if (!requireNamespace("Hmisc", quietly = TRUE) ||
      !requireNamespace("corrplot", quietly = TRUE)) {
    stop("Packages 'Hmisc' and 'corrplot' are required.")
  }
  
  # Validate input
  if (!all(variables %in% names(df))) {
    missing_vars <- setdiff(variables, names(df))
    stop("The following variables are not in the dataframe: ", 
         paste(missing_vars, collapse = ", "))
  }
  
  # Check all variables are numeric
  if (!all(sapply(df[variables], is.numeric))) {
    non_numeric <- variables[!sapply(df[variables], is.numeric)]
    stop("The following variables are not numeric: ", 
         paste(non_numeric, collapse = ", "))
  }
  
  # Compute correlation matrix and p-values
  cor_data <- as.matrix(df[variables])
  cor_results <- Hmisc::rcorr(cor_data)
  cor_mat <- cor_results$r
  p_mat <- cor_results$P
  
  # Open graphics device if filename is given
  if (!is.null(filename)) {
    png(filename = filename, units = units, width = width, height = height)
  }
  
  # Plot
  corrplot::corrplot(cor_mat, method = "number", type = "upper", 
                     tl.col = "black", tl.srt = 45,
                     p.mat = p_mat, sig.level = 0.05, insig = "blank",
                     diag = FALSE,
                     number.cex = 2,
                     cl.cex = 1.5,
                     tl.cex = 1.5,
                     col = colorRampPalette(c("blue", "purple", "red"))(200),
                     ...)
  
  title(main = title, line = 1, adj = 0, cex.main = 2.5)
  mtext("Values are Pearson's r\nBlank cells indicate non-significant correlations (p > 0.05)", 
        side = 1, line = 3, adj = 0, cex = 1.2)
  
  if (!is.null(filename)) dev.off()
}


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
