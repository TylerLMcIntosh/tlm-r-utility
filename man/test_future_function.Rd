% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parallel_process.R
\name{test_future_function}
\alias{test_future_function}
\title{Test a function in both sequential and parallel modes}
\usage{
test_future_function(fun, n_cores = parallel::detectCores(), ...)
}
\arguments{
\item{fun}{A string. The name of the function to be tested.}

\item{n_cores}{Integer. Number of cores to use for parallel execution. Defaults to the number of available cores.}

\item{...}{Additional parameters to pass to the function specified in `fun`.}
}
\value{
A list with the sequential and parallel outputs, and a logical value indicating
if the outputs are identical.
}
\description{
This function tests the performance of a specified function in both sequential
and parallel modes using the `future` package. It prints the time taken for
both modes and checks if the outputs are identical. The future plan is
reset to its original state after execution, regardless of success or failure.
}
\examples{
\dontrun{
test_future_function("some_function", arg1 = 5, arg2 = TRUE)
}
}
