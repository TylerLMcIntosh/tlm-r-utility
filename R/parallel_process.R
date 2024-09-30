#' Test a function in both sequential and parallel modes
#'
#' This function tests the performance of a specified function in both sequential
#' and parallel modes using the `future` package. It prints the time taken for
#' both modes and checks if the outputs are identical. The future plan is
#' reset to its original state after execution, regardless of success or failure.
#'
#' @param fun A string. The name of the function to be tested.
#' @param n_cores Integer. Number of cores to use for parallel execution. Defaults to the number of available cores.
#' @param ... Additional parameters to pass to the function specified in `fun`.
#'
#' @return A list with the sequential and parallel outputs, and a logical value indicating
#' if the outputs are identical.
#' @examples
#' \dontrun{
#' test_future_function("some_function", arg1 = 5, arg2 = TRUE)
#' }
#' @export
#' @importFrom future plan multisession sequential
#' @importFrom parallel detectCores
#' @importFrom tictoc tic toc
#' @importFrom glue glue

test_future_function <- function(fun, n_cores = parallel::detectCores(), ...) {
  
  # Get the current future plan
  initial_plan <- future::plan()
  
  on.exit(future::plan(initial_plan))  # Ensure plan is reset upon function exit
  
  # Sequential execution
  print("SEQUENTIAL MODE")
  future::plan("sequential")
  tictoc::tic(glue::glue('Time to run function {fun} in sequence'))
  s_test <- do.call(fun, list(...))
  tictoc::toc()
  
  # Parallel execution
  print("PARALLEL MODE")
  future::plan("multisession", workers = n_cores)
  tictoc::tic(glue::glue('Time to run function {fun} in parallel'))
  p_test <- do.call(fun, list(...))
  tictoc::toc()
  
  # Check if outputs are identical
  output_identical <- identical(s_test, p_test)
  print(glue::glue("Outputs identical? - {output_identical}"))
  
  # Return a list containing the results
  return(list(
    sequential_output = s_test,
    parallel_output = p_test,
    identical = output_identical
  ))
}
