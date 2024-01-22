


auxiliary_gk_tau <- function(series, lag = 1, features = FALSE) {

  check_cts(series$Value)
  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset
  vector_mp <- marginal_probabilities(series = series)
  sum_sq_mp <- sum(vector_mp^2)
  matrix_jp <- joint_probabilities(series = series, lag = lag)
  matrix_mp <- base::matrix(vector_mp, nrow = n_cat, ncol = n_cat, byrow = TRUE)

  matrix_prev <- matrix_jp^2/matrix_mp
  numerator <- sum(matrix_prev) - sum_sq_mp
  denominator <- 1 - sum_sq_mp

  if (features == FALSE) {

  return(numerator/denominator)

  } else {

  return(matrix_prev)

  }

}
