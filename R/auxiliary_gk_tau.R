


auxiliary_gk_tau <- function(series, lag = 1, categories, features = FALSE) {

  check_cts(series)
  n_cat <- length(categories)
  vector_mp <- marginal_probabilities(series = series, categories = categories)
  sum_sq_mp <- sum(vector_mp^2)
  matrix_jp <- joint_probabilities(series = series, lag = lag,
                                   categories = categories)
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
