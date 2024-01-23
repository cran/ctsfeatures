

auxiliary_phi2_measure <- function(series, lag = 1, features = FALSE) {

  vector_mp <- marginal_probabilities(series = series)
  matrix_mp <- vector_mp %*% t(vector_mp)
  matrix_jp <- joint_probabilities(series = series, lag = lag)
  matrix_prev <- (matrix_jp - matrix_mp)^2
  matrix_combined <- matrix_prev/matrix_mp

  if (features == FALSE) {

    return(sum(matrix_combined))

  } else {

    return(matrix_combined)

  }

}
