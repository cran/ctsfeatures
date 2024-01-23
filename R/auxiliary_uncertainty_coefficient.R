

auxiliary_uncertainty_coefficient <- function(series, lag = 1, features = FALSE) {

  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset
  vector_mp <- marginal_probabilities(series = series)
  matrix_mp <- vector_mp %*% t(vector_mp)
  matrix_jp <- joint_probabilities(series = series, lag = lag)
  matrix_combined <- matrix_jp/matrix_mp

  matrix_prev <- matrix_jp * log(matrix_combined)
  numerator <- sum(matrix_prev)
  denominator <- sum(vector_mp * log(vector_mp))

  if (features == FALSE) {

    return(-numerator/denominator)

  } else {

    return(matrix_prev)

  }

}
