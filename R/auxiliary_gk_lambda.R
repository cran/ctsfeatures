

auxiliary_gk_lambda <- function(series, lag = 1, features = FALSE) {

  check_cts(series$Value)
  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset
  vector_mp <- marginal_probabilities(series = series)
  max_mp <- max(vector_mp)
  matrix_jp <- joint_probabilities(series = series, lag = lag)
  max_jp <- base::apply(matrix_jp, 2, max)

  numerator <- sum(max_jp) - max_mp
  denominator <- 1 - max_mp

  if (features == FALSE) {

    return(numerator/denominator)

  } else {

    return(max_jp)

  }

}
