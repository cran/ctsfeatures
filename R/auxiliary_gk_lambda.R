

auxiliary_gk_lambda <- function(series, lag = 1, categories, features = FALSE) {

  check_cts(series)
  vector_mp <- marginal_probabilities(series = series, categories = categories)
  max_mp <- max(vector_mp)
  matrix_jp <- joint_probabilities(series = series, lag = lag,
                                   categories = categories)
  max_jp <- base::apply(matrix_jp, 2, max)

  numerator <- sum(max_jp) - max_mp
  denominator <- 1 - max_mp

  if (features == FALSE) {

    return(numerator/denominator)

  } else {

    return(max_jp)

  }

}
