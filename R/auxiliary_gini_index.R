

auxiliary_gini_index <- function(series, categories) {

  check_cts(series)
  n_categories <- length(categories)
  vector_mp <- marginal_probabilities(series = series, categories = categories)
  sum_sq_mp <- sum(vector_mp^2)


  return((n_categories/(n_categories - 1)) * (1 - sum_sq_mp))

}
