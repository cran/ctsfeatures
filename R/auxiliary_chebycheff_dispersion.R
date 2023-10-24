

auxiliary_chebycheff_dispersion <- function(series, categories) {

  check_cts(series)
  n_categories <- length(categories)
  vector_mp <- marginal_probabilities(series = series, categories = categories)
  max_mp <- max(vector_mp)


  return((n_categories/(n_categories - 1)) * (1 - max_mp))

}
