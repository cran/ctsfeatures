

auxiliary_gini_index <- function(series) {

  check_cts(series$Value)
  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset
  vector_mp <- marginal_probabilities(series = series)
  sum_sq_mp <- sum(vector_mp^2)


  return((n_cat/(n_cat - 1)) * (1 - sum_sq_mp))

}
