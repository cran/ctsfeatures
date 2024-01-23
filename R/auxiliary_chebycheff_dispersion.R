

auxiliary_chebycheff_dispersion <- function(series) {

  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset
  vector_mp <- marginal_probabilities(series = series)
  max_mp <- max(vector_mp)


  return((n_cat/(n_cat - 1)) * (1 - max_mp))

}

