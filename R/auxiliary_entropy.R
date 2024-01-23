

auxiliary_entropy <- function(series, features = FALSE) {

  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset
  vector_mp <- marginal_probabilities(series = series)
  vector_log_mp <- log(vector_mp)
  vector_product <- vector_mp * vector_log_mp


  if (features == FALSE) {

    return((-1/log(n_cat)) * sum(vector_product))

  } else {

    return(vector_product)

  }

}
