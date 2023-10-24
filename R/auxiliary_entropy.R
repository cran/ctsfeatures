

auxiliary_entropy <- function(series, categories, features = FALSE) {

  check_cts(series)
  n_categories <- length(categories)
  vector_mp <- marginal_probabilities(series = series, categories = categories)
  vector_log_mp <- log(vector_mp)
  vector_product <- vector_mp * vector_log_mp


  if (features == FALSE) {

    return((-1/log(n_categories)) * sum(vector_product))

  } else {

    return(vector_product)

  }

}
