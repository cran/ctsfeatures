

auxiliary_cohens_kappa_function <- function(series, lag = 1, features = FALSE) {

  check_cts(series$Value)
  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset
  vector_mp <- marginal_probabilities(series = series)
  vector_numerator <- numeric()

  for (i in 1 : n_cat) {

    vector_numerator[i] <- p_i_j_k_function(series = series, i_cat = categories[i], j_cat = categories[i],
                                            k = lag) - vector_mp[i]^2

  }

  numerator <- sum(vector_numerator)
  denominator <- 1 - sum(vector_mp^2)

  if (features == FALSE) {

    return(numerator/denominator)

  } else {

    return(vector_numerator)

  }

}
