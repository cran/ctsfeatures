

auxiliary_total_mixed_correlation_2 <- function(c_series, n_series, lag = 1,
                                      categories, features = FALSE) {
  check_cts(c_series)
  check_cts(n_series)
  series_length <- length(c_series)
  n_categories <- length(categories)
  grid_quantile <- seq(0, 1, by = 0.01)
  l_grid <- base::length(grid_quantile)
  binarized_series <- binarization(series = c_series, categories = categories)
  binarized_series_1 <- binarized_series[(lag + 1) : series_length,]
  binarized_series_2 <- binarized_series[1 : (series_length - lag),]

  correlation_matrix <- matrix(0, nrow = n_categories, ncol = l_grid)

  for (i in 1 : n_categories) {

    for (j in 1 : l_grid) {

    if (lag >= 0) {

      n_series_quantile <- as.numeric(n_series[1 : (series_length - lag)] <= grid_quantile[j])
      correlation_matrix[i, j] <- stats::cor(binarized_series_1[,i], n_series_quantile)^2

    } else {

      n_series_quantile <- as.numeric(n_series[(lag + 1) : series_length] <= grid_quantile[j])
      correlation_matrix[i, j] <- stats::cor(binarized_series_2[,i], n_series_quantile)^2

    }

    }

  }


  vector_integrals <- numeric()

  for (i in 1 : n_categories) {

    vector_integrals[i] <- Bolstad2::sintegral(grid_quantile, correlation_matrix[i,])$int

  }


  if (features == FALSE) {

    return(mean(vector_integrals))

  } else {

    return(vector_integrals)

  }



}
