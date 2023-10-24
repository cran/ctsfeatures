

auxiliary_total_mixed_correlation_1 <- function(c_series, n_series, lag = 1,
                                      categories, features = FALSE) {
  check_cts(c_series)
  check_cts(n_series)
  series_length <- length(c_series)
  n_categories <- length(categories)
  binarized_series <- binarization(series = c_series, categories = categories)
  binarized_series_1 <- binarized_series[(lag + 1) : series_length,]
  binarized_series_2 <- binarized_series[1 : (series_length - lag),]
  n_series_1 <- n_series[(lag + 1) : series_length]
  n_series_2 <- n_series[1 : (series_length - lag)]

  correlation_measure <- numeric(n_categories)

  for (i in 1 : n_categories) {

    if (lag >= 0) {

      correlation_measure[i] <- stats::cor(binarized_series_1[,i], n_series_2)

    } else {

      correlation_measure[i] <- stats::cor(binarized_series_2[,i], n_series_1)

    }

  }


  sum_sq_correlation_measure <- sum(correlation_measure^2)

  if (features == FALSE) {

  return(sum_sq_correlation_measure/n_categories)

  } else {

  return(correlation_measure)

  }



}
