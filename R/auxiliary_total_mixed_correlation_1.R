

auxiliary_total_mixed_correlation_1 <- function(c_series, n_series, lag = 1, features = FALSE) {

  series_length <- length(c_series$Value) # Series length
  categories <- levels(c_series$Value)
  n_cat <- length(categories) # Number of categories in the dataset
  binarized_series <- binarization(series = c_series)
  binarized_series_1 <- binarized_series[(lag + 1) : series_length,]
  binarized_series_2 <- binarized_series[1 : (series_length - lag),]
  n_series_1 <- n_series$Value[(lag + 1) : series_length]
  n_series_2 <- n_series$Value[1 : (series_length - lag)]

  correlation_measure <- numeric(n_cat)

  for (i in 1 : n_cat) {

    if (lag >= 0) {

      correlation_measure[i] <- stats::cor(binarized_series_1[,i], n_series_2)

    } else {

      correlation_measure[i] <- stats::cor(binarized_series_2[,i], n_series_1)

    }

  }


  sum_sq_correlation_measure <- sum(correlation_measure^2)

  if (features == FALSE) {

  return(sum_sq_correlation_measure/n_cat)

  } else {

  return(correlation_measure)

  }



}
