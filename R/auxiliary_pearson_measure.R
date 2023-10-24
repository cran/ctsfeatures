

auxiliary_pearson_measure <- function(series, lag = 1, categories, features = FALSE) {

  check_cts(series)
  series_length <- length(series)
  phi2 <- auxiliary_phi2_measure(series = series, lag = lag, categories = categories)

  if (features == FALSE) {

    return(series_length * phi2)

  } else {

    return(auxiliary_phi2_measure(series = series, lag = lag, categories = categories,
                        features = TRUE))

  }

}
