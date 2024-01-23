

auxiliary_pearson_measure <- function(series, lag = 1, features = FALSE) {

  series_length <- length(series$Value)
  phi2 <- auxiliary_phi2_measure(series = series, lag = lag)

  if (features == FALSE) {

    return(series_length * phi2)

  } else {

    return(auxiliary_phi2_measure(series = series, lag = lag,
                        features = TRUE))

  }

}
