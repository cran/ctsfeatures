

auxiliary_sakoda_measure <- function(series, lag = 1, categories, features = FALSE) {

  check_cts(series)
  if (features == FALSE) {

  n_categories <- length(categories)
  phi2 <- auxiliary_phi2_measure(series = series, lag = lag, categories = categories, features = features)
  numerator <- n_categories * phi2
  denominator <- (n_categories - 1) * (1 + phi2)


  return(sqrt(numerator/denominator))

  } else {

    phi2 <- auxiliary_phi2_measure(series = series, lag = lag, categories = categories, features = features)
    return(phi2)

  }

}
