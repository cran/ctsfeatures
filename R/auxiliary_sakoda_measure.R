

auxiliary_sakoda_measure <- function(series, lag = 1, features = FALSE) {

  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset

  if (features == FALSE) {

  phi2 <- auxiliary_phi2_measure(series = series, lag = lag, features = features)
  numerator <- n_cat * phi2
  denominator <- (n_cat - 1) * (1 + phi2)


  return(sqrt(numerator/denominator))

  } else {

    phi2 <- auxiliary_phi2_measure(series = series, lag = lag, features = features)
    return(phi2)

  }

}
