


auxiliary_cramers_vi_function <- function(series, lag = 1, features = FALSE) {

  check_cts(series$Value)
  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset
  phi2 <- auxiliary_phi2_measure(series = series, lag = lag,
                       features = features)

  if (features == FALSE) {

    return(sqrt(phi2/(n_cat - 1)))

  } else {

    return(phi2)

  }


}


