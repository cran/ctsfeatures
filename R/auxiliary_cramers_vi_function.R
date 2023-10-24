


auxiliary_cramers_vi_function <- function(series, lag = 1, categories, features = FALSE) {

  check_cts(series)
  n_categories <- length(categories)
  phi2 <- auxiliary_phi2_measure(series = series, lag = lag, categories = categories,
                       features = features)

  if (features == FALSE) {

    return(sqrt(phi2/(n_categories - 1)))

  } else {

    return(phi2)

  }


}


