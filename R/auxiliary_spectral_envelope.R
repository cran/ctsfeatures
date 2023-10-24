

auxiliary_spectral_envelope <- function(binarized_series, plot = TRUE) {

  check_cts(binarized_series)
  if (plot == FALSE) {

    return(astsa::specenv(binarized_series, plot = plot))

  } else {

    return(astsa::specenv(binarized_series, plot = plot))

  }

}



