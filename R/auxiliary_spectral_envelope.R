

auxiliary_spectral_envelope <- function(binarized_series, plot = TRUE) {

  if (plot == FALSE) {

    return(astsa::specenv(binarized_series, plot = plot))

  } else {

    return(astsa::specenv(binarized_series, plot = plot))

  }

}



