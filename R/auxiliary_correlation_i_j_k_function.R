

correlation_i_j_k_function <- function(series, i_cat, j_cat, k = 1, categories) {

  series_length <- length(series)
  binarized_series <- binarization(series, categories = categories)
  a <- binarized_series[(k + 1) : series_length, i_cat]
  b <- binarized_series[1 : (series_length - k), j_cat]


  return(stats::cor(a, b))

}

