

auxiliary_cramers_vi <- function(series, categories, max_lag = 10, alpha = 0.05) {

  series_length <- length(series)
  n_categories <- length(categories)
  values_cramers_vi <- numeric(max_lag)

  for (i in 1 : max_lag) {

    values_cramers_vi[i] <- cramers_vi(series = series, lag = i,
                                       categories = categories)

  }

  vector_test_statistic <- series_length * (n_categories - 1) * values_cramers_vi^2
  vector_p_values <- 1 - stats::pchisq(vector_test_statistic, df = (n_categories - 1)^2)

  critical_value_prev <- stats::qchisq(1 - alpha, df = (n_categories - 1)^2)
  critical_value <- sqrt(critical_value_prev/(series_length * (n_categories - 1)))

  return_list <- list(values_cramers_vi = values_cramers_vi,
                      vector_p_values = vector_p_values,
                      critical_value = critical_value)

  return(return_list)

}
