

auxiliary_cramers_vi <- function(series, max_lag = 10, alpha = 0.05) {

  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset
  values_cramers_vi <- numeric(max_lag)

  for (i in 1 : max_lag) {

    values_cramers_vi[i] <- auxiliary_cramers_vi_function(series = series, lag = i)

  }

  vector_test_statistic <- series_length * (n_cat - 1) * values_cramers_vi^2
  vector_p_values <- 1 - stats::pchisq(vector_test_statistic, df = (n_cat - 1)^2)

  critical_value_prev <- stats::qchisq(1 - alpha, df = (n_cat - 1)^2)
  critical_value <- sqrt(critical_value_prev/(series_length * (n_cat - 1)))

  return_list <- list(values_cramers_vi = values_cramers_vi,
                      vector_p_values = vector_p_values,
                      critical_value = critical_value)

  return(return_list)

}
