

auxiliary_cohens_kappa <- function(series, max_lag = 10, alpha = 0.05) {

  series_length <- length(series$Value)
  vector_mp <- marginal_probabilities(series = series)
  vector_mp_2 <- vector_mp^2
  vector_mp_3 <- vector_mp^3
  values_cohens_kappa <- numeric(max_lag)

  for (i in 1 : max_lag) {

    values_cohens_kappa[i] <- auxiliary_cohens_kappa_function(series = series, lag = i)

  }

  vector_test_statistic <- values_cohens_kappa
  a_variance_1 <- 1 + 2 * sum(vector_mp_3) - 3 * sum(vector_mp_2)
  a_variance_2 <- (1 - sum(vector_mp_2))^2
  a_variance <- 1/series_length * (1 - a_variance_1/a_variance_2)
  a_sd <- sqrt(a_variance)
  vector_p_values <- 2 * (1 - stats::pnorm(abs(vector_test_statistic), mean = -1/series_length, sd = a_sd))
  critical_value <- stats::qnorm(1 - alpha/2, mean = -1/series_length, sd = a_sd)

  return_list <- list(values_cohens_kappa = values_cohens_kappa,
                      vector_p_values = vector_p_values,
                      critical_value = critical_value)

  return(return_list)

}
