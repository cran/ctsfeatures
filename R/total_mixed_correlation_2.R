

#' Computes the total mixed q-correlation between a categorical and a
#' real-valued time series
#'
#' \code{total_mixed_correlation_2} returns the total mixed q-correlation
#' between a categorical and a real-valued time series
#'
#' @param c_series A CTS.
#' @param n_series A real-valued time series.
#' @param lag The considered lag (default is 1).
#' @param categories A vector of type factor containing the corresponding
#' categories for the CTS.
#' @param features Logical. If \code{features = FALSE} (default), the value of the total q-correlation is returned. Otherwise, the function
#' returns a vector with the individual components of the total l-correlation
#' @return If \code{features = FALSE} (default), returns the value of the total q-correlation. Otherwise, the function
#' returns a vector of features, i.e., the vector contains the features employed to compute the
#' total q-correlation.
#' @examples
#' tmc2 <- total_mixed_correlation_2(c_series = SyntheticData1$data[[1]],
#' n_series = rnorm(600),
#' categories = c('1', '2', '3')) # Computing the total mixed q-correlation
#' # between the first series in dataset SyntheticData1 and white noise
#' feature_vector <- total_mixed_correlation_2(c_series = SyntheticData1$data[[1]],
#' n_series = rnorm(600),
#' categories = c('1', '2', '3'), features = TRUE) # Computing the corresponding
#' # vector of features
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, and
#' the binarized time series, which is defined as
#' \eqn{\overline{\boldsymbol Y}_t=\{\overline{\boldsymbol Y}_1, \ldots, \overline{\boldsymbol Y}_T\}},
#' with \eqn{\overline{\boldsymbol Y}_k=(\overline{Y}_{k,1}, \ldots, \overline{Y}_{k,r})^\top}
#' such that \eqn{\overline{Y}_{k,i}=1} if \eqn{\overline{X}_k=i} (\eqn{k=1,\ldots,T,
#' , i=1,\ldots,r}), the function computes the estimated total mixed q-correlation given by
#' \deqn{\widehat{\Psi}_2(l)=\frac{1}{r}\sum_{i=1}^{r}\int_{0}^{1}\widehat{\psi}^\rho_{i}(l)^2d\rho,} where
#' \eqn{\widehat{\psi}_{i}^\rho(l)=\widehat{Corr}\big(Y_{t,i}, I(Z_{t-l}\leq q_{Z_t}(\rho)) \big)}, with
#' \eqn{\overline{Z}_t=\{\overline{Z}_1,\ldots, \overline{Z}_T\}} being a
#' \eqn{T}-length real-valued time series, \eqn{\rho \in (0, 1)} a probability
#' level, \eqn{I(\cdot)} the indicator function and \eqn{q_{Z_t}} the quantile
#' function of the corresponding real-valued process. If \code{features = TRUE}, the function
#' returns a vector whose components are the quantities \eqn{\int_{0}^{1}\widehat{\psi}^\rho_{i}(l)^2d\rho},
#' \eqn{i=1,2, \ldots,r}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @export

total_mixed_correlation_2 <- function(c_series, n_series, lag = 1,
                                      categories, features = FALSE) {
  check_cts(c_series)
  check_cts(n_series)
  series_length <- length(c_series)
  n_categories <- length(categories)
  grid_quantile <- seq(0, 1, by = 0.01)
  l_grid <- base::length(grid_quantile)
  binarized_series <- binarization(series = c_series, categories = categories)
  binarized_series_1 <- binarized_series[(lag + 1) : series_length,]
  binarized_series_2 <- binarized_series[1 : (series_length - lag),]

  correlation_matrix <- matrix(0, nrow = n_categories, ncol = l_grid)

  for (i in 1 : n_categories) {

    for (j in 1 : l_grid) {

    if (lag >= 0) {

      n_series_quantile <- as.numeric(n_series[1 : (series_length - lag)] <= grid_quantile[j])
      correlation_matrix[i, j] <- stats::cor(binarized_series_1[,i], n_series_quantile)^2

    } else {

      n_series_quantile <- as.numeric(n_series[(lag + 1) : series_length] <= grid_quantile[j])
      correlation_matrix[i, j] <- stats::cor(binarized_series_2[,i], n_series_quantile)^2

    }

    }

  }


  vector_integrals <- numeric()

  for (i in 1 : n_categories) {

    vector_integrals[i] <- Bolstad2::sintegral(grid_quantile, correlation_matrix[i,])$int

  }


  if (features == FALSE) {

    return(mean(vector_integrals))

  } else {

    return(vector_integrals)

  }



}
