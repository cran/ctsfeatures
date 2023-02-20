

#' Computes the total mixed l-correlation between a categorical and a
#' real-valued time series
#'
#' \code{total_mixed_correlation_1} returns the total mixed l-correlation
#' between a categorical and a real-valued time series
#'
#' @param c_series A CTS.
#' @param n_series A real-valued time series.
#' @param lag The considered lag (default is 1).
#' @param categories A vector of type factor containing the corresponding
#' categories for the CTS.
#' @param features Logical. If \code{features = FALSE} (default), the value of the total l-correlation is returned. Otherwise, the function
#' returns a vector with the individual components of the total l-correlation
#' @return If \code{features = FALSE} (default), returns the value of the total l-correlation. Otherwise, the function
#' returns a vector of features, i.e., the vector contains the features employed to compute the
#' total l-correlation.
#' @examples
#' tmc1 <- total_mixed_correlation_1(c_series = SyntheticData1$data[[1]],
#' n_series = rnorm(600), categories = c('1', '2', '3')) # Computing the total mixed l-correlation
#' # between the first series in dataset SyntheticData1 and white noise
#' feature_vector <- total_mixed_correlation_1(c_series = SyntheticData1$data[[1]],
#' n_series = rnorm(600), categories = c('1', '2', '3'), features = TRUE) # Computing the corresponding
#' # vector of features
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, and
#' the binarized time series, which is defined as
#' \eqn{\overline{\boldsymbol Y}_t=\{\overline{\boldsymbol Y}_1, \ldots, \overline{\boldsymbol Y}_T\}},
#' with \eqn{\overline{\boldsymbol Y}_k=(\overline{Y}_{k,1}, \ldots, \overline{Y}_{k,r})^\top}
#' such that \eqn{\overline{Y}_{k,i}=1} if \eqn{\overline{X}_k=i} (\eqn{k=1,\ldots,T,
#' , i=1,\ldots,r}), the function computes the estimated total mixed l-correlation given by
#' \deqn{\widehat{\Psi}_1(l)=\frac{1}{r}\sum_{i=1}^{r}\widehat{\psi}_{i}(l)^2,} where
#' \eqn{\widehat{\psi}_{i}(l)=\widehat{Corr}(Y_{t,i}, Z_{t-l})}, with
#' \eqn{\overline{Z}_t=\{\overline{Z}_1,\ldots, \overline{Z}_T\}} being a
#' \eqn{T}-length real-valued time series. If \code{features = TRUE}, the function
#' returns a vector whose components are the quantities \eqn{\widehat{\psi}_{i}(l)},
#' \eqn{i=1,2, \ldots,r}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @export

total_mixed_correlation_1 <- function(c_series, n_series, lag = 1,
                                      categories, features = FALSE) {
  check_cts(c_series)
  check_cts(n_series)
  series_length <- length(c_series)
  n_categories <- length(categories)
  binarized_series <- binarization(series = c_series, categories = categories)
  binarized_series_1 <- binarized_series[(lag + 1) : series_length,]
  binarized_series_2 <- binarized_series[1 : (series_length - lag),]
  n_series_1 <- n_series[(lag + 1) : series_length]
  n_series_2 <- n_series[1 : (series_length - lag)]

  correlation_measure <- numeric(n_categories)

  for (i in 1 : n_categories) {

    if (lag >= 0) {

      correlation_measure[i] <- stats::cor(binarized_series_1[,i], n_series_2)

    } else {

      correlation_measure[i] <- stats::cor(binarized_series_2[,i], n_series_1)

    }

  }


  sum_sq_correlation_measure <- sum(correlation_measure^2)

  if (features == FALSE) {

  return(sum_sq_correlation_measure/n_categories)

  } else {

  return(correlation_measure)

  }



}
