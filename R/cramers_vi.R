

#' Computes the Cramer's vi of a categorical time series
#'
#' \code{cramers_vi} returns the value of the Cramer's vi for
#' a categorical time series
#'
#' @param series A CTS.
#' @param lag The considered lag (default is 1).
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @param features Logical. If \code{features = FALSE} (default), the value of the Cramer's vi is returned. Otherwise, the function
#' returns a matrix with the individual components of the Cramer's vi.
#' @return If \code{features = FALSE} (default), returns the value of the Cramer's vi. Otherwise, the function
#' returns a matrix of features, i.e., the matrix contains the features employed to compute the
#' Cramer's vi.
#' @examples
#' cv <- cramers_vi(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't'))) # Computing the Cramer's vi
#' # for the first series in dataset GeneticSequences
#' feature_matrix <- cramers_vi(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't')), features = TRUE) # Computing the corresponding
#' # matrix of features
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function computes the
#' estimated Cramer's vi, \eqn{\widehat{v}(l)=\sqrt{\frac{1}{r-1}\sum_{i,j=1}^r\frac{(\widehat{p}_{ij}(l)-\widehat{p}_i\widehat{p}_j)^2}{\widehat{p}_i\widehat{p}_j}}},
#' where \eqn{\widehat{p}_i} is the natural estimate of the marginal probability of the \eqn{i}th
#' category, and \eqn{\widehat{p}_{ij}(l)} is the natural estimate of the joint probability
#' for categories \eqn{i} and \eqn{j} at lag l, \eqn{i,j=1, \ldots, r}. If \code{features = TRUE}, the function
#' returns the same output as the function \code{\link{pearson_measure}}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weiss2008measuring}{ctsfeatures}
#'
#' }
#' @export

cramers_vi <- function(series, lag = 1, categories, features = FALSE) {

  check_cts(series)
  n_categories <- length(categories)
  phi2 <- phi2_measure(series = series, lag = lag, categories = categories,
                       features = features)

  if (features == FALSE) {

    return(sqrt(phi2/(n_categories - 1)))

  } else {

    return(phi2)

  }


}


