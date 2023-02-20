

#' Computes the Phi2 measure of a categorical time series
#'
#' \code{phi2_measure} returns the value of the Phi2 measure for
#' a categorical time series
#'
#' @param series A CTS.
#' @param lag The considered lag (default is 1).
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @param features Logical. If \code{features = FALSE} (default), the value of the Phi2 measure is returned. Otherwise, the function
#' returns a matrix with the individual components of the Phi2 measure.
#' @return If \code{features = FALSE} (default), returns the value of the Phi2 measure. Otherwise, the function
#' returns a matrix of features, i.e., the matrix contains the features employed to compute the
#' Phi2 measure.
#' @examples
#' phi2m <- phi2_measure(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't'))) # Computing the Phi2 measure
#' # for the first series in dataset GeneticSequences
#' feature_matrix <- phi2_measure(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't')), features = TRUE) # Computing the corresponding
#' # matrix of features
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function computes the
#' estimated Phi2 measure, \eqn{\widehat{\Phi}^2(l)=\frac{\widehat{X}_T^2(l)}{T}},
#' where \eqn{\widehat{X}_T^2} is the estimated Pearson measure. If \code{features = TRUE}, the function
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

phi2_measure <- function(series, lag = 1, categories, features = FALSE) {

  check_cts(series)
  vector_mp <- marginal_probabilities(series = series, categories = categories)
  matrix_mp <- vector_mp %*% t(vector_mp)
  matrix_jp <- joint_probabilities(series = series, lag = lag,
                                   categories = categories)
  matrix_prev <- (matrix_jp - matrix_mp)^2
  matrix_combined <- matrix_prev/matrix_mp

  if (features == FALSE) {

    return(sum(matrix_combined))

  } else {

    return(matrix_combined)

  }

}
