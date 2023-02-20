

#' Computes the uncertainty coefficient of a categorical time series
#'
#' \code{uncertainty_coefficient} returns the value of the uncertainty coefficient for
#' a categorical time series
#'
#' @param series A CTS.
#' @param lag The considered lag (default is 1).
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @param features Logical. If \code{features = FALSE} (default), the value of the uncertainty coefficient is returned. Otherwise, the function
#' returns a matrix with the individual components of the uncertainty coefficient.
#' @return If \code{features = FALSE} (default), returns the value of the uncertainty coefficient. Otherwise, the function
#' returns a matrix of features, i.e., the matrix contains the features employed to compute the
#' uncertainty coefficient.
#' @examples
#' uc <- uncertainty_coefficient(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't'))) # Computing the uncertainty coefficient
#' # for the first series in dataset GeneticSequences
#' feature_matrix <- uncertainty_coefficient(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't')), features = TRUE) # Computing the corresponding
#' # matrix of features
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function computes the
#' estimated uncertainty coefficient, \eqn{\widehat{u}(l)=-\frac{\sum_{i, j=1}^{r}\widehat{p}_{ij}(l)\ln\big(\frac{\widehat{p}_{ij}(l)}{\widehat{p}_i\widehat{p}_j}\big)}{\sum_{i=1}^{r}\widehat{p}_i\ln \widehat{p}_i}},
#' where \eqn{\widehat{p}_i} is the natural estimate of the marginal probability of the \eqn{i}th
#' category, and \eqn{\widehat{p}_{ij}(l)} is the natural estimate of the joint probability
#' for categories \eqn{i} and \eqn{j} at lag l, \eqn{i,j=1, \ldots, r}. If \code{features = TRUE}, the function
#' returns a matrix whose components are the quantities \eqn{\widehat{p}_{ij}(l)\ln\Big(\frac{\widehat{p}_{ij}(l)}{\widehat{p}_i\widehat{p}_j}\Big)},
#' \eqn{i,j=1,2, \ldots,r}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weiss2008measuring}{ctsfeatures}
#'
#' }
#' @export

uncertainty_coefficient <- function(series, lag = 1, categories, features = FALSE) {

  check_cts(series)
  vector_mp <- marginal_probabilities(series = series, categories = categories)
  matrix_mp <- vector_mp %*% t(vector_mp)
  matrix_jp <- joint_probabilities(series = series, lag = lag,
                                   categories = categories)
  matrix_combined <- matrix_jp/matrix_mp

  matrix_prev <- matrix_jp * log(matrix_combined)
  numerator <- sum(matrix_prev)
  denominator <- sum(vector_mp * log(vector_mp))

  if (features == FALSE) {

    return(-numerator/denominator)

  } else {

    return(matrix_prev)

  }

}
