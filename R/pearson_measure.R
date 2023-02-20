

#' Computes the Pearson measure of a categorical time series
#'
#' \code{pearson_measure} returns the value of the Pearson measure for
#' a categorical time series
#'
#' @param series A CTS.
#' @param lag The considered lag (default is 1).
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @param features Logical. If \code{features = FALSE} (default), the value of the Pearson measure is returned. Otherwise, the function
#' returns a matrix with the individual components of the Pearson measure.
#' @return If \code{features = FALSE} (default), returns the value of the Pearson measure. Otherwise, the function
#' returns a matrix of features, i.e., the matrix contains the features employed to compute the
#' Pearson measure.
#' @examples
#' pm <- pearson_measure(series = SyntheticData1$data[[1]],
#' categories = factor(c(1, 2, 3))) # Computing the Pearson measure
#' # for the first series in dataset GeneticSequences
#' feature_matrix <- pearson_measure(series = SyntheticData1$data[[1]],
#' categories = factor(c(1, 2, 3)), features = TRUE) # Computing the corresponding
#' # matrix of features
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function computes the
#' estimated Pearson measure, \eqn{\widehat{X}_T^2(l)=T\sum_{i,j=1}^{r}\frac{(\widehat{p}_{ij}(l)-\widehat{p}_i\widehat{p}_j)^2}{\widehat{p}_i\widehat{p}_j}},
#' where \eqn{\widehat{p}_i} is the natural estimate of the marginal probability of the \eqn{i}th
#' category, and \eqn{\widehat{p}_{ij}(l)} is the natural estimate of the joint probability
#' for categories \eqn{i} and \eqn{j} at lag l, \eqn{i,j=1, \ldots, r}. If \code{features = TRUE}, the function
#' returns a matrix whose components are the quantities \eqn{\frac{(\widehat{p}_{ij}(l)-\widehat{p}_i\widehat{p}_j)^2}{\widehat{p}_i\widehat{p}_j}},
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

pearson_measure <- function(series, lag = 1, categories, features = FALSE) {

  check_cts(series)
  series_length <- length(series)
  phi2 <- phi2_measure(series = series, lag = lag, categories = categories)

  if (features == FALSE) {

    return(series_length * phi2)

  } else {

    return(phi2_measure(series = series, lag = lag, categories = categories,
                        features = TRUE))

  }

}
