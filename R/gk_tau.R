

#' Computes the Goodman and Kruskal's tau of a categorical time series
#'
#' \code{gk_tau} returns the value of the Goodman and Kruskal's tau for
#' a categorical time series
#'
#' @param series A CTS.
#' @param lag The considered lag (default is 1).
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @param features Logical. If \code{features = FALSE} (default), the value of Goodman and Kruskal's tau is returned. Otherwise, the function
#' returns a matrix with the individual components of Goodman and Kruskal's tau.
#' @return If \code{features = FALSE} (default), returns the value of the Goodman and Kruskal's tau. Otherwise, the function
#' returns a matrix of features, i.e., the matrix contains the features employed to compute the
#' Goodman and Kruskal's tau.
#' @examples
#' gkt <- gk_tau(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't'))) # Computing the Goodman and Kruskal's tau
#' # for the first series in dataset GeneticSequences
#' feature_matrix <- gk_tau(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't')), features = TRUE) # Computing the corresponding
#' # matrix of features
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function computes the
#' estimated Goodman and Kruskal's tau, \eqn{\widehat{\tau}(l)=\frac{\sum_{i,j=1}^{r}\frac{\widehat{p}_{ij}(l)^2}{\widehat{p}_j}-\sum_{i=1}^r\widehat{p}_i^2}{1-\sum_{i=1}^r\widehat{p}_i^2}},
#' where \eqn{\widehat{p}_i} is the natural estimate of the marginal probability of the \eqn{i}th
#' category, and \eqn{\widehat{p}_{ij}(l)} is the natural estimate of the joint probability
#' for categories \eqn{i} and \eqn{j} at lag l, \eqn{i,j=1, \ldots, r}. If \code{features = TRUE}, the function
#' returns a matrix whose components are the quantities \eqn{\frac{\widehat{p}_{ij}(l)^2}{\widehat{p}_j}},
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

gk_tau <- function(series, lag = 1, categories, features = FALSE) {

  check_cts(series)
  n_cat <- length(categories)
  vector_mp <- marginal_probabilities(series = series, categories = categories)
  sum_sq_mp <- sum(vector_mp^2)
  matrix_jp <- joint_probabilities(series = series, lag = lag,
                                   categories = categories)
  matrix_mp <- base::matrix(vector_mp, nrow = n_cat, ncol = n_cat, byrow = TRUE)

  matrix_prev <- matrix_jp^2/matrix_mp
  numerator <- sum(matrix_prev) - sum_sq_mp
  denominator <- 1 - sum_sq_mp

  if (features == FALSE) {

  return(numerator/denominator)

  } else {

  return(matrix_prev)

  }

}
