

#' Computes the Goodman and Kruskal's lambda of a categorical time series
#'
#' \code{gk_lambda} returns the value of the Goodman and Kruskal's lambda for
#' a categorical time series
#'
#' @param series A CTS.
#' @param lag The considered lag (default is 1).
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @param features Logical. If \code{features = FALSE} (default), the value of Goodman and Kruskal's lambda is returned. Otherwise, the function
#' returns a matrix with the individual components of Goodman and Kruskal's lambda
#' @return If \code{features = FALSE} (default), returns the value of the Goodman and Kruskal's lambda. Otherwise, the function
#' returns a matrix of features, i.e., the matrix contains the features employed to compute the
#' Goodman and Kruskal's lambda.
#' @examples
#' gkl <- gk_lambda(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't'))) # Computing the Goodman and Kruskal's lambda
#' # for the first series in dataset GeneticSequences
#' feature_matrix <- gk_lambda(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't')), features = TRUE) # Computing the corresponding
#' # matrix of features
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function computes the
#' estimated Goodman and Kruskal's lambda, \eqn{\widehat{\lambda}(l)=\frac{\sum_{j=1}^{r}\max_i\widehat{p}_{ij}(l)-\max_i\widehat{p}_i}{1-\max_i\widehat{p}_i}},
#' where \eqn{\widehat{p}_i} is the natural estimate of the marginal probability of the \eqn{i}th
#' category, and \eqn{\widehat{p}_{ij}(l)} is the natural estimate of the joint probability
#' for categories \eqn{i} and \eqn{j} at lag l, \eqn{i,j=1, \ldots, r}. If \code{features = TRUE}, the function
#' returns a vector whose components are the quantities \eqn{\max_i\widehat{p}_{ij}(l)},
#' \eqn{i=1,2, \ldots,r}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weiss2008measuring}{ctsfeatures}
#'
#' }
#' @export

gk_lambda <- function(series, lag = 1, categories, features = FALSE) {

  check_cts(series)
  vector_mp <- marginal_probabilities(series = series, categories = categories)
  max_mp <- max(vector_mp)
  matrix_jp <- joint_probabilities(series = series, lag = lag,
                                   categories = categories)
  max_jp <- base::apply(matrix_jp, 2, max)

  numerator <- sum(max_jp) - max_mp
  denominator <- 1 - max_mp

  if (features == FALSE) {

    return(numerator/denominator)

  } else {

    return(max_jp)

  }

}
