

#' Computes the Cohen's kappa of a categorical time series
#'
#' \code{cohens_kappa} returns the value of the Cohen's kappa for
#' a categorical time series
#'
#' @param series A CTS.
#' @param lag The considered lag (default is 1).
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @param features Logical. If \code{features = FALSE} (default), the value of the Cohen's kappa is returned. Otherwise, the function
#' returns a matrix with the individual components of the Cohen's kappa.
#' @return If \code{features = FALSE} (default), returns the value of the Cohen's kappa. Otherwise, the function
#' returns a matrix of features, i.e., the matrix contains the features employed to compute the
#' Cohen's kappa.
#' @examples
#' ck <- cohens_kappa(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't'))) # Computing the Cohen's kappa
#' # for the first series in dataset GeneticSequences
#' feature_matrix <- cohens_kappa(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't')), features = TRUE) # Computing the corresponding
#' # matrix of features
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function computes the
#' estimated Cohen's kappa, \eqn{\widehat{\kappa}(l)=\frac{\sum_{j=1}^{r}(\widehat{p}_{jj}(l)-\widehat{p}_j^2)}{1-\sum_{i=1}^r\widehat{p}_i^2}},
#' where \eqn{\widehat{p}_i} is the natural estimate of the marginal probability of the \eqn{i}th
#' category, and \eqn{\widehat{p}_{ij}(l)} is the natural estimate of the joint probability
#' for categories \eqn{i} and \eqn{j} at lag l, \eqn{i,j=1, \ldots, r}. If \code{features = TRUE}, the function
#' returns a vector whose components are the quantities \eqn{\widehat{p}_{ii}(l)-\widehat{p}_i^2},
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

cohens_kappa <- function(series, lag = 1, categories, features = FALSE) {

  check_cts(series)
  n_categories <- length(categories)
  vector_mp <- marginal_probabilities(series = series, categories = categories)
  vector_numerator <- numeric()

  for (i in 1 : n_categories) {

    vector_numerator[i] <- p_i_j_k_function(series = series, i_cat = categories[i], j_cat = categories[i],
                                            k = lag) - vector_mp[i]^2

  }

  numerator <- sum(vector_numerator)
  denominator <- 1 - sum(vector_mp^2)

  if (features == FALSE) {

    return(numerator/denominator)

  } else {

    return(vector_numerator)

  }

}
