

#' Computes the conditional probabilities of a categorical time series
#'
#' \code{conditional_probabilities} returns a matrix with the conditional
#' probabilities of a categorical time series
#'
#' @param series An object of type \code{tsibble} (see R package \code{tsibble}), whose column named Value
#' contains the values of the corresponding CTS. This column must be of class \code{factor} and its levels
#' must be determined by the range of the CTS.
#' @param lag The considered lag (default is 1).
#' @return A matrix with the conditional probabilities.
#' @examples
#' sequence_1 <- GeneticSequences[which(GeneticSequences$Series==1),]
#' matrix_cp <- conditional_probabilities(series = sequence_1) # Computing the matrix of
#' # joint probabilities for the first series in dataset GeneticSequences
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function computes the
#' matrix \eqn{\widehat{\boldsymbol P}^c(l) = \big(\widehat{p}^c_{ij}(l)\big)_{1 \le i, j \le r}},
#' with \eqn{\widehat{p}^c_{ij}(l)=\frac{TN_{ij}(l)}{(T-l)N_i}}, where
#' \eqn{N_i} is the number of elements equal to \eqn{i} in the realization \eqn{\overline{X}_t} and \eqn{N_{ij}(l)} is the number
#' of pairs \eqn{(\overline{X}_t, \overline{X}_{t-l})=(i,j)} in the realization \eqn{\overline{X}_t}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weiss2008measuring}{ctsfeatures}
#'
#' }
#' @export

conditional_probabilities <- function(series, lag = 1) {

  check_cts(series)
  n_cat <- length(levels(series$Value))
  matrix_probs <- matrix(0, n_cat, n_cat)

  vector_mp <- marginal_probabilities(series = series)
  matrix_jp <- joint_probabilities(series = series, lag = lag)
  matrix_mp <- base::matrix(vector_mp, nrow = n_cat, ncol = n_cat, byrow = TRUE)
  matrix_conditional_probabilities <- matrix_jp/matrix_mp




  return(matrix_conditional_probabilities)

}
