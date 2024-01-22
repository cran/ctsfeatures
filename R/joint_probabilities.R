

#' Computes the joint probabilities of a categorical time series
#'
#' \code{joint_probabilities} returns a matrix with the joint
#' probabilities of a categorical time series
#'
#' @param series An object of type \code{tsibble} (see R package \code{tsibble}), whose column named Values
#' contains the values of the corresponding CTS. This column must be of class \code{factor} and its levels
#' must be determined by the range of the CTS.
#' @param lag The considered lag (default is 1).
#' @return A matrix with the joint probabilities.
#' @examples
#' sequence_1 <- GeneticSequences[which(GeneticSequences$Series==1),]
#' matrix_jp <- joint_probabilities(series = sequence_1) # Computing the matrix of
#' # joint probabilities for the first series in dataset GeneticSequences
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function computes the
#' matrix \eqn{\widehat{\boldsymbol P}(l) = \big(\widehat{p}_{ij}(l)\big)_{1 \le i, j \le r}},
#' with \eqn{\widehat{p}_{ij}(l)=\frac{N_{ij}(l)}{T-l}}, where \eqn{N_{ij}(l)} is the number
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

joint_probabilities <- function(series, lag = 1) {

  check_cts(series$Value)
  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset
  matrix_joint_probabilities <- base::matrix(0, n_cat, n_cat)

  for (i in 1 : n_cat) {

    for (j in 1 : n_cat) {

      matrix_joint_probabilities[i, j] <- p_i_j_k_function(series = series, i_cat = categories[i],
                                             j_cat = categories[j], k = lag)

    }

  }

  return(matrix_joint_probabilities)

}
