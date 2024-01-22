

#' Computes the marginal probabilities of a categorical time series
#'
#' \code{marginal_probabilities} returns a vector with the marginal
#' probabilities of a categorical time series
#'
#' @param series An object of type \code{tsibble} (see R package \code{tsibble}), whose column named Values
#' contains the values of the corresponding CTS. This column must be of class \code{factor} and its levels
#' must be determined by the range of the CTS.
#' @return A vector with the marginal probabilities.
#' @examples
#' sequence_1 <- GeneticSequences[which(GeneticSequences$Series==1),]
#' vector_mp <- marginal_probabilities(series = sequence_1) # Computing the vector of
#' # marginal probabilities for the first series in dataset GeneticSequences
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function computes the
#' vector \eqn{\widehat{\boldsymbol p} =(\widehat{p}_1, \ldots, \widehat{p}_r)},
#' with \eqn{\widehat{p}_i=\frac{N_i}{T}}, where \eqn{N_i} is the number
#' of elements equal to \eqn{i} in the realization \eqn{\overline{X}_t}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weiss2008measuring}{ctsfeatures}
#'
#' }
#' @export

marginal_probabilities <- function(series) {

  check_cts(series$Value)
  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset


  # Computing the marginal probabilities in the series

  marginal_probabilities <- numeric()

  for (i in 1 : n_cat) {

    count_i <- sum(series == categories[[i]])
    marginal_probabilities[i] <- count_i/series_length

  }

  return(marginal_probabilities)

}
