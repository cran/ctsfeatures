

#' Constructs the binarized time series associated with a given
#' categorical time series
#'
#' \code{binarization} constructs the binarized time series associated with a given
#' categorical time series.
#'
#' @param series A CTS.
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @return The binarized time series.
#' @examples
#' binarized_series <- binarization(GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't'))) # Constructing the binarized
#' # time series for the first CTS in dataset GeneticSequences
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function
#' constructs the binarized time series, which is defined as
#' \eqn{\overline{\boldsymbol Y}_t=\{\overline{\boldsymbol Y}_1, \ldots, \overline{\boldsymbol Y}_T\}},
#' with \eqn{\overline{\boldsymbol Y}_k=(\overline{Y}_{k,1}, \ldots, \overline{Y}_{k,r})^\top}
#' such that \eqn{\overline{Y}_{k,i}=1} if \eqn{\overline{X}_k=i} (\eqn{k=1,\ldots,T,
#' , i=1,\ldots,r}). The binarized series is constructed in the form of a matrix
#' whose rows represent time observations and whose columns represent the
#' categories in the original series
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{lopez2023hard}{ctsfeatures}
#'
#' }
#' @export

binarization <- function(series, categories) {

 check_cts(series)
 series_length <- length(series)
 n_categories <- length(categories)
 matrix_binarization <- base::matrix(0, nrow = series_length, ncol = n_categories)

 for (i in 1 : n_categories) {

   matrix_binarization[,i][which(series == categories[i])] <- 1

 }

 return(matrix_binarization)

}
