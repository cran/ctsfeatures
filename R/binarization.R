

#' Constructs the binarized time series associated with a given
#' categorical time series
#'
#' \code{binarization} constructs the binarized time series associated with a given
#' categorical time series.
#'
#' @param series An object of type \code{tsibble} (see R package \code{tsibble}), whose column named Values
#' contains the values of the corresponding CTS. This column must be of class \code{factor} and its levels
#' must be determined by the range of the CTS.
#'
#' @return The binarized time series.
#' @examples
#' sequence_1 <- GeneticSequences[which(GeneticSequences$Series==1),]
#' binarized_series <- binarization(sequence_1) # Constructing the binarized
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

binarization <- function(series) {

 check_cts(series$Value)
 series_length <- length(series$Value) # Series length
 categories <- levels(series$Value)
 n_cat <- length(categories) # Number of categories in the dataset
 matrix_binarization <- base::matrix(0, nrow = series_length, ncol = n_cat)

 for (i in 1 : n_cat) {

   matrix_binarization[,i][which(series$Value == categories[i])] <- 1

 }

 return(matrix_binarization)

}
