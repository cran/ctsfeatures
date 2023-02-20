

#' Computes the total correlation of a categorical time series
#'
#' \code{total_correlation} returns the value of the total correlation for
#' a categorical time series
#'
#' @param series A CTS.
#' @param lag The considered lag (default is 1).
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @param features Logical. If \code{features = FALSE} (default), the value of the total correlation is returned. Otherwise, the function
#' returns a matrix with the individual components of the total correlation
#' @return If \code{features = FALSE} (default), returns the value of the total correlation. Otherwise, the function
#' returns a matrix of features, i.e., the matrix contains the features employed to compute the
#' total correlation.
#' @examples
#' tc <- total_correlation(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't'))) # Computing the total correlation
#' # for the first series in dataset GeneticSequences
#' feature_matrix <- total_correlation(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't')), features = TRUE) # Computing the corresponding
#' # matrix of features
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, and
#' the binarized time series, which is defined as
#' \eqn{\overline{\boldsymbol Y}_t=\{\overline{\boldsymbol Y}_1, \ldots, \overline{\boldsymbol Y}_T\}},
#' with \eqn{\overline{\boldsymbol Y}_k=(\overline{Y}_{k,1}, \ldots, \overline{Y}_{k,r})^\top}
#' such that \eqn{\overline{Y}_{k,i}=1} if \eqn{\overline{X}_k=i} (\eqn{k=1,\ldots,T,
#' , i=1,\ldots,r}), the function computes the estimated sum \eqn{\widehat{\Psi}(l)=\frac{1}{r^2}\sum_{i,j=1}^{r}\widehat{\psi}_{ij}(l)^2},
#' where \eqn{\widehat{\psi}_{ij}(l)} is the estimated correlation
#' \eqn{\widehat{Corr}(Y_{t, i}, Y_{t-l, j})}, \eqn{i,j=1,\ldots,r}. If \code{features = TRUE}, the function
#' returns a matrix whose components are the quantities \eqn{\widehat{\psi}_{ij}(l)},
#' \eqn{i,j=1,2, \ldots,r}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{lopez2023hard}{ctsfeatures}
#'
#' }
#' @export

total_correlation <- function(series, lag = 1, categories, features = FALSE) {

   check_cts(series)
   n_categories <- length(categories)
   matrix_prev <- base::matrix(0, nrow = n_categories, ncol = n_categories)

   for (i in 1 : n_categories) {

     for (j in 1 : n_categories) {

       matrix_prev[i, j] <- correlation_i_j_k_function(series = series,
                                                       i_cat = categories[i],
                                                       j_cat = categories[j],
                                                       k = lag,
                                                       categories = categories)

     }

   }

   matrix_sq <- matrix_prev^2

   if (features == FALSE) {

     return(sum(matrix_sq))

   } else {

     return(matrix_prev)

   }

}
