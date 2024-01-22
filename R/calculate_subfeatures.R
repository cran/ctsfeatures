

#' Computes several subfeatures associated with a categorical time series
#'
#' \code{calculate_features} computes several subfeatures associated with a
#' categorical time series or between a categorical and a real-valued time series
#'
#' @param series An object of type \code{tsibble} (see R package \code{tsibble}), whose column named Values
#' contains the values of the corresponding CTS. This column must be of class \code{factor} and its levels
#' must be determined by the range of the CTS.
#' @param n_series A real-valued time series.
#' @param lag The considered lag (default is 1).
#' @param type String indicating the subfeature one wishes to compute.
#' @return The corresponding subfeature
#' @examples
#' sequence_1 <- GeneticSequences[which(GeneticSequences$Series==1),]
#' suc <- calculate_subfeatures(series = sequence_1, type = 'uncertainty_coefficient')
#' # Computing the subfeatures associated with the uncertainty coefficient
#' # for the first series in dataset GeneticSequences
#' scv <- calculate_subfeatures(series = sequence_1, type = 'cramers_vi' )
#' # Computing the subfeatures associated with the cramers vi
#' # for the first series in dataset GeneticSequences
#' @details
#' Assume we have a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, with \eqn{\widehat{p}_i}
#' being the natural estimate of the marginal probability of the \eqn{i}th
#' category, and \eqn{\widehat{p}_{ij}(l)} being the natural estimate of the joint probability
#' for categories \eqn{i} and \eqn{j} at lag l, \eqn{i,j=1, \ldots, r}. Assume also that
#' we have a real-valued time series of length \eqn{T}, \eqn{\overline{Z}_t=\{\overline{Z}_1,\ldots, \overline{Z}_T\}}.
#' The function computes the following subfeatures depending on the argument
#' \code{type}:
#'
#' \itemize{
#'
#'   \item{If \code{type=entropy}, the function computes the
#'    subfeatures associated with the estimated entropy, \eqn{\widehat{p}_i\ln(\widehat{p}_i)},
#'   \eqn{i=1,2, \ldots,r}}.
#'
#'   \item{If \code{type=gk_tau}, the function computes the
#'    subfeatures associated with the estimated Goodman and Kruskal's tau, \eqn{\frac{\widehat{p}_{ij}(l)^2}{\widehat{p}_j}},
#'    \eqn{i,j=1,2, \ldots,r}}.
#'
#'   \item{If \code{type=gk_lambda}, the function computes the
#'    subfeatures associated with the estimated Goodman and Kruskal's lambda, \eqn{\max_i\widehat{p}_{ij}(l)},
#'   \eqn{i=1,2, \ldots,r}}.
#'
#'   \item{If \code{type=uncertainty_coefficient}, the function computes the
#'    subfeatures associated with the estimated uncertainty coefficient, \eqn{\widehat{p}_{ij}(l)\ln\Big(\frac{\widehat{p}_{ij}(l)}{\widehat{p}_i\widehat{p}_j}\Big)},
#'   \eqn{i,j=1,2, \ldots,r}}.
#'
#'   \item{If \code{type=pearson_measure}, the function computes the
#'    subfeatures associated with the estimated Pearson measure, \eqn{\frac{(\widehat{p}_{ij}(l)-\widehat{p}_i\widehat{p}_j)^2}{\widehat{p}_i\widehat{p}_j}},
#'   \eqn{i,j=1,2, \ldots,r}}.
#'
#'   \item{If \code{type=phi2_measure}, the function computes the
#'    subfeatures associated with the estimated Phi2 measure, \eqn{\frac{(\widehat{p}_{ij}(l)-\widehat{p}_i\widehat{p}_j)^2}{\widehat{p}_i\widehat{p}_j}},
#'   \eqn{i,j=1,2, \ldots,r}}.
#'
#'   \item{If \code{type=sakoda_measure}, the function computes the
#'    subfeatures associated with the estimated Sakoda measure, \eqn{\frac{(\widehat{p}_{ij}(l)-\widehat{p}_i\widehat{p}_j)^2}{\widehat{p}_i\widehat{p}_j}},
#'   \eqn{i,j=1,2, \ldots,r}}.
#'
#'   \item{If \code{type=cramers_vi}, the function computes the
#'    subfeatures associated with the estimated Cramer's vi, \eqn{\frac{(\widehat{p}_{ij}(l)-\widehat{p}_i\widehat{p}_j)^2}{\widehat{p}_i\widehat{p}_j}},
#'   \eqn{i,j=1,2, \ldots,r}}.
#'
#'   \item{If \code{type=cohens_kappa}, the function computes the
#'    subfeatures associated with the estimated Cohen's kappa, \eqn{\widehat{p}_{ii}(l)-\widehat{p}_i^2},
#'   \eqn{i=1,2, \ldots,r}}.
#'
#'   \item{If \code{type=total_correlation}, the function computes the
#'    subfeatures associated with the total correlation, \eqn{\widehat{\psi}_{ij}(l)},
#'    \eqn{i,j=1,2, \ldots,r} (see \code{type='total_mixed_cor'} in the function \code{calculate_features})}.
#'
#'   \item{If \code{type=total_mixed_correlation_1}, the function computes the
#'    subfeatures associated with the total mixed l-correlation, \eqn{\widehat{\psi}_{i}(l)},
#'   \eqn{i=1,2, \ldots,r} (see \code{type='total_mixed_correlation_1'} in the function \code{calculate_features})}.
#'
#'   \item{If \code{type=total_mixed_correlation_2}, the function computes the
#'    subfeatures associated with the total mixed q-correlation, \eqn{\int_{0}^{1}\widehat{\psi}^\rho_{i}(l)^2d\rho},
#'    \eqn{i=1,2, \ldots,r} (see \code{type='total_mixed_correlation_2'} in the function \code{calculate_features})}.
#'
#' }
#'
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weiss2008measuring}{ctsfeatures}
#'
#' }
#' @export

calculate_subfeatures <- function(series, n_series, lag = 1,
                                  type = NULL) {

  check_cts(series$Value)
  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset


  if (type == 'entropy') {

    return(auxiliary_entropy(series, features = TRUE))

  }


  if (type == 'gk_tau') {

    return(auxiliary_gk_tau(series, lag = lag, features = TRUE))

  }


  if (type == 'gk_lambda') {

    return(auxiliary_gk_lambda(series, lag = lag, features = TRUE))

  }


  if (type == 'uncertainty_coefficient') {

    return(auxiliary_uncertainty_coefficient(series, lag = lag, features = TRUE))

  }


  if (type == 'pearson_measure') {

    return(auxiliary_pearson_measure(series, lag = lag, features = TRUE))

  }


  if (type == 'phi2_measure') {

    return(auxiliary_phi2_measure(series, lag = lag, features = TRUE))

  }


  if (type == 'sakoda_measure') {

    return(auxiliary_sakoda_measure(series, lag = lag, features = TRUE))

  }


  if (type == 'cramers_vi') {

    return(auxiliary_cramers_vi_function(series, lag = lag, features = TRUE))

  }


  if (type == 'cohens_kappa') {

    return(auxiliary_cohens_kappa_function(series, lag = lag, features = TRUE))

  }


  if (type == 'total_correlation') {

    return(auxiliary_total_correlation(series, lag = lag, features = TRUE))

  }


  if (type == 'spectral_envelope') {

    binarized_series <- binarization(series)
    return(auxiliary_spectral_envelope(binarized_series, plot = FALSE))

  }


  if (type == 'total_mixed_correlation_1') {

    return(auxiliary_total_mixed_correlation_1(series, n_series, lag = lag, features = TRUE))

  }


  if (type == 'total_mixed_correlation_2') {

    return(auxiliary_total_mixed_correlation_2(series, n_series, lag = lag, features = TRUE))

  }


}





