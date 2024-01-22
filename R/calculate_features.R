

#' Computes several features associated with a categorical time series
#'
#' \code{calculate_features} computes several features associated with a
#' categorical time series or between a categorical and a real-valued time series
#'
#' @param series An object of type \code{tsibble} (see R package \code{tsibble}), whose column named Values
#' contains the values of the corresponding CTS. This column must be of class \code{factor} and its levels
#' must be determined by the range of the CTS.
#' @param n_series A real-valued time series.
#' @param lag The considered lag (default is 1).
#' @param type String indicating the feature one wishes to compute.
#' @return The corresponding feature.
#' @examples
#' sequence_1 <- GeneticSequences[which(GeneticSequences$Series==1),]
#' uc <- calculate_features(series = sequence_1, type = 'uncertainty_coefficient' )
#' # Computing the uncertainty coefficient
#' # for the first series in dataset GeneticSequences
#' se <- calculate_features(series = sequence_1, type = 'spectral_envelope' )
#' # Computing the spectral envelope
#' # for the first series in dataset GeneticSequences
#' @details
#' Assume we have a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, with \eqn{\widehat{p}_i}
#' being the natural estimate of the marginal probability of the \eqn{i}th
#' category, and \eqn{\widehat{p}_{ij}(l)} being the natural estimate of the joint probability
#' for categories \eqn{i} and \eqn{j} at lag l, \eqn{i,j=1, \ldots, r}. Assume also that
#' we have a real-valued time series of length \eqn{T}, \eqn{\overline{Z}_t=\{\overline{Z}_1,\ldots, \overline{Z}_T\}}.
#' The function computes the following quantities depending on the argument
#' \code{type}:
#'
#' \itemize{
#'   \item{If \code{type=gini_index}, the function computes the
#'    estimated gini index, \eqn{\widehat{g}=\frac{r}{r-1}(1-\sum_{i=1}^{r}\widehat{p}_i^2)}}.
#'
#'   \item{If \code{type=entropy}, the function computes the
#'    estimated entropy, \eqn{\widehat{e}=\frac{-1}{\ln(r)}\sum_{i=1}^{r}\widehat{p}_i\ln \widehat{p}_i}}.
#'
#'   \item{If \code{type=chebycheff_dispersion}, the function computes the
#'    estimated chebycheff dispersion, \eqn{\widehat{c}=\frac{r}{r-1}(1-\max_i\widehat{p}_i)}}.
#'
#'   \item{If \code{type=gk_tau}, the function computes the
#'    estimated Goodman and Kruskal's tau, \eqn{\widehat{\tau}(l)=\frac{\sum_{i,j=1}^{r}\frac{\widehat{p}_{ij}(l)^2}{\widehat{p}_j}-\sum_{i=1}^r\widehat{p}_i^2}{1-\sum_{i=1}^r\widehat{p}_i^2}}}.
#'
#'   \item{If \code{type=gk_lambda}, the function computes the
#'    estimated Goodman and Kruskal's lambda, \eqn{\widehat{\lambda}(l)=\frac{\sum_{j=1}^{r}\max_i\widehat{p}_{ij}(l)-\max_i\widehat{p}_i}{1-\max_i\widehat{p}_i}}}.
#'
#'   \item{If \code{type=uncertainty_coefficient}, the function computes the
#'    estimated uncertainty coefficient, \eqn{\widehat{u}(l)=-\frac{\sum_{i, j=1}^{r}\widehat{p}_{ij}(l)\ln\big(\frac{\widehat{p}_{ij}(l)}{\widehat{p}_i\widehat{p}_j}\big)}{\sum_{i=1}^{r}\widehat{p}_i\ln \widehat{p}_i}}}.
#'
#'   \item{If \code{type=pearson_measure}, the function computes the
#'    estimated Pearson measure, \eqn{\widehat{X}_T^2(l)=T\sum_{i,j=1}^{r}\frac{(\widehat{p}_{ij}(l)-\widehat{p}_i\widehat{p}_j)^2}{\widehat{p}_i\widehat{p}_j}}}.
#'
#'   \item{If \code{type=phi2_measure}, the function computes the
#'    estimated Phi2 measure, \eqn{\widehat{\Phi}^2(l)=\frac{\widehat{X}_T^2(l)}{T}}}.
#'
#'   \item{If \code{type=sakoda_measure}, the function computes the
#'    estimated Sakoda measure, \eqn{\widehat{p}^*(l)=\sqrt{\frac{r\widehat{\Phi}^2(l)}{(r-1)(1+\widehat{\Phi}^2(l))}}}}.
#'
#'   \item{If \code{type=cramers_vi}, the function computes the
#'    estimated Cramer's vi, \eqn{\widehat{v}(l)=\sqrt{\frac{1}{r-1}\sum_{i,j=1}^r\frac{(\widehat{p}_{ij}(l)-\widehat{p}_i\widehat{p}_j)^2}{\widehat{p}_i\widehat{p}_j}}}}.
#'
#'   \item{If \code{type=cohens_kappa}, the function computes the
#'    estimated Cohen's kappa, \eqn{\widehat{\kappa}(l)=\frac{\sum_{j=1}^{r}(\widehat{p}_{jj}(l)-\widehat{p}_j^2)}{1-\sum_{i=1}^r\widehat{p}_i^2}}}.
#'
#'   \item{If \code{type=total_correlation}, the function computes the
#'    the estimated sum \eqn{\widehat{\Psi}(l)=\frac{1}{r^2}\sum_{i,j=1}^{r}\widehat{\psi}_{ij}(l)^2},
#'    where \eqn{\widehat{\psi}_{ij}(l)} is the estimated correlation
#'    \eqn{\widehat{Corr}(Y_{t, i}, Y_{t-l, j})}, \eqn{i,j=1,\ldots,r}, being \eqn{\overline{\boldsymbol Y}_t=\{\overline{\boldsymbol Y}_1, \ldots, \overline{\boldsymbol Y}_T\}},
#'    with \eqn{\overline{\boldsymbol Y}_k=(\overline{Y}_{k,1}, \ldots, \overline{Y}_{k,r})^\top}, the
#'    binarized time series of \eqn{\overline{X}_t}}.
#'
#'
#'   \item{If \code{type=spectral_envelope}, the function computes the
#'    estimated spectral envelope}.
#'
#'   \item{If \code{type=total_mixed_correlation_1}, the function computes the
#'    estimated total mixed l-correlation given by
#'    \deqn{\widehat{\Psi}_1(l)=\frac{1}{r}\sum_{i=1}^{r}\widehat{\psi}_{i}(l)^2,} where
#'    \eqn{\widehat{\psi}_{i}(l)=\widehat{Corr}(Y_{t,i}, Z_{t-l})}, being \eqn{\overline{\boldsymbol Y}_t=\{\overline{\boldsymbol Y}_1, \ldots, \overline{\boldsymbol Y}_T\}},
#'    with \eqn{\overline{\boldsymbol Y}_k=(\overline{Y}_{k,1}, \ldots, \overline{Y}_{k,r})^\top}, the
#'    binarized time series of \eqn{\overline{X}_t}}.
#'
#'   \item{If \code{type=total_mixed_correlation_2}, the function computes the
#'    estimated total mixed q-correlation given by
#'    \deqn{\widehat{\Psi}_2(l)=\frac{1}{r}\sum_{i=1}^{r}\int_{0}^{1}\widehat{\psi}^\rho_{i}(l)^2d\rho,} where
#'    \eqn{\widehat{\psi}_{i}^\rho(l)=\widehat{Corr}\big(Y_{t,i}, I(Z_{t-l}\leq q_{Z_t}(\rho)) \big)}, being \eqn{\overline{\boldsymbol Y}_t=\{\overline{\boldsymbol Y}_1, \ldots, \overline{\boldsymbol Y}_T\}},
#'    with \eqn{\overline{\boldsymbol Y}_k=(\overline{Y}_{k,1}, \ldots, \overline{Y}_{k,r})^\top}, the
#'    binarized time series of \eqn{\overline{X}_t}, \eqn{\rho \in (0, 1)} a probability
#'    level, \eqn{I(\cdot)} the indicator function and \eqn{q_{Z_t}} the quantile
#'    function of the corresponding real-valued process}.
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

calculate_features <- function(series, n_series = NULL, lag = 1, type = NULL) {

  check_cts(series$Value)
  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset


  if (type == 'gini_index') {

    return(auxiliary_gini_index(series))

  }


  if (type == 'entropy') {

    return(auxiliary_entropy(series))

  }


  if (type == 'chebycheff_dispersion') {

    return(auxiliary_chebycheff_dispersion(series))

  }


  if (type == 'gk_tau') {

    return(auxiliary_gk_tau(series, lag = lag))

  }


  if (type == 'gk_lambda') {

    return(auxiliary_gk_lambda(series, lag = lag))

  }


  if (type == 'uncertainty_coefficient') {

    return(auxiliary_uncertainty_coefficient(series, lag = lag))

  }


  if (type == 'pearson_measure') {

    return(auxiliary_pearson_measure(series, lag = lag))

  }


  if (type == 'phi2_measure') {

    return(auxiliary_phi2_measure(series, lag = lag))

  }


  if (type == 'sakoda_measure') {

    return(auxiliary_sakoda_measure(series, lag = lag))

  }


  if (type == 'cramers_vi') {

    return(auxiliary_cramers_vi_function(series, lag = lag))

  }


  if (type == 'cohens_kappa') {

    return(auxiliary_cohens_kappa_function(series, lag = lag))

  }


  if (type == 'total_correlation') {

    return(auxiliary_total_correlation(series, lag = lag))

  }


  if (type == 'spectral_envelope') {

    binarized_series <- binarization(series)
    return(auxiliary_spectral_envelope(binarized_series, plot = FALSE))

  }


  if (type == 'total_mixed_correlation_1') {

    return(auxiliary_total_mixed_correlation_1(series, n_series, lag = lag))

  }


  if (type == 'total_mixed_correlation_2') {

    return(auxiliary_total_mixed_correlation_2(series, n_series, lag = lag))

  }


}
