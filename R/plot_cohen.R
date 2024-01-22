

#' Constructs a serial dependence plot based on Cohen's kappa
#'
#' \code{plot_cohen} constructs a serial dependence plot of a categorical
#' time series based on Cohen's kappa
#'
#' @param series An object of type \code{tsibble} (see R package \code{tsibble}), whose column named Values
#' contains the values of the corresponding CTS. This column must be of class \code{factor} and its levels
#' must be determined by the range of the CTS.
#' @param max_lag The maximum lag represented in the plot (default is 10).
#' @param alpha The significance level for the corresponding hypothesis test (default is 0.05).
#' @param plot Logical. If \code{plot = TRUE} (default), returns the serial dependence
#' plot. Otherwise, returns a list with the values of Cohens's kappa, the critical
#' value and the corresponding p-values.
#' @param title The title of the graph.
#' @param bar_width The width of the corresponding bars.
#' @param ... Additional parameters for the function.
#' @return If \code{plot = TRUE} (default), returns the serial dependence plot based on Cohens's kappa. Otherwise, the function
#' returns a list with the values of Cohens's kappa, the critical
#' value and the corresponding p-values.
#' @examples
#' sequence_1 <- GeneticSequences[which(GeneticSequences$Series==1),]
#' plot_ck <- plot_cohen(series = sequence_1, max_lag = 3) # Representing
#' # the serial dependence plot
#' list_ck <- plot_cohen(series = sequence_1, max_lag = 3, plot = FALSE) # Obtaining
#' # the values of Cohens's kappa, the critical value and the p-values
#' @details
#' Constructs a serial dependence plot based on Cohens's kappa, \eqn{\widehat{\kappa}(l)},
#' for several lags. A dashed lined is incorporated indicating the critical value
#' of the test based on the following asymptotic approximation (under the i.i.d. assumption):
#' \deqn{\sqrt{\frac{T}{V(\widehat{\boldsymbol p})}}\bigg(\widehat{\kappa}(l)+\frac{1}{T}\bigg)\sim N\big(0, 1\big),} where \eqn{T} is the series length,
#' \eqn{\widehat{\boldsymbol p}=(\widehat{p}_1, \ldots, \widehat{p}_r)} is the vector of estimated marginal probabilities for the \eqn{r} categories of
#' the series and \eqn{V(\boldsymbol {\widehat{p}})=1-\frac{1+2\sum_{i=1}^{r}\widehat{p}_i^3-3\sum_{i=1}^{r}\widehat{p}_i^2}{(1-\sum_{i=1}^{r}\widehat{p}_i^2)^2}}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weiss2011empirical}{ctsfeatures}
#'
#' }
#' @export

plot_cohen <- function(series, max_lag = 10, alpha = 0.05, plot = TRUE,
                              title = 'Serial dependence plot', bar_width = 0.12,...) {

  x <- y <- NULL
  check_cts(series$Value)
  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset
  auxiliary_list <- auxiliary_cohens_kappa(series = series,
                                         max_lag = max_lag, alpha = alpha)

  df_plot_1 <- data.frame(x = 1 : max_lag, y = auxiliary_list$values_cohens_kappa)
  df_plot_2 <- data.frame(x = 1 : (max_lag), y = rep(auxiliary_list$critical_value, max_lag))

  plot_cohen <- ggplot2::ggplot(data = df_plot_1, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_bar(stat = "identity", position = "identity", width = bar_width, fill = 'orange') +
    ggplot2::scale_x_continuous(breaks = 1 : max_lag) +
    ggplot2::ggtitle(title) +
    ggplot2::geom_line(data = df_plot_2, mapping = ggplot2::aes(x = x, y = y), linetype = 2, size = 0.7) +
    ggplot2::geom_line(data = df_plot_2, mapping = ggplot2::aes(x = x, y = -y), linetype = 2, size = 0.7) +
    ggplot2::xlab('Lag') + ggplot2::ylab(latex2exp::TeX("Cohen's $\\kappa$")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12),
          axis.text.y = ggplot2::element_text(size = 11),
          axis.title = ggplot2::element_text(size = 12),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 12),...)

  if (plot == TRUE) {

    return(plot_cohen)

  } else {

    return_list <- list(values = auxiliary_list$values_cohens_kappa,
                        p_values = auxiliary_list$vector_p_values,
                        critical_values = auxiliary_list$critical_value)

    return(return_list)

  }



}
