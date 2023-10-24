

#' Constructs the IFS circle transformation of a
#' categorical time series
#'
#' \code{plot_ifsct} constructs the IFS circle transformation of
#' a categorical time series.
#'
#' @param series A CTS.
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @param alpha Parameter alpha in the circle transformation.
#' @param beta  Parameter beta in the circle transformation.
#' @param title The title of the graph.
#' @param ... Additional parameters for the function.
#' @return The IFS circle transformation.
#' @examples
#' ct <- plot_ifsct(GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't')),
#' alpha = 0.1, beta = 0.1) # Constructing the IFS circle transformation
#' # for the first CTS in dataset GeneticSequences
#' @details
#' Constructs the IFS circle transformation for a given CTS, which is
#' useful to identify cycles of arbitrary length.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weiss2008visual}{ctsfeatures}
#'
#' }
#' @export

plot_ifsct <- function(series, categories, alpha, beta,
                                      title = 'IFS circle transformation',...) {

  x <- y <- z <- NULL
  check_cts(series)
  series_length <- length(series)
  n_categories <- length(categories)

  matrix_circle <- base::matrix(0, nrow = n_categories, ncol = 2)

  for (i in 1 : n_categories) {

    matrix_circle[i,] <- c(cos(2 * pi * (i - 1)/i), sin(2 * pi * (i - 1)/i))

  }

  transformed_series <- base::matrix(0, nrow = series_length, ncol = 2)

  for (i in 1 : series_length) {

    transformed_series[i,] <- matrix_circle[as.numeric(series)[i],]

  }

  fractal_series <- base::matrix(0, nrow = series_length, ncol = 2)
  fractal_series[1,] <- beta * transformed_series[1,]

  for (i in 2 : series_length) {

    fractal_series[i,] <- alpha * fractal_series[(i - 1),] + beta * transformed_series[i,]

  }

  df_plot <- data.frame(x = fractal_series[,1], y = fractal_series[,2], z = series)
  ifc_circle_plot <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y, color = factor(z))) +
    ggplot2::geom_point() + ggplot2::ggtitle(title) +
   ggplot2::xlab('x-comp (fractal series)') +
   ggplot2::ylab('y-comp (fractal series)') +
    ggplot2::theme(axis.title = ggplot2::element_text(size = 9),
                         axis.text = ggplot2::element_text(size = 11),
                         legend.text = ggplot2::element_text(size = 8),
                         legend.title = ggplot2::element_blank(),
                         plot.title = ggplot2::element_text(hjust = 0.5, size = 12),
                         legend.position = 'bottom',...)


  return(ifc_circle_plot)




}
