

#' Constructs the rate evolution graph for a categorical time series
#'
#' \code{rate_evolution_graph} constructs the rate evolution graph
#' proposed by \insertCite{ribler1997visualizing;textual}{ctsfeatures}.
#'
#' @param series A CTS.
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @param title The title of the graph.
#' @param ... Additional parameters for the function.
#' @return The rate evolution graph.
#' @examples
#' reg <- rate_evolution_graph(GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't'))) # Constructing the rate
#' # evolution graph for the first time series in dataset GeneticSequences
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, and the
#' corresponding binarized time series, \eqn{\overline{\boldsymbol Y}_t=\{\overline{\boldsymbol Y}_1, \ldots, \overline{\boldsymbol Y}_T\}},
#' the function constructs the rate evolution graph. Specifically, consider the
#' series of cumulated sums given by \eqn{\overline{\boldsymbol C}_t=\{\overline{\boldsymbol C}_1, \ldots, \overline{\boldsymbol C}_T\}}, with
#' \eqn{\overline{\boldsymbol C}_k=\sum_{s=1}^{k}\overline{\boldsymbol Y}_s},
#' \eqn{k=1,\ldots,T}. The rate evolution graph displays a standard time series
#' plot for each one of the components of \eqn{\overline{\boldsymbol C}_t}
#' simultaneously in one graph.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{ribler1997visualizing}{ctsfeatures}
#'
#' }
#' @export

rate_evolution_graph <- function(series, categories, title = 'Rate evolution graph', ...) {

  x <- y <- z <- NULL
  check_cts(series)
  series_length <- length(series) # Length of the series
  n_cat <- length(unique(series)) # Number of categories of X

  # Computing the binarization of the CTS

  bin_series <- binarization(series, categories = categories)

  # Computing the matrix of cumulative sums

  cum_matrix <- base::apply(bin_series, 2, cumsum)

  # Creating a data frame

  time_points <- base::rep(1 : series_length, n_cat)
  extended_categories <- factor(rep(categories, each = series_length))
  df <- data.frame(x = time_points, y = c(cum_matrix), z = extended_categories)

  # Constructing the graph

  plot <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, group = z)) +
    ggplot2::geom_line(ggplot2::aes(colour = z), size = 0.8) +
    ggplot2::xlab('Time') + ggplot2::ylab('Cumulative values') +
    ggplot2::ggtitle(title) +
    ggplot2::theme(axis.title = ggplot2::element_text(size = 12),
          axis.text = ggplot2::element_text(size = 11),
          legend.text = ggplot2::element_text(size = 8),
          legend.title = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 12),
          legend.position = 'bottom',...)


  return(plot)

}
