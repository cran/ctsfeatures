

#' Constructs a categorical time series plot
#'
#' \code{plot_cts} constructs a categorical time series plot
#'
#' @param series A CTS.
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @param title The title of the graph.
#' @return The categorical time series plot.
#' @examples
#' time_series_plot <- plot_cts(series = GeneticSequences$data[[1]][1 : 50],
#' categories = factor(c('a', 'c', 'g', 't'))) # Constructs a categorical
#' # time series plot for the first 50 observations of the first  time series in
#' # dataset GeneticSequences
#' @details
#' Constructs a categorial time series plot for a given CTS.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weiss2018introduction}{ctsfeatures}
#'
#' }
#' @export

plot_cts <- function(series, categories, title = 'Time series plot') {

  x <- y <- NULL
  check_cts(series)
  series_length <- length(series)
  n_categories <- length(categories)
  numeric_series <- numeric(series_length)

  for (i in 1 : n_categories) {

    indexes_i <- which(series == categories[i])
    numeric_series[indexes_i] <- i

  }

  df_plot <- data.frame(x = 1 : series_length, y = numeric_series)

  plot_cts <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y)) + ggplot2::geom_line(size = 1, col = 'blue') +
    ggplot2::geom_point(size = 1.5, col = 'blue') + ggplot2::xlab('Time') +
    ggplot2::scale_y_continuous(breaks = as.numeric(categories), labels = categories) +
    ggplot2::ylab('') + ggplot2::theme(axis.text = ggplot2::element_text(size = 11),
                     axis.title = ggplot2::element_text(size = 12),
                     plot.title = ggplot2::element_text(hjust = 0.5, size = 12)) +
    ggplot2::ggtitle(title) + ggplot2::ylab('Category')

  return(plot_cts)

}

