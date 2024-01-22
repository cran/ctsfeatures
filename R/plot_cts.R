

#' Constructs a categorical time series plot
#'
#' \code{plot_cts} constructs a categorical time series plot
#'
#' @param series An object of type \code{tsibble} (see R package \code{tsibble}), whose column named Values
#' contains the values of the corresponding CTS. This column must be of class \code{factor} and its levels
#' must be determined by the range of the CTS.
#' @param title The title of the graph.
#' @return The categorical time series plot.
#' @examples
#' sequence_1 <- GeneticSequences[which(GeneticSequences$Series==1),]
#' time_series_plot <- plot_cts(series = sequence_1) # Constructs a categorical
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

plot_cts <- function(series, title = 'Time series plot') {

  x <- y <- NULL
  check_cts(series$Value)
  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset
  numeric_series <- numeric(series_length)

  for (i in 1 : n_cat) {

    indexes_i <- which(series == categories[i])
    numeric_series[indexes_i] <- i

  }

  df_plot <- data.frame(x = 1 : series_length, y = numeric_series)

  plot_cts <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y)) + ggplot2::geom_line(size = 1, col = 'blue') +
    ggplot2::geom_point(size = 1.5, col = 'blue') + ggplot2::xlab('Time') +
    ggplot2::scale_y_continuous(breaks = as.numeric(factor(categories)), labels = categories) +
    ggplot2::ylab('') + ggplot2::theme(axis.text = ggplot2::element_text(size = 11),
                     axis.title = ggplot2::element_text(size = 12),
                     plot.title = ggplot2::element_text(hjust = 0.5, size = 12)) +
    ggplot2::ggtitle(title) + ggplot2::ylab('Category')

  return(plot_cts)

}

