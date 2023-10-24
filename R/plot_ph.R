

#' Constructs the pattern histogram associated with a given category of a
#' categorical time series
#'
#' \code{plot_ph} constructs the pattern histogram associated with a given category of a
#' categorical time series.
#'
#' @param series A CTS.
#' @param category The selected category.
#' @param plot Logical. If \code{plot = TRUE} (default), returns the pattern
#' histogram. Otherwise, returns the frequencies of cycle lengths associated
#' with the corresponding category.
#' @param title The title of the graph.
#' @param ... Additional parameters for the function.
#' @return The pattern histogram.
#' @examples
#' ph <- plot_ph(GeneticSequences$data[[1]],
#' category = 'a') # Constructing the pattern histogram
#' # for the first CTS in dataset GeneticSequences concerning the category 'a'
#' cycle_lengths <- plot_ph(GeneticSequences$data[[1]],
#' category = 'a', plot = FALSE) # Obtaining the frequencies of cycle lengths
#' @details
#' Constructs the pattern histogram for a specific category of a CTS. This graph
#' represents the frequencies of the cycles for the corresponding category according
#' to their length.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weiss2008visual}{ctsfeatures}
#'
#' }
#' @export

plot_ph <- function(series, category,
                              plot = TRUE,
                              title = paste0('Pattern histogram (', category, ')'), ...) {
  x <- NULL
  check_cts(series)
  indicator_series <- as.numeric(series == category)
  position_1s <- which(indicator_series == 1)
  cycles <- base::diff(position_1s)

  df_plot <- data.frame(cycles)
  df_plot[,1] <- as.numeric(df_plot[,1])
  colnames(df_plot) <- c('x')

  plot_ph_plot <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(fill = 'blue') + ggplot2::xlab('') + ggplot2::ylab('Count') +
    ggplot2::ggtitle(title) + ggplot2::xlab('Cycle length') +
    ggplot2::theme(axis.title = ggplot2::element_text(size = 12),
          axis.text = ggplot2::element_text(size = 11),
          legend.text = ggplot2::element_text(size = 11),
          legend.title = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 12),
          legend.position = 'bottom',...)
  df_histogram <- data.frame(table(df_plot))
  colnames(df_histogram) <- c('Cycle length', 'Count')

  if (plot == TRUE) {

    return(plot_ph_plot)

  } else {

    return(df_histogram)

  }


}
