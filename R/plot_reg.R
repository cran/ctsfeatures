

#' Constructs the rate evolution graph for a categorical time series
#'
#' \code{plot_reg} constructs the rate evolution graph
#' proposed by \insertCite{ribler1997visualizing;textual}{ctsfeatures}.
#'
#' @param series An object of type \code{tsibble} (see R package \code{tsibble}), whose column named Values
#' contains the values of the corresponding CTS. This column must be of class \code{factor} and its levels
#' must be determined by the range of the CTS.
#' @param title The title of the graph.
#' @param linear_fit Logical. I \code{TRUE}, the corresponding least squares
#' lines are incorporated to the graph
#' @param cat_res If this parameter is set to any of the categories of the
#' series, then the function returns a graph of residuals for the linear model
#' associated with the corresponding category
#' @param ... Additional parameters for the function.
#' @return The rate evolution graph.
#' @examples
#' sequence_1 <- GeneticSequences[which(GeneticSequences$Series==1),]
#' reg <- plot_reg(sequence_1) # Constructing the rate
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

plot_reg <- function(series, title = 'Rate evolution graph',
                     linear_fit = FALSE, cat_res = NULL, ...) {

  x <- y <- z <- NULL
  check_cts(series$Value)
  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset

  # Computing the binarization of the CTS

  bin_series <- binarization(series)

  # Computing the matrix of cumulative sums

  cum_matrix <- base::apply(bin_series, 2, cumsum)

  # Creating a data frame

  time_points <- base::rep(1 : series_length, n_cat)
  extended_categories <- factor(rep(categories, each = series_length))
  df <- data.frame(x = time_points, y = c(cum_matrix), z = extended_categories)

  # Constructing the graph

  if (linear_fit == TRUE) {

    plot <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, group = z)) +
      ggplot2::geom_line(ggplot2::aes(colour = z), size = 0.8) +
      ggplot2::xlab('Time') + ggplot2::ylab('Cumulative values') +
      ggplot2::geom_smooth() +
      ggplot2::ggtitle(title) +
      ggplot2::theme(axis.title = ggplot2::element_text(size = 12),
                     axis.text = ggplot2::element_text(size = 11),
                     legend.text = ggplot2::element_text(size = 8),
                     legend.title = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5, size = 12),
                     legend.position = 'bottom',...)

    return(plot)

  }


  if (!is.null(cat_res)) {

  indexes_z <- which(df$z == cat_res)
  df_cat <- df[indexes_z, c(1, 2)]
  lm_cat <- stats::lm(df_cat$y~df_cat$x)
  df_new <- base::data.frame(x = df_cat$x, y = lm_cat$residuals)
  plot <- ggplot2::ggplot(df_new, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() + ggplot2::geom_hline(yintercept = 0)
  return(plot)

  }


  if (linear_fit == FALSE & is.null(cat_res)) {

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

  }


  return(plot)

}
