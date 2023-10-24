

#' Constructs a control chart for the marginal distribution of a categorical
#' series
#'
#' \code{plot_mcc} constructs a control chart for the marginal distribution
#' of a categorical series
#'
#' @param series A CTS.
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @param c The hypothetical marginal distribution.
#' @param sigma A matrix containing the variances for each category (columns)
#' and each time t (rows).
#' @param lambda The constant lambda to construct the EWMA estimator.
#' @param k The constant k to construct the k sigma limits.
#' @param min_max Logical. If \code{min_max = FALSE} (default), the standard
#' control chart for the marginal distribution is plotted. Otherwise, the
#' reduced control chart is plotted, i.e., only the minimum and maximum values
#' of the standardized statistics (with respect to the set of categories) are considered.
#' @param plot Logical. If \code{plot = TRUE} (default), returns the control
#' chart. Otherwise, returns the standardized statistics or their maximum and
#' minimum value for each time t.
#' @param title The title of the graph.
#' @param ... Additional parameters for the function.
#' @return If \code{plot = TRUE} (default), represents the control chart for the marginal distribution. Otherwise, the function
#' returns a matrix with the values of the standardized statistics for each time t
#' @examples
#' cycle_md <- plot_mcc(series = SyntheticData1$data[[1]],
#' categories = factor(c('1', '2', '3')), c = c(0.3, 0.3, 0.4),
#' sigma = matrix(rep(c(1, 1, 1), 600), nrow = 600)) # Representing
#' # a control chart for the marginal distribution
#' cycle_md <- plot_mcc(series = SyntheticData1$data[[1]],
#' categories = factor(c('1', '2', '3')), c = c(0.3, 0.3, 0.4),
#' sigma = matrix(rep(c(1, 1, 1), 600), nrow = 600)) # Computing the
#' # corresponding standardized statistic
#' @details
#' Constructs a control chart of a CTS with range \eqn{\mathcal{V}=\{1, \ldots, r\}} based on the marginal distribution. The chart relies on the
#' standardized statistic \eqn{T_{t, i}=\frac{\hat{\pi}_{t, i}^{(\lambda)}-p_i}{k \cdot \sigma_{t, i}}}, where the \eqn{\hat{\pi}_{t, i}^{(\lambda)}},
#' \eqn{i=1,\ldots,r}, are the components of the EWMA estimator of the marginal
#' distribution, \eqn{p_i} is the marginal probability of category \eqn{i},
#' \eqn{\sigma_{t,i}} is the variance of \eqn{\hat{\pi}_{t, i}^{(\lambda)}} and \eqn{k}
#' is a constant set by the user. If \code{min_max = FALSE}, then only the
#' statistics \eqn{T_t^{\min }=\min_{i \in \mathcal{V}} T_{t, i}} and
#' \eqn{T_t^{\max }=\max_{i \in \mathcal{V}} T_{t, i}} are plotted.
#' An out-of-control alarm is signalled if the statistics are below -1 or
#' above 1.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weiss2008visual}{ctsfeatures}
#'
#' }
#' @export

plot_mcc <- function(series, categories, c, sigma, lambda = 0.99, k = 3.3, min_max = FALSE,
                                   plot = TRUE, title = 'Control chart (marginal)',...) {

  x1 <- y1 <- x2 <- y2 <- NULL
  check_cts(series)
  series_length <- length(series)
  n_categories <- length(categories)
  binarized_series <- binarization(series, categories = categories)
  matrix_c <- base::matrix(rep(c, series_length), nrow = series_length)

  ewma_estimator <- list()
  ewma_estimator[[1]] <- c

  for (i in 2 : (series_length + 1)) {

    ewma_estimator[[i]] <- lambda %*% ewma_estimator[[i - 1]] + (1 - lambda) %*% binarized_series[(i -1),]

  }

  series_ewma_estimator <- list_to_matrix(ewma_estimator)[2 : (series_length + 1),]
  series_t_statistic <- (series_ewma_estimator - matrix_c)/k * sqrt(sigma)
  colnames(series_t_statistic) <- categories

  x_values <- (1 : series_length)
  df_plot <- NULL
  df_plot_1 <- data.frame(x1 = 1 : series_length, y1 = 1)
  df_plot_2 <- data.frame(x2 = 1 : series_length, y2 = -1)

  for (i in 1 : n_categories) {

    temp_df_plot <- data.frame(x = x_values, y = series_t_statistic[, i], col = rep(categories[i], series_length))
    df_plot <- base::rbind(df_plot, temp_df_plot)

  }

  x <- df_plot$x
  y <- df_plot$y
  vector_labels <- categories


  if (min_max == FALSE) {

    if (plot == TRUE) {

  plot_control_chart <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y,
                                             group = col, colour = factor(col))) + ggplot2::geom_line(size = 0.5) +
    ggplot2::geom_line(data = df_plot_1, ggplot2::aes(x = x1, y = y1), size = 0.5, inherit.aes = FALSE) +
    ggplot2::geom_line(data = df_plot_2, ggplot2::aes(x = x2, y = y2), size = 0.5, inherit.aes = FALSE) +
    ggplot2::xlab('Time') +
    ggplot2::ylab('Control statistic') +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::ggtitle(title) + ggplot2::theme(legend.title = ggplot2::element_blank(),
                                             legend.text = ggplot2::element_text(size = 10), axis.text = ggplot2::element_text(size = 10),
                                             axis.title = ggplot2::element_text(size = 10), plot.title = ggplot2::element_text(hjust = 0.5,
                                                                                                                               size = 13)) + ggplot2::scale_color_discrete(labels = vector_labels)

  return(plot_control_chart)


    } else {

      return(series_t_statistic)

    }


  } else {

    series_t_statistic_reduced_min <- base::apply(series_t_statistic, 1, min)
    series_t_statistic_reduced_max <- base::apply(series_t_statistic, 1, max)
    series_t_statistic_reduced <- cbind(series_t_statistic_reduced_min,
                                        series_t_statistic_reduced_max)
    colnames(series_t_statistic_reduced) <- c('Min', 'Max')

    df_plot <- NULL

    for (i in 1 : 2) {

      temp_df_plot <- data.frame(x = x_values, y = series_t_statistic_reduced[, i], col = rep(i, series_length))
      df_plot <- base::rbind(df_plot, temp_df_plot)

    }

    x <- df_plot$x
    y <- df_plot$y
    vector_labels <- c('Min', 'Max')

    if (plot == TRUE) {

      plot_control_chart <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y,
                                                                  group = col, colour = factor(col))) + ggplot2::geom_line(size = 0.5) +
        ggplot2::geom_line(data = df_plot_1, ggplot2::aes(x = x1, y = y1), size = 0.5, inherit.aes = FALSE) +
        ggplot2::geom_line(data = df_plot_2, ggplot2::aes(x = x2, y = y2), size = 0.5, inherit.aes = FALSE) +
        ggplot2::xlab('Time') +
        ggplot2::ylab('Control statistic') +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::ggtitle(title) + ggplot2::theme(legend.title = ggplot2::element_blank(),
                                                 legend.text = ggplot2::element_text(size = 10), axis.text = ggplot2::element_text(size = 10),
                                                 axis.title = ggplot2::element_text(size = 10), plot.title = ggplot2::element_text(hjust = 0.5,
                                                                                                                                   size = 13)) + ggplot2::scale_color_discrete(labels = vector_labels)

      return(plot_control_chart)


    } else {

      return(series_t_statistic_reduced)




  }

  }

}
