

#' Constructs a control chart for the cycle lengths of a categorical series
#'
#' \code{plot_ccc} constructs a control chart for the cycle lengths of a categorical series
#'
#' @param series An object of type \code{tsibble} (see R package \code{tsibble}), whose column named Values
#' contains the values of the corresponding CTS. This column must be of class \code{factor} and its levels
#' must be determined by the range of the CTS.
#' @param mu_t The mean of the process measuring the cycle lengths.
#' @param lcl_t The lower control limit.
#' @param ucl_t The upper control limit.
#' @param plot Logical. If \code{plot = TRUE} (default), returns the control
#' chart. Otherwise, returns the standardized statistic.
#' @param title The title of the graph.
#' @param ... Additional parameters for the function.
#' @return If \code{plot = TRUE} (default), represents the control chart for the cycle lengths. Otherwise, the function
#' returns a matrix with the values of the standardized statistic for each time t
#' @examples
#' sequence_1 <- SyntheticData1[which(SyntheticData1$Series==1),]
#' cycle_cc <- plot_ccc(series = sequence_1, mu_t = c(1, 1.5, 1),
#' lcl_t = rep(10, 600), ucl_t = rep(10, 600)) # Representing
#' # a control chart for the cycle lengths
#' cycle_cc <- plot_ccc(series = sequence_1, mu_t = c(1, 1.5, 1),
#' lcl_t = rep(10, 600), ucl_t = rep(10, 600), plot = FALSE) # Computing the
#' # corresponding standardized statistic
#' @details
#' Constructs a control chart of a CTS based on cycle lengths. The chart is based on the
#' standardized statistic \eqn{T_t=T_t^{(L)}+T_t^{(U)}}, with \eqn{T_t^{(L)}=\min \left(0, \frac{C_t-\mu_t}{\left|L C L_t-\mu_t\right|}\right)}
#' and \eqn{T_t^{(U)}=\max \left(0, \frac{C_t-\mu_t}{\left|U C L_t-\mu_t\right|}\right)},
#' where \eqn{Z_t} expresses the length of a cycle ending with a specific category,
#' \eqn{\mu_t} denotes the mean of \eqn{Z_t} and \eqn{LCL_t} and \eqn{UCL_t} are
#' lower and upper individual control limits, respectively. Note that an
#' out-of-control alarm is signalled if \eqn{T_t<-1} or \eqn{T_t>1}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weiss2008visual}{ctsfeatures}
#'
#' }
#' @export

plot_ccc <- function(series, mu_t, lcl_t, ucl_t,
                             plot = TRUE, title = 'Control chart (cycles)',...) {

  x <- y <- NULL
  check_cts(series$Value)
  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset
  list_series_cycles <- list()

  for (i in 1 : n_cat) {

  indicator_series <- as.numeric(series$Value == categories[i])
  position_1s <- which(indicator_series == 1)
  cycles <- base::diff(position_1s)
  list_series_cycles[[i]] <- numeric(series_length)
  list_series_cycles[[i]][which(indicator_series == 1)][-1] <- cycles

  }

  z_t <- Reduce('+', list_series_cycles)
  subseries_1_prev <- (z_t - mu_t)/(abs(lcl_t - mu_t))
  subseries_2_prev <- (z_t - mu_t)/(abs(ucl_t - mu_t))
  subseries_1 <- subseries_1_prev
  subseries_2 <- subseries_2_prev
  subseries_1[subseries_1_prev > 0] <- 0
  subseries_2[subseries_2_prev < 0] <- 0
  t_t <- subseries_1 + subseries_2
  t_modified <- t_t[which(t_t != -1)]
  x_values <- (1 : series_length)[which(t_t != -1)]

  df_plot_1 <- data.frame(x = x_values, y = t_modified)
  df_plot_2 <- data.frame(x = 1 : series_length, y = 1)
  df_plot_3 <- data.frame(x = 1 : series_length, y = -1)

  plot_control_chart <- ggplot2::ggplot(df_plot_1, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(col = 'blue') +
    ggplot2::geom_line(data = df_plot_2, mapping = ggplot2::aes(x = x, y = y), size = 0.7) +
    ggplot2::geom_line(data = df_plot_3, mapping = ggplot2::aes(x = x, y = y), size = 0.7) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab('Time') +
    ggplot2::ylab('Control statistic') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
          axis.text.y = ggplot2::element_text(size = 10),
          axis.title = ggplot2::element_text(size = 10),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 13),...)

  if (plot == TRUE) {

    return(plot_control_chart)

  } else {

    return(t_t)

  }


}
