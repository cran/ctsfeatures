#'
#' @title SleepStages
#' @description Categorical time series (CTS) of sleep stages from different
#' subjects
#' @usage data(SleepStages)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 62 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes
#' associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a categorical time series
#' containing six categories (sleep stages). The numeric vector \code{classes} is formed
#' by integers from 1 to 2, indicating that there are 2 different classes in the database. Each class is associated with a different
#' sleep disease For more information, see \insertCite{li2022interpretable;textual}{ctsfeatures}.
#' @references{
#'
#'   \insertRef{li2022interpretable}{ctsfeatures}
#'
#' }
"SleepStages"


