

#' Computes the relative frequency of motifs in a categorical time series
#'
#' \code{calculate_motifs} computes the motifs of a categorical time series
#'
#' @param series An object of type \code{tsibble} (see R package \code{tsibble}), whose column named Value
#' contains the values of the corresponding CTS. This column must be of class \code{factor} and its levels
#' must be determined by the range of the CTS.
#' @param motif_length The length of the motif.
#' @return Returns an array with the relative frequency of motifs in a
#' categorical time series.
#' @examples
#' sequence_1 <- GeneticSequences[which(GeneticSequences$Series==1),]
#' calculate_motifs(sequence_1, motif_length = 3)
#' # Computing the relative frequencies of motifs of length 3 for the first
#' # series in dataset GeneticSequences
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, and a motif length \eqn{L},
#' the function returns an array of \eqn{r^L} elements, with the element
#' in the position \eqn{(i_1, i_2, \ldots, i_r)} being the relative frequency
#' of the motif ``\eqn{i_1i_2 \cdots i_r}'' in the corresponding time series.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{lonardi2002finding}{ctsfeatures}
#'
#' }
#' @export

calculate_motifs <- function(series, motif_length){


  check_cts(series)
  series_length <- length(series$Value) # Series length
  categories <- levels(series$Value)
  n_cat <- length(categories) # Number of categories in the dataset
  binarized_series <- binarization(series)



  if (motif_length == 2) {

    n_motifs <- series_length - motif_length + 1
    array_motifs <- array(0, dim = rep(n_cat, motif_length))

    for (i in 1 : n_cat) {

      for (j in 1 : n_cat) {

        vector_1 <- auxiliary_motifs(binarized_series[,i], 0, 1)
        vector_2 <- auxiliary_motifs(binarized_series[,j], 1, 0)

        array_motifs[i, j] <- sum(vector_1 * vector_2)

      }

    }

    return(array_motifs/(n_motifs))

  }




  if (motif_length == 3) {

    n_motifs <- series_length - motif_length + 1
    array_motifs <- array(0, dim = rep(n_cat, motif_length))

    for (i in 1 : n_cat) {

      for (j in 1 : n_cat) {

        for (k in 1 : n_cat) {


        vector_1 <- auxiliary_motifs(binarized_series[,i], 0, 2)
        vector_2 <- auxiliary_motifs(binarized_series[,j], 1, 1)
        vector_3 <- auxiliary_motifs(binarized_series[,k], 2, 0)


        array_motifs[i, j, k] <- sum(vector_1 * vector_2 * vector_3)

        }

      }

    }

    return(array_motifs/n_motifs)

  }


  if (motif_length == 4) {

    n_motifs <- series_length - motif_length + 1
    array_motifs <- array(0, dim = rep(n_cat, motif_length))

    for (i in 1 : n_cat) {

      for (j in 1 : n_cat) {

        for (k in 1 : n_cat) {

          for (l in 1 : n_cat) {


          vector_1 <- auxiliary_motifs(binarized_series[,i], 0, 3)
          vector_2 <- auxiliary_motifs(binarized_series[,j], 1, 2)
          vector_3 <- auxiliary_motifs(binarized_series[,k], 2, 1)
          vector_4 <- auxiliary_motifs(binarized_series[,l], 3, 0)


          array_motifs[i, j, k, l] <- sum(vector_1 * vector_2 * vector_3 * vector_4)

          }

        }

      }

    }

    return(array_motifs/n_motifs)

  }



}
