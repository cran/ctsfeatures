

auxiliary_motifs <- function(vector, left, right) {

  l_vector <- length(vector)

  if (left > 0 & right > 0) {

  vector_new <- vector[-c(1 : left, (l_vector - right + 1) : l_vector)]

  }

  if (left > 0 & right == 0) {

    vector_new <- vector[-c(1 : left)]

  }

  if (left == 0 & right > 0) {

    vector_new <- vector[- ((l_vector - right + 1) : l_vector)]

  }



  return(vector_new)

}
