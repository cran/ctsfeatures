

auxiliary_total_correlation <- function(series, lag = 1, features = FALSE) {

   check_cts(series$Value)
   series_length <- length(series$Value) # Series length
   categories <- levels(series$Value)
   n_cat <- length(categories) # Number of categories in the dataset
   matrix_prev <- base::matrix(0, nrow = n_cat, ncol = n_cat)

   for (i in 1 : n_cat) {

     for (j in 1 : n_cat) {

       matrix_prev[i, j] <- correlation_i_j_k_function(series = series,
                                                       i_cat = i,
                                                       j_cat = j,
                                                       k = lag)

     }

   }

   matrix_sq <- matrix_prev^2

   if (features == FALSE) {

     return(sum(matrix_sq))

   } else {

     return(matrix_prev)

   }

}
