

auxiliary_total_correlation <- function(series, lag = 1, categories, features = FALSE) {

   check_cts(series)
   n_categories <- length(categories)
   matrix_prev <- base::matrix(0, nrow = n_categories, ncol = n_categories)

   for (i in 1 : n_categories) {

     for (j in 1 : n_categories) {

       matrix_prev[i, j] <- correlation_i_j_k_function(series = series,
                                                       i_cat = categories[i],
                                                       j_cat = categories[j],
                                                       k = lag,
                                                       categories = categories)

     }

   }

   matrix_sq <- matrix_prev^2

   if (features == FALSE) {

     return(sum(matrix_sq))

   } else {

     return(matrix_prev)

   }

}
