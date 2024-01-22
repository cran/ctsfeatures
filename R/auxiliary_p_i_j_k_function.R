

p_i_j_k_function <- function(series, i_cat, j_cat, k = 1) {

series_length <- length(series$Value) # Series length
a <- series$Value[(k + 1) : series_length]
b <- series$Value[1 : (series_length - k)]


number <- series_length - k
count <- numeric(number)

for (i in 1 : number) {

  if (a[i] == i_cat & b[i] == j_cat) {

    count[i] <- 1

  } else {

    count[i] <- 0

  }

}


return(sum(count)/(series_length-k))

}
