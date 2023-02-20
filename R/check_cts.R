

# This is a function to check if an object is a CTS

# Input parameters
# X: an object

#--------------------------------------------------------------------------------------

check_cts <- function(X) {


  # The element must be a factor


  # if (!is.factor(X)) {

  #  stop('The object must be a factor')

  # }


  # The element can not contain NA entries

  check_nas <- sum(is.na(X))

  if (sum(check_nas) != 0) {

    stop('There are some NAs in the series')

  }

}
