
`%nin%` = Negate(`%in%`)


#' Pipe logger
#'
#' @param data A dataframe
#' @param msg Message to print
#' @param fun Function to apply to dataframe
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' library(dplyr)
#' iris %>% p_print("Dimensions of iris dataframe")

p_print <- function(data, msg = NULL, fun = dim){
  # Print arbitrary messages based on applying fxns to pipe output
  # Prints dim of dataframe by default
  # Useful to keep track of dplyr ops
  cat(msg, "\n",
      fun(data), "\n")
  return(data)
}


#' Check if variable is binary
#'
#' @param A numeric vector
#'
#' @return A logical vector
#' @export
#'
#' @examples
#' var_is_bin(c(0,0,0,1,NA))
var_is_bin <- function(x){
  all(unique(x) %in% c(0, 1, NA))
}

#' Factor variable with one level
#'
#' @param x A factor variable
#'
#' @return A logical vector
#' @export


cat_one_lvl <- function(x){
  # Check if no. levels for factor variable
  # is 1

  if (is.factor(x)) {
    return(nlevels(x) == 1)
  }
  else {
    return(FALSE)
  }
}

