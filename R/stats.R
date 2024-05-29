#' Calculate odds
#'
#' @param p Probability
#'
#' @return Numeric vector of length 1
#' @export
#'
#' @examples
odds <- function(p){
  p/(1-p)
}

#' Median IQR from numeric vector
#'
#' @param x Any numeric vector
#'
#' @return A character vector
#' @export
#' @import stats
#'
#' @examples
#' paste_median(iris$Sepal.Length)
paste_median <- function(x) {
  paste0(median(x, na.rm = T) %>% round() %>% as.integer, " [",
         quantile(x, 0.25, na.rm = T) %>% round() %>% as.integer(), ", ",
         quantile(x, 0.75, na.rm = T) %>% round() %>% as.integer(),
         "]")
}
