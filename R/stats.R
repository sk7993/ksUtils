#' Calculate odds
#'
#' @param p Probability
#'
#' @return Numeric vector of length 1
#' @family stats
#' @export
odds <- function(p){
  p/(1-p)
}

#' Median IQR from numeric vector
#'
#' @param x Any numeric vector
#'
#' @return A character vector
#' @family stats
#' @export
#'
#' @examples
#' paste_median(iris$Sepal.Length)
#'
#' d <- data.frame(x = paste_median(iris$Sepal.Length, pub = T))
#' flextable::flextable(d)
paste_median <- function(x, pub = F) {

  if (pub == F) {
    sprintf("%.0f [%.0f, %.0f]; n = %.0f; Missing = %.0f",
            stats::median(x, na.rm = T),
            stats::quantile(x, 0.25, na.rm = T),
            stats::quantile(x, 0.75, na.rm = T),
            length(x),
            sum(is.na(x))
    )
  }
  else if (pub == T) {
    sprintf("%.0f [%.0f, %.0f]\n(n = %.0f)",
            stats::median(x, na.rm = T),
            stats::quantile(x, 0.25, na.rm = T),
            stats::quantile(x, 0.75, na.rm = T),
            sum(!is.na(x))
    )

  }
}

#' Summary statistics, n (%), from binary variable
#' @param x: A vector with only two unique elements (0, 1)
#'
#' @return A character vector
#' @export
#'
#' @examples
#' paste_bin()
paste_bin <- function(x){
  x <- na.omit(x)
  sprintf("%.0f/%.0f (%.0f%%)",
          sum(x[x==1]),
          length(x),
          mean(x)*100
          )
}
