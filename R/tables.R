#' Create label list from dataframe
#'
#' @param df
#'
#' @return A list of name-label pairs
#' @export
#'
#' @examples
#' create_label(iris)

create_label <- function(df) {
  labels <- as.list(names(df))
  names(labels) <- names(df)
  return(dput(labels))
}


#' Create table 1
#'
#' @param vars Character vector of variables to summarize
#' @param strata Grouping variable
#' @param data Dataframe
#' @param smd Report ASD or not.
#' @param ...
#'
#' @return A flextable object
#' @export
#'
#' @import dplyr tableone flextable

tbl12ft <- function(vars = NULL,
                    strata,
                    data = NULL,
                    smd = T,
                    ...) {

  tbl1 <- CreateTableOne(vars,
                         strata,
                         data,
                         test=F)
  tbl1 <- tbl1 %>%
    print(varLabels = T,
          explain = F,
          smd = smd,
          dropEqual = T,
          noSpaces = T,
          printToggle = F,
          ...) %>%
    as_tibble(rownames = "Characteristic")

  if (smd == T) {

    tbl1 <- tbl1 %>%
      rename(ASD = SMD)
  }

  subcat_ind <- grep("^\\s+", tbl1$Characteristic)

  tbl1_ft <- tbl1 %>%
    flextable() %>%
    bold(i=1, part = "header") %>%
    italic(i = subcat_ind, j = 1) %>%
    padding(i = subcat_ind, j = 1, padding.left = 20) %>%
    font(fontname = "Arial",
         part = "all") %>%
    autofit()

  return(tbl1_ft)
}
