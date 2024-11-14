#' Project directories setup
#'
#' @return N/A
#' @export
#'
#' @family proj
dir.setup <- function(){

  folders <- c("./programs",
               "./cache",
               "./docs",
               "./data",
               "./o_data",
               "./o_reports",
               "./o_docs",
               "./notes",
               "./tmp")

  for (f in folders) {
    dir.create(f)
  }
}



#' Templates (unfinished)
#'
#' @return N/A
#' @export
#'
#' @family proj

templates <- function(){
  # Idea: copy over templates for protocols, reports etc.
  file.copy()
}
