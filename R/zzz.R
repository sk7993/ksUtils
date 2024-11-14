.onLoad <- function(libname, pkgname){
  utils::globalVariables(
    c("SMD", "term", "estimate", "conf.low", "conf.high",
      "p.value",
      # splitCrf
      "field_name", "field_type","form","form_name",
      "redcap_event_name", "unique_event_name")
  )
}
