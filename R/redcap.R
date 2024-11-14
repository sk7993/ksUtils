#' Split raw CRF by forms
#'
#' `split_crf()` splits a raw Redcap CRF into sets of variables based on forms and
#' instrument event mappings. Useful for CRFs with longitudinal data (i.e., repeating
#' instruments/measures).
#'
#' @param df_crf Raw CRF obtained from REDCAP
#' @param df_metadata CRF metadata
#' @param df_iem CRF instrument-event mapping data
#' @param form_names Character vector of unique form names
#'
#' @return A list
#' @import dplyr
#' @family redcap
#' @export
split_crf <- function(df_crf,
                     df_metadata,
                     df_iem,
                     form_names) {

  split_crf <- list()
  #form_names = unique(df_metadata$form_name)

  for (f in unique(form_names)) {

    fields <- df_metadata %>%
      filter(form_name==f &
               field_type != "descriptive") %>%
      pull(field_name)

    instruments <- df_iem %>%
      filter(form == f) %>%
      pull(unique_event_name)

    if (length(instruments)==0) {
      warning("Missing instrument-event mapping for form \"", f, "\"")
    }

    split_crf[[f]] <-  df_crf %>%
      # Get relevant fields for a form
      select(study_id,
             starts_with("redcap_"),
             contains(fields)) %>%
      # Only get relevant instruments
      filter(redcap_event_name %in%
               instruments)
  }

  return(split_crf)
}

#' Get REDCAP metadata
#'
#' @param url URL
#' @param token API token
#'
#' @return A tibble
#' @family redcap
#' @export
get_rc_metadata <- function(url="https://redcap.ccf.org/redcap/api/",
                         token){

    formData <- list("token"=token,
                   content='metadata',
                   format='csv',
                   returnFormat='json'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  response <- httr::content(response) %>%
    janitor::clean_names()

  return(response)
}

#' Get REDCAP instrument-event mapping
#'
#' @param url URL
#' @param token API token
#'
#' @return A tibble
#' @family redcap
#' @export
get_rc_iem <- function(url="https://redcap.ccf.org/redcap/api/",
                         token){

  formData <- list("token"=token,
                   content='formEventMapping',
                   format='csv',
                   returnFormat='json'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  response <- httr::content(response) %>%
    janitor::clean_names()

  return(response)
}

#' Get REDCAP records
#'
#' @param url URL
#' @param token API token
#'
#' @return A tibble
#' @family redcap
#' @export
get_rc_records <- function(url = "https://redcap.ccf.org/redcap/api/",
                           token){
  formData <- list("token"=token,
                   content='record',
                   action='export',
                   format='csv',
                   type='flat', # or EAV for long data
                   csvDelimiter='',
                   rawOrLabel='raw',
                   rawOrLabelHeaders='raw',
                   exportCheckboxLabel='false',
                   exportSurveyFields='false',
                   exportDataAccessGroups='false',
                   returnFormat='json'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  response <- httr::content(response) %>%
    janitor::clean_names()

  return(response)
}
