% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap.R
\name{split_crf}
\alias{split_crf}
\title{Split raw CRF by forms}
\usage{
split_crf(df_crf, df_metadata, df_iem, form_names)
}
\arguments{
\item{df_crf}{Raw CRF obtained from REDCAP}

\item{df_metadata}{CRF metadata}

\item{df_iem}{CRF instrument-event mapping data}

\item{form_names}{Character vector of unique form names}
}
\value{
A list
}
\description{
\code{split_crf()} splits a raw Redcap CRF into sets of variables based on forms and
instrument event mappings. Useful for CRFs with longitudinal data (i.e., repeating
instruments/measures).
}
\seealso{
Other redcap: 
\code{\link{get_rc_iem}()},
\code{\link{get_rc_metadata}()},
\code{\link{get_rc_records}()}
}
\concept{redcap}
