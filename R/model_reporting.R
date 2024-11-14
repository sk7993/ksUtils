#' Tidy broom tables
#'
#' @param tidy_df Tibble
#' @param est_round Number of digits to round to
#'
#' @return A tibble
#' @export
#' @import broom

tidy_p <- function(tidy_df, terms, dig_est = 2, dig_p = 3, conf.level) {

  res <- tidy_df %>%
    select(term, estimate,conf.low, conf.high, p.value) %>%
    filter(term %in% terms)

  res <- res %>%
    mutate(
      trt_eff = sprintf("%.*f (%s CI: %.*f, %.*f)",
                         dig_est, estimate,
                        conf.level,
                         dig_est, conf.low,
                         dig_est, conf.high),
      p_val = sprintf("%.*f",
                      dig_p, p.value)
    ) %>%
    select(term, trt_eff, p_val) %>%
    rename("Treatment effect (CI)" = trt_eff,
           "P value" = p_val)
}
