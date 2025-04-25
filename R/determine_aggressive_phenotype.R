#' Determine whether patients' MS can be regarded
#' as aggressive.
#'
#' The function uses the following criteria to
#' determined whether patient ought to be regarded
#' as suffering aggressive form MS:
#' (i) at least 10 years of disease duration,
#' (ii) EDSS ≥ 6 appeared within the first 10
#' years of the disease,
#' (iii) EDSS ≥ 6 sustained for at least 6 months
#' after it appeared,
#' (iv) EDSS ≥ 6 was NOT within 30 days after relapse,
#' (v) EDSS did not go below 6 until the end of
#' observation period.
#'
#' @param demographics A tibble with basic demographic
#' variables.
#' @param relapses A tibble with relapse dates.
#' @param edss A tibble with EDSS scores.
#'
#' @returns A list of tibbles containing relevant
#' variables.
#'
#' @examples
#' \dontrun{
#' p <- here::here("data-raw", "some cool file name.xlsx")
#' d <- prepare_data(p)
#' outcome_data <- determine_aggressive_phenotype(d$id, d$relapses, d$edss)
#' }
#' @export
determine_aggressive_phenotype <- function(demographics, relapses, edss) {
  # Add time stamps to EDSS assessments:
  d0 <-
    left_join(edss, demographics |> select(id, onset), by = "id") |>
    mutate(edss_time = time_length(difftime(visit_date, onset), "years"))
  # Print cases with negative disease duration and drop them from further analysis:
  negative_cases <- subset(d0, edss_time < 0)
  print(negative_cases, n = Inf)
  cat("\nThe cases printed above show negative disease
duration at the time of EDSS assessment. Patients with
these data are dropped in further analyses.")
  # Drop data of patients with negative cases:
  negative_ids <- unique(negative_cases$id)
  no_dropped <- length(negative_ids)
  d1 <- subset(d0, !(id %in% negative_ids))
  cat(glue::glue("\nDropping {no_dropped} patients due to negative disease duration observations ...\n"))
}
