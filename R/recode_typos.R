#' Manually recode typos in disease onset
#'
#' This function takes patient data and manually corrects known typos
#' in disease onset or duration information. It is intended to be used
#' within the \code{determine_aggressive_phenotype} workflow.
#'
#' Additionally, the function drops invalid data such as EDSS assessments
#' recorded before the documented disease onset.
#'
#' @param data A tibble used for determining the aggressive phenotype.
#'
#' @return A tibble identical to the input but with corrected disease duration
#' values and invalid data removed.
#'
#' @export
recode_typos <- function(data) {
  # Patients with a fixed different onset date from data:
  data <- data |> mutate(
    onset = case_when(
      id == 604 ~ as.Date("2006-01-02"), # first visit
      id == 1478 ~ as.Date("2011-09-07"), # first visit
      id == 4510 ~ as.Date("2019-05-30"), # one year sooner
      id == 4591 ~ as.Date("2011-11-01"), # first visit
      id == 4596 ~ as.Date("2020-01-14"), # first visit
      id == 4989 ~ as.Date("2017-09-06"), # fixed different date
      id == 5128 ~ as.Date("2024-11-01"), # fixed different date
      .default = onset
    )
  )
  # The disease onset was correct, dropping negative time assessments:
  data <- subset(data, !(id == 571 & edss_time < 0))
  data <- subset(data, !(id == 897 & edss_time < 0))
  data <- subset(data, !(id == 4952 & edss_time < 0))
  data <- subset(data, !(id == 4971 & edss_time < 0))
  data <- subset(data, !(id == 4972 & edss_time < 0))
  data <- subset(data, !(id == 4985 & edss_time < 0))
  # Patient 1147: drop because we do not have correct visit dates:
  data <- subset(data, id != 1147)
  # Re-calculate tome to assessment:
  data |> mutate(
    edss_time = time_length(difftime(visit_date, onset), "years")
  )
}
