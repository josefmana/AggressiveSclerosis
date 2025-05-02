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
    left_join(
      edss,
      demographics |> select(id, onset),
      by = "id"
    ) |>
    mutate(
      edss_time = time_length(difftime(visit_date, onset), "years")
    )
  # Unique patients IDs:
  unique_ids <- d0$id |> unique()
  # Print cases with negative disease duration and drop them from further analysis:
  negative_cases <- subset(d0, edss_time < 0)
  print(negative_cases, n = Inf)
  cat("\nThe cases printed above show negative disease
duration at the time of EDSS assessment.
Patients with these data are dropped in further analyses.\n\n")
  # Drop data of patients with negative cases:
  N0 <- length(unique(d0$id)) # Original number of patients.
  negative_ids <- unique(negative_cases$id)
  incl_ids0 <- unique_ids[!(unique_ids %in% negative_ids)]
  no_dropped0 <- length(negative_ids)
  d1 <- subset(d0, !(id %in% negative_ids))
  cat(glue::glue(
"\nDropping {no_dropped0} out of {N0} patients
due to negative disease duration observations ...\n\n"
  ))
  # Criterion i
  N1 <- length(unique(d1$id))
  message("\nEvaluating the first criterion - disease duration ≥ 10 years ...\n")
  incl_ids1 <- # All patients with at least one EDSS recorded at disease duration ≥ 10 years
    d1 |>
    filter(edss_time >= 10) |>
    select(id) |>
    pull() |>
    unique()
  excl_ids1 <- c(na.omit(incl_ids1[!(incl_ids0 %in% incl_ids1)]))
  no_dropped1 <- length(excl_ids1)
  d2 <- subset(d1, !(id %in% excl_ids1))
  cat(glue::glue(
"\nDropping {no_dropped1} out of {N1} remaining patients
due to not having any EDSS observation after
10 years of disease duration ...\n\n"
  ))
  # Criterion ii
  N2 <- length(unique(d2$id))
  message("\nEvaluating the second criterion - EDSS ≥ 6 within the first 10 years of disease ...\n")
  incl_ids2 <-
    d2 |>
    filter(edss_time < 10 & edss >= 6) |>
    select(id) |>
    pull() |>
    unique()
}

## ADD PERCENTAGES
## PREPARE HELPER FUNCTION/S FOR REPEATED ACTIONS
