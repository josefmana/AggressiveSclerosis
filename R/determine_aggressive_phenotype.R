#' Determine whether patients' MS can be regarded as aggressive.
#'
#' The function uses the following criteria to determined whether
#' patient ought to be regarded as suffering aggressive form MS:
#' (i) do not suffer Primary Progressive phenotype
#' (ii) at least 10 years of disease duration,
#' (iii) EDSS ≥ 6 appeared within the first 10
#' years of the disease,
#' (iii) EDSS ≥ 6 sustained for at least 6 months
#' after it appeared,
#' (iv) EDSS did not go below 6 until the end of
#' observation period.
#' Across all, the supplementary criterion needs to
#' be met:
#' (suppl.) EDSS ≥ 6 was NOT within 30 days after relapse
#'
#' @param demographics A tibble with basic demographic variables.
#' @param relapses A tibble with relapse dates.
#' @param edss A tibble with EDSS scores.
#' @param eye_check A logical indicating whether plots of patients
#' classified as suffering the aggressive disease form should be
#' plotted for manual control.
#'
#' @returns A list with a tibble containing demographic data with
#' added column denoting aggressive disease ($data) and text
#' summarising the process ($text).
#'
#' @examples
#' \dontrun{
#' p <- here::here("data-raw", "some cool file name.xlsx")
#' d <- prepare_data(p)
#' outcome_data <- determine_aggressive_phenotype(d$id, d$relapses, d$edss, T)
#' }
#' @export
determine_aggressive_phenotype <- function(demographics, relapses, edss, eye_check = TRUE) {

  # Prepare data:
  d0 <- # Add time stamps to EDSS assessments.
    left_join(
      edss,
      demographics |> select(id, onset, phenotype),
      by = "id"
    ) |>
    mutate(
      edss_time = time_length(difftime(visit_date, onset), "years")
    )
  unique_ids <- # Extract patients' IDs
    d0$id |>
    unique()

  #
  N0 <- length(unique(d0$id)) # Original number of patients.
  pp_ids <- unique(subset(d0, phenotype == "PP")$id)
  no_pp <- length(pp_ids)
  d0 <- subset(d0, phenotype != "PP")
  cat(glue::glue("\nDropping {no_pp} out of {N0} patients due to PP.\n\n"))

  # Print cases with negative disease duration and drop them from further analysis:
  negative_cases <- subset(d0, edss_time < 0)
  print(negative_cases, n = Inf)
  message("\nThe cases printed above show negative disease duration at the time of EDSS assessment.
Patients with these data are dropped from further analyses.\n\n")
  # Drop data of patients with negative cases:
  N0 <- length(unique(d0$id)) # New original number of patients.
  negative_ids <- unique(negative_cases$id)
  incl_ids0 <- unique_ids[!(unique_ids %in% negative_ids)]
  no_dropped0 <- length(negative_ids)
  cat(glue::glue("\nDropping {no_dropped0} out of {N0} patients.\n\n"))
  d1 <- subset(d0, !(id %in% negative_ids))

  # Criterion (i)
  N1 <- length(unique(d1$id))
  message("\nEvaluating the first criterion - disease duration ≥ 10 years ... \n\n")
  incl_ids1 <- # All patients with at least one EDSS recorded at disease duration ≥ 10 years
    d1 |>
    filter(edss_time >= 10) |>
    select(id) |>
    pull() |>
    unique()
  excl_ids1 <- c(na.omit(incl_ids0[!(incl_ids0 %in% incl_ids1)]))
  no_dropped1 <- length(excl_ids1)
  cat(glue::glue("\nDropping {no_dropped1} out of {N1} remaining patients.\n\n"))
  d2 <-
    subset(d1, !(id %in% excl_ids1)) |>
    mutate(
      days_after_relapse =
        sapply(
          seq_len(length(visit_date)),
          function(i) {
            rels <- relapses[relapses$id == id[i], "relapse_date"]
            sapply(
              rels,
              function(j) {
                days <- time_length(difftime(visit_date[i], j), "days")
                Filter(function(x) x >= 0, days)
              }
            ) |>
              unlist() |>
              min()
          }
        )
    )

  # Criterion (ii)
  N2 <- length(unique(d2$id))
  message("\nEvaluating the second criterion - EDSS ≥ 6 within the first 10 years of disease ...\n\n")
  incl_ids2 <-
    d2 |>
    filter(edss_time < 10 & edss >= 6 & days_after_relapse > 30) |>
    select(id) |>
    pull() |>
    unique()
  excl_ids2 <- c(na.omit(incl_ids1[!(incl_ids1 %in% incl_ids2)]))
  no_dropped2 <- length(excl_ids2)
  cat(glue::glue("\nDropping {no_dropped2} out of {N2} remaining patients.\n\n"))
  d3 <- subset(d2, !(id %in% excl_ids2))

  # Criterion (iii)
  N3 <- length(unique(d3$id))
  message("\nEvaluating the third criterion - EDSS ≥ 6 sustained for at least 6 months ...\n\n")
  incl_ids3 <-
    d3 |>
    filter(edss >= 6 & days_after_relapse > 30) |>
    group_by(id) |>
    filter(row_number() == 1 | row_number() == n()) |>
    summarise(diff = max(edss_time)-min(edss_time)) |>
    ungroup() |>
    filter(diff > 0.5) |>
    select(id) |>
    pull() |>
    unique()
  excl_ids3 <- c(na.omit(incl_ids2[!(incl_ids2 %in% incl_ids3)]))
  no_dropped3 <- length(excl_ids3)
  cat(glue::glue("\nDropping {no_dropped3} out of {N3} remaining patients.\n\n"))
  d4 <- subset(d3, !(id %in% excl_ids3))

  # Criterion (v)
  N4 <- length(unique(d4$id))
  message("\nEvaluating the final criterion - EDSS ≥ 6 remaining until the end of observation period ...\n\n")
  d5 <- # Keep only cases with a valid last EDSS observation ≥ 6
    d4 |>
    left_join(
      d4 |>
        group_by(id) |>
        filter(row_number() == n()) |>
        summarise(last_edss = edss, last_relapse_time = days_after_relapse),
      by = "id"
    ) |>
    filter(last_edss >= 6 & last_relapse_time > 30)
  d6 <- # All of these need to be EDSS ≥ 6
    full_join(
      d5 |>
        filter(edss >= 6 & last_relapse_time > 30 & edss_time < 10) |>
        group_by(id) |>
        filter(row_number() == n()),
      d5 |> filter(edss_time >= 10)
    ) |>
    suppressMessages() |>
    arrange(id)
  # Extract patients who fulfill criteria for aggressive MS
  aggressiveMS <- sapply(unique(d6$id), function(i) all(na.omit(subset(d6, id == i)$edss >= 6)))
  aggressiveMSids <- unique(d6$id)[aggressiveMS]
  excl_ids4 <- incl_ids3[!(incl_ids3 %in% aggressiveMSids)]
  no_dropped4 <- length(excl_ids4)
  K <- length(aggressiveMSids)
  perc_aggr <- paste0(sprintf("%.2f", round(100*K/N2, 2)),"%")
  cat(glue::glue("\nDropping {no_dropped4} out of {N4} remaining patients.\n\n"))
  message(glue::glue(
    "\n\nThis has left {K} out of total {N2} eligible patients ({perc_aggr})
being classified as suffering the aggressive form of MS.\n\n"
  ))

  # Sanity check:
  if(eye_check) {
    message(
      "\nPlots containing EDSS data of patients classified
as suffering the aggressive form are shown for control.\n"
    )
    bins <- rep(seq_len(ceiling(length(aggressiveMSids)/9)), 9) |> sort()
    spl <- split(aggressiveMSids, bins)
    pb <- txtProgressBar(min = 0, max = length(spl), style = 3) # text based bar
    for(i in seq_len(length(spl))) {
      plt <-
        d5 |>
        filter(id %in% spl[[i]]) |>
        mutate(`Following relapse` = if_else(days_after_relapse < 30, T, F)) |>
        ggplot() +
        aes(x = edss_time, y = edss, group = id) +
        geom_point(size = 1.3, aes(colour = `Following relapse`)) +
        geom_line() +
        scale_colour_manual(values = c("black", "red3")) +
        geom_vline(xintercept = 10, colour = "red", linetype = "dashed") +
        geom_hline(yintercept = 6, colour = "red", linetype = "dashed") +
        facet_wrap(~id, ncol = 3) +
        labs(x = "Disease duration at assessment (years)", y = "EDSS")
      plot(plt)
      setTxtProgressBar(pb, i)
      Sys.sleep(20)
    }
    close(pb)
  }

  # Prepare a text summarising the process:
  txt <- glue::glue(
    "Data from {N0} patients out of whom {N2} patients met the criterion
of a minimum of 10 years observation time, as defined by recorded
EDSS scores, were extracted. Of these, {no_dropped2} patients did not meet
the criterion of EDSS ≥ 6 within the first 10 years of disease.
Further {no_dropped3} patients did not meet the criterion of sustaining EDSS ≥ 6
until the end of the observation period and {no_dropped4} patients did not
meet the criterion of sustaining EDSS ≥ 6 logitudinally.
The final sample thus comprised {N2} patients, of whom {K}
({perc_aggr}) met criteria for aggressive disease."
  )
  message(glue::glue(paste0(txt, "\n\n")))

  # Prepare data:
  df <-
    demographics |>
    filter(id %in% incl_ids1) |>
    mutate(aggressive_disease = if_else(id %in% aggressiveMSids, 1, 0))

  # Return results:
  list(data = df, text = txt)
}
