#' Determine whether patients' MS can be regarded as aggressive.
#'
#' This function classifies patients as suffering from an aggressive form
#' of Multiple Sclerosis (MS) based on the following criteria:
#'
#' - (i) The patient does **not** have a Primary Progressive phenotype.
#' - (ii) Disease duration is at least 10 years.
#' - (iii) EDSS ≥ 6 occurred within the first 10 years of the disease.
#' - (iv) EDSS ≥ 6 was sustained for at least 6 months after it first appeared.
#' - (v) EDSS never dropped below 6 until the end of the observation period.
#'
#' Additionally, a supplementary criterion must be met:
#' - (suppl.) The EDSS ≥ 6 threshold did **not** occur within 30 days after a relapse.
#'
#' @param demographics A tibble containing basic demographic variables.
#' @param relapses A tibble containing relapse dates.
#' @param edss A tibble containing EDSS scores over time.
#' @param eye_check Logical; if `TRUE`, plots of patients classified as
#' having aggressive disease will be displayed for manual inspection.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{$data}{A tibble with the original demographic data plus an additional
#'   column indicating whether each patient was classified as having an aggressive
#'   disease course.}
#'   \item{$text}{A summary text describing the classification process.}
#' }
#'
#' @examples
#' \dontrun{
#' p <- here::here("data-raw", "some-cool-file-name.xlsx")
#' d <- prepare_data(p)
#' outcome_data <- determine_aggressive_phenotype(
#'   d$id, d$relapses, d$edss, eye_check = TRUE
#' )
#' }
#' @export
determine_aggressive_phenotype <- function(demographics, relapses, edss, eye_check = TRUE) {
  # Add time stamps to EDSS assessments:
  d0 <- left_join(
    edss,
    demographics |> select(id, onset, phenotype),
    by = "id"
  ) |>
    mutate(
      edss_time = time_length(difftime(visit_date, onset), "years")
    )
  # Correct cases with negative disease duration during assessment:
  d0 <- recode_typos(d0)
  # Extract patients' IDs:
  unique_ids <- unique(d0$id)
  # Drop patients with primary progressive phenotype:
  N0 <- length(unique_ids) # Original number of patients.
  lowobs_ids <- d0 |>
    group_by(id) |>
    summarise(obs = n()) |>
    filter(obs < 4) |>
    pull(id)
  no_lowobs <- length(lowobs_ids)
  d1 <- subset(d0, !id %in% lowobs_ids)
  incl_ids0 <- unique(d1$id)
  cat(glue::glue("\nDropping {no_lowobs} out of {N0} patients with less than four EDSS measurements.\n\n"))
  # Drop patients with primary progressive phenotype:
  N1 <- length(incl_ids0)
  pp_ids <- unique(subset(d1, phenotype == "PP" | is.na(phenotype))$id)
  no_pp <- length(pp_ids)
  d2 <- subset(d1, phenotype != "PP")
  incl_ids1 <- unique(d2$id)
  cat(glue::glue("\nDropping {no_pp} out of {N1} patients with the primary progressive or missing phenotype.\n\n"))
  # Disease duration has to be at least 10 years:
  message("\nEvaluating the first criterion - disease duration ≥ 10 years ... \n\n")
  N2 <- length(incl_ids1)
  incl_ids2 <- d2 |> # All patients with at least one EDSS recorded at disease duration ≥ 10 years
    filter(edss_time >= 10) |>
    pull(id) |>
    unique()
  excl_ids2 <- c(na.omit(incl_ids1[!incl_ids1 %in% incl_ids2]))
  no_dropped2 <- length(excl_ids2)
  cat(glue::glue("\nDropping {no_dropped2} out of {N2} remaining patients.\n\n"))
  d3 <- subset(d2, !id %in% excl_ids2) |>
    mutate(days_after_relapse = sapply(
      seq_len(length(visit_date)), function(i) {
        rels <- relapses[relapses$id == id[i], "relapse_date"]
        sapply(rels, function(j) {
          days <- time_length(difftime(visit_date[i], j), "days")
          Filter(function(x) x >= 0, days)
        }) |>
          unlist() |>
          min()
      }
    ))
  # EDSS of at minimum 6 appeared withing the first 10 years of disease:
  message("\nEvaluating the second criterion - EDSS ≥ 6 within the first 10 years of disease ...\n\n")
  N3 <- length(unique(d3$id))
  incl_ids3 <- d3 |>
    filter(edss_time < 10 & edss >= 6 & days_after_relapse > 30) |>
    pull(id) |>
    unique()
  excl_ids3 <- c(na.omit(incl_ids2[!incl_ids2 %in% incl_ids3]))
  no_dropped3 <- length(excl_ids3)
  cat(glue::glue("\nDropping {no_dropped3} out of {N3} remaining patients.\n\n"))
  d4 <- subset(d3, !id %in% excl_ids3)
  # EDSS 6 or more shall be sustained:
  N4 <- length(unique(d4$id))
  message("\nEvaluating the third criterion - EDSS ≥ 6 sustained for at least 6 months ...\n\n")
  incl_ids4 <- d4 |>
    filter(edss >= 6 & days_after_relapse > 30) |>
    group_by(id) |>
    filter(row_number() == 1 | row_number() == n()) |>
    summarise(diff = max(edss_time) - min(edss_time), .groups = "drop") |>
    filter(diff > 0.5) |>
    pull(id) |>
    unique()
  excl_ids4 <- c(na.omit(incl_ids3[!incl_ids3 %in% incl_ids4]))
  no_dropped4 <- length(excl_ids4)
  cat(glue::glue("\nDropping {no_dropped4} out of {N4} remaining patients.\n\n"))
  d5 <- subset(d4, !id %in% excl_ids4)
  # EDSS did not drop back below six:
  message("\nEvaluating the final criterion - EDSS ≥ 6 remaining until the end of observation period ...\n\n")
  N5 <- length(unique(d5$id))
  # Keep only cases with a valid last EDSS observation ≥ 6
  d6 <- d5 |>
    left_join(
      d5 |>
        group_by(id) |>
        filter(row_number() == n()) |>
        summarise(last_edss = edss, last_relapse_time = days_after_relapse),
      by = "id"
    ) |>
    filter(last_edss >= 6 & last_relapse_time > 30)
  # All of these need to be EDSS ≥ 6
  d7 <- full_join(
      d6 |>
        filter(edss >= 6 & last_relapse_time > 30 & edss_time < 10) |>
        group_by(id) |>
        filter(row_number() == n()),
      d6 |>
        filter(edss_time >= 10)
    ) |>
    suppressMessages() |>
    arrange(id)
  # Extract patients who fulfill criteria for aggressive MS:
  aggressiveMS <- sapply(unique(d7$id), function(i) {
    all(na.omit(subset(d7, id == i)$edss >= 6))
  })
  aggressiveMSids <- unique(d7$id)[aggressiveMS]
  excl_ids5 <- incl_ids4[!incl_ids4 %in% aggressiveMSids]
  no_dropped5 <- length(excl_ids5)
  K <- length(aggressiveMSids)
  perc_aggr <- paste0(sprintf("%.2f", round(100 * K / N3, 2)),"%")
  cat(glue::glue("\nDropping {no_dropped5} out of {N5} remaining patients.\n\n"))
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
      plt <- d5 |>
        filter(id %in% spl[[i]]) |>
        mutate(`Following relapse` = if_else(days_after_relapse < 30, TRUE, FALSE)) |>
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
    "Data from {N0} patients were extracted from a local database. Out of these,
{no_lowobs} were excluded due to having less than four EDSS observations,
and {no_pp} were excluded due to a diagnosis of primary progressive multiple
sclerosis or missing phenotype information. Out of the remaining {N2}
patients, {N3} patients met the criterion of a minimum of 10 years observation
time, as defined by recorded EDSS scores. Of these, {no_dropped3} patients did
not meet the criterion of EDSS ≥ 6 within the first 10 years of disease.
Further {no_dropped4} patients did not meet the criterion of sustaining EDSS ≥ 6
until the end of the observation period and {no_dropped5} patients did not meet
the criterion of sustaining EDSS ≥ 6 logitudinally.
The final sample thus comprised {N3} patients, of whom {K} ({perc_aggr}) met
criteria for aggressive disease."
  )
  message(glue::glue(paste0(txt, "\n\n")))
  # Prepare data:
  df <- demographics |>
    filter(id %in% incl_ids1) |>
    mutate(aggressive_disease = if_else(id %in% aggressiveMSids, 1, 0))
  # Return results:
  list(data = df, text = txt)
}
