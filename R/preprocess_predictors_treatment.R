#' Preprocess treatment variables.
#'
#'
#' @param d A tibble with the original treatment data.
#' @param onset A tibble containing id/disease onset pairs.
#'
#' @return A tibble with data.
#'
#' @export
preprocess_predictors_treatment <- function(d, onset) {
  d1 <- d |>
    left_join(
      onset |>
        mutate(plus5 = onset + years(5), plus10 = onset + years(10)),
      by = join_by(id)
    ) |>
    mutate(
      treatment = case_when(
        dmt_effect == "Platform" ~ 1,
        dmt_effect == "HET" ~ 2,
        dmt_effect == "LE_HET" ~ 3
      ) |> factor(
        levels = 1:3,
        labels = c("Platform", "HET", "LE_HET")
      ),
      end_date = if_else(
        is.na(end_date), as.Date("2024-12-31"), end_date
      ),
      duration = time_length(
        difftime(end_date, start_date),
        unit = "year"
      )
    ) |>
    filter(duration > 0) |>
    drop_na()
  # Treatment type percentages:
  tt <- map_dfr(c(quo(plus5), quo(plus10)), function(i) {
    y <- readr::parse_number(rlang::quo_text(i))
    d1 |>
      filter(start_date < !!i) |>
      mutate(end_date = !!i, duration = time_length(
        difftime(end_date, start_date),
        unit = "year"
      )) |>
      group_by(id, treatment) |>
      summarise(treatment_time = max(duration)/y, .groups = "drop") |>
      pivot_wider(names_from = "treatment", values_from = "treatment_time") |>
      add_column(years = y)
  }) |>
    pivot_wider(names_from = "years", values_from = c("Platform", "HET", "LE_HET"), id_cols = "id") |>
    mutate_all(~if_else(is.na(.x), 0, .x))
  # Time to the first treatment:
  ttt <- d1 |>
    group_by(id) |>
    summarise(time_to_treat = min(
      time_length(difftime(start_date, onset), unit = "year")
    ), .groups = "drop")
  # Prepare the final data:
  onset |>
    left_join(tt, by = "id") |>
    left_join(ttt, by = "id") |>
    select(-onset)
}
