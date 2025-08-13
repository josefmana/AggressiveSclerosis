#' Preprocess relapse counts.
#'
#'
#' @param d A tibble with the original treatment data.
#' @param onset A tibble containing id/disease onset pairs.
#'
#' @return A tibble with data.
#'
#' @export
preprocess_predictors_relapses <- function(d, onset) {
  d |>
    left_join(
      onset |>
        mutate(plus10 = onset + years(10)),
      by = join_by(id)
    ) |>
    filter(relapse_date < plus10) |>
    drop_na() |>
    group_by(id) |>
    summarise(relapse_count = n(), .groups = "drop")
}
