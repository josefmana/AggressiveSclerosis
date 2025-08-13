#' Preprocess MRI data.
#'
#'
#' @param d A tibble with the original treatment data.
#' @param onset A tibble containing id/disease onset pairs.
#'
#' @return A tibble with data.
#'
#' @export
preprocess_predictors_mri <- function(d, onset) {
  d1 <- d |>
    left_join(
      onset |>
        mutate(plus2 = onset + years(2), plus5 = onset + years(5), plus10 = onset + years(10)),
      by = join_by(id)
    ) |>
    mutate(
      close2 = time_length(difftime(plus2, mri_date), unit = "years"),
      close5 = time_length(difftime(plus5, mri_date), unit = "years"),
      close10 = time_length(difftime(plus10, mri_date), unit = "years")
    )
  dates <- map_dfr(c(quo(close2), quo(close5), quo(close10)), function(i) {
    y <- readr::parse_number(rlang::quo_text(i))
    d1 |>
      filter(!!i > 0) |>
      group_by(id) |>
      summarise(date = mri_date[which(!!i == min(!!i, na.rm = TRUE))], .groups = "drop") |>
      add_column(post = y)
  }) |>
    pivot_wider(names_from = "post", values_from = "date", id_cols = "id")
  for(i in seq_len(nrow(dates))) {
    if(sum(is.na(dates[i, ])) < 2) {
      if(isTRUE(all.equal(dates$`2`[i], dates$`5`[i], dates$`10`[i]))) {
        dates$`5`[i] <- dates$`10`[i] <- NA
      }
    }
  }
  # Put it all together:
  onset |>
    select(id) |>
    left_join(lapply(c(quo(close2), quo(close5), quo(close10)), function(i) {
      y <- readr::parse_number(rlang::quo_text(i))
      onset |>
        select(-onset) |>
        left_join(
          d1 |>
            left_join(dates, by = join_by(id)) |>
            mutate(keep = mri_date == .data[[as.character(y)]]) |>
            filter(keep) |>
            select(id, t2_lesions_volume, t1_blackholes_volume, normalised_brain_atrophz_bpf_sv, t1_tbv_sv,
                   corpus_callosum_volume, !!i) |>
            rename("mri_years_before" = !!i) |>
            rename_with(~paste0(.x, "_y", y), -c("id")),
          by = join_by(id)
        )
    }) |>
      reduce(left_join, by = join_by(id)),
    by = join_by(id))
}
