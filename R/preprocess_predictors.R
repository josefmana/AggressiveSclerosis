#' Preprocess potential predictors.
#'
#'
#' @param d0 A tibble containing data pre-processed by
#' \code{determine_aggressive_phenotype}.
#' @param t A tibble containing treatment variables.
#' @param r A tibble containing relapses data.
#' @param m A tibble containing MRI data.
#' @param c A tibble containing CSF data.
#' @param o A tibble containing OCB data.
#' @param chol A tibble containing cholesterol data.
#'
#' @return A tibble with data.
#'
#' @examples
#' \dontrun{
#' raw_data <- here::here("data-raw", "some-cool-file-name.xlsx") |>
#'   prepare_data()
#' clas_data <- determine_aggressive_phenotype(
#'   d$id, d$relapses, d$edss, eye_check = TRUE
#' )
#' df <- preprocess_predictors(
#'   d0 = clas_data$data,
#'   t = raw_data$treatment,
#'   r = raw_data$relapses,
#'   m = raw_data$mri,
#'   c = raw_data$csf,
#'   o = raw_data$ocb,
#'   chol = raw_data$cholesterol
#' )
#' }
#'
#' @export
preprocess_predictors <- function(d0, t, r, m, c, o, chol) {
  # Keep only included patients:
  incl <- unique(d0$id)
  t <- subset(t, id %in% incl)
  r <- subset(r, id %in% incl)
  m <- m |> mutate(id = id19/19) |> filter(id %in% incl)
  c <- c |> mutate(id = id_multiplied/19) |> filter(id %in% incl)
  #o <- subset(o, id %in% incl) Unsure about ID
  chol <- chol |> mutate(id = id19/19) |> filter(id %in% incl)
  # Prepare a table with disease onsets:
  ontab <- d0 |>
    select(id, onset)
  # Prepare predictor variables one-by-one:
  d0 |>
    select(id, gender, aggressive) |>
    left_join(preprocess_predictors_treatment(t, ontab), by = join_by(id)) |>
    left_join(preprocess_predictors_relapses(r, ontab), by = join_by(id)) |>
    left_join(preprocess_predictors_mri(m, ontab), by = join_by(id))
}
