#' Calculate time-to-event for aggressive sclerosis phenotype
#'
#' Based on data of patients who
#'
#' @param d0 A tibble containing basic demographic variables.
#' @param d1 A tibble containing processed EDSS data.
#'
#' @return A tibble
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
add_time_to_event <- function(d0) {
  # Extract ids of patients classified as suffering aggressive disease
  ids <- filter(d0, aggressive_disease == 1)$id
  # Loop through ids to get the data:
  res <- map_dfr(ids, function(i) {
    #
  })
}
