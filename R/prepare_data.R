#' Read and format data for further use.
#'
#' Using a path to the data (ideally on an external
#' drive to ensure patients' anonymity) and a list
#' containing specification of excel lists to be
#' imported.
#'
#'
#' @param path Path to an excel file with data.
#' @param list A list including data file specification.
#'
#' @returns A list of tibbles containing relevant
#' variables.
#'
#' @examples
#' \dontrun{
#' p <- here::here("data-raw", "some cool file name.xlsx")
#' data <- prepare_data(p)
#' }
#' @export
prepare_data <- function(p) {
  # Read all the lists:
  struct <- specify_data_structure(show = FALSE)
  with(
    struct,
    d0 <<-
      lapply(
        set_names(seq_along(labels), names(labels)),
        function(i)
          readxl::read_excel(
            path = p,
            sheet = i,
            col_types = coltypes[[names(labels)[i]]],
            col_names = names(coltypes[[names(labels)[i]]]),
            skip = 1
          )
      )
  )
}
