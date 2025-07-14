#' Read and format data for further use
#'
#' This function reads patient data from an Excel file, using a path
#' (ideally on an external drive to maintain patients' anonymity) and
#' a list that specifies which Excel sheets to import and how to process them.
#'
#' @param path A character string specifying the path to the Excel file containing the data.
#' @param list A list describing the data file structure, including which sheets
#' to read and how to format them.
#'
#' @return A list of tibbles, each containing the relevant variables
#' extracted and formatted from the Excel file.
#'
#' @examples
#' \dontrun{
#' p <- here::here("data-raw", "some-cool-file-name.xlsx")
#' data <- prepare_data(p, list = data_structure_list())
#' }
#' @export
prepare_data <- function(p) {
  # Read all the lists:
  struct <- specify_data_structure(show = FALSE)
  with(struct, {
    d0 <<- lapply(set_names(seq_along(labels), names(labels)), function(i) {
      readxl::read_excel(
        path = p,
        sheet = i,
        col_types = coltypes[[names(labels)[i]]],
        col_names = names(coltypes[[names(labels)[i]]]),
        skip = 1
      )
    })
  })
}
