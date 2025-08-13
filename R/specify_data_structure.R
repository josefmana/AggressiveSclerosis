#' Helper function to specify structure for data import
#'
#' This function defines the expected structure of the source data file.
#' It is intended to be used inside the \code{prepare_data} wrapper and
#' should be consulted whenever the structure of the source data file changes.
#'
#' @param show Logical; if \code{TRUE} (default), prints the list containing
#' the expected data structure. If \code{FALSE}, does not print.
#'
#' @return A list containing:
#' \describe{
#'   \item{sheet names}{Expected names of Excel sheets to import.}
#'   \item{labels}{Labels to be used within the dataset.}
#'   \item{col_types}{Specifications of expected column types.}
#' }
#'
#' @export
specify_data_structure <- function(show = TRUE) {
  # List lists to be loaded:
  l <- list(
    labels = set_names(
      c("id", "Treatment_corrected", "relapses", "edss", "pregnancy", "MRI 1.5Tesla", "Mícha po 2016", "CSF", "OCB", "Cholesterol"),
      c("id", "treatment", "relapses", "edss", "pregnancy", "mri", "spinal_cord", "csf", "ocb", "cholesterol")
    ),
    coltypes = list(
      id = set_names(
        c("numeric", "date", "text", "logical", "date", "text", "text", "text", "text", "text", "date", "text", "skip"),
        c("id", "birth", "gender", "deceased", "decease_date", "death_cause", "clinical_study_code", "ethnic", "dominant_dand", "smoking", "onset", "phenotype", "skip1")
      ),
      treatment = set_names(
        c("numeric", "text", "text", "text", "text", "text", "date", "date"),
        c("id", "treatment_type", "treatment_name", "dmt_name", "dmt_effect", "dmt_form", "start_date", "end_date")
      ),
      relapses = set_names(
        c("numeric", "date", "text"),
        c("id", "relapse_date", "severity")
      ),
      edss = set_names(
        c("numeric", "date", "numeric", rep("skip", 16)),
        c("id", "visit_date", "edss", paste0("skip", seq_len(16)))
      ),
      pregnancy = set_names(
        c("numeric", "date", "skip", "date", rep("skip", 6)),
        c("id", "start_date", "skip0", "end_date", paste0("skip", seq_len(6)))
      ),
      mri = set_names(
        c("numeric", "date", rep("skip", 3), "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "skip"),
        c("id19", "mri_date", paste0("skip", 1:3), "mri_age", "t2_lesions_volume", "t1_blackholes_volume", "normalised_brain_atrophz_bpf_sv", "t1_tbv_sv", "corpus_callosum_volume", "skip4")
      ),
      spinal_cord = set_names(
        c("numeric", "date", "numeric", "numeric"),
        c("id_multiplied", "cord_date", "cord_age", "medulla_volume")
      ),
      csf = set_names(
        c("numeric", "date", "skip", "numeric", "skip", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", rep("skip", 5)),
        c("id_multiplied", "csv_date", "skip0", "csf_elements", "skip1", "proteins_total", "alb", "alb_quotient", "igg", "igm", "igg_quotient", "igm_quotient", "igg_index", "intratek_igg_perc", "intratek_igg", "intratek_igm_perc", "intratek_igm", paste0("skip", 2:6))
      ),
      ocb = set_names(
        c("numeric", rep("skip", 2), "numeric", "numeric"),
        c("id_imed", paste0("skip", 1:2), "type", "no_of_assessments")
      ),
      cholesterol = set_names(
        c("numeric", "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"),
        c("id19", "cholesterol_date", "chol", "tag", "hdl", "ldl", "ldl_calculated", "aterogenity_index", "non_hdl_calculated", "crp")
      )
    )
  )
  # Print if asked for:
  if(show) {
    print(l)
  }
  l
}
