#' Expected AOI visit order
#'
#' A dataset containing the AOI names, their expected visit order, and color association. The variables are as follows:
#'
#' @docType data
#' @format A data frame with 37 rows and 3 variables:
#' \describe{
#'   \item{AOI}{the name of AOI}
#'   \item{AOI_order}{the expected visit order of AOIs; must be continuous integers}
#'   \item{color}{a set of color definitions in HEX code; a 1-to-1 mapping to AOI_order}
#' }
"aoi_names_pages_seq"


#' AOI visits (dwells)
#'
#' A synthetic eye-movement dataset containing a sequence of AOI visits for multiple participants. The variables are as follows:
#'
#' @docType data
#' @format A data frame with 568 rows and 3 variables:
#' \describe{
#'   \item{p_name}{participant name (P1--P3)}
#'   \item{AOI}{the visited AOI of one dwell}
#'   \item{dwell_duration}{the total dwell time of one dwell}
#' }
"eye_movement_data_systhetic"
