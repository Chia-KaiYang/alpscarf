#' translate durations into bar width
#'
#' @param dwell_df a data frame contains at least 3 columns, "p_name" "AOI" "dwell_duration"
#'
#' @return a tibble with bar width information
#' @export
#'
alpscarf_add_width <- function(dwell_df = NULL){
  # check if all necessary arguments existed
  if(missing(dwell_df)) stop("dwell_df is required")

  dwell_alp_df <- NULL

  for (a_p_name in unique(dwell_df$p_name)){
    df_p <-
      dwell_df %>%
      filter(p_name == a_p_name)

    # calculate the position of bars where bar width equals dewll duration
    df_p_trans <-
      df_p %>%
      mutate(trial = seq(length(df_p$AOI)),
             dwell_duration_category = 1 + round(log10(dwell_duration + 1)),
             bar_position = 0.5 * (cumsum(dwell_duration_category) + cumsum(c(0, dwell_duration_category[-length(dwell_duration_category)]))),
             dwell_lt_100ms = if_else(dwell_duration_category <= 3, 0, NULL))

    dwell_alp_df %<>% bind_rows(df_p_trans)
  }

  # return
  dwell_alp_df
}
