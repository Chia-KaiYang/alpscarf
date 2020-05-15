#' calculate conformity/revisiting scores and translate them into bar height
#'
#' @param dwell_df a data frame of AOI visits, contains at least 2 columns, "p_name" "AOI"
#' @param expected_order expected visit order, two columns "AOI" and "AOI_order"
#' @param height_mode to select between linear mode and exponential mode of mountain height
#' @param scale_factor to specify scale of mountain height
#' @param base_factor to specify the base of exponent which changes mountain shape, only valid in exponential mode
#'
#' @return a tibble with conformity/revisiting scores and bar height information
#' @export
#'
alpscarf_add_height <- function(dwell_df = NULL, expected_order = NULL, height_mode = c("linear", "exponential"), scale_factor = 0.1, base_factor = 2){
  # check if all necessary arguments existed
  if(missing(dwell_df)) stop("dwell_df is required")
  if(missing(expected_order)) stop("expected_order is required")

  dwell_alp_df <- NULL

  for (a_p_name in unique(dwell_df$p_name)){
    df_p <-
      dwell_df %>%
      filter(p_name == a_p_name)

    # calculation of scores
    df_p$conformity_score <- alpscarf_conform(df_p, expected_order)
    df_p$revisiting_score <- alpscarf_revisit(df_p)

    # transfer scores into bar heights
    incr_re_reading_length <- 0.3

    if(height_mode == "linear"){
      df_p %<>%
        mutate(seq_bar_length = 1 + scale_factor * conformity_score,
               re_reading_bar_length = incr_re_reading_length * revisiting_score)
    }
    if (height_mode == "exponential"){
      df_p %<>%
        mutate(seq_bar_length = base_factor ^ (scale_factor * conformity_score),
               re_reading_bar_length = incr_re_reading_length * revisiting_score)
    }

    dwell_alp_df %<>% bind_rows(df_p)
  }
  # return
  dwell_alp_df
}

# Calculation of revisiting score
#
# @param df_p dataset contains AOI visits
# @param w sequence length of revisir type to consider
#
# @return revisiting scores
#
alpscarf_revisit <- function(df_p = NULL, w = 3){
  # initialize revisiting score
  r <- rep(0, length(df_p$AOI))

  for(i in 1 : length(df_p$AOI)){
    if(i <= (length(df_p$AOI) - w + 1)){
      if(df_p$AOI[i] == df_p$AOI[i + w - 1]){
        r[i: (i + w - 1)] <- r[i: (i + w - 1)] + 1
      }
    }
  }
  # return
  r
}

# Calculation of conformity score
#
# @param df_p dataset contains AOI visits
# @param aoi_names_pages_seq expected visit order, two columns "AOI" and "AOI_order"
# @param s_min shortest length of sequence to consider
#
# @return conformity scores
#
alpscarf_conform <- function(df_p = NULL, aoi_names_pages_seq = NULL, s_min = 2){
  # initialize conformity score
  c <- rep(0, length(df_p$AOI))
  # merge with the expected visit order
  #df_p %<>%
  #  left_join(.,aoi_names_pages_seq, by = c("AOI"))

  # generate the sequence of interest in string
  seq_of_interest <-
    aoi_names_pages_seq %>%
    filter(AOI_order > 0) %>%
    arrange(AOI_order) %>%
    pull(AOI) %>%
    paste0(collapse = "_") %>%
    paste0("_")

  for(i in 1 : length(df_p$AOI)){
    if (i <= (length(df_p$AOI) - s_min + 1)){
      for(s in s_min : min(length(aoi_names_pages_seq$AOI_order), length(df_p$AOI) - i + 1)){

        #order_check <- df_p$AOI_order[i + s - 1] - df_p$AOI_order[i + s - 2]

        # generate the local sequence to compare (with seq_of_interest)
        seq_to_compare <-
          df_p$AOI[i: (i+ s - 1)] %>%
          paste0(collapse = "_") %>%
          paste0("_")
        # count the frequency of seq_to_compare in seq_of_interest
        freq_seq <-
          str_count(seq_of_interest, pattern = seq_to_compare)

        #if(order_check == 1){
        if(freq_seq >= 1){
          #c[i: (i + s - 1)] <- c[i: (i + s - 1)] + 1
          c[i: (i + s - 1)] <- c[i: (i + s - 1)] + freq_seq
        } else {
          break
        }
      }
    }
  }
  # return
  c
}
