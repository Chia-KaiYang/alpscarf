#' Calculate statistics (order_following/rereading ratio, Levenshtein/Jaro distance) to assist in reading Alpscarf
#'
#' @param alp_df a data frame with Alpscarf info, at least four columns "p_name" "AOI" "conformity_score" "revisiting_score"
#' @param expected_order expected visit order, two columns "AOI" and "AOI_order"
#'
#' @return stats (order_following/rereading ratio, Levenshtein/Jaro distance)
#' @export
#' @importFrom stringdist seq_dist
#'
alpscarf_stats <- function(alp_df = NULL, expected_order = NULL){
  # check if all necessary arguments existed
  if(missing(alp_df)) stop("alp_df is required")
  if(missing(expected_order)) stop("expected_order is required")

  stat_table_df <- NULL
  for(a_p_name in unique(alp_df$p_name)){
    df_p <-
      alp_df %>%
      filter(p_name == a_p_name)
    # for indexing
    a_p_nr <-
      strsplit(a_p_name,"P")[[1]][2] %>%
      as.numeric()

    # descriptive stats
    stat_temp <-
      df_p %>%
      mutate(graph_AOI = if_else(AOI %in% c("G1_SUBJ", "G2_OBJ", "G3_DIFF"), TRUE,FALSE),
             spatial_order_following = if_else(conformity_score > 0, TRUE, FALSE),
             re_reading = if_else(revisiting_score > 0, TRUE, FALSE)) %>%
      group_by(re_reading, spatial_order_following, graph_AOI) %>%
      summarise(count = n()) %>%
      ungroup()

    count_re_reading <-
      stat_temp %>%
      filter(re_reading) %>%
      select(count) %>%
      sum()

    count_order_follow <-
      stat_temp %>%
      filter(spatial_order_following) %>%
      select(count) %>%
      sum()

    count_no_order_follow_nor_re_reading <-
      stat_temp %>%
      filter(!re_reading & !spatial_order_following) %>%
      select(count) %>%
      sum()

    count_graph_AOI <-
      stat_temp %>%
      filter(graph_AOI) %>%
      select(count) %>%
      sum()

    count_graph_AOI_re_reading <-
      stat_temp %>%
      filter(graph_AOI & re_reading) %>%
      select(count) %>%
      sum()

    count_all <-
      stat_temp %>%
      select(count) %>%
      sum()

    stat_table <-
      tibble(p_name = a_p_name,
             total =count_all,
             order_follow = count_order_follow,
             re_reading = count_re_reading,
             no_order_follow_nor_re_reading = count_no_order_follow_nor_re_reading,
             graphAOI = count_graph_AOI,
             graphAOI_rereading = count_graph_AOI_re_reading,
             ratio_order_follow = count_order_follow / count_all,
             ratio_re_reading = count_re_reading / count_all,
             ratio_no_order_follow_nor_re_reading = count_no_order_follow_nor_re_reading / count_all,
             ratio_graphAOI_rereading_in_graphAOI = count_graph_AOI_re_reading / count_graph_AOI)

    # expected order
    spatial_order <- expected_order$AOI_order
    # merge with the expected visit order
    df_p %<>%
      left_join(.,expected_order, by = c("AOI"))
    # actual order
    actual_reading_order <- df_p$AOI_order
    # count Levenshtein distance
    stat_table %<>%
      mutate(distance_LV = seq_dist(actual_reading_order, spatial_order, method = 'lv'),
             distance_LV_normalised = distance_LV / length(actual_reading_order),
             distance_OSA = seq_dist(actual_reading_order, spatial_order, method = 'osa'),
             distance_OSA_normalised = distance_OSA / length(actual_reading_order),
             distance_DL = seq_dist(actual_reading_order, spatial_order, method = 'dl'),
             distance_DL_normalised = distance_DL / length(actual_reading_order),
             distance_Jaro = seq_dist(actual_reading_order, spatial_order, method = 'jw'))

    stat_table_df %<>% bind_rows(stat_table)
  }

  # return
  stat_table_df

}

#' Calculate Levenshtein distance of each pair of the given set of sequences
#'
#' @param alp_df a data frame with Alpscarf info, at least two columns "p_name" "AOI"
#' @param expected_order expected visit order, two columns "AOI" and "AOI_order"
#'
#' @return LV_matrix_df, a Levenshtein-distance matrix of each pair of "p_name"
#' @export
#' @importFrom stringdist seq_dist
#'
alpscarf_LV_matrix <- function(alp_df = NULL, expected_order = NULL){
  # check if all necessary arguments existed
  if(missing(alp_df)) stop("alp_df is required")
  if(missing(expected_order)) stop("expected_order is required")

  alp_df %<>%
    left_join(.,expected_order, by = c("AOI"))

  LV_matrix_df <- NULL
  for(a_p_name in unique(alp_df$p_name)){
    df_a <-
      alp_df %>%
      filter(p_name == a_p_name)
    for(b_p_name in unique(alp_df$p_name)){
      df_b <-
        alp_df %>%
        filter(p_name == b_p_name)

      LV_result <- tibble(reading_seq_X = a_p_name, reading_seq_Y = b_p_name, LV = seq_dist(df_a$AOI_order, df_b$AOI_order, method = 'lv'))

      LV_matrix_df %<>% bind_rows(LV_result)
    }
  }

  # return
  LV_matrix_df

}

