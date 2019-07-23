#' Alpscarf plot generation
#'
#' @param alp_df a data frame containing the bar width/height info of Alpscarf, contains at least 6 columns "AOI" "trial" "bar_position" "dwell_duration_log" "seq_bar_length" "re_reading_bar_length"
#' @param palette the color definition
#' @param focus_mode to select between transition-focus mode and duration-focus mode
#' @param ymax the range (in y-axis) of plot
#' @param plot_type to select between Alpscarf plot and traditional scarf plot
#' @param NORMALIZED_VIEW to select between normalized view (TRUE) and unnormalized view (FALSE), default = TRUE. In normalized view, all generated Alpscarf are in the same length. In unnormalized view, users need to specify the maximum length of generated Alpscarf.
#' @param max_nr_transitions specify the maximum number of transitions in given dataset. Only used in unnormalized view (NORMALIZED_VIEW= FALSE).
#' @param max_sum_dwell_duration_log specify the maximum number of the sum of total dwell durations (in log scale) in given dataset. Only used in unnormalized view. (NORMALIZED_VIEW= FALSE)
#' @param creek_offset to adjust the position of creeks, default = 0.04
#' @param creek_size to adjust the size of creeks, default = 2
#' @param title print title; e.g., participant name
#'
#' @return Alpscarf plot as a ggplot2 object
#' @export
#'
alpscarf_plot <- function(alp_df = NULL, palette = NULL, focus_mode = c("transition", "duration"), ymax = 4, plot_type = c("alpscarf", "scarf"), creek_offset = 0.04, creek_size = 2, title = NULL, NORMALIZED_VIEW = TRUE, max_nr_transitions = 0, max_sum_dwell_duration = 0) {
  # check if all necessary arguments existed
  if(missing(alp_df)) stop("alp_df is required")
  if(missing(palette)) stop("palette is required")

  # combine seq_bar and re-reading bar into one bar
  alp_df %<>%
    mutate(re_reading_bar_length = -re_reading_bar_length) %>%
    gather(key = "bar_type", value = "bar_length", c("seq_bar_length", "re_reading_bar_length"))

  # alpscarf information collection
  if (focus_mode == "transition"){
    # position of bars
    alp_df %<>%
      mutate(x_alp = trial,
             width_alp = 1)
    # creeks
    x_start_alp <- 0.5
    x_end_alp <- max(alp_df$trial) + 0.5
    # plot range (x-axis)
    xmax <- max_nr_transitions + 1
  }
  if (focus_mode == "duration"){
    # position of bars
    alp_df %<>%
      mutate(x_alp = bar_position,
             width_alp = dwell_duration)
    # creeks
    x_start_alp <- 0
    x_end_alp <- sum(alp_df$dwell_duration) / 2
    # plot range (x-axis)
    xmax <- max_sum_dwell_duration + 1
  }
  if (plot_type == "alpscarf"){
    alp_df %<>%
      mutate(y_alp = bar_length)
    creek_alpha <- 0.01
  }
  if (plot_type == "scarf") {
    alp_df %<>%
      mutate(y_alp = 1)
    creek_alpha <- 0
  }

  # check if xmax is feasible for unnormalized view
  if (!NORMALIZED_VIEW & (xmax < x_end_alp)) stop("xmax is too small; unnormalized view Alpscarf cannot be generated!!")

  # plot generation
  max_y <- if_else(ymax > max(alp_df$y_alp), ymax, max(alp_df$y_alp))
  alpscarf_plot <-
    alp_df %>%
    ggplot(aes(x = x_alp, y = y_alp, fill = AOI, width = width_alp)) +
    geom_bar(stat = "identity", position = "identity") +
    scale_fill_manual(values = palette, drop = TRUE, limits = levels(alp_df$AOI)) +
    theme(legend.position="none",
          #panel.grid.major.y = element_line(size = 0.1, colour = "grey80"),
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          panel.background = element_rect(fill = "transparent"), # bg of the panel
          plot.background = element_rect(fill = "transparent"), # bg of the plot
          plot.title = element_blank(),
          axis.line = element_blank(),
          axis.title.x= element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank()) +
    geom_segment(aes(x = x_start_alp, xend = x_end_alp, y = creek_offset, yend = creek_offset), size = creek_size, colour = "white", alpha = creek_alpha) +
    geom_segment(aes(x = x_start_alp, xend = x_end_alp, y = 1 - creek_offset, yend = 1 - creek_offset), size = creek_size, colour = "white", alpha = creek_alpha) +
    #geom_point(aes(y = no_order_follow_nor_re_reading), color = "green", size = 2) +
    #geom_point(aes(y = graph_AOI), color = "red", size = 2)
    #geom_point(aes(y = dwell_lt_100ms), color = "red", size = 2)
    ylim(-1, max_y) +
    {if(!NORMALIZED_VIEW) xlim(0, xmax)} +
    ylab(title)


  # return
  alpscarf_plot

}

