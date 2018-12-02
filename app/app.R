library(shiny)
library(tidyverse)
import::from(cowplot, plot_grid)
import::from(cowplot, get_legend)
import::from(stringr, str_sort)
import::from(magrittr, "%<>%")
library(alpscarf)

merge_sequence <- function(lsa_df_) {

  lsa_reduced_dwell_df <- NULL

  for (a_p_name in unique(lsa_df_$p_name)){
    df_p <-
      lsa_df_ %>%
      filter(p_name == a_p_name) %>%
      mutate(cum_duration = cumsum(duration))

    runlength <- rle(df_p$AOI)

    rl_table <- tibble(AOI = runlength$values, length = runlength$lengths)
    rl_table %<>%
      mutate(total_duration_current_index = cumsum(length),
             total_duration_pre_index = lag(total_duration_current_index, default = 0),
             dwell_duration = df_p$cum_duration[total_duration_current_index] - c(0, df_p$cum_duration[total_duration_pre_index]),
             p_name = a_p_name
      ) %>%
      select(p_name, AOI, dwell_duration)

    lsa_reduced_dwell_df %<>% bind_rows(rl_table)
  }
  # return
  lsa_reduced_dwell_df
}

# specify dataset name & path
dataset_path = "../data"
example_dataset <- file.path(dataset_path, "example_eye_movement_data.RData")
expected_aoi_sequence <- file.path(dataset_path, "aoi_sequence.RData")

# load data
load(example_dataset)
load(expected_aoi_sequence)

# put AOIs in sorted order
aoi_names_pages_seq %<>%
  arrange(AOI_order)

# define colors
my_palette <-
  aoi_names_pages_seq$color[]

# generate legend
legend_plot <-
  aoi_names_pages_seq %>%
  ggplot(aes(x = AOI_order, y = 1, fill = AOI, width = 1)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(values = my_palette, drop = TRUE, limits = levels(aoi_names_pages_seq$AOI)) +
  theme(legend.position = "right")
legend <- get_legend(legend_plot)

# default values of the App - generated with systhetic data
systhetic <- list()
# systhetic df
systhetic$eye_movement_data <- eye_movement_data_systhetic
# Alpscarf dataset
systhetic$eye_movement_data_alp_df <-
  systhetic$eye_movement_data %>%

  # merge the fixations within the same AOI and derive the dwell duration
  rename(duration = dwell_duration) %>%
  merge_sequence() %>%

  alpscarf_add_height(., aoi_names_pages_seq, height_mode = "linear") %>%
  alpscarf_add_width()
# specify plot height
systhetic$plot_height <- max(systhetic$eye_movement_data_alp_df$seq_bar_length)
# specify plot width range; used in unnormalized view
systhetic$max_nr_transitions <-
  systhetic$eye_movement_data_alp_df$trial %>%
  max()
systhetic$max_sum_dwell_duration_log <-
  systhetic$eye_movement_data_alp_df %>%
  group_by(p_name) %>%
  summarise(total = sum(dwell_duration_log)) %>%
  select(total) %>%
  max()
# participant list
systhetic$participant_list <-
  systhetic$eye_movement_data_alp_df %>%
  pull(p_name) %>%
  unique() %>%
  str_sort(numeric = TRUE)

# Define UI for application of Alpscarf
ui <- fluidPage(

  fluidRow(
    column(2, titlePanel("Interactive Alpscarf")),
    column(2, tags$img(height = 100, width = 200, src = "headpic.svg"))
  ),

  #tags$img(height = 50, width = 300, src = "headpic.svg"),

  # Application title
  #titlePanel("Interactive Alpscarf"),

  # Allow users to specify Alpscarf mode
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "plot_type", label = "Visualization Type",
                   choiceNames = c("alpscarf", "traditional scarf"), choiceValues = c("alpscarf", "scarf"),
                   selected = "alpscarf"),
      radioButtons(inputId = "NORMALIZED_VIEW", label = "Visualizations View",
                   choiceNames = c("unnormalized", "normalized"), choiceValues = c("original", "normalized"),
                   selected = "normalized"),
      radioButtons(inputId = "focus_mode", label = "Focus Mode",
                   choiceNames = c("transition-focus", "duration-focus"), choiceValues = c("transition", "duration"),
                   selected = "transition"),
      sliderInput(inputId = "alpscarf_height", label = "Height of Alpscarf",
                  min = 10, max = 500,
                  value = 100),
      uiOutput("selectPtcpnt"),

      tags$hr(),

      fileInput("file1", "To play with your own data, choose a CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      )
    ),

    # Show a plot of Alpscarf
    mainPanel(
      plotOutput("distPlot", height="auto")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  values <- reactiveValues(
    eye_movement_data = systhetic$eye_movement_data,
    eye_movement_data_alp_df = systhetic$eye_movement_data_alp_df,
    plot_height = systhetic$plot_height,
    max_nr_transitions = systhetic$max_nr_transitions,
    max_sum_dwell_duration_log = systhetic$max_sum_dwell_duration_log,
    participant_list = systhetic$participant_list
    )

  observeEvent(input$file1, {
    eye_movement_data <- read_csv(input$file1$datapath, guess_max = 100000)

    # update eye_movement_data
    values$eye_movement_data <- eye_movement_data

    # generate Alpscarf dataset
    values$eye_movement_data_alp_df <-
      eye_movement_data %>%

      # merge the fixations within the same AOI and derive the dwell duration
      rename(duration = dwell_duration) %>%
      merge_sequence() %>%

      alpscarf_add_height(., aoi_names_pages_seq, height_mode = "linear") %>%
      alpscarf_add_width()

    # specify plot height
    values$plot_height <- max(values$eye_movement_data_alp_df$seq_bar_length)

    # specify plot width range; used in unnormalized view
    values$max_nr_transitions <-
      values$eye_movement_data_alp_df$trial %>%
      max()
    values$max_sum_dwell_duration_log <-
      values$eye_movement_data_alp_df %>%
      group_by(p_name) %>%
      summarise(total = sum(dwell_duration_log)) %>%
      select(total) %>%
      max()

    # generate list of participant in dataset
    values$participant_list <-
      values$eye_movement_data_alp_df %>%
      pull(p_name) %>%
      unique() %>%
      str_sort(numeric = TRUE)
  })

  # to allow users to choose the participants they want to visualize Alpscarf
  output$selectPtcpnt <- renderUI({
    checkboxGroupInput(inputId = "Ptcpnt", label = "Participant", choices = values$participant_list, selected = values$participant_list)
  })

  # to render Alpscarf
  output$distPlot <- renderPlot({

    # initialise the list storing Alpscarfs, one plot per participant
    lsa_scarf_vis <- list()
    for(a_p_name in unique(input$Ptcpnt)){
      df_p <-
        values$eye_movement_data_alp_df %>%
        filter(p_name == a_p_name)

      # for indexing
      a_p_nr <-
        strsplit(a_p_name,"P")[[1]][2] %>%
        as.numeric()

      # specify the color association of AOIs
      aoi_name_in_order <-
        aoi_names_pages_seq$AOI[]
      df_p$AOI <- factor(df_p$AOI, levels = aoi_name_in_order)

      # Alpscarf plot generation
      lsa_scarf_vis[[a_p_nr]] <- alpscarf_plot(df_p, my_palette, focus_mode = input$focus_mode, plot_type = input$plot_type, ymax = values$plot_height, NORMALIZED_VIEW = (input$NORMALIZED_VIEW == "normalized"), max_nr_transitions = values$max_nr_transitions, max_sum_dwell_duration_log = values$max_sum_dwell_duration_log, title = a_p_name)
    }

    # plot Alpscarf for all participants
    if (length(input$Ptcpnt) > 0){
      plot_grid(plotlist = lsa_scarf_vis, ncol = 1)
      #alp_plot <- plot_grid(plotlist = lsa_scarf_vis, ncol = 1)
      #plot_grid(alp_plot, legend, nrow = 1, rel_widths = c(3, 1))
      }
  }, height = function() {
    input$alpscarf_height * length(values$participant_list)
  }
  )
}

# Run the application
shinyApp(ui = ui, server = server)

