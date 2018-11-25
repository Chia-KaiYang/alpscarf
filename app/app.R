library(shiny)
library(tidyverse)
import::from(cowplot, plot_grid)
import::from(magrittr, "%<>%")
library(alpscarf)

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

# generate Alpscarf dataset
eye_movement_data_systhetic_alp_df <-
  eye_movement_data_systhetic %>%
  alpscarf_add_height(., aoi_names_pages_seq, height_mode = "linear") %>%
  alpscarf_add_width()

# specify plot height
plot_height <- max(eye_movement_data_systhetic_alp_df$seq_bar_length)
# specify plot width range; used in unnormalized view
max_nr_transitions <-
  eye_movement_data_systhetic_alp_df$trial %>%
  max()
max_sum_dwell_duration_log <-
  eye_movement_data_systhetic_alp_df %>%
  group_by(p_name) %>%
  summarise(total = sum(dwell_duration_log)) %>%
  select(total) %>%
  max()

# generate the list of participant in dataset
participant_list <-
  eye_movement_data_systhetic_alp_df %>%
  select(p_name) %>%
  unique() %>%
  unlist() %>%
  unname()

# Define UI for application of Alpscarf
ui <- fluidPage(

  # Application title
  titlePanel("Interactive Alpscarf"),

  # Allow users to specify Alpscarf mode
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "plot_type", label = "select the type of visualization", choiceNames = c("alpscarf", "traditional scarf"), choiceValues = c("alpscarf", "scarf"), selected = "alpscarf"),
      radioButtons(inputId = "NORMALIZED_VIEW", label = "select the view of visualizations", choiceNames = c("unnormalized", "normalized"), choiceValues = c("original", "normalized"), selected = "normalized"),
      radioButtons(inputId = "focus_mode", label = "select the focus mode", choiceNames = c("transition-focus", "duration-focus"), choiceValues = c("transition", "duration"), selected = "transition"),
      uiOutput("selectPtcpnt")
    ),

    # Show a plot of Alpscarf
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # to allow users to choose the participants they want to visualize Alpscarf
  output$selectPtcpnt <- renderUI({
    selectInput(inputId = "Ptcpnt", label = "Participant", choices = participant_list, selected = participant_list, multiple = TRUE)
  })

  # to render Alpscarf
  output$distPlot <- renderPlot({

    # initialise the list storing Alpscarfs, one plot per participant
    lsa_scarf_vis <- list()
    for(a_p_name in unique(input$Ptcpnt)){
      df_p <-
        eye_movement_data_systhetic_alp_df %>%
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
      lsa_scarf_vis[[a_p_nr]] <- alpscarf_plot(df_p, my_palette, focus_mode = input$focus_mode, plot_type = input$plot_type, ymax = plot_height, NORMALIZED_VIEW = (input$NORMALIZED_VIEW == "normalized"), max_nr_transitions = max_nr_transitions, max_sum_dwell_duration_log = max_sum_dwell_duration_log)
    }

    # plot Alpscarf for all participants
    if (length(input$Ptcpnt) > 0){plot_grid(plotlist = lsa_scarf_vis, ncol = 1)}
  })
}

# Run the application
shinyApp(ui = ui, server = server)

