
library(shiny)
library(tidyverse)
library(cowplot)
library(stringr)
library(rlist)
library(magrittr)
#import::from(cowplot, plot_grid)
#import::from(cowplot, get_legend)
#import::from(stringr, str_sort)
#import::from(rlist, list.append)
#import::from(magrittr, "%<>%")
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
dataset_path = "./data"
example_dataset <- file.path(dataset_path, "example_eye_movement_data.RData")
expected_aoi_sequence <- file.path(dataset_path, "aoi_sequence.RData")

# load data
load(example_dataset)
load(expected_aoi_sequence)

# default values of the App - generated with systhetic data
systhetic <- list()
# synthetic AOI order
systhetic$aoi_order <-
  aoi_names_pages_seq %>%
  arrange(AOI_order)
# synthetic colors
systhetic$palette <-
  systhetic$aoi_order$color[]
# synthetic raw dataset
systhetic$eye_movement_data <-
  eye_movement_data_systhetic
# Alpscarf dataset
systhetic$eye_movement_data_alp_df <-
  systhetic$eye_movement_data %>%

  # filter out glitch
  filter(dwell_duration >= 0) %>%

  # merge the fixations within the same AOI and derive the dwell duration
  rename(duration = dwell_duration) %>%
  merge_sequence() %>%

  alpscarf_add_height(., systhetic$aoi_order, height_mode = "linear") %>%
  alpscarf_add_width(., width_mode = "linear")
# specify plot height
systhetic$plot_height <- max(systhetic$eye_movement_data_alp_df$seq_bar_length)
# specify plot width range; used in unnormalized view
systhetic$max_nr_transitions <-
  systhetic$eye_movement_data_alp_df$trial %>%
  max()
systhetic$max_sum_dwell_duration <-
  systhetic$eye_movement_data_alp_df %>%
  group_by(p_name) %>%
  summarise(total = sum(dwell_duration)) %>%
  select(total) %>%
  max()
# participant list
systhetic$participant_list <-
  systhetic$eye_movement_data_alp_df %>%
  pull(p_name) %>%
  unique() %>%
  str_sort(numeric = TRUE)

# generate legend
legend_plot <-
  systhetic$aoi_order %>%
  ggplot(aes(x = AOI_order, y = 1, fill = AOI, width = 1)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(values = aoi_names_pages_seq$color, drop = TRUE, limits = aoi_names_pages_seq$AOI) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(
    direction = "vertical",
    ncol = 1,
    label.position = "right",
    reverse = FALSE))
systhetic$legend <- get_legend(legend_plot)

# Define UI for application of Alpscarf
ui <- fluidPage(

  fluidRow(
    column(2, titlePanel("Interactive Alpscarf")),
    column(2, tags$img(height = 100, width = 200, src = "headpic.svg"))
  ),

  # Allow users to specify Alpscarf mode
  #sidebarLayout(
  #  sidebarPanel(
  fluidRow(
    column(2,
      #==============================================

      code("Data", align = "center"),
      tags$hr(),

      #==============================================
      # data selection (demo data or user data)
      radioButtons(inputId = "data_src", label = "Visualization Data",
                   choiceNames = c("Demo data", "My own data"), choiceValues = c("demo_data", "user_data"),
                   selected = "demo_data"),

      conditionalPanel(
        condition = "input.data_src == 'user_data'",
        fileInput(inputId = "file1",
                  label = "Choose a CSV File for eye movement",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        fileInput(inputId = "file2",
                  label = "Choose a CSV File for expected AOI order and color coding",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        actionButton(inputId = "go",
                     label = "Update")
      ),
      #==============================================

      tags$hr(),
      code("Settings", align = "center"),
      tags$hr(),

      #==============================================
      # Alpscarf settings
      radioButtons(inputId = "plot_type", label = "Visualization Type",
                   choiceNames = c("alpscarf", "traditional scarf"), choiceValues = c("alpscarf", "scarf"),
                   selected = "alpscarf"),
      radioButtons(inputId = "NORMALIZED_VIEW", label = "Visualizations View",
                   choiceNames = c("normalized", "unnormalized"), choiceValues = c("normalized", "original"),
                   selected = "normalized"),
      radioButtons(inputId = "focus_mode", label = "Focus Mode",
                   choiceNames = c("transition-focus", "duration-focus"), choiceValues = c("transition", "duration"),
                   selected = "transition"),
      sliderInput(inputId = "glitch_width", label = "Glitch Width (ms)",
                  min = 0, max = 500,
                  value = 0),
      sliderInput(inputId = "alpscarf_height", label = "Height of Alpscarf",
                  min = 10, max = 500,
                  value = 100),
      uiOutput("selectPtcpnt")
      #==============================================
    ),

    # Show a plot of Alpscarf
    #mainPanel(
    column(8,
           downloadButton(outputId = "down", label = "Download the plot"),
           plotOutput("distPlot", height = "auto")
      ),

    column(2,
           plotOutput("distLegend", height = "auto")
           )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  #==============================================
  # values: synthetic data
  syn_values <- reactiveValues(
    eye_movement_data = systhetic$eye_movement_data,
    eye_movement_data_alp_df = systhetic$eye_movement_data_alp_df,
    plot_height = systhetic$plot_height,
    max_nr_transitions = systhetic$max_nr_transitions,
    max_sum_dwell_duration = systhetic$max_sum_dwell_duration,
    participant_list = systhetic$participant_list,
    palette = systhetic$palette,
    legend = systhetic$legend,
    aoi_order = systhetic$aoi_order
  )

  observeEvent(input$glitch_width, {

    # generate Alpscarf dataset
    syn_values$eye_movement_data_alp_df <-
      syn_values$eye_movement_data %>%

      # filter out glitch
      filter(dwell_duration >= input$glitch_width) %>%

      # merge the fixations within the same AOI and derive the dwell duration
      rename(duration = dwell_duration) %>%
      merge_sequence() %>%

      alpscarf_add_height(., syn_values$aoi_order, height_mode = "linear") %>%
      alpscarf_add_width(., width_mode = "linear")

    # specify plot height
    syn_values$plot_height <- max(syn_values$eye_movement_data_alp_df$seq_bar_length)

    # specify plot width range; used in unnormalized view
    syn_values$max_nr_transitions <-
      syn_values$eye_movement_data_alp_df$trial %>%
      max()
    syn_values$max_sum_dwell_duration <-
      syn_values$eye_movement_data_alp_df %>%
      group_by(p_name) %>%
      summarise(total = sum(dwell_duration)) %>%
      select(total) %>%
      max()

    # generate list of participant in dataset
    syn_values$participant_list <-
      syn_values$eye_movement_data_alp_df %>%
      pull(p_name) %>%
      unique() %>%
      str_sort(numeric = TRUE)

  }, ignoreInit = TRUE)
  #==============================================

  #==============================================
  # values: read-in data
  values <- reactiveValues(
    eye_movement_data = systhetic$eye_movement_data,
    eye_movement_data_alp_df = systhetic$eye_movement_data_alp_df,
    plot_height = systhetic$plot_height,
    max_nr_transitions = systhetic$max_nr_transitions,
    max_sum_dwell_duration = systhetic$max_sum_dwell_duration,
    participant_list = systhetic$participant_list,
    palette = systhetic$palette,
    legend = systhetic$legend,
    aoi_order = systhetic$aoi_order
    )

  toListen_readData <- reactive({
    list(input$go, input$glitch_width)
  })
  observeEvent(toListen_readData(), {
    # read-in dataset
    if(input$go){
      # read-in eye movement data
      values$eye_movement_data <-
        read.table(
          file = input$file1$datapath,
          sep = ",",
          header = TRUE,
          colClasses = c("p_name" = "character",
                         "AOI" = "character",
                         "dwell_duration" = "numeric")) %>%
        as.tibble()

      # read-in AOI order and color coding data
      aoi_names_pages_seq <-
        read.table(
          file = input$file2$datapath,
          sep = ",",
          header = TRUE,
          colClasses = c("AOI" = "character",
                         "AOI_order" = "numeric",
                         "color" = "character")) %>%
        as.tibble()

      # AOI order
      values$aoi_order <-
        aoi_names_pages_seq %>%
        arrange(AOI_order)

      # define colors
      values$palette <-
        values$aoi_order$color[]

      # generate legend
      legend_plot <-
        values$aoi_order %>%
        ggplot(aes(x = AOI_order, y = 1, fill = AOI, width = 1)) +
        geom_bar(stat = "identity", position = "identity") +
        scale_fill_manual(values = aoi_names_pages_seq$color, drop = TRUE, limits = aoi_names_pages_seq$AOI) +
        theme(legend.position = "bottom") +
        guides(fill = guide_legend(direction = "vertical", ncol = 1,
                                   label.position = "right",
                                   reverse = FALSE))
      values$legend <- get_legend(legend_plot)
    }

    # generate Alpscarf dataset
    values$eye_movement_data_alp_df <-
      values$eye_movement_data %>%

      # filter out glitch
      filter(dwell_duration >= input$glitch_width) %>%

      # merge the fixations within the same AOI and derive the dwell duration
      rename(duration = dwell_duration) %>%
      merge_sequence() %>%

      alpscarf_add_height(., values$aoi_order, height_mode = "linear") %>%
      alpscarf_add_width(., width_mode = "linear")

    # specify plot height
    values$plot_height <- max(values$eye_movement_data_alp_df$seq_bar_length)

    # specify plot width range; used in unnormalized view
    values$max_nr_transitions <-
      values$eye_movement_data_alp_df$trial %>%
      max()
    values$max_sum_dwell_duration <-
      values$eye_movement_data_alp_df %>%
      group_by(p_name) %>%
      summarise(total = sum(dwell_duration)) %>%
      select(total) %>%
      max()

    # generate list of participant in dataset
    values$participant_list <-
      values$eye_movement_data_alp_df %>%
      pull(p_name) %>%
      unique() %>%
      str_sort(numeric = TRUE)

  }, ignoreInit = TRUE)
  #==============================================

  #==============================================
  # select between demo and read-in data
  values_for_viz <- reactiveValues(
    eye_movement_data_alp_df = systhetic$eye_movement_data_alp_df,
    plot_height = systhetic$plot_height,
    max_nr_transitions = systhetic$max_nr_transitions,
    max_sum_dwell_duration = systhetic$max_sum_dwell_duration,
    participant_list = systhetic$participant_list,
    palette = systhetic$palette,
    legend = systhetic$legend,
    aoi_order = systhetic$aoi_order
  )

  toListen_vizData <- reactive({
    list(input$go, input$glitch_width, input$data_src)
  })
  observeEvent(toListen_vizData(), {
    # select user input
    if(input$data_src == "user_data"){
      values_for_viz$eye_movement_data_alp_df = values$eye_movement_data_alp_df
      values_for_viz$plot_height = values$plot_height
      values_for_viz$max_nr_transitions = values$max_nr_transitions
      values_for_viz$max_sum_dwell_duration = values$max_sum_dwell_duration
      values_for_viz$participant_list = values$participant_list
      values_for_viz$palette = values$palette
      values_for_viz$legend = values$legend
      values_for_viz$aoi_order = values$aoi_order
    } else {
    # select demo data
      values_for_viz$eye_movement_data_alp_df = syn_values$eye_movement_data_alp_df
      values_for_viz$plot_height = syn_values$plot_height
      values_for_viz$max_nr_transitions = syn_values$max_nr_transitions
      values_for_viz$max_sum_dwell_duration = syn_values$max_sum_dwell_duration
      values_for_viz$participant_list = syn_values$participant_list
      values_for_viz$palette = syn_values$palette
      values_for_viz$legend = syn_values$legend
      values_for_viz$aoi_order = syn_values$aoi_order
    }
  }, ignoreInit = TRUE)
  #==============================================

  # to allow users to choose the participants they want to visualize Alpscarf
  output$selectPtcpnt <- renderUI({
    checkboxGroupInput(inputId = "Ptcpnt", label = "Participant", choices = values_for_viz$participant_list, selected = values_for_viz$participant_list)
  })

  # to render Legend
  cdata <- session$clientData

  plotLegend <- reactive({
    plot_grid(values_for_viz$legend)
  })

  output$distLegend <- renderPlot({
    print(plotLegend())
  }, height = function() {
    length(values_for_viz$palette) * 20
  })

  # to render Alpscarf
  plotInput <- reactive({

    # initialise the list storing Alpscarfs, one plot per participant
    lsa_scarf_vis <- list()
    for(a_p_name in unique(input$Ptcpnt)){
      df_p <-
        values_for_viz$eye_movement_data_alp_df %>%
        filter(p_name == a_p_name)

      # for indexing
      a_p_nr <-
        strsplit(a_p_name,"P")[[1]][2] %>%
        as.numeric()

      # specify the color association of AOIs
      aoi_name_in_order <-
        values_for_viz$aoi_order$AOI[]
      df_p$AOI <- factor(df_p$AOI, levels = aoi_name_in_order)

      # remove empty plot
      lsa_scarf_vis <- lsa_scarf_vis[lengths(lsa_scarf_vis) != 0]
      # Alpscarf plot generation
      lsa_scarf_vis[[a_p_nr]] <- alpscarf_plot(df_p, values_for_viz$palette, focus_mode = input$focus_mode, plot_type = input$plot_type, ymax = values_for_viz$plot_height, NORMALIZED_VIEW = (input$NORMALIZED_VIEW == "normalized"), max_nr_transitions = values_for_viz$max_nr_transitions, max_sum_dwell_duration = values_for_viz$max_sum_dwell_duration, title = a_p_name)
    }

    # remove empty plot
    lsa_scarf_vis_to_plot <- lsa_scarf_vis[lengths(lsa_scarf_vis) != 0]

    # plot Alpscarf for all participants
    if (length(input$Ptcpnt) > 0){
      plot_grid(plotlist = lsa_scarf_vis_to_plot, ncol = 1)
      }
  })

  # plot on screen
  output$distPlot <- renderPlot({
    print(plotInput())
  }, height = function() {
    input$alpscarf_height * length(values_for_viz$participant_list)
  })

  plotImage <- reactive({
    plot_grid(plotInput(), plotLegend(), nrow = 1, rel_widths = c(6, 1), align = "v", axis = "tblr")
  })

  output$down <- downloadHandler(
    # specify the file name
    filename = function(){
      paste("alpscarf", "pdf", sep = ".")
    },
    content = function(file){
      height_in_px = input$alpscarf_height * length(values_for_viz$participant_list)
      height_in_cm = min(100, height_in_px/20)
      ggsave(file, plot = plotImage(), device = "pdf", width = 50, height = height_in_cm, unit = "cm")
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)

