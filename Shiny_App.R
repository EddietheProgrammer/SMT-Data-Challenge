# You may need to set your working directory. 

library(data.table)
library(shiny)
library(plotly)
library(grid) 
library(gridExtra)
library(tidyverse)
library(gganimate)
library(sportyR)
library(tidyr)
library(gt)
library(shinycssloaders)
library(reticulate)
library(gifski)
library(conflicted)
library(rsconnect)
library(shinythemes)

# Install pandas
py_install("pandas")

# Install numpy
py_install("numpy")

# Install xgboost
py_install("xgboost")


# Set preferences for conflicting functions
conflicts_prefer(dplyr::filter)
conflicts_prefer(plotly::layout)



#################################################################################### 

###################### Data Loading and Helper Functions ###########################

####################################################################################


flyball_data <- fread("./Clean/Team062-results.csv")


plotflyballlocation <- function(flyball_data) {
  avg_x <- mean(flyball_data$field_x, na.rm = TRUE)
  avg_y <- mean(flyball_data$field_y, na.rm = TRUE)
  max_route <- max(flyball_data$route_efficiency)
  
  p <- geom_baseball(league = 'MiLB') +
    geom_point(data = flyball_data, aes(x = field_end_x, y = field_end_y, color = route_efficiency, shape = as.factor(is_caught)), size = 5, alpha= 0.6, stroke = 1.4) +
    geom_point(data = flyball_data, aes(x = avg_x, y = avg_y), shape = 1, color = "black", size = 10, stroke = 3) +
    geom_text(data = flyball_data, aes(x = avg_x, y = avg_y, label = player_position, fontface = "bold", size = 20), color = "black", show.legend = FALSE) +
    scale_color_gradient2(low = "blue", high = "red", midpoint = 0.5, limits = c(0, 1)) +
    scale_shape_manual(values = c("0" = 13, "1" = 16), labels = c("0" = "No Catch", "1" = "Catch")) +
    labs(color = "Route Efficiency", shape = "Catch Status", 
         caption="Efficiency values are from 0-1. Values > 1 are grey.") +
    theme(
      legend.position = c(1, 0.225),
      legend.justification = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.box.background = element_rect(),
      plot.caption = element_text(hjust = 0.5, vjust = 20, size = 14, face = "italic", color = "white"),
      legend.key = element_rect(fill = "white")
    )
  
  p_grob <- ggplotGrob(p)
  
  title_grob <- textGrob(
    label = "Flyball Location with Route Efficiency",
    gp = gpar(fontsize = 24, fontface = "bold"),
    just = "center"
  )
  
  grid.arrange(
    title_grob, p_grob,
    heights = c(0.05, 0.4) 
  )
}

cubic_bezier <- function(p0, p1, p2, p3) {
  t_range <- seq(0, 1, length.out = 100)
  bezier_points <- map(t_range, function(t) {
    x <- (1 - t)^3 * p0[1] + 3 * (1 - t)^2 * t * p1[1] + 3 * (1 - t) * t^2 * p2[1] + t^3 * p3[1]
    y <- (1 - t)^3 * p0[2] + 3 * (1 - t)^2 * t * p1[2] + 3 * (1 - t) * t^2 * p2[2] + t^3 * p3[2]
    c(x, y)
  })
  
  bezier_x <- map_dbl(bezier_points, 1)
  bezier_y <- map_dbl(bezier_points, 2)
  
  list(bezier_x = bezier_x, bezier_y = bezier_y)
}

quadratic_bezier <- function(p0, p1, p2) {
  t_range <- seq(0, 1, length.out = 100)
  bezier_points <- map(t_range, function(t) {
    x <- (1 - t)^2 * p0[1] + 2 * (1 - t) * t * p1[1] + t^2 * p2[1]
    y <- (1 - t)^2 * p0[2] + 2 * (1 - t) * t * p1[2] + t^2 * p2[2]
    c(x, y)
  })
  
  bezier_x <- map_dbl(bezier_points, 1)
  bezier_y <- map_dbl(bezier_points, 2)
  
  list(bezier_x = bezier_x, bezier_y = bezier_y)
}

straight_line <- function(p0, p1) {
  t_range <- seq(0, 1, length.out = 100)
  bezier_points <- map(t_range, function(t) {
    x <- (1 - t) * p0[1] + t * p1[1]
    y <- (1 - t) * p0[2] + t * p1[2]
    c(x, y)
  })
  bezier_x <- map_dbl(bezier_points, 1)
  bezier_y <- map_dbl(bezier_points, 2)
  
  list(bezier_x = bezier_x, bezier_y = bezier_y)
}



#################################################################################### 

###################################### UI ##########################################

####################################################################################

ui <- fluidPage(
  theme = shinytheme("paper"),
  tags$style(HTML("
    .sidebar {
    position: fixed;
    width: 30%;
    height: 80vh; 
    overflow-y: scroll; 
    padding-right: 10px;
    box-sizing: border-box; 
  }
  ")),
  
  titlePanel("Outfielder Metrics Analysis"),
  tabsetPanel(
    tabPanel("Metrics",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar",
                 selectInput("league", "Select League:", choices = c("1A" = "Home1A",
                                                                     "2A" = "Home2A",
                                                                     "3A" = "Home3A",
                                                                     "4A" = "Home4A")),
                 selectInput("position", "Select Position:", choices = unique(flyball_data$player_position)),
                 checkboxInput("filter_by_player", "Filter by Player ID", value = FALSE),
                 conditionalPanel(
                   condition = "input.filter_by_player == true",
                   selectInput("player_id", "Select Outfielder:", choices = NULL)
                 ),
                 hr(),
                 h4("Outfielder Metrics"),
                 textOutput("max_player_speed"),
                 textOutput("mean_speed"),
                 textOutput("route_efficiency"),
                 textOutput("q_route_efficiency"),
                 textOutput("c_route_efficiency"),
                 hr(),
                 selectInput("metric", "Select Metric:", 
                             choices = c("Standard Route Efficiency" = "route_efficiency", 
                                         "Quadratic Route Efficiency" = "q_route_efficiency", 
                                         "Cubic Route Efficiency"= "c_route_efficiency")),
                 br(),
                 HTML("
                  <p><strong>Disclaimer:</strong> All data comes from SportsMEDIA Technology.</p>
                  <p>This page contains 3 Plots. We hope you enjoy using them.</p>
                  
                  <p>Some Key Definitions:</p>
                  <ul>
                    <li><u>Standard Route Efficiency</u>: Straight Line Method</li>
                    <li><u>Quadratic Route Efficiency</u>: Bezier Curve with <strong>one</strong> control point</li>
                    <li><u>Cubic Route Efficiency</u>: Bezier Curve with <strong>two</strong> control points</li>
                  </ul>
                  ")
               ),
               
               mainPanel(
                 withSpinner(plotOutput("flyballLocationPlot",  height = "100%", width = "100%")),
                 br(), br(),
                 plotlyOutput("scatterPlot"),
                 br(), br(),
                 gt_output("flyballTable"),
                 br(), br()
               )
             )
    ),
    tabPanel("Route Animation",
             h4("Features:"),
             sidebarLayout(
               sidebarPanel(
                 HTML("
                      <p><strong>Disclaimer:</strong> This page allows you to customize your own Route Efficiency score with our
                      model. Note that the control points are used based on what metric you select (so standard = 0, 
                      <span style='color:#39FF14; font-weight:bold;'>quadratic = 1</span>, and 
                      <span style='color:blue; font-weight:bold;'>cubic = 2</span>). 
                      For demonstration purposes, the first control point is plotted at 25% of the route
                      and the second control point is plotted at 75% of the route.</p>
                      <p>** The plot may take 5 seconds to load.</p>")
                 ,
                 selectInput(
                   "route_metric", "Select Metric:", choices = c("Standard Route Efficiency" = "route_efficiency", 
                                                                 "Quadratic Route Efficiency" = "q_route_efficiency", 
                                                                 "Cubic Route Efficiency"= "c_route_efficiency")
                 ),
                 fluidRow(
                   column(6, textInput("field_x", "Starting Field X:", 0)),
                   column(6, textInput("field_y", "Starting Field Y:", 250))
                 ),
                 sliderInput("spray_angle", "Spray Angle:", 
                             min = round(-60, 2), 
                             max = round(60, 2), 
                             value = round(0, 2), step = 0.01),
                 sliderInput("launch_angle", "Launch Angle:", 
                             min = 10, 
                             max = 80, 
                             value = 30),
                 sliderInput("exit_velocity", "Exit Velocity:", 
                             min = 20, 
                             max = 130, 
                             value = 80),
                 fluidRow(
                   column(6, textInput("field_end_x", "Ending Field X:", 0)),
                   column(6, textInput("field_end_y", "Ending Field Y:", 230))
                 ),
                 hr(),
                 actionButton("route", "Animate Route"),
                 actionButton("reset", "Reset"),
                 hr(),
                 checkboxInput("compute_route", "Calculate Route Efficiency", value = FALSE),
                 conditionalPanel(
                   condition = "input.compute_route == true",
                   column(6, textInput("actual_distance", "Actual Distance:", 1)),
                   textOutput("new_efficiency")
                 ),
               ),
               mainPanel(
                 withSpinner(imageOutput("routePlot"))
               )
             )
    ),
  )
)

#################################################################################### 

###################################### SERVER ###################################### 

####################################################################################


server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$league, input$position)
    flyball_data %>%
      dplyr::filter(league == input$league & player_position == input$position)
  })
  
  observe({
    req(filtered_data())
    player_ids <- unique(filtered_data()$player_id)
    player_ids <- player_ids[!is.na(player_ids)]
    updateSelectInput(session, "player_id", choices = player_ids)
  })
  
  
  selected_data <- reactive({
    req(filtered_data())
    if (input$filter_by_player) {
      req(input$player_id)
      filtered_data() %>%
        dplyr::filter(player_id == input$player_id)
    } else {
      filtered_data()
    }
  })
  
  
  
  ######   ######   ######  1st Page  ######   ######   ###### 
  ######   ######   ######  1st Page  ######   ######   ######  
  ######   ######   ######  1st Page  ######   ######   ######
  ######   ######   ######  1st Page  ######   ######   ###### 
  
  
  output$max_player_speed <- renderText({
    req(selected_data())
    paste("Max Speed (ft/sec):", round(max(selected_data()$max_player_speed, na.rm = TRUE), 2))
  })
  
  output$mean_speed <- renderText({
    req(selected_data())
    paste("Average Speed (ft/sec):", round(mean(selected_data()$mean_speed, na.rm = TRUE), 2))
  })
  
  output$route_efficiency <- renderText({
    req(selected_data())
    paste("Standard Route Efficiency:", round(mean(selected_data()$route_efficiency, na.rm = TRUE), 2))
  })
  
  output$q_route_efficiency <- renderText({
    req(selected_data())
    paste("Quadratic Route Efficiency:", round(mean(selected_data()$q_route_efficiency, na.rm = TRUE), 2))
  })
  
  output$c_route_efficiency <- renderText({
    req(selected_data())
    paste("Cubic Route Efficiency:", round(mean(selected_data()$c_route_efficiency, na.rm = TRUE), 2))
  })
  output$scatterPlot <- renderPlotly({
    req(input$metric)
    
    data <- selected_data()
    
    # Determine the label for the selected metric
    metric_label <- switch(input$metric,
                           "route_efficiency" = "Standard Route Efficiency",
                           "q_route_efficiency" = "Quadratic Route Efficiency",
                           "c_route_efficiency" = "Cubic Route Efficiency")
    
    # Generate the text for hover information
    hover_text <- ~paste(
      "Speed (ft/sec): ", round(mean_speed, 2), "<br>",
      "Hang Time: ", round(actual_time, 2), "<br>",
      "Distance Covered: ", round(actual_distance, 2), "<br>",
      paste0(metric_label, ": ", round(get(input$metric), 2)), "<br>",
      "Catch Probability: ", round(catch_prob, 2), "%", "<br>",
      "Catch: ", ifelse(is_caught == 1, "Yes", "No")
    )
    plot_ly(
      data, 
      x = ~actual_distance,  
      y = ~actual_time,    
      type = 'scatter', 
      mode = 'markers',
      marker = list(
        size = 10,
        color = ~catch_prob, 
        colorscale = "RdBu", 
        colorbar = list(title = "Catch Probability (%)"),
        cmin = 0,  # Set the minimum color scale value to 0%
        cmax = 100  # Set the maximum color scale value to 100%
      ),
      text = hover_text,
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Play Info Scatter Plot",
        xaxis = list(title = "Distance (ft)", range = c(0, max(data$actual_distance) + 5)),
        yaxis = list(title = "Hang Time (sec)", range = c(0, max(data$actual_time) + 1), tickvals = seq(1, max(data$actual_time) + 1))
      )
  })
  
  
  output$flyballLocationPlot <- renderPlot({
    req(selected_data(), input$player_id, input$metric)
    
    metric_df <- selected_data() %>%
      mutate(route_efficiency = .data[[input$metric]])
    
    plotflyballlocation(metric_df)
  }, height = 800, width = 800)
  
  
  # Create function to approximate the route based on slider values
  approximate_route <- reactive({
    req(input$mean_speed_slider, input$route_eff_slider)
    
    flyball_data$route_efficiency <- input$route_eff_slider
    flyball_data$mean_speed <- input$mean_speed_slider
    
    flyball_data
  })
  

  output$flyballTable <- render_gt({
    req(input$league, input$position)
    
    league_names <- c(
      "Home1A" = "1A",
      "Home2A" = "2A",
      "Home3A" = "3A",
      "Home4A" = "4A"
    )
    
    selected_league_name <- league_names[input$league]
    
    table_data <- flyball_data %>%
      filter(league == input$league, player_position == input$position) %>%
      group_by(game_state) %>%
      summarize(
        avg_route_efficiency = sprintf("%.3f", mean(route_efficiency, na.rm = TRUE)),
        avg_q_route_efficiency = sprintf("%.3f", mean(q_route_efficiency, na.rm = TRUE)),
        increase_q_route_efficiency = sprintf("%.3f", mean(q_route_efficiency, na.rm = TRUE) - mean(route_efficiency, na.rm = TRUE)),
        avg_c_route_efficiency = sprintf("%.3f", mean(c_route_efficiency, na.rm = TRUE)),
        increase_c_route_efficiency = sprintf("%.3f", mean(c_route_efficiency, na.rm = TRUE) - mean(route_efficiency, na.rm = TRUE))
      ) %>%
      mutate(
        game_state = factor(game_state, levels = c(
          "No runners", "Runner on 1st", "Runner on 2nd", "Runner on 3rd",
          "Runners on 1st and 2nd", "Runners on 1st and 3rd", "Runners on 2nd and 3rd",
          "Bases loaded"
        )),
        avg_q_route_efficiency = glue::glue("{avg_q_route_efficiency} ({ifelse(increase_q_route_efficiency > 0, '<span style=\"color:green\">', '<span style=\"color:red\">')}{ifelse(increase_q_route_efficiency > 0, '+', '')}{increase_q_route_efficiency}</span>)"),
        avg_c_route_efficiency = glue::glue("{avg_c_route_efficiency} ({ifelse(increase_c_route_efficiency > 0, '<span style=\"color:green\">', '<span style=\"color:red\">')}{ifelse(increase_c_route_efficiency > 0, '+', '')}{increase_c_route_efficiency}</span>)")
      ) %>%
      select(-increase_q_route_efficiency, -increase_c_route_efficiency) %>%
      arrange(game_state)
    
    gt_table <- gt(table_data) %>%
      tab_header(
        title = paste("Efficient Routes by Game State:", selected_league_name, " || ", input$position)
      ) %>%
      cols_label(
        game_state = "Game State",
        avg_route_efficiency = "Straight Line Route Efficiency",
        avg_q_route_efficiency = "Quadratic Route Efficiency",
        avg_c_route_efficiency = "Cubic Route Efficiency"
      ) %>%
      fmt_markdown(
        columns = c(avg_q_route_efficiency, avg_c_route_efficiency)
      ) %>% 
      cols_align(
        align = "center",
        columns = c(avg_route_efficiency)
      )
  })
  
  
  
  
  ######   ######   ######  2nd Page  ######   ######   ######   
  ######   ######   ######  2nd Page  ######   ######   ######  
  ######   ######   ######  2nd Page  ######   ######   ######  
  ######   ######   ######  2nd Page  ######   ######   ###### 
  
  pd <- import("pandas")
  np <- import("numpy")
  
  predict_points <- reactive({
    # First control point
    p1_y <- pd$read_pickle("./Clean/p1_y.pkl")
    # Second control point
    p2_y <- pd$read_pickle("./Clean/p2_y.pkl")
    
    field_x <- as.numeric(input$field_x)
    field_y <- as.numeric(input$field_y)
    field_end_x <- as.numeric(input$field_end_x)
    field_end_y <- as.numeric(input$field_end_y)
    
    # Control point x values will be 25% and 75% of the overall route:
    field_int_x1 <- field_x + 0.25 * (field_end_x - field_x)
    field_int_x2 <- field_x + 0.75 * (field_end_x - field_x)
    
    launch_angle <- input$launch_angle
    exit_velocity <- input$exit_velocity
    spray_angle <- input$spray_angle
    
    # Time to predict control points then draw a line
    array <- np$array(list(list(launch_angle, exit_velocity, spray_angle, field_y)))
    
    # Ensure predictions are in the correct format
    p1_pred <- p1_y$predict(array)
    if (is.atomic(p1_pred)) {
      p1_pred <- p1_pred[[1]]
    }
    
    array <- np$array(list(list(launch_angle, exit_velocity, spray_angle, p1_pred)))
    p2_pred <- p2_y$predict(array)
    if (is.atomic(p2_pred)) {
      p2_pred <- p2_pred[[1]]
    }
    
    list(
      field_x = field_x,
      field_y = field_y,
      field_end_x = field_end_x,
      field_end_y = field_end_y,
      field_int_x1 = field_int_x1,
      field_int_x2 = field_int_x2,
      p1_pred = p1_pred,
      p2_pred = p2_pred
    )
  })
  
  # Animation plot
  rv <- reactiveValues(no_animation = TRUE)
  
  observe({
    req(rv$no_animation)
    
    output$routePlot <- renderPlot({
      points <- predict_points()
      
      if (!is.na(points$field_x) & !is.na(points$field_y) & !is.na(points$field_end_x) & !is.na(points$field_end_y)) {
        geom_baseball(league = 'MiLB') + 
          geom_point(aes(x = points$field_x, y = points$field_y), color = "yellow", size = 3, show.legend = FALSE) + 
          geom_point(aes(x = points$field_int_x1, y = points$p1_pred, color = "Control Point 1"), size = 3) + 
          geom_point(aes(x = points$field_int_x2, y = points$p2_pred, color = "Control Point 2"), size = 3) + 
          geom_segment(aes(x = points$field_x, y = points$field_y, xend = points$field_end_x, yend = points$field_end_y),
                       color = "black", linetype = "dashed", size = 0.7) +
          scale_color_manual(values = c("Control Point 1" = "green", "Control Point 2" = "blue"))+
          labs(color = "Legend") +
          theme(legend.title = element_text(size = 16), legend.text = element_text(size = 16), legend.position = c(0.85, 0.25),
                legend.key = element_rect(fill = "white"))
      } else {
        geom_baseball(league = 'MiLB')
      }
    }, height = 775, width = 775)
  })
  
  # Generate and render the animation when "Animate Route" button is clicked
  observeEvent(input$route, {
    rv$no_animation <- FALSE
    req(input$route_metric)
    
    # Display the loading spinner
    showNotification("Generating animation, please wait...", type = "message", duration = 8)
    
    points <- predict_points()
    
    bezier_values <- switch(input$route_metric,
                            "route_efficiency" = straight_line(c(points$field_x, points$field_y),
                                                               c(points$field_end_x, points$field_end_y)),
                            "q_route_efficiency" = quadratic_bezier(c(points$field_x, points$field_y), 
                                                                    c(points$field_int_x1, points$p1_pred), 
                                                                    c(points$field_end_x, points$field_end_y)),
                            "c_route_efficiency" = cubic_bezier(c(points$field_x, points$field_y), 
                                                                c(points$field_int_x1, points$p1_pred), 
                                                                c(points$field_int_x2, points$p2_pred), 
                                                                c(points$field_end_x, points$field_end_y))
    )
    
    df <- data.frame(
      x = bezier_values$bezier_x,
      y = bezier_values$bezier_y,
      time = seq_along(bezier_values$bezier_x)
    )
    
    p <- geom_baseball(league = 'MiLB') + 
      geom_point(data = df, aes(x = x, y = y), color = "yellow", size = 2, show.legend = FALSE) + 
      transition_time(time) +
      shadow_mark(past = TRUE, future = FALSE, alpha = 0.3, size = 0.5, color = "black", linetype = "dashed")
    
    anim <- animate(p, nframes = nrow(df), fps = 20, renderer = gifski_renderer("play.gif"))
    
    
    output$routePlot <- renderImage({
      list(src = "play.gif", contentType = "image/gif", height = 775, width = 775)
    }, deleteFile = TRUE)
  })
  
  # Reset to default plot and set no_animation to TRUE
  observeEvent(input$reset, {
    rv$no_animation <- TRUE
  })
  
  route_efficiency <- reactive({
    req(input$actual_distance, input$route_metric)
    
    points <- predict_points()
    
    bezier_values <- switch(input$route_metric,
                            "route_efficiency" = straight_line(c(points$field_x, points$field_y),
                                                               c(points$field_end_x, points$field_end_y)),
                            "q_route_efficiency" = quadratic_bezier(c(points$field_x, points$field_y), 
                                                                    c(points$field_int_x1, points$p1_pred), 
                                                                    c(points$field_end_x, points$field_end_y)),
                            "c_route_efficiency" = cubic_bezier(c(points$field_x, points$field_y), 
                                                                c(points$field_int_x1, points$p1_pred), 
                                                                c(points$field_int_x2, points$p2_pred), 
                                                                c(points$field_end_x, points$field_end_y))
    )
    
    df <- data.frame(
      x = bezier_values$bezier_x,
      y = bezier_values$bezier_y,
      time = seq_along(bezier_values$bezier_x)
    )
    
    expected_distance <- sum(sqrt(diff(df$x)^2 + diff(df$y)^2))
    
    efficiency <- expected_distance / as.numeric(input$actual_distance)
    round(efficiency, 3)
  })
  
  output$new_efficiency <- renderText({
    req(route_efficiency())
    paste("Route Efficiency:", route_efficiency())
  })
  
  
}

shinyApp(ui = ui, server = server)
