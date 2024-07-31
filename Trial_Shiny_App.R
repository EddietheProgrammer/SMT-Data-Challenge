#setwd("C:/Users/braed.BRAEDYNSLAPTOP/Downloads/SMT_Data")

library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(gganimate)
library(data.table)
library(baseballr)
library(sportyR)
library(tidyr)
library(gt)
library(shinycssloaders)

# Load data set 
flyball_data <- fread("flyball_data.csv")

plotflyballlocation <- function(flyball_data) {
  avg_x <- mean(flyball_data$field_x, na.rm = TRUE)
  avg_y <- mean(flyball_data$field_y, na.rm = TRUE)
  max_route <- max(flyball_data$route_efficiency)
  
  p <- geom_baseball(league = 'MiLB') +
    geom_point(data = flyball_data, aes(x = field_end_x, y = field_end_y, color = route_efficiency, shape = as.factor(is_caught)), size = 5, alpha= 0.5, stroke = 1.4) +
    geom_point(data = flyball_data, aes(x = avg_x, y = avg_y), shape = 1, color = "black", size = 10, stroke = 3) +
    geom_text(data = flyball_data, aes(x = avg_x, y = avg_y, label = player_position, fontface = "bold", size = 20), color = "black", show.legend = FALSE) +
    scale_color_gradient2(low = "blue", high = "red", midpoint = 0.5, limits = c(0, 1)) +
    scale_shape_manual(values = c("0" = 13, "1" = 16), labels = c("0" = "No Catch", "1" = "Catch")) +
    labs(color = "Route Efficiency", shape = "Catch Status", 
         caption="Efficiency values are from 0-1. Values > 1 are grey.") +
    theme(
      legend.position = c(1.2, 0.8),
      legend.justification = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.box.background = element_rect(),
      plot.caption = element_text(hjust = 0.5, size = 14, face = "italic", color = "white")
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


ui <- fluidPage(
  titlePanel("Outfielder Metrics Analysis"),
  tabsetPanel(
    tabPanel("Metrics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("league", "Select League:", choices = c("Home1A",
                                                                     "Home2A",
                                                                     "Home3A",
                                                                     "Home4A")),
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
                 p("Disclaimer: INPUT DISCLAIMERS")
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
    tabPanel("Game State",
             sidebarLayout(
               sidebarPanel(
                 selectInput("league", "Select League:", choices = c("Home1A",
                                                                     "Home2A",
                                                                     "Home3A",
                                                                     "Home4A")),
                 selectInput("position", "Select Position:", choices = unique(flyball_data$player_position))
               ),
               mainPanel(
                 gt_output("flyballTable")
               )
             )
             ),
    
    tabPanel("Route Animation",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("mean_speed_slider", "Mean Speed:", min = round(min(flyball_data$mean_speed),2), max = round(max(flyball_data$mean_speed),2), value = median(flyball_data$mean_speed)),
                 sliderInput("route_eff_slider", "Route Efficiency:", min = round(min(flyball_data$route_efficiency),2), max = round(max(flyball_data$route_efficiency),2), value = 0.5, step = 0.01)
               ),
               mainPanel(
                 plotOutput("routePlot")
               )
             )
    )
)
)

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
  
  # Display metrics
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
      "Catch Probability: ", round(catch_prob * 100, 2), "%"
    )
    
    plot_ly(
      data, 
      x = ~actual_distance,  
      y = ~actual_time,    
      type = 'scatter', 
      mode = 'markers',
      marker = list(
        size = 10,
        color = ~catch_prob, # Adding color gradient based on catch_prob
        colorscale = "RdBu", # Color gradient
        colorbar = list(title = "Catch Probability")
      ),
      text = hover_text,
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Play Info Scatter Plot",
        xaxis = list(title = "Distance (ft)", range = c(0, max(data$actual_distance) + 5)),
        yaxis = list(title = "Actual Time (sec)", range = c(0, max(data$actual_time) + 1), tickvals = seq(1, max(data$actual_time) + 1))
      )
  })
  
  
  
  
  
  output$flyballLocationPlot <- renderPlot({
    req(selected_data(), input$player_id, input$metric)
    
    metric_df <- selected_data() %>%
      mutate(route_efficiency = .data[[input$metric]])
    
    plotflyballlocation(metric_df)
  }, height = 700, width = 1000)
  
  
  # Create function to approximate the route based on slider values
  approximate_route <- reactive({
    req(input$mean_speed_slider, input$route_eff_slider)
    
    flyball_data$route_efficiency <- input$route_eff_slider
    flyball_data$mean_speed <- input$mean_speed_slider
    
    flyball_data
  })
  
  # Plot the approximated route
  output$routePlot <- renderPlot({
    flyball_data <- approximate_route()
    
    ggplot(flyball_data, aes(x = actual_distance , y = actual_time)) +
      geom_line() +
      geom_point(aes(color = route_efficiency)) +
      scale_color_gradient(low = "blue", high = "red") +
      labs(title = "Predicted Route of Outfielder",
           x = "Distance (ft)", y = "Time (s)",
           color = "Route Efficiency", size = "Max Speed") +
      theme_minimal()
  })
  
  output$flyballTable <- render_gt({
    req(input$league, input$position)
    
    
    table_data <- flyball_data %>%
      filter(league == input$league, player_position == input$position) %>%
      group_by(game_state) %>%
      summarize(
        avg_route_efficiency = round(mean(route_efficiency, na.rm = TRUE), 3),
        avg_q_route_efficiency = round(mean(q_route_efficiency, na.rm = TRUE), 3),
        increase_q_route_efficiency = round(mean(q_route_efficiency, na.rm = TRUE) - mean(route_efficiency, na.rm = TRUE), 3),
        avg_c_route_efficiency = round(mean(c_route_efficiency, na.rm = TRUE), 3),
        increase_c_route_efficiency = round(mean(c_route_efficiency, na.rm = TRUE) - mean(route_efficiency, na.rm = TRUE), 3)
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
        title = paste("Efficient Routes by Game State:", input$league, " || ", input$position)
      ) %>%
      cols_label(
        game_state = "Game State",
        avg_route_efficiency = "Straight Line Route Efficiency",
        avg_q_route_efficiency = "Quadratic Route Efficiency",
        avg_c_route_efficiency = "Cubic Route Efficiency"
      ) %>%
      fmt_markdown(
        columns = c(avg_q_route_efficiency, avg_c_route_efficiency)
      )
    
  },width = 800,height = 700)
  
  
  
  
}

shinyApp(ui = ui, server = server)