#setwd("C:/Users/braed.BRAEDYNSLAPTOP/Downloads/SMT_Data")

library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(gganimate)
library(data.table)
library(baseballr)
library(tidyr)


# Load data set 
flyball_data <- fread("flyball_data.csv")




# Define plotflyballlocation function
plotflyballlocation <- function(flyball_data) {
  ggplot(flyball_data, aes(x = ball_position_x, y = ball_position_y)) +
    geom_point(aes(color = route_efficiency), size = 3) +
    scale_color_gradient2(low = "blue", high = "red", midpoint = mean(flyball_data$route_efficiency, na.rm = TRUE)) +
    xlim(-330, 330) +
    ylim(0, 450) +
    geom_segment(aes(x = 0, xend = -315, y = 0, yend = 315), size = 1.2) +
    geom_segment(aes(x = 0, xend = 315, y = 0, yend = 315), size = 1.2) +
    geom_curve(aes(x = -315, xend = 315, y = 315, yend = 315), curvature = -.35, size = 1.2) +
    geom_curve(aes(x = -90, xend = 90, y = 88, yend = 88), curvature = -.45, size = 1.2) +
    coord_fixed() +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ggtitle(paste("Scatter Plot of Caught Balls")) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
}

# Define function to calculate route efficiency
calculate_route_efficiency <- function(mean_speed, launch_angle, game_state, initial_angle, straight_line, actual_time) {
  # Placeholder function for calculating route efficiency based on inputs
  # Replace with actual logic
  route_efficiency <- mean_speed * launch_angle * game_state * initial_angle * straight_line / actual_time
  return(route_efficiency)
}

ui <- fluidPage(
  titlePanel("Outfielder Metrics Analysis"),
  tabsetPanel(
    tabPanel("Metrics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("league", "Select League:", choices = unique(flyball_data$league)),
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
                 hr(),
                 selectInput("metric", "Select Metric:", 
                             choices = c("exit_velocity", "launch_angle", "mean_speed", 
                                         "actual_distance", 
                                         "route_efficiency", "cubic_expected_distance", 
                                         "c_route_efficiency")),
                 selectInput("game_state", "Select Game State:", choices = unique(flyball_data$game_state))
               ),
               
               mainPanel(
                 plotlyOutput("scatterPlot"),
                 plotOutput("flyballLocationPlot"),
                 plotOutput("metricHistogram"),
                 plotOutput("gameStateBoxPlot")
               )
             )
    ),
    tabPanel("Route",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("mean_speed_slider", "Mean Speed:", min = round(min(flyball_data$mean_speed),2), max = round(max(flyball_data$mean_speed),2), value = median(flyball_data$mean_speed)),
                 sliderInput("route_eff_slider", "Route Efficiency:", min = round(min(flyball_data$route_efficiency),2), max = round(max(flyball_data$route_efficiency),2), value = 0.5, step = 0.01)
               ),
               mainPanel(
                 plotOutput("routePlot")
               )
             )
    ),
    tabPanel("Route Efficiency",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("mean_speed_slider2", "Mean Speed:", min = round(min(flyball_data$mean_speed),2), max = round(max(flyball_data$mean_speed),2), value = median(flyball_data$mean_speed)),
                 sliderInput("launch_angle_slider", "Launch Angle:", min = round(min(flyball_data$launch_angle),2), max = round(max(flyball_data$launch_angle),2), value = median(flyball_data$launch_angle)),
                 selectInput("game_state_dropdown", "Game State:", choices = c("Nobody on Base" = 1, "Runner on 1st" = 1.5,"Runner on 2nd" = 2,"Runner on 3rd" = 2.5,"Runner on 1st & 2nd" = 3,"Runner on 1st & 3rd" = 3.5,"Runner on 2nd & 3rd" = 4,"Bases Loaded" = 4.5)),
                 sliderInput("initial_angle_slider", "Initial Angle:", min = round(min(flyball_data$initial_angle),2), max = round(max(flyball_data$initial_angle),2), value = median(flyball_data$initial_angle)),
                 sliderInput("straight_line_slider", "Straight Line Distance:", min = round(min(flyball_data$straight_line),2), max = round(max(flyball_data$straight_line),2), value = median(flyball_data$straight_line)),
                 sliderInput("actual_time_slider", "Hang Time:", min = round(min(flyball_data$actual_time),2), max = round(max(flyball_data$actual_time),2), value = median(flyball_data$actual_time))
               ),
               mainPanel(
                 textOutput("calculated_route_efficiency")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Filter data based on selected league and position
  filtered_data <- reactive({
    req(input$league, input$position)  # Ensure inputs are not NULL
    flyball_data %>%
      dplyr::filter(league == input$league & player_position == input$position)
  })
  
  # Update player choices based on the filtered data
  observe({
    req(filtered_data())  # Ensure filtered_data is reactive
    updateSelectInput(session, "player_id", choices = unique(filtered_data()$player_id))
  })
  
  # Filter data based on selected player if checkbox is selected
  selected_data <- reactive({
    req(filtered_data())  # Ensure filtered_data is reactive
    if (input$filter_by_player) {
      req(input$player_id)  # Ensure player_id is selected if checkbox is checked
      filtered_data() %>%
        dplyr::filter(player_id == input$player_id)
    } else {
      filtered_data()
    }
  })
  
  # Display metrics
  output$max_player_speed <- renderText({
    req(selected_data())  # Ensure selected_data is reactive
    paste("Max Speed:", round(max(selected_data()$max_player_speed, na.rm = TRUE), 2))
  })
  
  output$mean_speed <- renderText({
    req(selected_data())
    paste("Mean Speed:", round(mean(selected_data()$mean_speed, na.rm = TRUE), 2))
  })
  
  output$route_efficiency <- renderText({
    req(selected_data())
    paste("Route Efficiency:", round(mean(selected_data()$route_efficiency, na.rm = TRUE), 2))
  })
  
  # Create scatter plot of actual time vs. straight line
  output$scatterPlot <- renderPlotly({
    data <- selected_data()
    
    plot_ly(
      data, 
      x = ~straight_line,  
      y = ~actual_time,    
      type = 'scatter', 
      mode = 'markers',
      marker = list(size = 10),
      text = ~paste(
        "Max Speed: ", round(max_speed, 2), "<br>",
        "Hang Time: ", round(actual_time, 2), "<br>",
        "Distance Covered: ", round(straight_line, 2), "<br>",
        "Route Efficiency: ", round(route_efficiency, 2), "<br>"
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Scatter Plot of Distance vs. Hang Time",
        xaxis = list(title = "Straight Line Distance"),
        yaxis = list(title = "Actual Time")
      )
  })
  
  
  # Create scatter plot of where each ball is caught
  output$flyballLocationPlot <- renderPlot({
    req(selected_data(), input$player_id)
    plotflyballlocation(selected_data())
  })
  
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
  
  # Calculate and display route efficiency based on user inputs
  output$calculated_route_efficiency <- renderText({
    mean_speed <- input$mean_speed_slider2
    launch_angle <- input$launch_angle_slider
    game_state <- as.numeric(input$game_state_dropdown)
    initial_angle <- input$initial_angle_slider
    straight_line <- input$straight_line_slider
    actual_time <- input$actual_time_slider
    
    route_efficiency <- calculate_route_efficiency(mean_speed, launch_angle, game_state, initial_angle, straight_line, actual_time)
    
    paste("Calculated Route Efficiency:", round(route_efficiency, 2))
  })
  
  # Create histogram of selected metric
  output$metricHistogram <- renderPlot({
    req(input$metric)
    
    # Define the desired order of player positions
    position_order <- c("LF", "CF", "RF")
    
    # Summarize the data and convert player_position to a factor with specified levels
    metric_data <- flyball_data %>%
      group_by(player_position, league) %>%
      summarize(mean_metric = mean(.data[[input$metric]], na.rm = TRUE)) %>%
      mutate(player_position = factor(player_position, levels = position_order))
    
    # Create the plot
    ggplot(metric_data, aes(x = player_position, y = mean_metric, fill = league)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste("Average", input$metric, "by Position and League"),
           x = "Player Position", y = paste("Average", input$metric)) +
      theme_minimal()
  })
  
  
  # Create box plots of c_route_efficiency, q_route_efficiency, and route_efficiency filtered by game_state
  output$gameStateBoxPlot <- renderPlot({
    req(input$game_state)
    
    filtered_data <- flyball_data %>%
      filter(game_state == input$game_state)
    
    box_plot_data <- filtered_data %>%
      pivot_longer(cols = c(c_route_efficiency, q_route_efficiency, route_efficiency), 
                   names_to = "metric", values_to = "value")
    
    ggplot(box_plot_data, aes(x = metric, y = value)) +
      geom_boxplot(aes(fill = metric)) +
      labs(title = paste("Box Plots of Route Efficiency Metrics for Game State", input$game_state),
           x = "Metric", y = "Efficiency") +
      theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)