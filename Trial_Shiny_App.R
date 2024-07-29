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


# Load data set 
flyball_data <- fread("flyball_data.csv")



# Define plotflyballlocation function
plotflyballlocation <- function(flyball_data, metric) {
  ggplot(flyball_data, aes(x = ball_position_x, y = ball_position_y)) +
    geom_point(aes(color = .data[[metric]]), size = 3) +
    scale_color_gradient2(low = "blue", high = "red", midpoint = mean(flyball_data[[metric]], na.rm = TRUE)) +
    xlim(-330, 330) +
    ylim(0, 450) +
    geom_segment(aes(x = 0, xend = -315, y = 0, yend = 315), size = 1.2) +
    geom_segment(aes(x = 0, xend = 315, y = 0, yend = 315), size = 1.2) +
    geom_curve(aes(x = -315, xend = 315, y = 315, yend = 315), curvature = -.35, size = 1.2) +
    geom_curve(aes(x = -90, xend = 90, y = 88, yend = 88), curvature = -.45, size = 1.2) +
    coord_fixed() +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ggtitle("Scatter Plot of Caught Balls") +
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
                             choices = c( "route_efficiency", "q_route_efficiency", 
                                         "c_route_efficiency"))
               ),
               
               mainPanel(
                 plotlyOutput("scatterPlot"),
                 plotOutput("flyballLocationPlot"),
                 gt_output("flyballTable")
               )
             )
    ),
    tabPanel("Route",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("field_y_slider", "Feild_y:", min = round(min(flyball_data$field_y),2), max = round(max(flyball_data$field_y),2), value = median(flyball_data$field_y)),
                 sliderInput("exit_velocity_slider", "Exit Velocity:", min = round(min(flyball_data$exit_velocity),2), max = round(max(flyball_data$exit_velocity),2), value = median(flyball_data$exit_velocity)),
                 sliderInput("launch_angle_slider", "Launch Angle:", min = round(min(flyball_data$launch_angle),2), max = round(max(flyball_data$launch_angle),2), value = round(median(flyball_data$launch_angle)),2),
                 sliderInput("spray_angle_slider", "Spray Angle:", min = round(min(flyball_data$spray_angle),2), max = round(max(flyball_data$spray_angle),2), value = 0.5, step=1),
                 sliderInput("route_eff_slider", "Route Efficiency:", min = round(min(flyball_data$route_efficiency),2), max = round(max(flyball_data$route_efficiency),2), value = 0.5, step = 0.01)
               ),
               mainPanel(
                 plotOutput("routePlot")
               )
             )
    ),
    tabPanel("Play Animation",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("mean_speed_slider2", "Mean Speed:", min = round(min(flyball_data$mean_speed),2), max = round(max(flyball_data$mean_speed),2), value = median(flyball_data$mean_speed)),
                 sliderInput("launch_angle_slider", "Launch Angle:", min = round(min(flyball_data$launch_angle),2), max = round(max(flyball_data$launch_angle),2), value = round(median(flyball_data$launch_angle)),2),
                 selectInput("game_state_dropdown", "Game State:", choices = c("Nobody on Base" = 1, "Runner on 1st" = 1.5,"Runner on 2nd" = 2,"Runner on 3rd" = 2.5,"Runner on 1st & 2nd" = 3,"Runner on 1st & 3rd" = 3.5,"Runner on 2nd & 3rd" = 4,"Bases Loaded" = 4.5)),
                 sliderInput("initial_angle_slider", "Initial Angle:", min = round(min(flyball_data$initial_angle),2), max = round(max(flyball_data$initial_angle),2), value = round(median(flyball_data$initial_angle)),2),
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
    req(selected_data(), input$player_id, input$metric)
    plotflyballlocation(selected_data(),input$metric)
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
  
  


  output$flyballTable <- render_gt({
    req(input$league, input$position)
    
    # Filter data based on selected league and position
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
      mutate(game_state = factor(game_state, levels = c(
        "No runners", "Runner on 1st", "Runner on 2nd", "Runner on 3rd",
        "Runners on 1st and 2nd", "Runners on 1st and 3rd", "Runners on 2nd and 3rd",
        "Bases loaded"
      ))) %>%
      arrange(game_state)
    
    # Function to add sign formatting
    format_increase <- function(x) {
      formatted <- ifelse(x > 0, paste0("+", x), x)
      return(formatted)
    }
    
    # Create the gt table
    gt_table <- gt(table_data) %>%
      tab_header(
        title = paste("Efficient Routes by Game State:", input$league, " ", input$position)
      ) %>%
      cols_label(
        game_state = "Game State",
        avg_route_efficiency = "Straight Line Route Efficiency",
        avg_q_route_efficiency = "Quadratic Route Efficiency",
        increase_q_route_efficiency = "Change in Quadratic Efficiency",
        avg_c_route_efficiency = "Cubic Route Efficiency",
        increase_c_route_efficiency = "Change in Cubic Efficiency"
      ) %>%
      text_transform(
        locations = cells_body(columns = c(increase_q_route_efficiency, increase_c_route_efficiency)),
        fn = function(x) format_increase(x)
      ) %>%
      tab_style(
        style = list(
          cell_text(color = "green", weight = "bold")
        ),
        locations = cells_body(
          columns = increase_q_route_efficiency,
          rows = increase_q_route_efficiency > 0
        )
      ) %>%
      tab_style(
        style = list(
          cell_text(color = "red", weight = "bold")
        ),
        locations = cells_body(
          columns = increase_q_route_efficiency,
          rows = increase_q_route_efficiency < 0
        )
      ) %>%
      tab_style(
        style = list(
          cell_text(color = "green", weight = "bold")
        ),
        locations = cells_body(
          columns = increase_c_route_efficiency,
          rows = increase_c_route_efficiency > 0
        )
      ) %>%
      tab_style(
        style = list(
          cell_text(color = "red", weight = "bold")
        ),
        locations = cells_body(
          columns = increase_c_route_efficiency,
          rows = increase_c_route_efficiency < 0
        )
      )
    
    gt_table
  })
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

