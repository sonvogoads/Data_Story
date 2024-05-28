#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(broom)

function(input, output, session) {
  # Read datasets kidney_stone_data.csv into data
  data <- read_csv("kidney_stone_data.csv")
  
  # Calculate the number and frequency of success and failure of each treatment
  data_summary <- reactive({
    data %>%
      group_by(treatment, success) %>%
      summarise(N = n()) %>%
      mutate(Freq = round(N/sum(N), 3))
  })
  
  # Function to calculate percentage
  calculate_percentage <- function(x) {
    return(round((x / sum(x)) * 100, 2))
  }
  
  # Calculate total success and failure cases for Treatment A
  total_A <- reactive({
    aggregate(N ~ success, data_summary()[data_summary()$treatment == "A", ], sum)
  })
  
  total_A_percentage <- reactive({
    total_A() %>%
      mutate(Percentage = calculate_percentage(N))
  })
  
  # Calculate total success and failure cases for Treatment B
  total_B <- reactive({
    aggregate(N ~ success, data_summary()[data_summary()$treatment == "B", ], sum)
  })
  
  total_B_percentage <- reactive({
    total_B() %>%
      mutate(Percentage = calculate_percentage(N))
  })
  
  # Calculate total success cases for Treatments A and B
  total_AB_success <- reactive({
    aggregate(N ~ treatment, data_summary()[data_summary()$success == 1, ], sum)
  })
  
  total_AB_success_percentage <- reactive({
    total_AB_success() %>%
      mutate(Percentage = calculate_percentage(N))
  })
  
  # Calculate total failure cases for Treatments A and B
  total_AB_failure <- reactive({
    aggregate(N ~ treatment, data_summary()[data_summary()$success == 0, ], sum)
  })
  
  total_AB_failure_percentage <- reactive({
    total_AB_failure() %>%
      mutate(Percentage = calculate_percentage(N))
  })
  
  #-----------------
  output$plot_pie1 <- renderPlot({
    if (input$plot_choice == 1) {
      pie_A <- ggplot(total_A_percentage(), aes(x = "", y = N, fill = factor(success))) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5), size = 4) +
        labs(title = "Success and Failure for Treatment A",
             fill = "Success") +
        theme_void()
      pie_A
    } else {
      pie_AB_success <- ggplot(total_AB_success_percentage(), aes(x = "", y = N, fill = treatment)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5), size = 4) +
        labs(title = "Comparison of Success Rates",
             fill = "Treatment") +
        theme_void()
      pie_AB_success
    }
  })
  
  output$plot_pie2 <- renderPlot({
    if (input$plot_choice == 1) {
      pie_B <- ggplot(total_B_percentage(), aes(x = "", y = N, fill = factor(success))) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5), size = 4) +
        labs(title = "Success and Failure for Treatment B",
             fill = "Success") +
        theme_void()
      pie_B
    } else {
      pie_AB_failure <- ggplot(total_AB_failure_percentage(), aes(x = "", y = N, fill = treatment)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5), size = 4) +
        labs(title = "Comparison of Failure Rates",
             fill = "Treatment") +
        theme_void()
      pie_AB_failure
    }
  })
  
  #---------------------- part 2 - question 1
  data_N <- data.frame(
    treatment = c("A", "A", "B", "B"),
    success = c(0, 1, 0, 1),
    N = c(77, 273, 61, 289),
    Freq = c(0.220, 0.780, 0.174, 0.826)
  )
  
  output$frequency_plot <- renderPlot({
    transparency <- switch(input$transparency_control,
                           "success" = c(1 - input$transparency_level, input$transparency_level, 1 - input$transparency_level, input$transparency_level),
                           "failure" = c(input$transparency_level, 1 - input$transparency_level, input$transparency_level, 1 - input$transparency_level))
    
    ggplot(data_fre, aes(x = treatment, y = Freq, fill = factor(success), alpha = transparency)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("0" = "red", "1" = "green"), labels = c("Failure", "Success")) +
      labs(title = "Relative Frequency of Success by Treatment",
           x = "Treatment",
           y = "Relative Frequency") +
      theme_minimal()
  })
  
  output$bar_plot <- renderPlot({
    transparency <- switch(input$transparency_control,
                           "success" = c(1 - input$transparency_level, input$transparency_level, 1 - input$transparency_level, input$transparency_level),
                           "failure" = c(input$transparency_level, 1 - input$transparency_level, input$transparency_level, 1 - input$transparency_level))
    
    ggplot(data_N, aes(x = treatment, y = N, fill = factor(success), alpha = transparency)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5) +
      scale_fill_manual(values = c("0" = "red", "1" = "green"), labels = c("Failure", "Success")) +
      labs(title = "Relationship between Treatment, Success, Number of Cases, and Frequency",
           x = "Treatment",
           y = "Number of Cases",
           fill = "Success",
           caption = "Frequency represented by text labels") +
      theme_minimal()
  })
  output$frequency_table <- renderDataTable({
    data_N
  }, options = list(pageLength = 4))
  #-------------------------------question 2 ----
  sum_data_heat <- data.frame(
    treatment = c("A", "A", "A", "A", "B", "B", "B", "B"),
    stone_size = c("large", "large", "small", "small", "large", "large", "small", "small"),
    success = c(0, 1, 0, 1, 0, 1, 0, 1),
    N = c(71, 192, 6, 81, 25, 55, 36, 234),
    Freq = c(0.270, 0.730, 0.069, 0.931, 0.312, 0.688, 0.133, 0.867)
  )
  
  output$corr_plot <- renderPlot({
    if (input$corr_x == "success") {
      # Pivot the data for success and stone size
      sum_data_pivot <- sum_data_heat %>%
        filter(success == 1) %>%
        group_by(stone_size, treatment) %>%
        summarise(Freq = sum(Freq))
      
      # Plot heatmap
      ggplot(sum_data_pivot, aes(x = treatment, y = stone_size, fill = Freq)) +
        geom_tile() +
        geom_text(aes(label = round(Freq, 2)), vjust = 0.5, fontface = "bold") +
        scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, 1)) +  # Set fill limits
        labs(title = "Frequency Heatmap",
             x = "Treatment",
             y = "Stone Size") +
        theme_minimal()
    } else {
      # Pivot the data for failure and stone size
      sum_data_pivot <- sum_data_heat %>%
        filter(success == 0) %>%
        group_by(stone_size, treatment) %>%
        summarise(Freq = sum(Freq))
      
      # Plot heatmap
      ggplot(sum_data_pivot, aes(x = treatment, y = stone_size, fill = Freq)) +
        geom_tile() +
        geom_text(aes(label = round(Freq, 2)), vjust = 0.5, fontface = "bold") +
        scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, 1)) +  # Set fill limits
        labs(title = "Frequency Heatmap",
             x = "Treatment",
             y = "Stone Size") +
        theme_minimal()
    }
  })
  #-----------------------------
  output$bar_plot1 <- renderPlot({
    ggplot(sum_data_bar1, aes(x = stone_size, y = success_rate, fill = treatment)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Comparison of Treatment Outcomes by Stone Size",
           x = "Stone Size",
           y = "Success Rate",
           fill = "Treatment") +
      scale_fill_manual(values = c("A" = "blue", "B" = "red")) +
      geom_text(aes(label = success_rate), vjust = -0.5, position = position_dodge(width = 0.9)) +
      theme_minimal()
  })
  
  #--------------------------
  
  output$bar_plot2 <- renderPlot({
    ggplot(sum_data_bar2, aes(x = stone_size, y = success_rate_percent, fill = treatment, group = treatment)) +
      geom_area(position = "stack", color = "white") +
      labs(title = "Comparison of Treatment Outcomes by Stone Size",
           x = "Stone Size",
           y = "Percentage Success Rate",
           fill = "Treatment") +
      geom_text(aes(label = paste0(round(success_rate_percent), "%")), position = position_stack(vjust = 0.5), size = 3) +
      scale_fill_manual(values = c("A" = "blue", "B" = "red")) +
      theme_minimal()
  })
  
  output$bar_plot3 <- renderPlot({
    sum_data %>%
      ggplot(aes(x = treatment, y = N)) +
      geom_bar(aes(fill = stone_size), stat = "identity") +
      labs(title = "Stone Size Count within Each Treatment",
           x = "Treatment",
           y = "Count",
           fill = "Stone Size") +
      theme_minimal()
  })
  
  output$data_table <- renderDataTable({
    sum_data
  })
  
  #------------------------------------------Question 3 -------------
  # Run a multiple logistic regression
  m <- reactive({
    glm(data = data, success ~ treatment + stone_size, family = "binomial")
  })
  
  # Render the regression table
  output$regression_table <- renderDataTable({
    tidy(m())
  })
  
  # Render the coefficient plot with annotations
  output$coefficient_plot <- renderPlot({
    tidy_m <- tidy(m())
    
    p <- ggplot(tidy_m, aes(x = term, y = estimate)) +
      geom_pointrange(aes(ymin = estimate - 1.96 * std.error,
                          ymax = estimate + 1.96 * std.error)) +
      geom_hline(yintercept = 0) +
      theme_bw() +
      labs(x = "Term", y = "Coefficient Estimate")
    
    # Add annotations for high values
    high_values <- tidy_m$estimate > 1
    if (any(high_values)) {
      p <- p +
        geom_text(data = tidy_m[high_values, ],
                  aes(label = round(estimate, 2)),
                  vjust = -1, color = "red", size = 4)
    }
    
    p
  })
  #-----------------------------------------------------------------
  
}