#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(shinydashboard)
library(DT)

vars <- setdiff(names(iris), "Species")

dashboardPage(
  skin = "green",
  dashboardHeader(title = "Kidney Stone"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overall treatment comparision", tabName = "pie", icon = icon("pie-chart")),
      menuItem("Frequency Pass/Fail Treatment", tabName = "frequency_pass_fail", icon = icon("bar-chart")),
      menuItem("Effect of Stone Size", tabName = "stone_size_effect", icon = icon("gem")),  # Add this line
      menuItem("Logistic Regression", tabName = "logistic_regression", icon = icon("chart-line"))
      
    )
  ),
  dashboardBody(
    tabItems(
     
      tabItem(
        tabName = "pie",
        box(
          selectInput("plot_choice", "Select Plots",
                      choices = list("Treatment A and B" = 1, "Success and Failure Rates" = 2),
                      selected = 1),
          width = 12
        ),
        fluidRow(
          column(6, plotOutput("plot_pie1")),
          column(6, plotOutput("plot_pie2"))
        )
      ),
      tabItem(
        tabName = "frequency_pass_fail",
        tabsetPanel(
          tabPanel("Plot",
                   radioButtons("transparency_control", "Transparency Control",
                                choices = c("Success" = "success",
                                            "Failure" = "failure"),
                                selected = "success"),
                   sliderInput("transparency_level", "Transparency Level", min = 0.1, max = 1, value = 0.5),
                   plotOutput("frequency_plot"),
                   plotOutput("bar_plot")
          ),
          tabPanel("Table", dataTableOutput("frequency_table"))
        )
      ),
      
      tabItem(
        tabName = "stone_size_effect",
        fluidPage(
          titlePanel("Effect of Stone Size on Results"),
          tabsetPanel(
            tabPanel("Correlation",
                     fluidRow(
                       column(4,
                              selectInput("corr_x", "Correlation X-axis:", c("success", "failure")),
                              selectInput("corr_y", "Correlation Y-axis:", c("stone_size"))
                       ),
                       column(8,
                              plotOutput("corr_plot")
                       )
                     )
            ),
            tabPanel("Bar Chart - Stone Size vs Success Rate",
                     plotOutput("bar_plot1")
            ),
            tabPanel("Treatment Outcomes by Stone Size",
                     plotOutput("bar_plot2")
            ),
            tabPanel("Stone Size Count within Each Treatment",
                     plotOutput("bar_plot3")
            ),
            tabPanel("Data Table",
                     dataTableOutput("data_table")
            )
          )
        )
      ),
      tabItem(
        tabName = "logistic_regression",
        fluidPage(
          titlePanel("Multiple Logistic Regression"),
          tabsetPanel(
            tabPanel("Data Table", dataTableOutput("regression_table")),
            tabPanel("Coefficient Estimates", plotOutput("coefficient_plot"))
          )
        )
      )
      
    )
  )
)