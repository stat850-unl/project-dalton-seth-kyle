#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)

Pig_Data <- read.csv("ST_project.csv") %>%
  select(-c(Trt1, tr2)) %>%
  mutate(FarrowTime = hms(FarrowTime),
         FarrowTime = hour(FarrowTime) + minute(FarrowTime)/60,
         across(c(TNB, SB, SB_perc),
                as.numeric),
         Treatment = as.factor(Treatment)) %>%
  drop_na(TNB, SB, SB_perc) %>%
  filter(Induce == 0) %>%
  rename(`TotalNumberBorn` = TNB,
         `NumberStillborn` = SB,
         `PercentStillborn` = SB_perc,
         `FarrowTime` = FarrowTime,
         `GestationLength` = GestationLength)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(div(h1("STAT 850 Project"),
               h3("Dalton, Kyle, Seth"))),
    navlistPanel("Table of Contents",
                 tabPanel("1. Summary Statistics",
                          mainPanel(h1("Summary Statistics"),
                                    h4("[Insert text explaining the dataset and summary statistics]")
                                    ),
                          ),
                 tabPanel("2. Data Visualization",
                          mainPanel(h1("Data Visualization"),
                                    h4("[Insert text explaining choices to be made for visualizing data]"),
                                    fluidRow(
                                      column(6,
                                             selectInput("x_axis",
                                                         "X-Axis",
                                                         choices = c("FarrowTime", "GestationLength"),
                                                         selected = "FarrowTime")
                                      ),
                                      column(6,
                                             selectInput("y_axis",
                                                         "Y-Axis",
                                                         choices = c("FarrowTime", "GestationLength",
                                                                     "TotalNumberBorn", "NumberStillborn", "PercentStillborn"),
                                                         selected = "PercentStillborn")
                                      )
                                    ),
                                    fluidRow(
                                      column(6,
                                             selectInput("include_line",
                                                "Include Smooth:",
                                                choices = c("Yes", "No"),
                                                selected = "No"),
                                      ),
                                      column(6,
                                             selectInput("color_by_treatment",
                                                "Visualize Data by Treatment?",
                                                choices = c("Yes", "No"),
                                                selected = "No")
                                      )
                                    ),
                                    plotOutput("plot_out")
                                    ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$plot_out <- renderPlot({
    ggplot(data = Pig_Data,
           aes_string(x = input$x_axis,
                      y = input$y_axis)) +
      theme_classic() +
      labs(x = input$x_axis,
           y = input$y_axis,
           color = "Treatment") +
      geom_point(aes(color = if(input$color_by_treatment == "Yes") Treatment,
                     group = if(input$color_by_treatment == "Yes") Treatment)) +
      if(input$include_line == "Yes"){
        geom_smooth(aes(color = if(input$color_by_treatment == "Yes") Treatment,
                        group = if(input$color_by_treatment == "Yes") Treatment))
      }

    })

}

# Run the application
shinyApp(ui = ui, server = server)
