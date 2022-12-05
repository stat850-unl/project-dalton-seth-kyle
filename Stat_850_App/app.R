library(shiny)
library(tidyverse)
library(lubridate)

if(file.exists("User_Feedback.csv") == FALSE){
  write_csv(tibble(userID = NA_character_,
                   userInput = NA_character_),
            "User_Feedback.csv")
}

Pig_Data <- read_csv("ST_project_cleaned.csv") %>%
  rename(`Total Number Born` = TNB,
         `Percent Stillborn` = SB_perc,
         `Gestation Length` = GestationLength,
         `Start Minutes` = Start_Minutes,
         `Finish Minutes` = Finish_Minutes,
         `Start Time` = Start_H,
         `Finish Time` = Finish_H) %>%
  mutate(Treatment = as.factor(Treatment),
         `Start Time` = as.numeric(`Start Time`),
         `Finish Time` = as.numeric(`Finish Time`))

std.error <- function(x){
  var(x)/(sqrt(length(x)))
}

Summary_Statistics = Pig_Data %>%
  group_by(Treatment) %>%
  summarise(across(c(`Total Number Born`,
                     `Percent Stillborn`,
                     `Gestation Length`),
                   .fns = c(Min = min,
                            Max = max,
                            Mean = mean,
                            Std.Dev = sd,
                            Std.Err = std.error))) %>%
  pivot_longer(cols = `Total Number Born_Min`:`Gestation Length_Std.Err`,
               names_to = "Stat") %>%
  separate(Stat, into = c("Variable", "Statistic"), sep = "_") %>%
  pivot_wider(names_from = "Statistic",
              values_from = "value")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(div(h1("Sow Feeding Time Trial"),
               h4("Treatment Group 1: fed between 7:00-10:00 AM"),
               h4("Treatment Group 2: fed between 2:00-5:00 AM"))),
    tabsetPanel(id = "inTabset",
                 tabPanel("1. Project Overview",
                          mainPanel(h1("Project Overview"),
                                    h4("This application is designed to allow the user to explore data collected during a feeding trial Dalton conducted at a hog farm in NE Nebraska in the summer of 2020."),
                                    h4("The goal of this trial was to decrease the incidences of stillborns within litters of pigs. To do this, they tried feeding pre-farrowing sows at different times of day to alter their time of parturition. The sows in this trial were either fed between 7:00-10:00 AM (Treatment Group 1), or between 2:00-5:00 AM (Treatment Group 2)."),
                                    h4("If the feeding time influences the time of farrowing, feedings may be timed such that farrowing is more likely to occur during times when staff are present, thereby reducing the number of mortalities."),
                                    actionButton('totab2', 'Next Tab: Summary Statistics')
                                    ),
                          ),
                 tabPanel("2. Summary Statistics",
                          mainPanel(h1("Summary Statistics"),
                                    h4('In this tab, select "Gestation Length", "Total Number Born", or "Percent Stillborn" to view summary statistics for each treatment for the variable of interest.'),
                                    selectInput("var_to_summarize",
                                                "Variable to Summarize",
                                                choices = c("Gestation Length",
                                                            "Total Number Born",
                                                            "Percent Stillborn")),
                                    tableOutput("summary_stats"),
                                    actionButton('totab3', 'Next Tab: Data Visualization'))),
                 tabPanel("3. Data Visualization",
                          mainPanel(h1("Data Visualization"),
                                    h4('In this tab, first select the variables to be plotted on the X and Y axes. Note that if "Count" is selected for the Y axis, a histogram will be produced. For all other pairs of variables, a scatterplot will be produced. If plotting a scatterplot, you may add smoothed contional means by selecting "Yes" under "Include Smooth". For all plots, you may also visualize the data by treatment by selecting "Yes". However, the treatment is not expected to influence all variables. For example, feeding time would have no effect on total number of piglets born.'),
                                    fluidRow(
                                      column(6,
                                             selectInput("x_axis",
                                                         "X-Axis",
                                                         choices = c("Gestation Length",
                                                                     "Start Time",
                                                                     "Finish Time",
                                                                     "Duration",
                                                                     "Percent Stillborn"))
                                      ),
                                      column(6,
                                             selectInput("y_axis",
                                                         "Y-Axis",
                                                         choices = c("Percent Stillborn",
                                                                     "Total Number Born",
                                                                     "Count"),
                                                         selected = "Percent Stillborn")
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
                                    plotOutput("plot_out"),
                                    textInput("userID", "User"),
                                    textAreaInput("userInput", "Conclusions, Questions, Feedback",
                                              width = "100%",
                                              height = "10%"),
                                    actionButton('submit', 'Submit')
                                    ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observeEvent(input$totab2, {
    updateTabsetPanel(session, "inTabset", selected = "2. Summary Statistics")
  })

  observeEvent(input$totab3, {
    updateTabsetPanel(session, "inTabset", selected = "3. Data Visualization")
  })

  output$summary_stats <- renderTable({
    Summary_Statistics %>%
      filter(Variable == input$var_to_summarize) %>%
      select(-Variable)
  })

  output$plot_out <- renderPlot({

    if(input$y_axis == "Count"){
      ggplot(data = Pig_Data,
             aes_string(x = paste0("`", input$x_axis, "`"))) +
        geom_histogram(aes(fill = if(input$color_by_treatment == "Yes") Treatment,
                           group = if(input$color_by_treatment == "Yes") Treatment),
                       position = "identity",
                       alpha = 0.5,
                       bins = 24) +
        theme_classic() +
        labs(x = input$x_axis,
             fill = "Treatment") +
        if(input$x_axis %in% c("Start Time", "Finish Time")){
          geom_vline(xintercept = c(8, 17),
                     lty = 2)
        }
    } else{

    ggplot(data = Pig_Data,
           # aes(x = parse(text = input$x_axis)))
           aes_string(x = paste0("`", input$x_axis, "`"),
                      y = paste0("`", input$y_axis, "`"))) +
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
    }
    })

  observeEvent(input$submit, {

    write_csv(read_csv("User_Feedback.csv",
                       col_types = list(.default = "c")) %>%
                bind_rows(tibble(userID = input$userID,
                                 userInput = input$userInput)) %>%
                distinct(),
              "User_Feedback.csv",
              append = FALSE)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
