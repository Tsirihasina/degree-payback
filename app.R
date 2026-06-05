library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)

dt <- read.csv2("career.csv")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    title = tagList(icon("dollar-sign"), "Discipline Salary")
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Discipline", tabName = "domaine", icon = icon("graduation-cap")),
      menuItem("Ranking", tabName = "classement", icon = icon("list"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "domaine",
        pickerInput(
          inputId = "search",
          label = "Discipline search:", 
          choices = c(unique(dt$undergraduate_major)),
          options = pickerOptions(container = "body", liveSearch = TRUE),
          width = "100%"
        ),
        fluidRow(
          box(
            title = "Starting median salary",
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            textOutput("starting")
          ),
          box(
            title = "Mid-Career median salary",
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            textOutput("mid")
          ),
          box(
            title = "Salary Growth from Starting to Mid-Career",
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            textOutput("percent_change")
          )
        ),
        fluidRow(
          box(
            title = textOutput("discipline"),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("graphe_percentile")
          )
        )
      ),
      
      tabItem(
        tabName = "classement",
        virtualSelectInput(
          inputId = "top",
          label = "Total Ranked :", 
          choices = c(1:nrow(unique(dt %>% select(undergraduate_major)))),
          width = "100%",
          dropboxWrapper = "body"
        ),
        tags$h2("Best"),
        fluidRow(
          box(
            title = "Starting",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            tableOutput("strating_head")
            ),
          box(
            title = "Mid-Career",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            tableOutput("midcareer_head")
          )
        ),
        tags$h2("Worst"),
        fluidRow(
          box(
            title = "Starting",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            tableOutput("strating_tail")
          ),
          box(
            title = "Mid-Career",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            tableOutput("midcareer_tail")
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
#Domaine
  output$starting <- renderText(
    {
      val = dt %>%
        filter(undergraduate_major==input$search) %>% 
        select(starting_median_salary) %>% 
        unique()
      paste("$", as.character(val))
    }
  )
  
  output$mid <- renderText(
    {
      val = dt %>%
        filter(undergraduate_major==input$search) %>% 
        select(mid_career_median_salary) %>% 
        unique()
      paste("$", as.character(val))
    }
  )
  
  output$percent_change <- renderText(
    {
      val = dt %>%
        filter(undergraduate_major==input$search) %>% 
        select(percent_change_from_starting_to_mid_career_salary) %>% 
        unique()
      paste(as.character(val), "%")
    }
  )
  
  output$graphe_percentile <- renderPlot(
    {
       dt %>%
        filter(undergraduate_major==input$search) %>% 
        select(mid_career_percentile_salary, value) %>% 
        ggplot() + 
          aes(x = as.factor(mid_career_percentile_salary), y = value, fill = as.factor(mid_career_percentile_salary)) +
          geom_col() +
          labs(x = "Percentiles", y = "Annual Salary ($)", fill = "Percentiles") +
          theme_light()
    }
  )
  
  output$discipline <- renderText(
    {
      paste("Mid-Career Salary Percentiles for ", input$search, sep = "")
    }
  )
  
#Classement
  dt_starting <- reactive(
    {
      dt %>% 
        select(undergraduate_major, starting_median_salary) %>% 
        unique()
    }
  )
  
  dt_mid <- reactive(
    {
      dt %>% 
        select(undergraduate_major, mid_career_median_salary) %>% 
        unique() 
    }
  )
  
  output$strating_head <- renderTable(
    {
      dt_starting() %>% 
        arrange(desc(starting_median_salary))  %>% 
        rename(
          'Discipline' = 'undergraduate_major',
          'Median salary' = 'starting_median_salary'
        ) %>% 
        head(as.numeric(input$top))
    }
  )
  
  output$midcareer_head <- renderTable(
    {
      dt_mid() %>% 
        arrange(desc(mid_career_median_salary))  %>% 
        rename(
          'Discipline' = 'undergraduate_major',
          'Median salary' = 'mid_career_median_salary'
        ) %>% 
        head(as.numeric(input$top))
    }
  )
  
  output$strating_tail <- renderTable(
    {
      dt_starting() %>% 
        arrange(starting_median_salary)  %>% 
        rename(
          'Discipline' = 'undergraduate_major',
          'Median salary' = 'starting_median_salary'
        ) %>% 
        head(as.numeric(input$top))
    }
  )
  
  output$midcareer_tail <- renderTable(
    {
      dt_mid() %>% 
        arrange(mid_career_median_salary) %>% 
        rename(
          'Discipline' = 'undergraduate_major',
          'Median salary' = 'mid_career_median_salary'
        ) %>% 
        head(as.numeric(input$top))
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
