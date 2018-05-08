## anything you load here can be seen by both ui and server

library(shiny)
library(tidyverse)
library(fivethirtyeight)



  

##load data in plots.rmd
select_year_total <- c("`Total Homeless, 2017`","`Total Homeless, 2016`","`Total Homeless, 2015`","`Total Homeless, 2014`","`Total Homeless, 2013`","`Total Homeless, 2012`","`Total Homeless, 2011`","`Total Homeless, 2010`","`Total Homeless, 2009`","`Total Homeless, 2008`","`Total Homeless, 2007`")
select_year_change <- c('2016' = 'as.numeric(Change_2016)', '2015' = 'as.numeric(Change_2015)', '2014' = 'as.numeric(Change_2014)', '2013' = 'as.numeric(Change_2013)', '2012' = 'as.numeric(Change_2012)', '2011' =  'as.numeric(Change_2011)', '2010' = 'as.numeric(Change_2010)', '2009' =  'as.numeric(Change_2009)', '2008' = 'as.numeric(Change_2008)', '2007' = 'as.numeric(Change_2007)')
select_states <- c('`Missouri`', '`Oregon`', '`California`', '`Washington`', '`Kentucky`', '`New York`', '`Florida`', '`Alaska`')
select_x_variables <- c('`% Unemployed`', '`Gini Index`', '`Median Gross Rent`', '`Highschool Dropout Rate`', '`Total Population`', '`% In Poverty`', '`Housing Units`')
select_regression <- c('FALSE', 'loess', 'lm')


# Define UI for application that plots 
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('year_total', 'Select Year for Oregon Counties', 
                  choices = select_year_total),         
      selectInput('year_change', 'Select Year for US States (Change in Homelessness)', choices = select_year_change),
      selectInput('states', 'Select State(s)', choices = select_states, multiple = TRUE),
      selectInput('x_variable','Select X Variable', choices = select_x_variables),
      selectInput('model', 'Fit Line', choices = select_regression)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("oregon_areas"),
      plotOutput("usa_states")
    )
  )
)

##Server is where all of the computations happen
server <- function(input, output) {
  
  output$oregon_areas <- renderPlot({
    Ultimate_Oregon %>% ggplot(aes_string(x=input$x_variable, y=input$year_total, color = 'Area')) + 
      geom_point() +
      stat_smooth(method = input$model, se = FALSE, color = 'black') +
      theme_tufte() 
  })
  
  output$usa_states <- renderPlot({
    Ultimate_States %>% ggplot(aes_string(x=input$x_variable, y=input$year_change,
                                                                             color='State_Name')) + 
      geom_point() +
      stat_smooth(method = input$model, se = FALSE, color = 'black') +
      theme_tufte()
      #  %>% filter(State_Name == input$states) - WANT TO ADD THIS IN SOMEHOW
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
