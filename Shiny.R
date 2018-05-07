## anything you load here can be seen by both ui and server

library(shiny)
library(tidyverse)
library(fivethirtyeight)

##load data in plots.rmd
select_year_total <- c("Total_Homeless_2017","Total_Homeless_2016","Total_Homeless_2015","Total_Homeless_2014","Total_Homeless_2013","Total_Homeless_2012","Total_Homeless_2011","Total_Homeless_2010","Total_Homeless_2009","Total_Homeless_2008","Total_Homeless_2007")
select_year_change <- c('as.numeric(Change_2016)', 'as.numeric(Change_2015)', 'as.numeric(Change_2014)', 'as.numeric(Change_2013)', 'as.numeric(Change_2012)', 'as.numeric(Change_2011)', 'as.numeric(Change_2010)', 'as.numeric(Change_2009)', 'as.numeric(Change_2008)', 'as.numeric(Change_2007)')
select_states <- c('Missouri', 'Oregon', 'California', 'Washington', 'Kentucky', 'New York', 'Florida', 'Alaska')
select_proportion_beds <- c('per100_2014', 'per100_2009')



# Define UI for application that plots 
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('year_total', 'Select Year for Oregon Counties', 
                  choices = select_year_total),         
      selectInput('year_change', 'Select Year for US States', choices = select_year_change),
      selectInput('states', 'Select State(s)', choices = select_states, multiple = TRUE),
      selectInput('proportion_beds','Select X Variable', choices = select_proportion_beds)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("oregon_counties"),
      plotOutput("usa_states")
    )
  )
)

##Server is where all of the computations happen
server <- function(input, output) {
  
  output$oregon_counties <- renderPlot({
    PIT_Housing_Oregon_Merge %>% ggplot(aes_string(x=input$proportion_beds, y=input$year_total, color = 'Area')) + 
      geom_point() +
      xlab('Number of Affordable Housing Units per 100 E.L.I. Individuals')
      
    
  })
  
  output$usa_states <- renderPlot({
    PIT_Housing_State_Merge %>% filter(State_Name == input$states) %>% ggplot(aes_string(x=input$proportion_beds, y=input$year_change, 
                                  color='State_Name')) + 
      geom_point() +
      xlab('Number of Affordable Housing Units per 100 E.L.I. Individuals') +
      ylab('Change in Homelessness')
      
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
