## anything you load here can be seen by both ui and server

library(shiny)
library(tidyverse)
library(fivethirtyeight)

##load data in plots.rmd
select_color_options <- c("Area")

# Define UI for application that plots 
ui <- fluidPage(
  
  # Application title
  titlePanel("Part 1: Connecting UI and Server"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('color_opts', 'Select Category to Color With', 
                  choices = select_color_options),         
      selectInput('color_opts2', 'Select Category to Color With', choices = select_color_options) ## Add User Interface element here ## Add User Interface element here
      
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
    PIT_Housing_Oregon_Merge %>% ggplot(aes_string(y= 'per100', x="Unsheltered", label = 'Area', 
                                                   color=input$color_opts)) + 
      geom_point()
    
  })
  
  output$usa_statee <- renderPlot({
    PIT_Housing_State_Merge %>% ggplot(aes_string(x='per100', y="Change_2017", 
                                  fill=State_Name)) + #input$color_opts2
      geom_point() 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
