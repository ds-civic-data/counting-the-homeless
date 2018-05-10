## anything you load here can be seen by both ui and server

library(shiny)
library(tidyverse)
library(fivethirtyeight)
library(ggrepel)
library(ggthemes)


  

##load data in plots.rmd
select_oregon_year_change <- c('Total') 
  #c('2017' = "`Total Homeless, 2017`", '2016' = "`Total Homeless, 2016`", '2015' = "`Total Homeless, 2015`", '2014' = "`Total Homeless, 2014`", '2013' = "`Total Homeless, 2013`",'2012' = "`Total Homeless, 2012`",'2011' = "`Total Homeless, 2011`", '2010' = "`Total Homeless, 2010`",'2009' = "`Total Homeless, 2009`", '2008' = "`Total Homeless, 2008`", '2007' = "`Total Homeless, 2007`")
#select_oregon_year_change <- c('2017' = "(as.numeric(`Total Homeless, 2017`) - as.numeric(`Total Homeless, 2016`))/as.numeric(`Total Homeless, 2016`)", '2016' = "(as.numeric(`Total Homeless, 2016`) - `Total Homeless, 2015`)/as.numeric(`Total Homeless, 2015`)", '2015' = "(`Total Homeless, 2015` - `Total Homeless, 2014`)/as.numeric(`Total Homeless, 2014`)", '2014' = "(`Total Homeless, 2014` - `Total Homeless, 2013`)/as.numeric(`Total Homeless, 2013`)", 
 #                              '2013' = "(`Total Homeless, 2013` - as.numeric(`Total Homeless, 2012`))/as.numeric(`Total Homeless, 2012`)",'2012' = "(as.numeric(`Total Homeless, 2012`) - as.numeric(`Total Homeless, 2011`))/as.numeric(`Total Homeless, 2011`)",'2011' = "(as.numeric(`Total Homeless, 2011`) - `Total Homeless, 2010`)/as.numeric(`Total Homeless, 2010`)", '2010' = "(`Total Homeless, 2010` - `Total Homeless, 2009`)/as.numeric(`Total Homeless, 2009`)",
  #                             '2009' = "(`Total Homeless, 2009` - `Total Homeless, 2008`)/as.numeric(`Total Homeless, 2008`)", '2008' = "(`Total Homeless, 2008` - as.numeric(`Total Homeless, 2007`))/as.numeric(`Total Homeless, 2007`)")

select_year_change <- c('2016' = 'as.numeric(Change_2016)', '2015' = 'as.numeric(Change_2015)', '2014' = 'as.numeric(Change_2014)', '2013' = 'as.numeric(Change_2013)', '2012' = 'as.numeric(Change_2012)', '2011' =  'as.numeric(Change_2011)', '2010' = 'as.numeric(Change_2010)', '2009' =  'as.numeric(Change_2009)', '2008' = 'as.numeric(Change_2008)', '2007' = 'as.numeric(Change_2007)')
select_states <- c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Deleware', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana','Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming', 'District of Columbia')
select_x_variables <- c('`% Unemployed`', '`Gini Index`', '`Median Gross Rent`', '`Highschool Dropout Rate`', '`Total Population`', '`% In Poverty`', '`Housing Units`')
select_regression <- c('FALSE', 'loess', 'lm')
select_error <- c(FALSE, TRUE)

# Define UI for application that plots 
ui <- fluidPage(
  
  # Application title
  titlePanel("Change in Homeless Populations"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # selectInput('year_total', 'Select Year for Oregon Coc Regions', choices = select_year_total),         
      selectInput('year_change_oregon', 'Select Year for Oregon CoC Regions',choices = select_oregon_year_change),
      selectInput('x_variable_CoC','Select X Variable for Oregon CoC Regions', choices = select_x_variables),
      
      
      selectInput('year_change', 'Select Year for US States', choices = select_year_change),
      selectInput('x_variable_states','Select X Variable for US States', choices = select_x_variables),
      
      selectInput('states', 'Select State(s)', choices = select_states, multiple = TRUE),
      selectInput('model', 'Fit Line', choices = select_regression)
      #selectInput('se', 'S.E.', choices = select_error)
      
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
  
  Inputdata <- reactive ({
    
    new.df <- Ultimate_States %>%
      dplyr::filter(State_Name %in% c('Oregon', input$states)) 
    return(new.df)
    
  })
  
  
  
  output$oregon_areas <- renderPlot({
    Ultimate_Oregon %>% ggplot(aes_string(x=input$x_variable_CoC, y=input$year_change_oregon, label = 'Area')) + 
      geom_point() +
      geom_text_repel() +
      geom_smooth(method = input$model, se = FALSE, color = 'black') +
      theme_tufte() +
      ylab('Change in Homelessness for Given Year')
  })
  
  output$usa_states <- renderPlot({
    data_states <- Inputdata() 
    options = list(scrollX = TRUE)
    data_states%>% ggplot(aes_string(x=input$x_variable_states, y=input$year_change, label = 'State_Name')) + 
      geom_point() +
      geom_text_repel() +
      geom_smooth(method = input$model, se = FALSE, color = 'black') +
      theme_tufte() +
      ylab('Change in Homelessness for Given Year')
      #   - WANT TO ADD THIS IN SOMEHOW
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
