## anything you load here can be seen by both ui and server

library(shiny)
library(tidyverse)
library(fivethirtyeight)
library(ggrepel)
library(ggthemes)


  

##load data in plots.rmd
select_oregon_year_change <- c('Homeless' = 'total_2011_2017', 'Chronically Homeless' = 'chronic_2011_2017', 'Homeless Families' = 'families_20011_2017') 
  #c('2017' = "`Total Homeless, 2017`", '2016' = "`Total Homeless, 2016`", '2015' = "`Total Homeless, 2015`", '2014' = "`Total Homeless, 2014`", '2013' = "`Total Homeless, 2013`",'2012' = "`Total Homeless, 2012`",'2011' = "`Total Homeless, 2011`", '2010' = "`Total Homeless, 2010`",'2009' = "`Total Homeless, 2009`", '2008' = "`Total Homeless, 2008`", '2007' = "`Total Homeless, 2007`")
#select_oregon_year_change <- c('2017' = "(as.numeric(`Total Homeless, 2017`) - as.numeric(`Total Homeless, 2016`))/as.numeric(`Total Homeless, 2016`)", '2016' = "(as.numeric(`Total Homeless, 2016`) - `Total Homeless, 2015`)/as.numeric(`Total Homeless, 2015`)", '2015' = "(`Total Homeless, 2015` - `Total Homeless, 2014`)/as.numeric(`Total Homeless, 2014`)", '2014' = "(`Total Homeless, 2014` - `Total Homeless, 2013`)/as.numeric(`Total Homeless, 2013`)", 
 #                              '2013' = "(`Total Homeless, 2013` - as.numeric(`Total Homeless, 2012`))/as.numeric(`Total Homeless, 2012`)",'2012' = "(as.numeric(`Total Homeless, 2012`) - as.numeric(`Total Homeless, 2011`))/as.numeric(`Total Homeless, 2011`)",'2011' = "(as.numeric(`Total Homeless, 2011`) - `Total Homeless, 2010`)/as.numeric(`Total Homeless, 2010`)", '2010' = "(`Total Homeless, 2010` - `Total Homeless, 2009`)/as.numeric(`Total Homeless, 2009`)",
  #                             '2009' = "(`Total Homeless, 2009` - `Total Homeless, 2008`)/as.numeric(`Total Homeless, 2008`)", '2008' = "(`Total Homeless, 2008` - as.numeric(`Total Homeless, 2007`))/as.numeric(`Total Homeless, 2007`)")

select_year_change <- c('2016' = 'as.numeric(Change_2016)', '2015' = 'as.numeric(Change_2015)', '2014' = 'as.numeric(Change_2014)', '2013' = 'as.numeric(Change_2013)', '2012' = 'as.numeric(Change_2012)', '2011' =  'as.numeric(Change_2011)', '2010' = 'as.numeric(Change_2010)', '2009' =  'as.numeric(Change_2009)', '2008' = 'as.numeric(Change_2008)', '2007' = 'as.numeric(Change_2007)')
select_states <- c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Deleware', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana','Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming', 'District of Columbia')
select_x_variables <- c('`% Obtaining Affordable Housing, 2009`' = '`% Housing Units per ELI in 2009`', '`% Unemployed`', '`Median Gross Rent`', '`Highschool Dropout Rate`', '`% In Poverty`', '`Total Population`')
select_regression <- c('FALSE', 'loess', 'lm')
select_error <- c(FALSE, TRUE)

# Define UI for application that plots 
ui <- fluidPage(
  
  # Application title
  titlePanel("Trends in Homelessness"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # selectInput('year_total', 'Select Homelessness Category Coc Regions', choices = select_year_total),         
      selectInput('year_change_oregon', 'Select Homelessness Category Coc Regions',choices = select_oregon_year_change),
      selectInput('x_variable_CoC','Select X1 Variable for Oregon CoC Regions', choices = select_x_variables),
      
      selectInput('additional_variables', 'Select Added Variables', choices = select_x_variables, multiple = TRUE),
      #selectInput('year_change', 'Select Year for US States', choices = select_year_change),
      selectInput('x_variable_states','Select X2 Variable for US States', choices = select_x_variables),

      selectInput('states', 'Select Additional States', choices = select_states, multiple = TRUE),
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
  
  
  lm_oregon_x <- reactive ({
    
    lm_oregon_x <- lm(formula = input$x_variable_CoC ~ 'Total Population' * input$additional_variables, data = Ultimate_Oregon)
    return(lm_oregon_x)
    
  })
  
  
  lm_oregon_y <- reactive ({
    
    lm_oregon_y <- lm(formula = input$year_change_oregon ~ 'Total Population' * input$additional_variables, data = Ultimate_Oregon)
    return(lm_oregon_y)
  })

  
  output$oregon_areas <- renderPlot({
      lm_x <- lm_oregon_x()
      lm_y <- lm_oregon_y()
      Ultimate_Oregon$resid_y <- lm_y$resid
      Ultimate_Oregon$resid_x <- lm_x$resid
      Ultimate_Oregon %>% ggplot(aes_string(x= 'resid_x', y='resid_y', label = 'Area')) + 
      geom_point() +
      geom_text_repel() +
      geom_smooth(method = input$model, se = FALSE, color = 'black') +
      theme_tufte() +
      ylab('Change in Homelessness, 2011-2017')+
      xlab('X1, 2007-2011')+
      ylim(-1, 1)+
      ggtitle('Oregon CoC Regions')
  })
  
  
  lm_state_x <- reactive ({
    
    lm_state_x <- lm(formula = input$x_variable_CoC ~ 'Total Population' * input$additional_variables, data = Ultimate_Oregon)
    return(lm_state_x)
    
  })
  
  
  lm_state_y <- reactive ({
    
    lm_state_y <- lm(formula =  '`Change in Total Homelessness, 2011-2017`' ~ 'Total Population' * input$additional_variables, data = Ultimate_Oregon)
    return(lm_state_y)
  })
  
  
  
  output$usa_states <- renderPlot({
    data_states <- Inputdata()
    lm_x <- lm_state_x()
    lm_y <- lm_state_y()
    data_states$resid_y <- lm_y$resid
    data_states$resid_x <- lm_x$resid
    options = list(scrollX = TRUE)
    data_states%>% ggplot(aes_string(x=resid_x, y=resid_y, label = 'State_Name')) + 
      geom_point() +
      geom_text_repel() +
      geom_smooth(method = input$model, se = FALSE, color = 'black') +
      theme_tufte() +
      ylim(-1, 1) +
      xlab('X2, 2007-2011')+
      ggtitle('US States')
   
  })
}

#'`Change in Total Homelessness, 2011-2017`'
# Run the application 
shinyApp(ui = ui, server = server)
