


lm_oregon <- lm(total_2011_2017 ~ `% Unemployed`, data = Ultimate_Oregon)
#residuals_oregon  <- data.frame(resid(lm_oregon)) 


Ultimate_Oregon$resid <- NA
Ultimate_Oregon$resid <- lm_oregon$resid


#Ultimate_Oregon %>%
 # mutate(residuals <- residuals_oregon) %>%
  #select(Area, residuals_oregon)

lm_oregon_x <- lm(`% Housing Units per ELI in 2009` ~ `Highschool Dropout Rate` + `Total Population` , Ultimate_Oregon)

lm_oregon_y <- lm(formula = `total_2011_2017` ~ `Highschool Dropout Rate` + `Total Population` , data = Ultimate_Oregon)
new.data <- Ultimate_Oregon
new.data$resid_y <- lm_oregon_y$resid
new.data$resid_x <- lm_oregon_x$resid
ggplot(new.data, aes (x =resid_x, y = resid_y, label = Area)) +
  geom_point() +
  geom_text_repel() +
  xlab('Residuals: Housing by Highschool Dropout and Total Population')+
  ylab('Residuals: Change in Homelessness by Highschool Dropout and Total Population')
  



lm_state_y <- lm(formula = `Change in Total Homelessness, 2011-2017` ~ `% Unemployed` + `Median Gross Rent` + `Highschool Dropout Rate` + `Total Population` + `% In Poverty` , data = Ultimate_States)

lm_state_x <- lm(formula =  + `% Housing Units per ELI in 2009`~ `% Unemployed` + `Median Gross Rent` + `Highschool Dropout Rate` + `Total Population` + `% In Poverty` , data = Ultimate_States)
new.data <- Ultimate_States
new.data$resid_y <- lm_state_y$resid
new.data$resid_x <- lm_state_x$resid
ggplot(new.data, aes (x =resid_x, y = resid_y, label = State_Name)) +
  geom_point() +
  geom_text_repel() +
  xlab('Residuals: Housing by Unemployment, Median Gross Rent, Highschool Dropout, Percent in Poverty, and Total Population')+
  ylab('Residuals: Change in Homelessness by all Independent Variables Except for Housing ')