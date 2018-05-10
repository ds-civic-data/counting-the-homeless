


lm_oregon <- lm(total_2011_2017 ~ `% Unemployed`, data = Ultimate_Oregon)
#residuals_oregon  <- data.frame(resid(lm_oregon)) 


Ultimate_Oregon$resid <- NA
Ultimate_Oregon$resid <- lm_oregon$resid


#Ultimate_Oregon %>%
 # mutate(residuals <- residuals_oregon) %>%
  #select(Area, residuals_oregon)

lm_oregon_x <- lm(`% Housing Units per ELI in 2009` ~ `% Unemployed` , Ultimate_Oregon)
lm_oregon_y <- lm(`total_2011_2017` ~ `% Unemployed` , Ultimate_Oregon)
new.data <- Ultimate_Oregon
new.data$resid_y <- lm_oregon_y$resid
new.data$resid_x <- lm_oregon_x$resid
ggplot(new.data, aes (x =resid_x, y = resid_y)) +
  geom_point()
