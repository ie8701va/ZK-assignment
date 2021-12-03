library(psych)
library(lmtest)
library(lm.beta)
library(tidyverse)	
library(gridExtra)
library(car)

postop_pain_data = read.csv("https://tinyurl.com/ha-dataset1")

# In order to re-create the Theory based model from the first exercise, we must transform the erroneous data points as well as create mean Cortisol level variable as previously we decided to fix multicollinearity by taking the mean of the two cortisol measurements.

postop_pain_data1 <- postop_pain_data %>%	
  mutate(pain = replace(pain,  pain=="55", "5"))


postop_pain_data2 <- postop_pain_data1 %>%	
  mutate(STAI_trait = replace(STAI_trait,  STAI_trait=="4.2", "42"))


postop_pain_data2 <- postop_pain_data2 %>%	
  mutate(meanCortisol = (cortisol_saliva + cortisol_serum)/2)

postop_pain_data2 %>% 	
  summary()	


postop_pain_data2$pain <- as.integer(postop_pain_data2$pain)

postop_pain_data2$STAI_trait <- as.integer(postop_pain_data2$STAI_trait)



InitialModel <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = postop_pain_data2)
InitialModel

summary(InitialModel)

# INITIAL MODEL DIAGNOSTICS
# Checking the Initial Model for influential outliers 


InitialModel_plot1 = postop_pain_data2 %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	


InitialModel_plot2 = postop_pain_data2 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	

grid.arrange(InitialModel_plot1, InitialModel_plot2, nrow = 1)


InitialModel_plot3 = postop_pain_data2 %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")


InitialModel_plot4 = postop_pain_data2 %>% 	
  ggplot() +	
  aes(x = pain_cat, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	

grid.arrange(InitialModel_plot3, InitialModel_plot4, nrow = 1)


InitialModel_plot5 <- postop_pain_data2 %>% 	
  mutate(rownum = row.names(postop_pain_data2)) %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain, label = rownum) +	
  geom_label()

InitialModel_plot5


InitialModel_plot6 = postop_pain_data2 %>% 	
  ggplot() +	
  aes(x = IQ, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	


InitialModel_plot7 = postop_pain_data2 %>% 	
  ggplot() +	
  aes(x = household_income, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	

grid.arrange(InitialModel_plot6, InitialModel_plot7, nrow = 1)


InitialModel_plot8 = postop_pain_data2 %>% 	
  ggplot() +	
  aes(x = sex, y = pain) +	
  geom_boxplot()+	
  geom_smooth(method = "lm")	


InitialModel_plot9 = postop_pain_data2 %>% 	
  ggplot() +	
  aes(x = age, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	

grid.arrange(InitialModel_plot8, InitialModel_plot9, nrow = 1)

# There seems to be no influential outliers in these plots that would significantly affect the model. 

# Plotting the Residual Leverage, and Cook's Distance plots to identify highly influential cases in the Initial Model.

InitialModel %>% 	
  plot(which = 5)	

InitialModel %>% 	
  plot(which = 4)	

#Examining the 3 highly influential cases recognised by the plots.

postop_pain_data2 %>% 	
  slice(c(47, 85, 86))	# none of these observations have non-viable values.


# Checking the normality assumption of the Initial Model

## QQ plot	
InitialModel %>% 	
  plot(which = 2)	# The data points seem to be following the dashed line for the most part.

### Histogram of the residuals in Initial Model

residuals_InitialModel = enframe(residuals(InitialModel))	
residuals_InitialModel %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	# These residuals look very close to being normally distributed


#### Skew and kurtosis measures	in Initial Model
describe(residuals(InitialModel))	 # both skew and kurtosis values are within the range indicating no issues with asymmetry or "pointiness" (kurtosis) of a data set.

# Checking the linearity assumption of the Initial Model

InitialModel %>% 	
  residualPlots()	# No statistically significant values indidate no violation of the linearity assumption.

# Checking the homoscedasticity assumption of the Initial Model

InitialModel %>% 	
  plot(which = 3)	

InitialModel %>% 	
  ncvTest()

InitialModel %>% 	
  bptest() 

## The tests indicate that there is no violation of the homoscedasticity assumption of the Initial Model. None of the predictors can be linearly determined by the other predictor. 

# Checking the assumption of no multicollinearity 

InitialModel  %>% 	
  vif()		# Assumption of no multicollinearity confirmed

# Running backwards regression analysis

step(InitialModel, direction = "backward")

# Setting a new backward model regression based on the output of backwards regression analysis

BackwardModel <- lm(formula = pain ~ age + pain_cat + mindfulness + cortisol_serum, 
   data = postop_pain_data2)

summary(BackwardModel)

BackwardModel
TheoryBasedModel

# Contstructing table for the Backward Model regression coefficients. The results are shown in the figure below. 

coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

coef_table(BackwardModel)

# Finding out whether initial model was a better model fit to the data than the backward model.

AIC(InitialModel)
AIC(BackwardModel) # AIC is lower for the backward model, therefore, it is safe to assume that it is worthwhile completing a backward regression model for finding out the most influential predictors of pain outcome. 

# Setting a Theory based model regression from the part 1 of the assignment. (Model2.1)

TheoryBasedModel = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + meanCortisol, data = postop_pain_data2)
TheoryBasedModel


# Running AIC on both models

AIC(TheoryBasedModel)


AIC(BackwardModel)

# The theory based model seems to be only slightly better than the Backward Model (more than 2 points difference).



summary(TheoryBasedModel)$adj.r.squared
summary(BackwardModel)$adj.r.squared



# The Theory Based Model explains 51.89% of the dependent variable's variation, whilst the Backward Model explains 50.68% of the variation. 

#Running ANOVA & r.sq.adj on the two models

anova(BackwardModel, TheoryBasedModel)

# All of the above model comparison methods indicate that the backward regression model does not perform better than the theory based model.
# There seems to be no statistically significant difference between the two models. 

# Testing the models on additional datasets

home_sample_2 = read.csv("https://tinyurl.com/87v6emky")

summary(home_sample_2) # Reviewing data for coding errors. Nothing out of ordinary. 


# To be able to test the theory based model, we must once again, create the meanCortisol variable in the new data set. 
home_sample_2.1 <- home_sample_2 %>%	
  mutate(meanCortisol = (cortisol_saliva + cortisol_serum)/2)	


# Determining the model performances by testing it on the new data set by calculating the predicted values. 

predict_BackwardMod_test_data <- predict(BackwardModel, home_sample_2.1)


predict_TheoryMod_test_data <- predict(TheoryBasedModel, home_sample_2.1)

# Calculating the sum of squared residuals

RSS_test_backwardModel = sum((home_sample_2.1[,"pain"] - predict_BackwardMod_test_data)^2)	
RSS_test_theoryModel = sum((home_sample_2.1[,"pain"] - predict_TheoryMod_test_data)^2)	
RSS_test_backwardModel	
RSS_test_theoryModel	

## Since the residual sum of squares in the Theory Based Model (234.65) is smaller than the Backward model (249.68), the test reveals that the backward regression model has more error than the theory based model. Therefore, one can conclude, that the Theory Based Model is a better model fit for the data. 

# The regression equation for the BackwardModel: Pain = 1.16 + (-0.04)*age + 0.11* pain_cat + (-.06)*mindfulness + 0.4*cortisol_serum.


