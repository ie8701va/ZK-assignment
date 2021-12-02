# 1st part of the assignment. 


library(psych)
library(lm.beta)
library(tidyverse)	
library(gridExtra)
library(car)

postop_pain_data = read.csv("https://tinyurl.com/ha-dataset1")

# Checking the dataset for irregularities
postop_pain_data %>% 	
  summary()	


# Correcting for coding errors, changing the type of data to assist with further analysis. 

postop_pain_data1 <- postop_pain_data %>%	# As the scale is from 0-10, the 55 must have been the error of the coder, where they meant to type 5. 
  mutate(pain = replace(pain,  pain=="55", "5"))


postop_pain_data2 <- postop_pain_data1 %>%	# As the scale is from 20-80, the 4.2 must have been the error of the coder, where they meant to type 42. 
  mutate(STAI_trait = replace(STAI_trait,  STAI_trait=="4.2", "42"))

postop_pain_data2 %>% 	
  summary()	

postop_pain_data2$pain <- as.integer(postop_pain_data2$pain)

postop_pain_data2$STAI_trait <- as.integer(postop_pain_data2$STAI_trait)


# Visualization to check if the errors have been corrected for (comparing old and new plots after coding error correction)


postop_pain_data2 %>% 
  summary()



old_plot_pain <-	
  postop_pain_data %>% 	
  ggplot()+	
  aes(x = pain)+	
  geom_histogram()	


new_plot_pain <-	
  postop_pain_data2 %>% 	
  ggplot()+	
  aes(x = pain)+	
  geom_histogram()	

grid.arrange(old_plot_pain, new_plot_pain, ncol=2)	


old_plot_STAI_trait <-	
  postop_pain_data %>% 	
  ggplot()+	
  aes(x = STAI_trait)+	
  geom_histogram()	


new_plot_STAI_trait <-	
  postop_pain_data2 %>% 	
  ggplot()+	
  aes(x = STAI_trait)+	
  geom_histogram()	

grid.arrange(old_plot_STAI_trait, new_plot_STAI_trait, ncol=2)	


# Further data exploration.	

postop_pain_data2 %>% 	
  select(pain, sex, age, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness, weight, IQ, household_income) %>% 	
  pairs.panels(col = "red", lm = T) # Exploring correlations between the variables, to see if any can be reasonable predictors for the model.

postop_pain_data2 %>% 	
  ggplot() +	
  aes(x = age, y = pain, color = sex) +	
  geom_point() +
  geom_smooth() # It appears that both men and women have a decreased sense of pain as they age. 

postop_pain_data %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram()	# Age data seems to be normally distributed as it follows the regular bell shape 

postop_pain_data %>% 	
  ggplot() +	
  aes(x = weight) +	
  geom_histogram()	 # both very low and very high values of weight are viable 

postop_pain_data %>% 	
  ggplot() +	
  aes(x = IQ) +	
  geom_histogram()	# both very low and very high values of IQ are viable

postop_pain_data %>% 	
  ggplot() +	
  aes(y = household_income, x = IQ) +	
  geom_point() +
  geom_smooth()  # both very low and very high values of household income are viable


# Prediction with linear regression - Model 1

# In a linear regression model we predict that sex and age would improve our understanding of postoperative pain. In other words, we expect that both sex and age are good predictors of postoperative pain experienced by patients who undergo surgical extraction of the third mandibular molar i.e. wisdom tooth.  

Model1 <- lm(pain ~ age + sex, data = postop_pain_data2)	
summary(Model1)

# MODEL 1 DIAGNOSTICS
# Checking Model 1 for influential outliers 

Model1_plot1 = postop_pain_data2 %>% 	
  ggplot() +	
  aes(x = sex, y = pain) +	
  geom_boxplot()+	
  geom_smooth(method = "lm")	


Model1_plot2 = postop_pain_data2 %>% 	
  ggplot() +	
  aes(x = age, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	

grid.arrange(Model1_plot1, Model1_plot2, nrow = 1)  # There seems to be no influential outliers in these plots that would significantly affect the data

# Identifying row numbers which may potentially be troublesome 

postop_pain_data2 %>% 	
  mutate(rownum = row.names(postop_pain_data2)) %>% 	
  ggplot() +	
  aes(x = age, y = pain, label = rownum) +	
  geom_label() # there may be a few values that could be out of tune. 
  
# Plotting the Residual Leverage, and Cook's Distance plots to identify highly influential cases in Model 1.

Model1 %>% 	
  plot(which = 5)	

Model1 %>% 	
  plot(which = 4)	

#Examining the 3 highly influential cases recognised by the plots.

postop_pain_data2 %>% 	
  slice(c(8, 23, 47))	# none of these observations have non-viable values.

# Checking the normality assumption of the Model 1

## QQ plot	
Model1 %>% 	
  plot(which = 2)	# The data points seem to be following the dashed line for the most part.

### Histogram of the residuals in Model 1

residuals_Model1 = enframe(residuals(Model1))	
residuals_Model1 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	# These residuals look very close to being normally distributed


#### Skew and kurtosis measures	in Model 1
describe(residuals(Model1))	 # both skew and kurtosis values are within the range indicating no issues with asymmetry or "pointiness" (kurtosis) of a data set.

# Checking the linearity assumption of the Model 1

Model1 %>% 	
  residualPlots()	



## Conclusion on linearity: visually, it includes some level of non-normality. There may be a non-linear trend within this scatterplot. There is some indication of the curvature.
### However, since the p-value of predictor (age) is not significant, we can confidently say that there is no linear relationship between the variable and the residuals.

# Checking the homoscedasticity assumption of the Model 1

Model1 %>% 	
  plot(which = 3)	# there is some indication of funnel shape relationship in this plot.


Model1 %>% 	
  ncvTest() # p-values are not significant therefore there is no violation of homomscedasticity assumption.


Model1 %>% 	
  ncvTest()


## The tests indicate that there is no violation of the homoscedasticity assumption of Model 1. None of the predictors can be linearly determined by the other predictor. 

# Checking the assumption of no multicollinearity 

Model1  %>% 	
  vif()	# Assumption of no multicollinearity confirmed

# Prediction with linear regression - Model 2
# In a linear regression model we predict that sex, age, STAI, pain catastrophising, mindfulness and cortisol measures would improve our understanding of postoperative pain. In other words, we expect that sex, age, STAI, pain catastrophising, mindfulness and cortisol measures are good predictors of postoperative pain experienced by patients who undergo surgical extraction of the third mandibular molar i.e. wisdom tooth.  	

Model2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = postop_pain_data2)
summary(Model2)

# MODEL 2 DIAGNOSTICS
# Checking Model 2 for influential outliers (plots with age and sex have already been performed above)


Model2_plot1 = postop_pain_data2 %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	


Model2_plot2 = postop_pain_data2 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	

grid.arrange(Model2_plot1, Model2_plot2, nrow = 1)  # There seems to be no influential outliers in these plots that would significantly affect the analysis.



Model2_plot3 = postop_pain_data2 %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")


Model2_plot4 = postop_pain_data2 %>% 	
  ggplot() +	
  aes(x = pain_cat, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")	

grid.arrange(Model2_plot3, Model2_plot4, nrow = 1)  # There seems to be no influential outliers in these plots that would significantly affect the analysis.



postop_pain_data2 %>% 	
  mutate(rownum = row.names(postop_pain_data2)) %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain, label = rownum) +	
  geom_label()  # There seems to be no influential outliers in these plots that would significantly affect the analysis.

# Plotting the Residual Leverage, and Cook's Distance plots to identify highly influential cases in Model 2.

Model2 %>% 	
  plot(which = 5)	

Model2 %>% 	
  plot(which = 4)	


# Examining the 3 highly influential cases recognised by the plots in Model 2.

postop_pain_data2 %>% 	
  slice(c(47, 74, 86))	# There seems to be nothing unusual about the subjects. 



# Checking the normality assumption of the Model 2.

## QQ plot	

Model2 %>% 	
  plot(which = 2)	# The data points seem to be following the dashed line for the most part.

### Histogram of the residuals in Model 2

residuals_Model2 = enframe(residuals(Model2))	
residuals_Model2 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	# These residuals look very close to being normally distributed

#### Skew and kurtosis measures	in Model 2
describe(residuals(Model2))	 # both skew and kurtosis values are within the range indicating no issues with asymmetry or "pointiness" (kurtosis) of a data set.


# Checking the linearity assumption of the Model 2

Model2 %>% 	
  residualPlots()	# The p-values are all above significance levels indicating no violation of linearity assumption

## The assumptions on linearity of the model can be confirmed as there is no statistically significant non-linear trend within this scatterplot. There is little indication of the curvature, and it implies linear regression for Model 2. 
###There exists a linear relationship between the independent variables in the model, and the dependent variable, pain.

# Checking the homoscedasticity assumption of the Model 2

Model2 %>% 	
  plot(which = 3)	# there is no funnel shaped relationship apparent in this plot that would indicate the violation of homoscedasticity assumption

Model2 %>% 	
  ncvTest() # no significant result of Chisquare test.

## The tests indicate no violation of the homoscedasticity assumption for Model 2.

# Checking the no-multicollinearity assumption


Model2 %>% 	
  vif()	

# Cortisol measures in blood and the saliva have been shown to be of high linear correlation, as their values are higher than 3 (an approach suggested by Zuur, Ieno, and Elphick, 2010). In other words, the predictors can be linearly determined by the other predictor. 
# This is data multicollinearity as the variables are naturally highly correlated with each other. 


# Exploring the reasons for the multicollinearity
## The correlation matrix of the predictors

postop_pain_data2 %>% 	
  select(pain, cortisol_serum, cortisol_saliva) %>% 	
  pairs.panels(col = "red", lm = T) %>%
  title("Correlation matrix of the predictors")
 
# The correlation matrix clearly indicates that the correlation of cortisol serum and saliva measures is very high (.85).	
# The type of collinearity  (data collinearity) suggests that we may linearly combine the two predictors by using the mean of the two values for each observation.

postop_pain_data2 <- postop_pain_data2 %>%	
  mutate(meanCortisol = (cortisol_saliva + cortisol_serum)/2)	

# Creating a new model with cortisol levels in blood and saliva accounted for. 
Model2.1 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + meanCortisol, data = postop_pain_data2)

summary(Model2.1)
summary(Model2)	

#Double checking the assumptions for the newly created Model 2.1


#Plotting the Residual Leverage, and Cook's Distance plots to identify highly influential cases in Model 2.1.

Model2.1 %>% 	
  plot(which = 5)	

Model2.1 %>% 	
  plot(which = 4)	# Nothing's out of ordinary.

# Checking the normality assumption of the Model 2.1.

## QQ plot	
Model2.1 %>% 	
  plot(which = 2)	# The data points seem to be following the dashed line for the most part.

### Histogram of the residuals in Model 2.1

residuals_Model2.1 = enframe(residuals(Model2.1))	
residuals_Model2.1 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	# These residuals look very close to being normally distributed

#### Skew and kurtosis measures	in Model 2.1
describe(residuals(Model2.1))	 # Both skew and kurtosis values are within the range indicating no issues with asymmetry or "pointiness" (kurtosis) of a data set.


# Checking the linearity assumption of the Model 2.1

Model2.1 %>% 	
  residualPlots()	# The p-values are all above significance levels indicating no violation of linearity assumption

## Conclusion on linearity of the model can be confirmed as there is no statistically significant non-linear trend within this scatterplot. There is little indication of the curvature, and it implies linear regression for Model 2.1. 
###There exists a linear relationship between the independent variables in the model, and the dependent variable, pain.

# Checking the homoscedasticity assumption of the Model 2.1

Model2.1 %>% 	
  plot(which = 3)	# there is no funnel shaped relationship apparent in this plot that would indicate the violation of homoscedasticity assumption

Model2.1 %>% 	
  ncvTest()  

# no multicollinearity assumption after data transformation. 
vif(Model2.1)
  
# The results show that the issue of mulitcollinearity has been solved in Model 2, now renamed to Model2.1.

# Looking at the adj. R squared statistic to see how much variance is explained by the new and the old model with only two predictors.
summary(Model1)$adj.r.squared

summary(Model2.1)$adj.r.squared

# The variance explained has increased substantially by adding information about STAI, pain catastrophising, mindfulness and cortisol measures to the model.

# Comparing model fit using the AIC() function and residual error through the anova() function.

AIC(Model1)

AIC(Model2.1)

# The difference in AIC of the two models is larger than 2, therefore the two models are significantly different in their model fit. Model 2.1 has a smaller AIC measure than Model 1 therefore is has less error and is a better model fit. 

#running an ANOVA to compare the two  models

anova(Model1, Model2.1)

sm = summary(Model1)	
sm # Sex does not have any statistically significant influence on the pain outcomes in this model.  

sm1 = summary(Model2.1)	
sm1 # Pain catastrophising, mindfulness and mean coritsol levels are significant predictors of pain outcome in this model. 

# Since we have p < .001 we reject the null hypothesis. The Analysis of Variance showed that the second model was significantly better than the first one, explaining 51.88% of the variance in postoperative pain (F (6, 153) = 29.58, p < .001, Adj. R^2 = 0.52, AIC = 478.35).
# Conclusion: adding additional predictors to the model bettered our chances at explaining pain outcome in patients undergoing wisdom tooth removal surgeries. 
# The following table describes the the amount by which the outcome variable's (pain) estimate would change if the predictor's values are increased by 1. The regression coefficient linked to pain catastrophising, mindfulness and mean cortisol level indicate a statistically significant change in pain measure of the given sample. This means that an increase in pain catastrophising measure by 1 point results in an increased estimate of pain by 0.14 points. An increase in mindfulness measure by 1 point results in a decreased estimate of pain by 0.25 points. Finally, an increase in cortisol measure 1 point results in an increased estimate of pain by 0.66 points.

# Constructing table for the regression coefficients.

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
coef_table(Model1)
coef_table(Model2.1)

# Constructing regression equation based on the values using the information obtained using the function shown below. 

Model2.1

# Regression Equation:  Pain = 0.33 + (-0.02)*age + 0.16*sexmale + (-0.02)*STAI_trait + 0.13*pain_cat + (-0.26)*mindfulness + 0.66*meanCortisol

