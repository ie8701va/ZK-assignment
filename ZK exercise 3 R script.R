library(dplyr)
library(psych) 	
library(tidyverse) 	
library(lme4) 	
library(lmerTest)  
library(cAIC4) 
library(r2glmm) 
library(MuMIn) 
library(gridExtra)
library(naniar) 
library(influence.ME)
library(car)
library(lattice)

# Opening the data files and assigning type of hospital as factor

data_file_3 = read.csv("https://tinyurl.com/b385chpu")
View(data_file_3)


data_file_4 = read.csv("https://tinyurl.com/4f8thztv")
View(data_file_4)

data_file_3 = data_file_3 %>% 	
  mutate(hospital = factor(hospital))	

data_file_4 = data_file_4 %>% 	
  mutate(hospital = factor(hospital))	



# Examining the data for coding errors, transforming the outliers and preparing the data set as per last assignment

data_file_3 %>% 	
  summary()	

data_file_4 %>% 	
  summary()	


# Replacing the negative household income value to NA, as there cannot be a negative income value. 

data_file_3.1 <- data_file_3 %>%
  replace_with_na(replace = list(household_income = -7884))


# Renaming variable name "woman" to "female" - data entry error.

data_file_3.2 <- data_file_3.1 %>%	
  mutate(sex = replace(sex,  sex=="woman", "female"))


# Changing some of the variables to factors.

data_file_3.2 = data_file_3.2 %>% 	
  mutate(sex = factor(sex))

data_file_4 = data_file_4 %>% 	
  mutate(sex = factor(sex))


# Adding Mean Cortisol variable in the datasets as we will need to use the model predictors from assignment part 1 where the final model included meanCortisol as a predictor variable. 


data_file_3.2 <- data_file_3.2 %>%	
  mutate(meanCortisol = (cortisol_saliva + cortisol_serum)/2)

data_file_4 <- data_file_4 %>%	
  mutate(meanCortisol = (cortisol_saliva + cortisol_serum)/2)

# Reviewing the data to see if the troublesome values have been corrected for and the data set prepared for analysis.

data_file_3.2 %>% 	
  summary()	

data_file_4 %>% 	
  summary()	


str(data_file_3.2)
str(data_file_4)



# Checking if different hospital can explain some of the variability in the model from home assignment part 1, bringing the regression lines closer to the actual observations.

plot1 = data_file_3.2 %>% 		
  ggplot() +		
  aes(y = pain, x = age, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE) %>% 
  

plot2 = data_file_3.2 %>% 		
  ggplot() +		
  aes(y = pain, x = STAI_trait, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)		

grid.arrange(plot1, plot2, nrow = 1)

# It seems that there is a clear negative relationship between age and the pain levels and a clearly positive relationship between STAI_trait and pain pain levels, however the variability seems pretty large. 


plot3 = data_file_3.2 %>% 		
  ggplot() +		
  aes(y = pain, x = pain_cat, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)		


plot4 = data_file_3.2 %>% 		
  ggplot() +		
  aes(y = pain, x = mindfulness, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)		

grid.arrange(plot3, plot4, nrow = 1)

# It seems that there is a clear negative relationship between minfulness and the pain levels and a clearly positive relationship between pain catastrophising measures and pain pain levels, however the variability seems larger than in the mindfulness plot. 



plot5 = data_file_3.2 %>% 		
  ggplot() +		
  aes(y = pain, x = meanCortisol, color = hospital) +		
  geom_point(size = 3) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)		

plot5

plot6 = data_file_3.2 %>% 		
  ggplot() +		
  aes(y = pain, x = sex, color = hospital) +		
  geom_boxplot()

grid.arrange(plot5, plot6, nrow = 1)
plot6

# It seems that there is a clear positive relationship between mean cortisol and the pain levels.


plot1+		
  xlim(-1, 50)+		
  geom_hline(yintercept=0)+		
  geom_vline(xintercept=0)

plot2+		
  xlim(-1, 50)+		
  geom_hline(yintercept=0)+		
  geom_vline(xintercept=0)		

plot3+		
  xlim(-1, 50)+		
  geom_hline(yintercept=0)+		
  geom_vline(xintercept=0)		

plot4+		
  xlim(-1, 50)+		
  geom_hline(yintercept=0)+		
  geom_vline(xintercept=0)		

plot5+		
  xlim(-1, 50)+		
  geom_hline(yintercept=0)+		
  geom_vline(xintercept=0)	

# Seems like the type of the hospital one is taken to can explain some of the variability in the predictor outcomes as can be seen from the plotted regression lines for each hospital separately.  
# What we can tell from these plots is that the predictor measurements seem to have a linear relationship with the pain measurement therefore the data is clustered between different hospitals and the observations are not completely independent of each other. Given a value in pain outcome in one of the hospitals it is likely you may be considerably accurate at predicting the pain outcome of a patient with a particular measurement in the other hospital.
# Regression lines are both divergent and roughly parallel to each other, depending on the predictor variable, therefore the slope or the effect of the predictors can be different in different hospitals. 

	

# Building a random intercept model where the clustering variable (random effect predictor - hospital) has an effect on the outcome variable - pain, and has no interaction with the predictor variables, that is, the hospital one is placed at has no influence on the effect of the fixed effect predictors.
## Predictor variables are the same as the Theory-based Model (Part 2) or Model 2.1 (Part 1). 

Model_random_intercept = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + meanCortisol + (1|hospital), data = data_file_3.2)

data_file_3.2 = data_file_3.2 %>% 	
  mutate(resid = residuals(Model_random_intercept))	

random_effects = as.data.frame(ranef(Model_random_intercept)[[1]])	
names(random_effects) = c("intercept")	


# Getting a coefficients table and confidence intervals for the random intercept model.
# The comparison of the model coefficients and the confidence  intervals of the coefficients for all fixed effect predictors, and the ones obtained in assignment part 1.

# Model coefficients	
summary(Model_random_intercept)

# Confidence intervals for the coefficients	
confint(Model_random_intercept)	

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}
stdCoef.merMod(Model_random_intercept) # Stand. Beta 

summary(Model_random_intercept)


# sum(residuals(Model2.1)^2) value of 224.46 from the Exercise 1	
sum(residuals(Model_random_intercept)^2)		# value of 168.5 - smaller than of Model 2.1 therefore we can claim that it is a better model fit. 



# Marginal R squared: the variance explained by the the fixed factors alone, when not taking into account the random effect terms

r2beta(Model_random_intercept, method = "nsj", data = data_file_3.2)	#Cortisol level is the most influential predictor, pain_cat and age are also significant predictors



# marginal and conditional R squared values: the variance explained by the fixed and random effect terms combined using conditional R2
r.squaredGLMM(Model_random_intercept)	

## Fixed and random effect terms combined (46.28%) seems to be better fit at explaining variance in the data than the fixed effect predictors only (38.5%). 




#Testing the models on the additional dataset (data_file_4)

View(data_file_4)

data_file_4 %>% 	
  summary()	


predict_model_random_intercept <- predict(Model_random_intercept, data_file_4, allow.new.levels = TRUE)


# Learning more about the total amount of error when using the model



RSS = sum((data_file_4[,"pain"] - predict_model_random_intercept)^2)
RSS	


TSS = sum((data_file_4$pain - predict(Model_random_intercept))^2)	
TSS


# Computing the total amount of information gained about the variability of the outcome.


R2 = 1-(RSS/TSS)	
R2	 # Random Intercept Model explains 54.9% of the variability in the pain outcome.

	


# Building a new linear mixed effects model on dataset 3 predicting pain. Only including the most influential predictor from the previous model.  

r2beta(Model_random_intercept, method = "nsj", data = data_file_3.2)  # Mean cortisol levels explain more of variability compared to other predictors
confint(Model_random_intercept)  # A beta of .35 has the strongest effect therefore we will build a mixed effect model using the meanCortisol as a predictor and allow for both random intercept and random slope.


# The new models with a single predictor; allowing for both random intercept and random slope.

Model_ran_int_cortisol <- lmer(pain ~ meanCortisol + (1|hospital), data = data_file_3.2)		

Model_ran_slope_cortisol <- lmer(pain ~ meanCortisol + (meanCortisol|hospital), data = data_file_3.2)


# Visualising the fitted regression lines for each hospital separately.



data_file_3.2 = data_file_3.2 %>% 		
  mutate(pred_int = predict(Model_ran_int_cortisol),		
         pred_slope = predict(Model_ran_slope_cortisol))



# Regression line of the random intercept model	



int_reg_line <- data_file_3.2 %>% 		
  ggplot() +		
  aes(y = pain, x = meanCortisol, group = hospital)+		
  geom_point(aes(color = hospital), size = 3) +		
  geom_line(color='red', aes(y=pred_int, x=meanCortisol))+		
  facet_wrap( ~ hospital, ncol = 3)+
  ggtitle("Random Intercept Model")+
  theme(legend.position="bottom")		

int_reg_line

# Regression line of the random slope(&intercept) model	


slope_reg_line <- data_file_3.2 %>% 		
  ggplot() +		
  aes(y = pain, x = meanCortisol, group = hospital)+		
  geom_point(aes(color = hospital), size = 3) +		
  geom_line(color='red', aes(y=pred_slope, x=meanCortisol))+		
  facet_wrap( ~ hospital, ncol = 3)+
  guides(fill = guide_legend(title.position = "bottom"))+
  ggtitle("Random Slope Model")+
  theme(legend.position="bottom")

slope_reg_line

# The difference seems unremarkable. 
 
# Final statistical comparison of the two models

sum(residuals(Model_ran_int_cortisol)^2)
sum(residuals(Model_ran_slope_cortisol)^2)

cAIC(Model_ran_int_cortisol)$caic
cAIC(Model_ran_slope_cortisol)$caic

anova(Model_ran_int_cortisol, Model_ran_slope_cortisol)
