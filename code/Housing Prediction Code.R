######## TEAM CODE ##############

library(plyr)
library(MASS)
library(ggplot2)
library(plotly)
require(GGally)
library(faraway)
library(lmtest)
library(corrplot)
library(ggcorrplot)
library(caret)
library(scales)
library(tidyverse)
require(Metrics)

###------------------------------- Functions------------------------------- ####

diagnostics = function(model, pcol = "grey", lcol = "dodgerblue", alpha = 0.05, plotit = TRUE,
                       testit = FALSE) {
  
  if (plotit == TRUE) {
    par(mfrow = c(1, 2))
    
    plot(fitted(model) , resid(model), col = pcol, pch = 20,
         xlab = "Fitted", ylab = "Residuals", main = "Fitted vs Residuals")
    abline(h = 0, col = lcol, lwd = 2)
    
    qqnorm(resid(model), col = pcol, pch = 20)
    qqline(resid(model), col = lcol, lwd = 2)
  }
  
  if (testit == TRUE) {
    p_val = shapiro.test(resid(model))$p.value
    decision = ifelse(p_val < alpha, "Reject", "Fail to Reject")
    list(p_val = p_val, decision = decision)
  }
}

get_model_vars = function(model){
  
  string_var = as.character(model$formula[3])
  string_var = gsub("\\+","",string_var)
  list_var = strsplit(string_var, split = "\\s+")[[1]]
  
  list_var[[length(list_var)+1]] = as.character(model$formula[2])
  
  return(list_var)
}

###------------------------------- Read Data------------------------------- ####

house_data_raw = read.table("C:\\Users\\17788\\Desktop\\DANA (2)\\2. DANA 4810-001\\Project\\kc_house_data.csv", header = T, sep = ",")
house_data_raw = as_tibble(house_data_raw)
glimpse(house_data_raw)

###-----------------------Cleaning and Data Type -----------------####
#Nan value
nrow(na.omit(house_data_raw)) == nrow(house_data_raw)
glimpse(house_data_raw %>%
          select(everything()) %>%
          summarise_all(funs(sum(is.na(.)))))

glimpse(house_data_raw)

filtered_house_data_raw = house_data_raw %>%
  select(-c(id, 
            date,
            #zipcode #id is an identifier do not provide meaningful interpretation, we do not remove lat and long
            #initially they were significant within the model
  ))

###-----------------------Data Type transformations -----------------####
# filtered_house_data = filtered_house_data_raw
filtered_house_data = filtered_house_data_raw %>%
  mutate_each_(funs(factor(.)),c("waterfront","view","condition","grade")) #only this variables because have a categorical ordinal behavior
glimpse(filtered_house_data)

###-----------------------Full Model -----------------####

#Use stepwise
filtered_house_data.mstep = step(lm(price ~., data = filtered_house_data),direction = "both")

#Visualize residuals
summary(filtered_house_data.mstep)
summary(filtered_house_data.mstep)$adj.r.squared
rmse(predict(filtered_house_data.mstep), filtered_house_data$price)
mape(predict(filtered_house_data.mstep), filtered_house_data$price)
diagnostics(filtered_house_data.mstep)

#We keep the model filtered_house_data.mstep

###------------Fix heterocedasticity -----------------####

#Transformation to response variable looks like a cone, according to lecture 8 it should be log of var
filtered_house_data.mstep_and_ytransf = step(lm(log(price) ~., data = filtered_house_data),direction = "both")
summary(filtered_house_data.mstep_and_ytransf)$adj.r.squared
rmse(exp(predict(filtered_house_data.mstep_and_ytransf)), filtered_house_data$price)
mape(exp(predict(filtered_house_data.mstep_and_ytransf)), filtered_house_data$price)
diagnostics(filtered_house_data.mstep_and_ytransf)

#Get columns used in stepwise model
columns_in_model = colnames(filtered_house_data.mstep_and_ytransf$model)[2:length(colnames(filtered_house_data.mstep_and_ytransf$model))]
columns_in_model[length(columns_in_model)+1] = "price" #Add response variable
#Have a dataset from model 
house_data_model = filtered_house_data %>%
  select(columns_in_model)
glimpse(house_data_model)

###-----------------------Check for multicollinearity -----------------####

vif_model <- lm(price ~ sqft_living + grade, data=house_data_model)
vif(vif_model)
car::vif(filtered_house_data.mstep_and_ytransf)
summary(filtered_house_data.mstep_and_ytransf)

corr <- round(cor(house_data_model %>%
                    select(where(is.numeric))), 1)
ggcorrplot(corr,method="square",
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,ggtheme = ggplot2::theme_classic, tl.cex = 18,
           title = "Collinearity in King County housing dataset")

hist(house_data_model$yr_renovated)
hist(house_data_model$yr_built)

# Year build is not normal at all
house.reduced_model = step(lm(log(price) ~., data = house_data_model), direction = "both")
car::vif(house.reduced_model) #Better metrics

summary(house.reduced_model)
mape(exp(predict(house.reduced_model)), house_data_model$price)
diagnostics(house.reduced_model)

summary(filtered_house_data.mstep_and_ytransf)$adj.r.squared
mape(exp(predict(filtered_house_data.mstep_and_ytransf)), filtered_house_data$price)

###-----------------------Influential Points cook distance -----------------####

cd = cooks.distance(house.reduced_model)
house.reduced_model

boolean = cd < 4/nrow(house_data_model) #Number of observations that are less than ratio for cook distance 4/n 
table(boolean)
#Final model
house.reduced_model.cook = lm(formula = log(price) ~ (bathrooms + bedrooms + sqft_living + yr_built 
                                                      + yr_renovated + sqft_lot + floors + waterfront + view + condition + grade + 
                                                        + lat + long + sqft_living15 + sqft_lot15 + zipcode), data = house_data_model,
                              subset = boolean)#Apply cook distance
diagnostics(house.reduced_model.cook)
summary(house.reduced_model.cook)$adj.r.squared


house_data_model_cooks_distance = as_tibble(cbind(house_data_model, boolean)) %>% filter(boolean == TRUE)
nrow(house_data_model_cooks_distance)
mape(exp(predict(house.reduced_model.cook)), house_data_model_cooks_distance$price)

###-----------------------Interaction -----------------####
car::vif(house.reduced_model.cook) #Check again for interactions

#Correlation matrix
corr <- round(cor(house_data_model %>%
                    select(where(is.numeric))), 1)
ggcorrplot(corr,method="square",
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,ggtheme = ggplot2::theme_classic, tl.cex = 18,
           title = "Collinearity in King County housing dataset")

#Try interactions
house.reduced_model_interaction = lm(formula = log(price) ~ (bathrooms + bedrooms 
                                                             #+ zipcode #at this point we decided to keep only 1 set of variable for geographic location
                                                             + sqft_living + sqft_lot
                                                             + floors + waterfront + view + condition + grade  
                                                             +yr_built 
                                                             + yr_renovated +
                                                               +lat 
                                                             + long 
                                                             + sqft_living15 
                                                             + sqft_lot15
                                                             #+sqft_living*grade
                                                             #+sqft_living*bedrooms
                                                             #+sqft_lot15*long
                                                             #+sqft_lot*sqft_lot15
                                                             #+sqft_living*sqft_living15
                                                             #+sqft_living*bathrooms
), data = house_data_model_cooks_distance)

summary(house.reduced_model_interaction)
summary(house.reduced_model_interaction)$adj.r.squared #None of the interactions commented worked or improved significally the R2 and residuals/qqplot
mape(exp(predict(house.reduced_model_interaction)), house_data_model_cooks_distance$price)
diagnostics(house.reduced_model_interaction) 

###-----------------------Check for behavior of residuals with the remaining variables -----------------####

#Creating a new dataset with residuals and predictions

house_data_model_cooks_distance$residuals = log(house.reduced_model_interaction$residuals)
house_data_model_cooks_distance$predictions = predict(house.reduced_model_interaction)
glimpse(house_data_model_cooks_distance)

#Residuals vs predictions
ggplot(aes(x = predictions, y = residuals), data = house_data_model_cooks_distance)+ geom_point()

###-----------------------Final Model -----------------####

final_model = lm(formula = log(price) ~ (bathrooms + bedrooms 
                                         + sqft_living + sqft_lot
                                         + floors + waterfront + view + condition + grade  
                                         +yr_built 
                                         + yr_renovated +
                                           +lat 
                                         + long 
                                         + sqft_living15 
                                         + sqft_lot15
                                         
),  data = house_data_model_cooks_distance)

summary(final_model)
rmse(exp(predict(final_model)), house_data_model_cooks_distance$price)
mape(exp(predict(final_model)), house_data_model_cooks_distance$price)

diagnostics(house.reduced_model_interaction) 

##################  Checking and Testing  ######################

check_model = lm(formula = log(price) ~ (bathrooms + bedrooms 
                                         + sqft_living + sqft_lot
                                         + floors + waterfront + view + condition + grade  
                                         +yr_built 
                                         + yr_renovated +
                                           +lat 
                                         + long 
                                         + sqft_living15 
                                         + sqft_lot15)^2,  data = house_data_model_cooks_distance)

summary(check_model)

mape(exp(predict(check_model)), house_data_model_cooks_distance$price)
rmse(exp(predict(check_model)), house_data_model_cooks_distance$price)
diagnostics(house.reduced_model_interaction) 

interaction_model = lm(formula = log(price) ~ (bathrooms + bedrooms 
                                           + sqft_living + sqft_lot
                                           + floors + waterfront + view + condition + grade  
                                           +yr_built 
                                           + yr_renovated +
                                             +lat 
                                           + long 
                                           + sqft_living15 
                                           + sqft_lot15 + bathrooms:yr_built),  data = house_data_model_cooks_distance)
summary(interaction_model)  
mape(exp(predict(interaction_model)), house_data_model_cooks_distance$price)
rmse(exp(predict(interaction_model)), house_data_model_cooks_distance$price)


# Partial F-Test
anova(final_model, interaction_model)
  
##################### Train and Test ####################

set.seed(420)
sample <- sample.int(n = nrow(house_data_model_cooks_distance), 
                     size = floor(.7*nrow(house_data_model_cooks_distance)), replace = F) #70% of dataset 
train <- house_data_model_cooks_distance[sample, ]
test <- house_data_model_cooks_distance[-sample, ]
nrow(test)
nrow(train)  

model_result <- lm(formula = log(price) ~ (bathrooms + bedrooms 
                             + sqft_living + sqft_lot
                             + floors + waterfront + view + condition + grade  
                             +yr_built 
                             + yr_renovated +
                               +lat 
                             + long 
                             + sqft_living15 
                             + sqft_lot15),  data = train)

full_model <- exp(predict(model_result, test))
CAPE = (1/nrow(test))*sum(abs((test$price-full_model)/test$price))*100
MAE = (1/nrow(test))*sum(abs(test$price-full_model))
MSE = (1/nrow(test))*sum((test$price-full_model)^2)
RMSE = sqrt((1/nrow(test))*sum((test$price-full_model)^2))


########## Interaction Plot ############
floor1 = subset(house_data_raw, floors == "1")
floor2 = subset(house_data_raw, floors == "2")

par(mar = c(3, 3, 3, 3))
x_axis = seq(1900,2015,1)
result = data.frame(x_axis)

floor1_model <- lm(formula = log(price) ~ (bathrooms + bedrooms 
                             + sqft_living + sqft_lot
                             + floors + waterfront + view + condition + grade  
                             +yr_built 
                             + yr_renovated +
                               +lat 
                             + long 
                             + sqft_living15 
                             + sqft_lot15),  data = floor1)

summary(floor1_model)  

floor2_model <- lm(formula = log(price) ~ (bathrooms + bedrooms 
                                           + sqft_living + sqft_lot
                                           + floors + waterfront + view + condition + grade  
                                           +yr_built 
                                           + yr_renovated +
                                             +lat 
                                           + long 
                                           + sqft_living15 
                                           + sqft_lot15),  data = floor2)

summary(floor2_model)  
  
house_data_raw$floor1_value <- exp(predict(floor1_model, house_data_raw))
house_data_raw$floor2_value <- exp(predict(floor2_model, house_data_raw))


plot(x=house_data_raw$yr_built, y=house_data_raw$floor1_value, main = "Interaction Plot of Price vs. year built with floor Levels",
     xlab="Year Built", ylab="Price ($)", col="red", lwd=1, type="l")

lines(x=house_data_raw$yr_built, y=house_data_raw$floor2_value, col="blue")

legend(1,0.2,lty=1, legend=c("1 Floor", "2 Floors"), 
       col=c("red", "blue"), cex=0.5)


library(jtools) 
library(interactions)
library(cat)
library("dplyr")  

interact <- lm(formula = log(price) ~ (floors + yr_built)^2
               , data = house_data_raw)

interact_plot(interact, pred = yr_built, modx = floors)

