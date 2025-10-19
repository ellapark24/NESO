# ---------------------------------- Packages ---------------------------------


packages <- c("ggplot2", "ecostats", "tidyverse", "patchwork", "caret", 
              "gridExtra", "viridis", "rsample", "dplyr")
lapply(packages, library, character.only = TRUE)

# Read excel data files into R 
original.data <- read_csv("20161213_uk_wind_solar_demand_temperature_SCS2.csv")
data <- read_csv("20161213_uk_wind_solar_demand_temperature_SCS2.csv")


############################ Data Preprocessing ############################

# Extract month, week, year, etc. from Local_DateTime column
data <- data %>%
  mutate(
    Local_DateTime = dmy_hm(`Local Time`), # Convert to POSIXct datetime object
    Month = month(Local_DateTime),
    WeekdayNum = wday(Local_DateTime) - 1,
    Year = year(Local_DateTime),
    Date = as.Date(Local_DateTime),
    Hour = hour(Local_DateTime),
    winter_year = ifelse(Month >= 11, Year, Year - 1),
    winter_start = as.Date(paste0(winter_year, "-11-01")),
    DSN = as.numeric(Date - winter_start)
  )

# TO calculation 
TO_data <- data %>%
  filter(Hour >= 15 & Hour <= 18) %>% # Avg temp between 3pm and 6pm
  group_by(Date) %>%
  summarise(TO = mean(temp_merra1, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Date)

# TE calculation
TE_data <- TO_data %>%
  mutate(TE = NA_real_)

# Initial condition: 1 Nov 1991 = 11.37 °C
TE_data$TE[1] <- 11.37  

# Compute recursively
for (i in 2:nrow(TE_data)) {
  TE_data$TE[i] <- mean(c(TE_data$TO[i], TE_data$TE[i-1]), na.rm = TRUE)
}

# Join TO and TE back into main dataset
data <- data %>%
  select(-Hour) %>%
  left_join(TE_data, by = "Date")

# Restrict the dates and convert the predictor variable to average daily demand

# Define the months you want to keep
selected_months <- c(11, 12, 1, 2, 3)

# Filter for dates between 1 November and 31 March and remove the data from 2015
data <- data[data$Month %in% selected_months & data$Year != 2015, ]


# Average over the day
processed.data <- data %>%
  group_by(Date) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE)
  ) %>%
  ungroup()

comparison <- function(formulas, names, data){
  
  # Initialise a dataframe for the results
  results <- data.frame(
    Model = character(),
    R_squared = numeric(),
    Adjusted_R_squared = numeric(),
    RMSE = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop over the models
  for (i in 1:length(formulas)) {
    
    # Extract models and names from the inputted dataframe
    model <- lm(formulas[[i]], data)
    model_name <- names[i]
    
    # Calculate R-squared and Adjusted R-squared
    model_summary <- summary(model)
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    
    # Calculate RMSE (Root Mean Squared Error)
    predictions <- predict(model, newdata = data)
    mse <- mean((data[["demand_gross"]] - predictions)^2)
    rmse <- sqrt(mse)
    
    # Append the results to the data frame
    results <- rbind(results, data.frame(
      Model = model_name,
      R_squared = r_squared,
      Adjusted_R_squared = adj_r_squared,
      RMSE = rmse
    ))
  }
  # Return the comparison data frame
  return(results)
}


# -------------------------- Define Model & Formula  ---------------------------

m.final <- lm(demand_gross ~ TE + solar_sarah + factor(WeekdayNum) + I(DSN^2) +
                factor(Month) + factor(Year) + I(DSN^2):factor(Month), 
              data = processed.data)

formula.final <- formula(m.final)

m.temp <- lm(demand_gross ~ temp_merra1 + solar_sarah + factor(WeekdayNum) + I(DSN^2) +
               factor(Month) + factor(Year) + I(DSN^2):factor(Month), 
             data = processed.data)

m.to <- lm(demand_gross ~ TO + solar_sarah + factor(WeekdayNum) + I(DSN^2) +
             factor(Month) + factor(Year) + I(DSN^2):factor(Month), 
           data = processed.data)

# -------------------------- Comparison of model -------------------------------
comparison <- function(formulas, names, data){
  
  # Initialise a dataframe for the results
  results <- data.frame(
    Model = character(),
    R_squared = numeric(),
    Adjusted_R_squared = numeric(),
    RMSE = numeric(),
    AIC = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop over the models
  for (i in 1:length(formulas)) {
    
    # Extract models and names from the inputted dataframe
    model <- lm(formulas[[i]], data)
    model_name <- names[i]
    
    # Calculate R-squared and Adjusted R-squared
    model_summary <- summary(model)
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    
    # Calculate RMSE (Root Mean Squared Error)
    predictions <- predict(model, newdata = data)
    mse <- mean((data[["demand_gross"]] - predictions)^2)
    rmse <- sqrt(mse)
    
    # Calculate AIC
    aic_value <- AIC(model)
    
    # Append the results to the data frame
    results <- rbind(results, data.frame(
      Model = model_name,
      R_squared = r_squared,
      Adjusted_R_squared = adj_r_squared,
      RMSE = rmse,
      AIC = aic_value
    ))
  }
  # Return the comparison data frame
  return(results)
}

# ------------------------------------------------------------------------------
# -------------------------- Random 10-Fold Model ------------------------------

set.seed(123)
folds <- createFolds(data$demand_gross, k = 10, list = TRUE, returnTrain = TRUE)

train.control <- trainControl(method = "cv", number = 10, savePredictions = "final")  

train.model <- train(formula.final, data = processed.data,
                     method = "lm",           
                     trControl = train.control)

kfold.pred <- train.model$pred
kfold.results <- train.model$results

p1 <- ggplot(data = kfold.pred, aes(x = obs, y = pred, colour = as.factor(Resample))) +
  
  geom_point() +
  
  geom_abline(slope = 1, intercept = 0, color = "black") +
  
  labs(title = "Predicted vs. Actual Demand using Random Cross Validation",
       
       x = "Actual Demand",
       
       y = "Predicted Demand") +
  
  scale_colour_discrete(name = "Training Set") +
  
  theme(text = element_text(size = 16))


# --------------------------- Add in year effect -------------------------------

# Set training and test data 
train.data <- processed.data %>% filter(Year <= as.character(2000))
test.data <- processed.data %>% filter(Year %in% c("2001"))

# # Compute Model and Prediction
# train.model  <- lm(formula.final, data = train.data)
# 
# # Take out year effect from the trained model's matrix
# terms <- attr(terms(train.model), "term.labels")
# new_terms <- terms[terms != "factor(Year)"]
# new_formula <- reformulate(new_terms, response = "demand_gross")
# X.noyear <- model.matrix(new_formula, data = test.data)
# 
# # Take out year coefficient from the trained model
# coefs <- coef(train.model)
# coefs.noyear <- coefs[!grepl("factor\\(Year\\)", names(coefs))]
# pred.noyear <- X.noyear %*% coefs.noyear
# 
# # Extract all coefficients from the full model
# coefs.full <- coef(m.final)
# 
# # Extract the coefficient for the test year from the full model 
# year.effect <- coefs.full["factor(Year)2001"]
# 
# # Add in year effect
# pred.adjusted <- pred.noyear + year.effect
# actual <- test.data$demand_gross
# 
# p2 <- ggplot(data = data.frame(actual, pred.adjusted), aes(x = actual, y = pred.adjusted)) +
#   
#   geom_point() +
#   
#   geom_abline(slope = 1, intercept = 0, color = "black") +
#   
#   labs(title = "Predicted vs. Actual Demand",
#        
#        x = "Actual Demand",
#        
#        y = "Predicted Demand") +
#   
#   scale_colour_discrete(name = "Training Set") +
#   
#   theme(text = element_text(size = 16))

rolling.cv <- function(formula, data, start.year, end.year, year_col, alpha){
  
  scores <- data.frame()
  
  for(year in start.year:(end.year-1)){
    
    
    # Set training and test data 
    train.data <- data %>% filter(Year <= as.character(year))
    test.data <- data %>% filter(Year == as.character(year + 1))
    
    if(nrow(test.data) == 0) {
      message(paste("No test data for year", year + 1))
      next
    }
    
    # Compute Model and Prediction
    train.model  <- lm(formula, data = train.data)
    
    
    # Take out year effect from the trained model's matrix
    terms <- attr(terms(train.model), "term.labels")
    new_terms <- terms[terms != "factor(Year)"]
    new_formula <- reformulate(new_terms, response = "demand_gross")
    X.noyear <- model.matrix(new_formula, data = test.data)
    
    # Take out year coefficient from the trained model
    coefs <- coef(train.model)
    coefs.noyear <- coefs[!grepl("factor\\(Year\\)", names(coefs))]
    pred.noyear <- X.noyear %*% coefs.noyear
    
    # Extract all coefficients from the full model
    coefs.full <- coef(m.final)
    
    # Extract the coefficient for the test year from the full model 
    year.effect <- coefs.full[paste0("factor(Year)", year + 1)]
    
    
    # Store predictions in a dataframe
    store.pred <- data.frame(
      observed = test.data$demand_gross,
      predictions = pred.noyear + year.effect,
      test_set = year + 1
    )
    
    # prediction <- predict(train.model, newdata = test.data,
    #                       se.fit = TRUE, interval = "prediction", level = 1 - alpha)
    # 
    # # Observed Values
    # obs <- test.data$demand_gross
    # 
    # # Compute Prediction Results
    # pred.mean <- prediction$fit[,1]
    # pred.sd <- sqrt(prediction$se.fit^2 + prediction$residual.scale^2)
    # pred.lwr <- prediction$fit[,2]  
    # pred.upr <- prediction$fit[,3]  
    # 
    # # Compute Scores
    # test.data$se <- (obs - pred.mean)^2
    # test.data$int <- pred.upr - pred.lwr + 2/(alpha) * ((pred.lwr - obs) *
    #                                                       (obs < pred.lwr) + (obs - pred.upr) * (obs > pred.upr))
    # test.data$ds <-  ((obs - pred.mean) / pred.sd)^2 + 2 * log(pred.sd)
    # 
    # test.data$observed <- obs
    # test.data$test.year <- year + 1
    # test.data$mean <- pred.mean
    
    
    
    # Store in data-frame
    scores <- rbind(scores, store.pred)
    
  }
  return(scores)
}

crossvalid.final <- rolling.cv(formula = formula.final, data = processed.data, start.year = 2000, end.year = 2014, alpha = 0.05)

view(crossvalid.final)

p2 <- ggplot(data = crossvalid.final, aes(x = observed, y = predictions, colour = as.factor(test_set))) +
  
  geom_point() +
  
  geom_abline(slope = 1, intercept = 0, color = "black") +
  
  labs(title = "Predicted vs. Actual Demand using Rolling Cross Validation",
       
       x = "Actual Demand",
       
       y = "Predicted Demand") +
  
  scale_colour_discrete(name = "Training Set") +
  
  theme(text = element_text(size = 16))


p1 + p2