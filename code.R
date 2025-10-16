packages <- c("ggplot2", "ecostats", "tidyverse", "patchwork", "gridExtra", "viridis")
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
    Hour = hour(Local_DateTime)  
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
