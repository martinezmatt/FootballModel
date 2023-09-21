# Loading libraries
library(tidyverse)
library(nflfastR)
library(caret)
library(randomForest) 

# Load data
data <- load_player_stats(2022)

# Cleaning data
clean_data <- data %>%
  filter(position == "WR" | position == "TE" | position == "RB") %>%
  select(player_display_name, position, fantasy_points_ppr, recent_team,
         rushing_yards, rushing_tds, rushing_epa, receptions, targets, 
         receiving_yards, receiving_tds, receiving_yards_after_catch, 
         receiving_epa, racr, wopr) %>%
  mutate_if(is.numeric, ~replace_na(., 0))

# Feature Selection
features <- clean_data %>%
  select(player_display_name, position, recent_team, rushing_yards, rushing_tds,
         rushing_epa, receptions, targets, receiving_yards, receiving_tds, 
         receiving_yards_after_catch, receiving_epa, racr, wopr)

# Target variable?
target_variable <- clean_data$fantasy_points_ppr

# Train-Test Split
set.seed(170)
train_indices <- createDataPartition(target_variable, p = 0.8, list = FALSE)
train_data <- features[train_indices, ]
test_data <- features[-train_indices, ]
train_target <- target_variable[train_indices]
test_target <- target_variable[-train_indices]

# Model Selection
model <- randomForest(train_target ~ ., data = train_data)

# Model Evaluation
predictions <- predict(model, newdata = test_data)
rmse <- sqrt( mean( (predictions - test_target)^2 ) )


combined_data <- cbind(test_data, Predicted_Points = predictions)

predicted_results <- combined_data %>%
  group_by(player_display_name) %>%
  summarize(Total_Predicted_Points = sum(Predicted_Points),
            Appearances = n())

predicted_results <- predicted_results %>%
  mutate(Normalized_Predicted_Points = Total_Predicted_Points / Appearances)


print(predicted_results)



