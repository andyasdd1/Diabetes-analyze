library(dplyr)
library(readr)
library(ggplot2)
library(GGally)
library(readr)
library(broom)

# Read the CSV file
df <- read_csv("diabetes.csv")

# Ensure 'Outcome' is a factor (for logistic regression)
df$Outcome <- factor(df$Outcome)

# Create the logistic regression model
model <- glm(Outcome ~ Age + DiabetesPedigreeFunction + BMI + Insulin +
               SkinThickness + BloodPressure + Glucose + Pregnancies, 
             family = binomial, data = df)

# Summary of the model
summary(model)

plot(model)

# Exclude the 'Outcome' variable from the correlation matrix calculation
numeric_data <- select(df, -Outcome)

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data)

# Print the correlation matrix
print(correlation_matrix)

# Using GGally::ggpairs to create the scatter plot matrix
Scatter_Matrix <- ggpairs(df)

# Saving the scatter plot matrix to a file
ggsave("Scatter_plot_matrix.png", Scatter_Matrix, width = 30, height = 30, units = "in") 

# Display the scatter plot matrix
Scatter_Matrix


# Summary of the model
model_summary <- summary(model)

# Extract coefficients
coefficients <- model_summary$coefficients

# Extracting p-values and variable names
p_values <- coefficients[, "Pr(>|z|)"]
variable_names <- rownames(coefficients)

# Create a data frame for p-values
p_value_df <- data.frame(Variable = variable_names, P_Value = p_values)

# Print the p-value comparison table
print(p_value_df)

# Reduced Model: Example with only the intercept (null model)
reduced_model <- glm(Outcome ~ 1, family = binomial, data = df)

# Perform Likelihood Ratio Test
anova_test <- anova(reduced_model, model, test = "Chisq")

# Print the result of the ANOVA test
print(anova_test)

# Extract coefficients and convert them to odds ratios
odds_ratios <- exp(coefficients[, "Estimate"])

# Create a data frame for coefficients, odds ratios, and p-values
results_df <- data.frame(Variable = variable_names, 
                         Coefficient = coefficients[, "Estimate"], 
                         Odds_Ratio = odds_ratios, 
                         P_Value = p_values)

# Print the results table
print(results_df)

library(car)  # for vif() function

# Calculate VIF
vif_model <- vif(model)
print(vif_model)

library(pROC)
roc_response <- roc(df$Outcome, fitted(model))
plot(roc_response)
print(auc(roc_response))

# Function to perform logistic regressions with interaction terms for all pairs of variables
run_pairwise_interactions_logit <- function(data, outcome_var, predictor_vars) {
  results <- list()
  
  # Create all unique pairs of predictor variables
  for (i in 1:(length(predictor_vars)-1)) {
    for (j in (i+1):length(predictor_vars)) {
      predictor1 <- predictor_vars[i]
      predictor2 <- predictor_vars[j]
      
      # Create a formula with the interaction term between the pair of predictors
      formula <- as.formula(paste(outcome_var, "~", predictor1, "*", predictor2))
      model <- glm(formula, family = binomial, data = data)
      tidy_model <- tidy(model)
      
      # Extract the interaction term row
      interaction_term <- paste(predictor1, ":", predictor2, sep="")
      interaction_result <- tidy_model %>%
        dplyr::filter(term == interaction_term)
      
      # Store the result with a name representing the interaction
      results_name <- paste(predictor1, "_x_", predictor2, sep = "")
      results[[results_name]] <- interaction_result
    }
  }
  
  return(results)
}

# List of predictor variables
predictor_vars <- setdiff(names(df), "Outcome")

# Run logistic regressions for all pairs of variables
pairwise_interaction_results <- run_pairwise_interactions_logit(df, "Outcome", predictor_vars)

# Print the results for interaction terms
pairwise_interaction_results




# Remove rows where BloodPressure, SkinThickness, or Insulin are 0
df_filtered <- df %>%
  filter(BloodPressure != 0, SkinThickness != 0, Insulin != 0)

# Create the logistic regression model with interaction terms
model_filtered2 <- glm(Outcome ~ Age + DiabetesPedigreeFunction + BMI + Insulin +
                         SkinThickness + BloodPressure + Glucose + Pregnancies,
                       family = binomial, data = df_filtered)

#Insulin:DiabetesPedigreeFunction + Insulin:BMI +
  #SkinThickness:Age + BloodPressure:BMI +
 # Glucose:DiabetesPedigreeFunction +
  #Glucose:Insulin, 

# Calculate Cook's distance for the model
cooks_distance <- cooks.distance(model_filtered2)

# Determine a threshold for Cook's distance
#threshold <- 4/(nrow(df_filtered)-(length(coef(model_filtered2))-1))
threshold <- quantile(cooks_distance, 0.95)

# Identify the row numbers that are above the threshold
outliers <- which(cooks_distance > threshold)

# Exclude these rows from your data frame
df_filtered_no_outliers <- df_filtered[-outliers, ]

# Refit the model with the new filtered data without outliers
model_filtered_no_outliers <- update(model_filtered2, data = df_filtered_no_outliers)

# Plot the new model's Cook's distance
plot(cooks.distance(model_filtered_no_outliers), pch="*", cex=2, main="Cook's distance for Model without Outliers")
abline(h = threshold, col="red")

# List of predictor variables
predictor_vars <- setdiff(names(df_filtered_no_outliers), "Outcome")

# Run logistic regressions for all pairs of variables
pairwise_interaction_results <- run_pairwise_interactions_logit(df_filtered_no_outliers, "Outcome", predictor_vars)

# Print the results for interaction terms
pairwise_interaction_results

                    

df_filtered <- df %>%
  filter(BloodPressure != 0, SkinThickness != 0, Insulin != 0)

# Add the interaction term to the dataset
df_filtered$SkinThickness_BMI <- df_filtered$SkinThickness * df_filtered$BMI

# Create the logistic regression model with interaction terms
model_filtered2 <- glm(Outcome ~ Age + DiabetesPedigreeFunction + BMI + Insulin +
                         SkinThickness + BloodPressure + Glucose + Pregnancies + 
                         SkinThickness:BMI, # Include the interaction term in the model
                       family = binomial, data = df_filtered)

df_filtered_no_outliers$SkinThickness_BMI <- df_filtered_no_outliers$SkinThickness * df_filtered_no_outliers$BMI

# Refit the model with the new filtered data without outliers
model_filtered_no_outliers <- update(model_filtered2, data = df_filtered_no_outliers)

plot(model_filtered_no_outliers)

# Calculate VIF for the new model
vif_model_no_outliers <- vif(model_filtered_no_outliers)
print(vif_model_no_outliers)

# ROC and AUC for the new model
roc_response_no_outliers <- roc(df_filtered_no_outliers$Outcome, fitted(model_filtered_no_outliers))
plot(roc_response_no_outliers)
print(auc(roc_response_no_outliers))

# Get summary of the new model
model_summary_no_outliers <- summary(model_filtered_no_outliers)
print(model_summary_no_outliers)

# Extract coefficients, p-values, and calculate odds ratios for the new model
coefficients_no_outliers <- model_summary_no_outliers$coefficients
p_values_no_outliers <- coefficients_no_outliers[, "Pr(>|z|)"]
variable_names_no_outliers <- rownames(coefficients_no_outliers)
odds_ratios_no_outliers <- exp(coefficients_no_outliers[, "Estimate"])

# Create a data frame for coefficients, odds ratios, and p-values for the new model
results_df_no_outliers <- data.frame(Variable = variable_names_no_outliers, 
                                     Coefficient = coefficients_no_outliers[, "Estimate"], 
                                     Odds_Ratio = odds_ratios_no_outliers, 
                                     P_Value = p_values_no_outliers)

# Print the results table for the new model
print(results_df_no_outliers)

