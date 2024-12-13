# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(cluster)
library(randomForest)


# Load the dataset
file_path <- "/Users/zohaahmed/Dropbox/Mac/Documents/GitHub/DataAnalytics/UnicefAnalysisProject/Cleaned_Child_Health_Matched_PDF.csv"
data <- read.csv(file_path)

# Remove empty rows and assign corrected column names
data_cleaned <- data %>%
  filter(Countries.and.areas != "" & !is.na(Countries.and.areas))

# Assign the corrected column names
colnames(data_cleaned) <- c("Country", "Coverage_BCG", "Coverage_DTP1", "Coverage_DTP3",
                            "Coverage_Polio3", "Coverage_MCV1", "Coverage_MCV2",
                            "Coverage_HepB3", "Coverage_Hib3", "Coverage_Rota",
                            "Coverage_PCV3", "Coverage_PAB_Tetanus", "ARI_Seeking",
                            "Diarrhoea_ORS", "Fever_Care", "ITN_Children", "ITN_Households")

# Convert columns to numeric by replacing '-' with NA
data_cleaned <- data_cleaned %>%
  mutate(
    Coverage_BCG = as.numeric(gsub("-", NA, Coverage_BCG)),
    Coverage_DTP1 = as.numeric(gsub("-", NA, Coverage_DTP1)),
    Coverage_DTP3 = as.numeric(gsub("-", NA, Coverage_DTP3)),
    Coverage_Polio3 = as.numeric(gsub("-", NA, Coverage_Polio3)),
    Coverage_MCV1 = as.numeric(gsub("-", NA, Coverage_MCV1)),
    Coverage_MCV2 = as.numeric(gsub("-", NA, Coverage_MCV2)),
    Coverage_HepB3 = as.numeric(gsub("-", NA, Coverage_HepB3)),
    Coverage_Hib3 = as.numeric(gsub("-", NA, Coverage_Hib3)),
    Coverage_Rota = as.numeric(gsub("-", NA, Coverage_Rota)),
    Coverage_PCV3 = as.numeric(gsub("-", NA, Coverage_PCV3)),
    Coverage_PAB_Tetanus = as.numeric(gsub("-", NA, Coverage_PAB_Tetanus)),
    ARI_Seeking = as.numeric(gsub("-", NA, ARI_Seeking)),
    Diarrhoea_ORS = as.numeric(gsub("-", NA, Diarrhoea_ORS)),
    Fever_Care = as.numeric(gsub("-", NA, Fever_Care)),
    ITN_Children = as.numeric(gsub("-", NA, ITN_Children)),
    ITN_Households = as.numeric(gsub("-", NA, ITN_Households))
  )

# Check if the data structure is now correct
str(data)

# Save the cleaned dataset
write.csv(data_cleaned, "/Users/zohaahmed/Dropbox/Mac/Documents/GitHub/DataAnalytics/UnicefAnalysisProject/Cleaned_Child_Health_Replaced.csv", row.names = FALSE)


# Descriptive Statistics
# Compute summary statistics for all numeric columns
descriptive_stats <- data_cleaned %>%
  summarise(across(where(is.numeric), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE)
  )))

# Print descriptive statistics
print("Descriptive Statistics:")
print(descriptive_stats)



# Step 4: Visualize the Correlation Matrix
library(corrplot)

# Compute correlation matrix
correlation_matrix <- data_cleaned %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

# Visualize the correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         title = "Correlation Matrix", mar = c(0, 0, 2, 0))

# Identify highly correlated pairs (threshold: 0.8)
high_correlations <- as.data.frame(as.table(correlation_matrix)) %>%
  filter(Var1 != Var2 & abs(Freq) > 0.8) %>%
  arrange(desc(abs(Freq)))

# Print highly correlated pairs
print("Highly Correlated Pairs:")
print(high_correlations)

# Load necessary libraries
library(ggplot2)
library(tidyr)

# Transform the dataset into a long format for faceting
data_long <- data_cleaned %>%
  select(where(is.numeric)) %>%  # Select only numeric columns
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  )

# Create the multi-variable histogram plot
ggplot(data_long, aes(x = Value, fill = Variable)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free", ncol = 4) +  # Arrange in a grid
  labs(
    title = "Histograms of Variables",
    x = "Value",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 10)
  ) +
  scale_fill_manual(values = rainbow(length(unique(data_long$Variable))))  # Unique colors


# Select key columns for interventions
key_columns <- c("Coverage_BCG", "Coverage_DTP1", "Coverage_DTP3", "Coverage_Polio3", 
                 "Coverage_MCV1", "Coverage_MCV2", "Coverage_HepB3", "Coverage_Hib3", 
                 "Coverage_Rota", "Coverage_PCV3")

# Filter out rows with missing values in key columns
data_filtered <- data_cleaned %>% 
  drop_na(all_of(key_columns))

# Transform the dataset into long format for ggplot
data_melted <- data_filtered %>% 
  pivot_longer(cols = all_of(key_columns), 
               names_to = "Intervention", 
               values_to = "Coverage")

# Create the boxplot
ggplot(data_melted, aes(x = Intervention, y = Coverage, fill = Intervention)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(
    title = "Variability of Key Interventions Across Regions",
    x = "Intervention",
    y = "Coverage (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette = "Set2")

# Linear Regression: Predict Coverage_BCG using other variables
linear_model <- lm(Coverage_BCG ~ ., data = data_cleaned %>% select(-Country))
summary(linear_model)

# Residuals plot
ggplot(data = data.frame(Fitted = linear_model$fitted.values, Residuals = linear_model$residuals), aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Fit the linear regression model
linear_model <- lm(Coverage_BCG ~ ., data = data_cleaned %>% select(-Country))

# Summary of the model
model_summary <- summary(linear_model)

# Extract R-squared and Adjusted R-squared
r_squared <- model_summary$r.squared
adj_r_squared <- model_summary$adj.r.squared

# Extract other key statistics
f_statistic <- model_summary$fstatistic[1]  # F-statistic value
p_value <- pf(f_statistic, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)  # P-value
coefficients <- model_summary$coefficients  # Coefficients with p-values

# Print the statistics
cat("R-squared:", r_squared, "\n")
cat("Adjusted R-squared:", adj_r_squared, "\n")
cat("F-statistic:", f_statistic, "\n")
cat("P-value for the F-statistic:", p_value, "\n")
cat("Coefficients:\n")
print(coefficients)

# Step 1: Ensure all columns are numeric
cluster_data <- cluster_data %>%
  mutate(across(everything(), as.numeric))

# Verify structure to confirm all columns are numeric
str(cluster_data)

# Step 2: Handle Missing Values (if not already done)
cluster_data <- cluster_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Step 3: Perform PCA
pca_result <- prcomp(cluster_data, scale. = TRUE)

# Step 4: Add PCA Results to Dataset for Visualization
pca_data <- as.data.frame(pca_result$x)
pca_data$Cluster <- data_imputed$Cluster

# Step 5: Visualize Clusters
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "K-means Clustering (PC1 vs PC2)", 
    x = "Principal Component 1", 
    y = "Principal Component 2"
  )

# Remove non-predictor columns like "Country"
data <- data[ , !(names(data) %in% c("Country"))]

# Convert the target variable to numeric
data[[target_column]] <- as.numeric(data[[target_column]])

# Ensure no missing values in the entire dataset
data <- na.omit(data)
# Remove non-numeric or non-factor columns to prevent errors
data <- data[sapply(data, is.numeric)]
# Set up cross-validation
control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation
# Train Random Forest model
set.seed(123)  # For reproducibility
rf_model <- train(
      Coverage_BCG ~ .,
      data = data,
      method = "rf",
     trControl = control
  )
# Output the results
print(rf_model)




