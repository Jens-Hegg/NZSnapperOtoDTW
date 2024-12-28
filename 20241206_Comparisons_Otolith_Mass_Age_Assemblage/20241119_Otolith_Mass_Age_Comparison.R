# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(emmeans)
library(car)

# Load and prepare the data
data <- X202241119_DTW_Otolith_Samples_Age  # Replace with your dataset name

# Convert relevant columns to numeric
data$`Weight [g]` <- as.numeric(data$`Weight [g]`)
data$`Age for Analysis [years]` <- as.numeric(data$`Age for Analysis [years]`)

# Ensure 'Epoch' is a factor
data$Epoch <- as.factor(data$Epoch)

# Reorder the Epoch factor based on chronological order
data$Epoch <- factor(data$Epoch, levels = c(
  "Northland Tauranga Bay 13th - 14th Century",
  "Hauraki Gulf Otata Island 14th Century",
  "Hauraki Gulf Long Bay 15th Century",
  "Hauraki Gulf Otata Island 15th Century",
  "Hauraki Gulf Otata Island 17th - 18th Century",
  "Hauraki Gulf MPI collection 20th Century",
  "Hauraki Gulf 21st Century",
  "Northland Doubtless Bay 21st Century"
))

# Remove rows with missing or non-finite values and check for NAs
plot_data <- data %>%
  filter(!is.na(`Weight [g]`) & !is.na(`Age for Analysis [years]`) & !is.na(Epoch))

# Plot data to visualize any patterns or potential issues
plot <- ggplot(plot_data, aes(x = `Age for Analysis [years]`, y = `Weight [g]`, color = Epoch)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "",
    x = "Age [years]",
    y = "Otolith Mass [g]"
  ) +
  theme_minimal()

print(plot)

# Highlight any rows with NA values
na_rows <- data[is.na(data$`Weight [g]`) | is.na(data$`Age for Analysis [years]`) | is.na(data$Epoch), ]
if (nrow(na_rows) > 0) {
  cat("Rows with NA values:\n")
  print(na_rows)
}

# Remove rows with NAs
data <- data %>%
  drop_na(`Weight [g]`, `Age for Analysis [years]`, Epoch)

# Fit the ANCOVA model
ancova_model <- aov(`Weight [g]` ~ `Age for Analysis [years]` * Epoch, data = data)

# Check for rows excluded by ANCOVA
excluded_rows <- data[is.na(predict(ancova_model, newdata = data)), ]
if (nrow(excluded_rows) > 0) {
  cat("Rows excluded by the ANCOVA model:\n")
  print(excluded_rows)
}

# Refit the ANCOVA model after removing excluded rows
data <- data[!is.na(predict(ancova_model, newdata = data)), ]
ancova_model <- aov(`Weight [g]` ~ `Age for Analysis [years]` * Epoch, data = data)

# Pairwise comparison of slopes using emmeans
emm_slopes <- emmeans(ancova_model, specs = pairwise ~ Epoch | `Age for Analysis [years]`)

# View pairwise comparisons for slopes
print(emm_slopes$contrasts)

# Convert the contrasts to a data frame
contrast_table <- as.data.frame(emm_slopes$contrasts)

# Write the data frame to a CSV file
write.csv(contrast_table, "Pairwise_Slope_Comparisons.csv", row.names = FALSE)

## Checking ANCOVA assumptions ##

# Set up a 2x2 plot layout
par(mfrow = c(2, 2))

# Plot 1: Histogram of residuals
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals", col = "skyblue", border = "white")

# Plot 2: Q-Q plot of residuals
qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red")

# Plot 3: Residuals vs fitted values
plot(fitted_values, residuals,
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 19, col = "blue")
abline(h = 0, col = "red")

# Plot 4: Scatter plot of Age vs Weight with linear fit
plot(data$`Age for Analysis [years]`, data$`Weight [g]`,
     main = "Scatter Plot with Linear Fit",
     xlab = "Age [years]", ylab = "Weight [g]",
     pch = 19, col = "darkgreen")
abline(lm(data$`Weight [g]` ~ data$`Age for Analysis [years]`), col = "red")

# Reset plot layout to default
par(mfrow = c(1, 1))

