# Install tidyverse
install.packages("tidyverse")
library(tidyverse)

# Load the data
data <- read_csv("~/Desktop/LiveLongerData.csv")

# Look at the first few rows
head(data)
summary(data)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create a new column with corrected matching order
data <- data %>%
  mutate(
    Gender_Affected = case_when(
      grepl("both but especially for women", `sexes affected`, ignore.case = TRUE) ~ "Esp for Women",  # Check 'both but esp for female' FIRST
      grepl("both but esp for male", `sexes affected`, ignore.case = TRUE) ~ "Esp for Men",       # Then 'both but esp for male'
      grepl("Both, but esp. for men", `sexes affected`, ignore.case = TRUE) ~ "Esp for Men",
      grepl("female", `sexes affected`, ignore.case = TRUE) ~ "Esp for Women",                   # Then 'female'
      grepl("male", `sexes affected`, ignore.case = TRUE) ~ "Esp for Men",                        # Then 'male'
      grepl("both", `sexes affected`, ignore.case = TRUE) ~ "Both",                               # Then 'both'
      TRUE ~ "Unknown"  # Anything else
    )
  )

# Now plot
ggplot(data, aes(x = `Factor`, y = `Years gained / lost`, fill = Gender_Affected)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c(
      "Esp for Men" = "blue",
      "Esp for Women" = "pink",
      "Both" = "green",
      "Unknown" = "gray"
    )
  ) +
  labs(
    title = "Effect of Various Factors on Years Gained / Lost by Gender",
    x = "Factor",
    y = "Years Gained / Lost",
    fill = "Group Affected"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# Load the data
data <- read_csv("~/Desktop/Europe_Mental.csv")

# Look at the first few rows
head(data)
summary(data)

# Clean data and create factors if necessary
data <- data %>%
  mutate(
    Location = factor(location, levels = c("Western Europe", "Central Europe", "Eastern Europe")),
    Sex = factor(sex, levels = c("Male", "Female")),
    Disorder = factor(cause, levels = c("Schizophrenia", "Depressive disorders", "Eating disorders", "Other mental disorders", "Anxiety disorders", "Bipolar disorder")),
    DALY = as.numeric(val),
    Year = as.factor(year)
  )

# Descriptive statistics by region, sex, and mental illness (for all rows)
summary_stats <- data %>%
  group_by(Location, Sex, Disorder) %>%
  summarise(
    mean_DALY = mean(DALY, na.rm = TRUE),
    sd_DALY = sd(DALY, na.rm = TRUE),
    min_DALY = min(DALY, na.rm = TRUE),
    max_DALY = max(DALY, na.rm = TRUE),
    median_DALY = median(DALY, na.rm = TRUE),
    count = n()
  )

print(summary_stats)

library(ggplot2)
library(dplyr)

# Create a new variable that combines Location and Sex
data <- data %>%
  mutate(Location_Sex = paste(Location, Sex, sep = " - "))

# Dot plot with combined shape and color legend
ggplot(data, aes(x = Disorder, y = DALY, color = Location_Sex, shape = Location_Sex)) +
  geom_point(position = position_jitter(width = 0.2), size = 2.5, alpha = 0.8) +
  scale_color_manual(values = c(
    "Western Europe - Male" = "blue",
    "Western Europe - Female" = "pink",
    "Central Europe - Male" = "darkblue",
    "Central Europe - Female" = "deeppink",
    "Eastern Europe - Male" = "navy",
    "Eastern Europe - Female" = "hotpink"
  )) +
  scale_shape_manual(values = c(
    "Western Europe - Male" = 16,    # Circle
    "Western Europe - Female" = 16,  # Circle
    "Central Europe - Male" = 15,    # Square
    "Central Europe - Female" = 15,  # Square
    "Eastern Europe - Male" = 8,     # Star
    "Eastern Europe - Female" = 8    # Star
  )) +
  labs(
    title = "DALYs by Mental Illness, Region, and Sex",
    x = "Mental Illness",
    y = "DALY",
    color = "Region and Sex",
    shape = "Region and Sex"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )


library(ggplot2)
library(dplyr)

# First, fix the data by creating a custom column for Location and Sex combined
data <- data %>%
  mutate(Location_Sex = paste(Location, Sex, sep = " - "))

# Dot plot with logarithmic scale
ggplot(data, aes(x = Disorder, y = DALY, color = Location_Sex, shape = Location_Sex)) +
  geom_point(position = position_jitter(width = 0.1), size = 2.5, alpha = 0.7) +  # Jittered points
  scale_y_log10() +  # Apply log scale to y-axis
  scale_color_manual(values = c(
    "Western Europe - Male" = "blue",
    "Western Europe - Female" = "pink",
    "Central Europe - Male" = "darkblue",
    "Central Europe - Female" = "deeppink",
    "Eastern Europe - Male" = "navy",
    "Eastern Europe - Female" = "hotpink"
  )) +
  scale_shape_manual(values = c(
    "Western Europe - Male" = 16,    # Circle
    "Western Europe - Female" = 16,  # Circle
    "Central Europe - Male" = 15,    # Square
    "Central Europe - Female" = 15,  # Square
    "Eastern Europe - Male" = 8,     # Star
    "Eastern Europe - Female" = 8    # Star
  )) +
  labs(
    title = "DALY by Mental Illness, Region, and Sex (Log Scale)",
    x = "Mental Illness",
    y = "DALY (Log Scale)",
    color = "Region and Sex",
    shape = "Region and Sex"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

library(ggplot2)
library(dplyr)

# Grouped bar chart
ggplot(data, aes(x = Disorder, y = val, fill = Sex)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ Location, scales = "free_x", nrow = 1) +  # Split by Region horizontally
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
  labs(
    title = "DALYs by Mental Illness, Region, and Sex",
    x = "Mental Illness",
    y = "DALYs",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    panel.spacing = unit(2, "lines"),         # Add space between facets
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1), # Black borders around each facet
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )

# Boxplot to compare DALY distributions across regions, sexes, and mental illnesses
ggplot(data, aes(x = Disorder, y = DALY, fill = interaction(Location, Sex))) +  # Use `interaction()` in lowercase
  geom_boxplot() +
  labs(title = "DALY by Mental Illness, Region, and Sex",
       x = "Mental Illness",
       y = "DALY") +
  scale_fill_manual(values = c("Western Europe.Male" = "blue", "Western Europe.Female" = "pink", 
                               "Central Europe.Male" = "blue", "Central Europe.Female" = "pink",
                               "Eastern Europe.Male" = "blue", "Eastern Europe.Female" = "pink")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Two-way ANOVA
anova_result <- aov(DALY ~ Location * Sex, data = data)
summary(anova_result)


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Clean and prepare the data if needed (ensure it's a numeric column for DALY)
data$DALY <- as.numeric(data$DALY)  # In case DALY was factor or character

# Create a new factor to explicitly control the order on the x-axis
data$Location_Sex <- with(data, paste(Location, Sex))

# Convert this new factor to have a custom order
data$Location_Sex <- factor(data$Location_Sex, 
                            levels = c("Western Europe Female", "Western Europe Male", 
                                       "Central Europe Female", "Central Europe Male", 
                                       "Eastern Europe Female", "Eastern Europe Male"))

# Prepare the summary data for plotting
summary_data <- data %>%
  group_by(Location_Sex) %>%
  summarize(
    median_DALY = median(DALY, na.rm = TRUE),
    max_DALY = max(DALY, na.rm = TRUE)  # To position the annotation just above the max value
  )

# Boxplot to compare DALYs by Sex and Region (log-scaled)
ggplot(data, aes(x = Location_Sex, y = DALY, fill = Location_Sex)) +
  geom_boxplot(alpha = 0.7, outlier.size = 2, outlier.colour = "red") +  # Add outliers and transparency
  scale_y_log10() +  # Apply log scale to y-axis
  labs(title = "Distribution of DALYs by Sex and Location (Log Scale)", 
       x = "Sex and Location", 
       y = "DALY (Log Scale)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  # Add annotations for significance (e.g., placing a label at the top of each group)
  geom_text(data = summary_data, aes(x = Location_Sex, 
                                     y = max_DALY * 1.1, 
                                     label = paste("Median:", round(median_DALY, 2))),
            color = "black", size = 3, hjust = 0.5)  # Annotate with the median value

# Assuming 'Year' is a factor and already exists in your data

# Loop over each year and perform the ANOVA for each year separately
years <- unique(data$Year)

# Store results
anova_results <- list()

for (year in years) {
  data_year <- filter(data, Year == year)  # Filter data for each year
  anova_results[[as.character(year)]] <- aov(DALY ~ Location * Sex, data = data_year)
}

summary(anova_results)

for (year in names(anova_results)) {
  cat("\nANOVA summary for year:", year, "\n")
  print(summary(anova_results[[year]]))
}

aov(DALY ~ Year, data = data)

# Two-way ANOVA
anova_result <- aov(DALY ~ Location * Sex, data = data)
summary(anova_result)

# Tukey's HSD for Location
tukey_location <- TukeyHSD(anova_result, "Location")
summary(tukey_location)

# Tukey's HSD for Sex
tukey_sex <- TukeyHSD(anova_result, "Sex")
summary(tukey_sex)

# View Tukey HSD for Location
tukey_location

# View Tukey HSD for Sex
tukey_sex

library(tidyr)

data_wide <- data %>%
  pivot_wider(
    names_from = Disorder,
    values_from = DALY
  )

table(data$Location)
table(data$Sex)

colnames(data_wide)

# Check for missing values
colSums(is.na(data_wide[, c("Schizophrenia", "Depressive disorders", "Bipolar disorder", 
                            "Anxiety disorders", "Eating disorders", "Other mental disorders")]))

# Summarize to get one DALY per Location, Sex, Year, Disorder
data_summary <- data %>%
  group_by(Location, Sex, Year, Disorder) %>%
  summarize(DALY = mean(DALY, na.rm = TRUE)) %>%
  ungroup()

# Now pivot wider
data_wide <- data_summary %>%
  pivot_wider(names_from = Disorder, values_from = DALY)

colSums(is.na(data_wide[, c("Schizophrenia", "Depressive disorders", "Bipolar disorder", 
                            "Anxiety disorders", "Eating disorders", "Other mental disorders")]))

# Create the MANOVA model
manova_model <- manova(cbind(Schizophrenia, `Depressive disorders`, `Bipolar disorder`, 
                             `Anxiety disorders`, `Eating disorders`, `Other mental disorders`) ~ 
                         Location * Sex, data = data_wide)

# Summary using Pillai's Trace (for the MANOVA test)
summary(manova_model, test = "Pillai")

# Reshape data for easier plotting
data_long <- data_wide %>%
  pivot_longer(cols = c("Schizophrenia", "Depressive disorders", "Bipolar disorder", 
                        "Anxiety disorders", "Eating disorders", "Other mental disorders"),
               names_to = "Disorder", values_to = "DALY")

install.packages("plotly")
install.packages("rgl")

library(plotly)
library(rgl)

# Example data (replace with your own data)
data_wide <- data %>%
  mutate(
    Location = factor(location, levels = c("Western Europe", "Central Europe", "Eastern Europe")),
    Sex = factor(sex, levels = c("Male", "Female")),
    Disorder = factor(cause, levels = c("Schizophrenia", "Depressive disorders", "Eating disorders", "Other mental disorders", "Anxiety disorders", "Bipolar disorder")),
    DALY = as.numeric(val)
  )

# Create 3D scatter plot
fig <- plot_ly(data_wide, 
               x = ~Location, 
               y = ~Sex, 
               z = ~DALY, 
               color = ~Disorder, 
               colors = c("blue", "green", "red", "purple", "orange", "brown"),
               type = "scatter3d", 
               mode = "markers", 
               marker = list(size = 5)) 

# Customize layout
fig <- fig %>% layout(
  title = "3D Visualization of DALY by Location, Sex, and Disorder",
  scene = list(
    xaxis = list(title = "Location"),
    yaxis = list(title = "Sex"),
    zaxis = list(title = "DALY")
  )
)

fig


# Get residuals from the MANOVA model
residuals_manova <- residuals(manova_model)
print(residuals_manova)

# PCA on residuals
pca_result <- prcomp(residuals_manova, scale. = TRUE)
print(pca_result)
summary(pca_result)

# Biplot
biplot(pca_result, main = "PCA Biplot of MANOVA Residuals")

# Install ggrepel if not installed
install.packages("ggrepel")

# Load libraries
library(ggplot2)
library(ggrepel)

data_pca <- data_wide %>%
  select(Location, Sex, 
         Schizophrenia, 
         `Depressive disorders`, 
         `Bipolar disorder`,
         `Anxiety disorders`, 
         `Eating disorders`, 
         `Other mental disorders`)

library(tidyr)

data_pca <- data_wide %>%
  select(Location, Sex, Disorder, DALY) %>%
  pivot_wider(
    names_from = Disorder,
    values_from = DALY
  )
library(dplyr)

data_fixed <- data_wide %>%
  group_by(Location, Sex, Disorder) %>%
  summarise(DALY = mean(DALY, na.rm = TRUE), .groups = "drop")

library(tidyr)

data_pca <- data_fixed %>%
  pivot_wider(
    names_from = Disorder,
    values_from = DALY
  )

# Perform PCA
pca_result <- prcomp(data_pca[, c("Schizophrenia", "Depressive disorders", "Bipolar disorder",
                                  "Anxiety disorders", "Eating disorders", "Other mental disorders")],
                     scale. = TRUE)

library(ggplot2)
library(ggrepel)

scores <- as.data.frame(pca_result$x)
scores$Location <- data_pca$Location
scores$Sex <- data_pca$Sex
scores$Group <- paste(scores$Location, scores$Sex)

loadings <- as.data.frame(pca_result$rotation)

# Prettier Biplot: Distinguishing between sample points and disorder vectors
ggplot(scores, aes(x = PC1, y = PC2, color = Group)) +
  geom_point(size = 3, alpha = 0.8) +  # Data points (samples)
  geom_segment(data = loadings, 
               aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5), 
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +  # Disorder arrows
  geom_text_repel(data = loadings, 
                  aes(x = PC1 * 5, y = PC2 * 5, label = rownames(loadings)), 
                  size = 4, color = "black") +  # Labels for mental disorders
  labs(title = "PCA Biplot of Mental Disorders (from MANOVA Results)",
       x = paste0("PC1 (", round(summary(pca_result)$importance[2,1] * 100, 1), "% variance)"),
       y = paste0("PC2 (", round(summary(pca_result)$importance[2,2] * 100, 1), "% variance)"),
       color = "Group") + 
  theme_minimal() + 
  theme(legend.position = "bottom")

set.seed(123)
# Simulate data
group <- factor(rep(c("A", "B"), each = 50))
var1 <- c(rnorm(50, mean = 5), rnorm(50, mean = 6))
var2 <- c(rnorm(50, mean = 7), rnorm(50, mean = 8))
sim_data <- data.frame(group, var1, var2)

# Perform MANOVA
sim_manova <- manova(cbind(var1, var2) ~ group, data = sim_data)
summary(sim_manova)

# Install only if needed:

install.packages("tidyverse")

library(tidyverse)

data_wide <- data %>%
  pivot_wider(names_from = Disorder, values_from = DALY)

manova_model <- manova(cbind(dep1, dep2, dep3) ~ group, data = data_wide)
summary(manova_model, test = "Pillai")

summary.aov(manova_model)

































