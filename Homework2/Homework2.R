library(ggplot2)
library(ggthemes)

lung_df <- read.csv("lung_cancer_prediction_dataset.csv")
print(head(lung_df))

# 1. Create a Boxplot of Lung Cancer Deaths Distribution. 
ggplot(lung_df, aes(x = factor(0), y = Annual_Lung_Cancer_Deaths)) +
  geom_boxplot() +
  labs(title = "Distribution of Annual Lung Cancer Deaths Across Countries",
       x = "All Countries",
       y = "Annual Deaths per Country")

# 3. Create a Density Plot of the Lung Cancer Mortality Rate.
ggplot(lung_df, aes(x = Mortality_Rate)) +
  geom_density() +
  labs(title = "Density Plot of Lung Cancer Mortality Rate",
       x = "Mortality Rate",
       y = "Density")

global_df <- read.csv("global_air_pollution_dataset.csv")
cat("Global Air Pollution Overview:\n")
print(head(global_df))

# 2. Create a Histogram of PM2.5 AQI Values.
ggplot(global_df, aes(x = PM2.5_AQI_Value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of PM2.5 AQI Values",
       x = "PM2.5 AQI Value",
       y = "Frequency")

# 4. Create a Scatter Plot by generating 100 random values from both the normal and logistic
# distributions. The points should be brown and use theme_solarized with argument light
# set to false. (R, not related to the datasets provided)

normal_values <- rnorm(100, mean = 0, sd = 1)
logistic_values <- rlogis(100, location = 0, scale = 1)

data <- data.frame(Normal = normal_values, Logistic = logistic_values)

ggplot(data, aes(x = Normal, y = Logistic)) +
  geom_point(color = "brown", size = 2) +
  labs(title = "Scatter Plot of Normal vs Logistic Distribution",
       x = "Normal Distribution",
       y = "Logistic Distribution") +
  theme_solarized(light = FALSE)

# Part 4: Recreate the following graphs

# 2. Use the gpplot2 package for this graph. (Hint: Aggregate the data then merge the two
# datasets. Use only the necessary columns.)

library(ggrepel)
library(dplyr)

lung_agg <- lung_df %>%
  group_by(Country) %>%
  summarise(Annual_Lung_Cancer_Deaths = sum(Annual_Lung_Cancer_Deaths, na.rm = TRUE))

pollution_agg <- global_df %>%
  group_by(Country) %>%
  summarise(PM2.5_AQI_Value = mean(PM2.5_AQI_Value, na.rm = TRUE))

merged_df <- merge(lung_agg, pollution_agg, by = "Country")

ggplot(merged_df, aes(x = PM2.5_AQI_Value, 
                      y = Annual_Lung_Cancer_Deaths, 
                      color = Country)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = Country), size = 3, show.legend = FALSE) +
  labs(title = "PM2.5 AQI vs. Annual Lung Cancer Deaths",
       x = "PM2.5 AQI Value",
       y = "Annual Lung Cancer Deaths") +
       theme_minimal() # For prettier output

# 3. Use the ggplot2 package for this graph. (Hint: use geom_jitter since y axis contains
# categorical data, also use the following colors: #5469f1 , #d554f1)

lung_df$Cancer_Stage <- factor(lung_df$Cancer_Stage, levels = c("Stage 1", "Stage 2", "Stage 3", "Stage 4"))

# NOTE: 
# For some reason my dataset does not have stage 4 cancer so stage 4 shows NA? 
# I am using the file from moodle
ggplot(lung_df, aes(x = Years_of_Smoking, y = Cancer_Stage, color = Gender, shape = Gender)) +
  geom_jitter(alpha = 0.6, size = 1.5) +
  scale_color_manual(values = c("Female" = "#d554f1", "Male" = "#5469f1")) + 
  scale_shape_manual(values = c("Female" = 17, "Male" = 16)) +
  facet_wrap(~Gender) +
  labs(title = "Lung Cancer Stage vs. Smoking Years",
       subtitle = "Comparison by Gender",
       x = "Years of Smoking",
       y = "Cancer Stage",
       color = "Gender",
       shape = "Gender")

# 4. Use the ggplot2 package for this graph. (Hint: use scale_fill_viridis_d(option = "plasma"
# to get the same colors)

selected_countries <- c("Brazil", "Germany", "India", "Italy", "Russian Federation", "United States of America")

filtered_data <- global_df %>%
  filter(Country %in% selected_countries)

ggplot(filtered_data, aes(x = PM2.5_AQI_Value, fill = Country)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.8) +
  scale_fill_viridis_d(option = "plasma") +
  facet_wrap(~ Country, scales = "free_y") +
  labs(title = "PM2.5 AQI Distribution Across Countries",
       subtitle = "Comparison of Air Pollution Levels",
       x = "PM2.5 AQI Value",
       y = "Frequency",
       fill = "Country")

