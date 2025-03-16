library(dplyr)
library(tidyr)
library(ggplot2)

#############################################
# Setup and Conversion
# This is an equivalent of the Python Script
# Please find comments and explanations in the Python version 
# under https://github.com/talk-san/DataViz/blob/master/Homework%203/homework3.ipynb
#############################################
df <- read.csv("mobiles_dataset.csv", stringsAsFactors = FALSE)

head(df)

# Convert currencies
conversion_rates <- c(
  "Launched.Price.Pakistan.PKR" = 0.0036,
  "Launched.Price.India.INR"    = 0.011,
  "Launched.Price.China.CNY"    = 0.14,
  "Launched.Price.Dubai.AED"    = 0.27
)

df$Launched.Price.Pakistan.PKR <- df$Launched.Price.Pakistan.PKR * conversion_rates["Launched.Price.Pakistan.PKR"]
df$Launched.Price.India.INR    <- df$Launched.Price.India.INR    * conversion_rates["Launched.Price.India.INR"]
df$Launched.Price.China.CNY    <- df$Launched.Price.China.CNY    * conversion_rates["Launched.Price.China.CNY"]
df$Launched.Price.Dubai.AED    <- df$Launched.Price.Dubai.AED    * conversion_rates["Launched.Price.Dubai.AED"]

colnames(df)[colnames(df) == "Launched.Price.Pakistan.PKR"] <- "Launched.Price.Pakistan.USD"
colnames(df)[colnames(df) == "Launched.Price.India.INR"]    <- "Launched.Price.India.USD"
colnames(df)[colnames(df) == "Launched.Price.China.CNY"]    <- "Launched.Price.China.USD"
colnames(df)[colnames(df) == "Launched.Price.Dubai.AED"]    <- "Launched.Price.Dubai.USD"

df_avg_brand <- df %>%
  group_by(Company.Name) %>%
  summarise(
    across(
      c(Launched.Price.Pakistan.USD,
        Launched.Price.India.USD,
        Launched.Price.China.USD,
        Launched.Price.Dubai.USD),
      mean,
      na.rm = TRUE
    )
  )

print(df_avg_brand)

#############################################
# Part 1: Analytical Questions
#############################################

# 1. Does battery capacity influence the launched price of a smartphone? Check this variability
# across all currencies. Is there any type of difference between behaviors?
price_columns <- c(
  "Launched.Price.Pakistan.USD",
  "Launched.Price.India.USD",
  "Launched.Price.China.USD",
  "Launched.Price.USA.USD",
  "Launched.Price.Dubai.USD"
)
  

df$Average.Launched.Price.USD <- rowMeans(df[price_columns], na.rm = TRUE)

correlation_value <- cor(
  df$Battery.Capacity.mAh,
  df$Average.Launched.Price.USD,
  use = "complete.obs"
)
cat("Correlation between Battery Capacity and Average Launched Price (USD):", 
    round(correlation_value, 4), "\n")

ggplot(df, aes(x = Battery.Capacity.mAh, y = Average.Launched.Price.USD)) +
  geom_point(color = "blue") +
  labs(
    title = "Battery Capacity vs Average Launched Price (USD)",
    x = "Battery Capacity (mAh)",
    y = "Average Launched Price (USD)"
  )

# 2. Does RAM size impact the price of smartphones? Check this variability across all currencies.
# Is there any type of difference between behaviors?
df$RAM_numeric <- as.numeric(gsub("GB", "", df$RAM))

df <- df %>% filter(!is.na(RAM_numeric))

ram_correlation <- cor(
  df$RAM_numeric,
  df$Average.Launched.Price.USD,
  use = "complete.obs"
)
cat("Correlation between RAM size and Average Launched Price (USD):", 
    round(ram_correlation, 4), "\n")

# 3. Do Apple devices have a higher price variation across different regions compared to other
# brands? In which country do Apple devices have the highest markup? Are there brands with
# more stable pricing across regions?
brand_price_variation <- df %>%
  group_by(Company.Name) %>%
  summarise(
    across(all_of(price_columns), sd, na.rm = TRUE)
  )

print(brand_price_variation)

# Apple vs Other Brands
apple_variation <- df %>%
  filter(Company.Name == "Apple") %>%
  summarise(across(all_of(price_columns), sd, na.rm = TRUE))

other_brands_variation <- df %>%
  filter(Company.Name != "Apple") %>%
  summarise(across(all_of(price_columns), sd, na.rm = TRUE))

cat("\nApple Price Variation Across Regions:\n")
print(apple_variation)
cat("\nOther Brands Price Variation Across Regions:\n")
print(other_brands_variation)

# 4. Do all smartphone brands have flagship and budget-friendly models, or do some brands
# only focus on premium devices?
categorize_price <- function(price) {
  if (price < 300) {
    return("Budget")
  } else if (price <= 700) {
    return("Mid-range")
  } else {
    return("Premium")
  }
}

df$Price.Segment <- sapply(df$Average.Launched.Price.USD, categorize_price)

brand_segment_count <- df %>%
  group_by(Company.Name, Price.Segment) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = Price.Segment, values_from = count, values_fill = 0)

print(brand_segment_count)

brand_segment_coverage <- brand_segment_count %>%
  mutate(covered_segments = rowSums(across(where(is.numeric), ~ . > 0)))

brands_with_all_segments <- brand_segment_coverage %>%
  filter(covered_segments == 3) %>%
  pull(Company.Name)

brands_limited_segments <- brand_segment_coverage %>%
  filter(covered_segments < 3) %>%
  pull(Company.Name)

cat("\nBrands covering all three segments:\n")
print(brands_with_all_segments)
cat("\nBrands that do NOT cover all segments:\n")
print(brands_limited_segments)

# 5. Which region offers the most affordable smartphone prices on average? Are there any
# brands that price their phones significantly lower in one region compared to others?

average_prices_by_region <- colMeans(df[price_columns], na.rm = TRUE)
cat("\nAverage smartphone prices per region:\n")
print(average_prices_by_region)

cheapest_region <- names(average_prices_by_region)[which.min(average_prices_by_region)]
cat("\nRegion with the most affordable smartphone prices:", cheapest_region, "\n")

#############################################
# Part 2: Visualization
#############################################

# 1. Plot a bar chart for average price per region in USD.

region_data <- data.frame(
  Region = names(average_prices_by_region),
  Price = as.numeric(average_prices_by_region)
)

ggplot(region_data, aes(x = Region, y = Price)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Average Smartphone Prices by Region (USD)",
    x = "Region",
    y = "Average Price (USD)"
  ) +
  theme_minimal()

# 2. Create a pie chart of the market share of smartphone brands.

brand_counts <- table(df$Company.Name)
brand_df <- data.frame(
  Brand = names(brand_counts),
  Count = as.numeric(brand_counts)
)

ggplot(brand_df, aes(x = "", y = Count, fill = Brand)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Market Share of Smartphone Brands (Based on Number of Models)",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  geom_text(
    aes(label = scales::percent(Count / sum(Count), accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    color = "white"
  )


#############################################
# Part 3: Recreate
#############################################

# 1.
ggplot(df, aes(x = reorder(Company.Name, Launched.Price.USA.USD, FUN=median), 
               y = Launched.Price.USA.USD, 
               fill = Company.Name)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  
  geom_jitter(width = 0.2, alpha = 0.5, size = 1) +  
  theme_minimal() +
  labs(
    title = "Price Distribution by Company in USA",
    subtitle = "A boxplot showing how the price varies by company, with individual data points overlaid",
    x = "Company",
    y = "Price in USD",
    fill = "Company Name"
  )

# 2.

ggplot(df, aes(x = Battery.Capacity.mAh, 
               y = Launched.Price.USA.USD, 
               color = Company.Name, 
               size = Screen.Size.inches)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  scale_color_manual(values = rainbow(length(unique(df$Company.Name)))) +
  labs(
    title = "Battery Capacity vs. Price in USA",
    subtitle = "The relationship between battery capacity, price, and screen size across different smartphone brands",
    x = "Battery Capacity",
    y = "Price",
    color = "Brand",
    size = "Screen Size (inches)"
  )

# 3.

top_brands <- df %>%
  count(Company.Name, sort = TRUE) %>%
  top_n(5, n) %>%
  pull(Company.Name)

df_filtered <- df %>% filter(Company.Name %in% top_brands)

shape_mapping <- c("Apple" = 16, "Honor" = 17, "Oppo" = 15, "Samsung" = 18, "Vivo" = 19)

ggplot(df_filtered, aes(x = Battery.Capacity.mAh, 
                        y = Launched.Price.USA.USD, 
                        shape = Company.Name, 
                        color = Screen.Size.inches)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_shape_manual(values = shape_mapping) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(
    title = "Battery Capacity vs. Price for Top 5 Brands",
    subtitle = "Different Shapes for Each Brand, Color by Screen Size (USA)",
    x = "Battery Capacity (mAh)",
    y = "Price (USD)",
    shape = "Brand",
    color = "Screen Size (inches)"
  )