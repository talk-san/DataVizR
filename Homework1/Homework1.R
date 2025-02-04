library(dplyr)
library(lubridate)

# --- Part 1 ---

# 1. Load the dataset. Check the first 5 rows.
df <- read.csv("crime_data.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))
cat("First 5 rows:\n")
print(head(df, 5))

# 2. Identify columns with missing values and their respective counts. Drop columns where more than
# 50% of the data is missing (store this version as a new dataset).
missing_counts <- sapply(df, function(x) sum(is.na(x)))
cat("\nMissing values count per column:\n")
print(missing_counts)

threshold <- nrow(df) * 0.5
cols_to_drop <- names(missing_counts[missing_counts > threshold])
cat("\nColumns to drop (more than 50% missing):", paste(cols_to_drop, collapse = ", "), "\n")

df_clean <- df %>% select(-all_of(cols_to_drop))
cat("\nData after dropping columns with >50% missing values:\n")
print(head(df_clean, 5))

# 3. Convert the DATE OCC column to a datetime format. Extract the year, month, and day into separate
# columns. Create a new column for the hour using the TIME OCC column.
df_clean <- df_clean %>%
  mutate(
    `DATE.OCC` = as.POSIXct(`DATE.OCC`, format = "%m/%d/%Y %I:%M:%S %p"),
    Year = year(`DATE.OCC`),
    Month = month(`DATE.OCC`),
    Day = day(`DATE.OCC`),
    `TIME.OCC` = sprintf("%04d", as.numeric(`TIME.OCC`)),
    Hour = as.integer(substr(`TIME.OCC`, 1, 2))
  )
cat("\nAfter date and time processing:\n")
print(head(df_clean, 5))

# 4. Filter the dataset for crimes that occurred in 2023. Further filter crimes with the description
# BURGLARY in the Crm Cd Desc column
df_2023_burglary <- df_clean %>%
  filter(
    Year == 2023,
    grepl("BURGLARY", `Crm.Cd.Desc`, ignore.case = TRUE)
  )
cat("\nFiltered dataset (2023 & 'BURGLARY'):\n")
print(head(df_2023_burglary, 5))

# 5. Group the data by AREA NAME and calculate the total number of crimes and the average victim age.
# Sort the results by total crimes in descending order
grouped_area <- df_clean %>%
  group_by(`AREA.NAME`) %>%
  summarise(
    Total_Crimes = n(),
    Avg_Victim_Age = mean(`Vict.Age`, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Crimes))
cat("\nGrouped by AREA NAME (total crimes and average victim age):\n")
print(grouped_area)

# --- Part 3 (R Only) ---

# Creating another df for this step as the df_clean drops Weapons.Used.Cd
df_date <- df %>%
  mutate(
    DATE.OCC = as.POSIXct(DATE.OCC, format = "%m/%d/%Y %I:%M:%S %p"),
    Month = month(DATE.OCC)
  )

# 1. Group the data by Month and count the number of crimes.
crimes_by_month <- df_date %>%
  group_by(Month) %>%
  summarise(Count = n()) %>%
  arrange(Month)  

cat("\nNumber of crimes by Month:\n")
print(crimes_by_month)

# 2. Count the number of crimes where a weapon was used (Weapon Used Cd is not null).
weapon_crimes_count <- df_date %>%
  filter(!is.na(Weapon.Used.Cd)) %>%  # using the dot notation if check.names = TRUE
  summarise(Count = n())

cat("\nNumber of crimes where a weapon was used:\n")
print(weapon_crimes_count)

# 3. Group the data by Premis Desc and count the number of crimes.
crimes_by_premis <- df_date %>%
  group_by(Premis.Desc) %>%  # if spaces were converted, "Premis Desc" becomes "Premis.Desc"
  summarise(Count = n()) %>%
  arrange(desc(Count))

cat("\nNumber of crimes by Premis Desc:\n")
print(crimes_by_premis)


# --- Part 4 (Python and R) ---

# 1. Create a new column, Severity Score, based on the following rules:
# • Assign 5 points if a weapon was used (Weapon Used Cd is not null).
# • Assign 3 points for crimes under BURGLARY.
# • Assign 1 point for all other crimes.
# • Group by AREA NAME and find the total severity score for each area.
df <- df %>%
  mutate(
    `Severity.Score` = ifelse(
      !is.na(`Weapon.Used.Cd`),
      5,
      ifelse(
        grepl("BURGLARY", `Crm.Cd.Desc`, ignore.case = TRUE),
        3,
        1
      )
    )
  )

grouped_severity <- df %>%
  group_by(`AREA.NAME`) %>%
  summarise(`Total Severity Score` = sum(`Severity.Score`, na.rm = TRUE))
cat("\nTotal Severity Score by AREA NAME:\n")
print(grouped_severity)
