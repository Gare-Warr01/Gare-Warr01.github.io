# Load libraries
library(readr)
library(dplyr)

# Read the raw WHO data
who_data <- read_csv('./Data/WHOData.csv')

# Select relevant columns
clean_who_data <- who_data %>%
  select(WHOREGION, FLUSEASON, COUNTRY_AREA_TERRITORY, ISO_WEEKSTARTDATE, ISO_YEAR,
         INF_A, INF_B, INF_ALL, INF_NEGATIVE, ILI_ACTIVITY) %>%
  # Remove rows with all case data missing
  filter(!(is.na(INF_A) & is.na(INF_B) & is.na(INF_ALL) & is.na(INF_NEGATIVE) & is.na(ILI_ACTIVITY)))

# Optional: View cleaned data
View(clean_who_data)
summary(clean_who_data)

# Save cleaned data
write_csv(clean_who_data, "data/clean_who_influenza_data.csv")
