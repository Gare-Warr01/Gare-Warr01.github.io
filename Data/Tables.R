# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load cleaned data
who_data <- read_csv("data/clean_who_influenza_data.csv")

# Quick look
glimpse(who_data)
summary(who_data)

# Total cases per year
who_data %>%
  group_by(ISO_YEAR) %>%
  summarize(Total_Positive = sum(INF_ALL, na.rm = TRUE),
            Total_Negative = sum(INF_NEGATIVE, na.rm = TRUE),
            Total_ILI = sum(ILI_ACTIVITY, na.rm = TRUE))

# Total cases by region
who_data %>%
  group_by(WHOREGION) %>%
  summarize(Total_Positive = sum(INF_ALL, na.rm = TRUE))

# Total Positive Cases over Years
who_data %>%
  group_by(ISO_YEAR) %>%
  summarize(Total_Positive = sum(INF_ALL, na.rm = TRUE)) %>%
  ggplot(aes(x = ISO_YEAR, y = Total_Positive)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red") +
  labs(title = "Total Influenza Positive Cases Per Year",
       x = "Year", y = "Total Positive Cases") +
  theme_minimal()

# Bar Plot by Region
who_data %>%
  group_by(WHOREGION) %>%
  summarize(Total_Positive = sum(INF_ALL, na.rm = TRUE)) %>%
  ggplot(aes(x = WHOREGION, y = Total_Positive, fill = WHOREGION)) +
  geom_bar(stat = "identity") +
  labs(title = "Influenza Positive Cases by WHO Region",
       x = "Region", y = "Total Positive Cases") +
  theme_minimal() +
  theme(legend.position = "none")

