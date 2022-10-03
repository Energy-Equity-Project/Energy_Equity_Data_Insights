
library(tidyverse)

rm(list = ls())

# File paths
datadir <- "data"
outdir <- "data_insight_3"

# Reading in EEP data
df <- read.csv(file.path(datadir, "eep_final_data.csv"))

df <- df %>%
  # Format GEOID as text and 0 padding
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(GEOID = case_when(
    str_length(GEOID) == 10 ~ paste("0", GEOID, sep = ""),
    TRUE ~ GEOID
  ))

df <- df %>%
  # Adding energy burden classification
  mutate(energy_burden_rating = case_when(
    energy_burden > 15 ~ ">15% (Extreme)",
    energy_burden >= 12 & energy_burden <= 15 ~ "12 to 15% (Severe)",
    energy_burden >= 9 & energy_burden < 12 ~ "9 to 12% (Very High)",
    energy_burden >= 6 & energy_burden < 9 ~ "6 to 9% (High)",
    energy_burden >= 3 & energy_burden < 6 ~ "3 to 6% (Affordable)",
    energy_burden < 3 ~ "<3% (Cheap)"
  )) %>%
  # Adding majority BIPOC classification
  mutate(demographics = case_when(
    bipoc_percent >= 50 ~ "People of Color",
    TRUE ~ "White"
  ))

# Creating an ordering for energy burden
df$energy_burden_rating <- factor(df$energy_burden_rating,
                                  c("<3% (Cheap)", "3 to 6% (Affordable)", "6 to 9% (High)", "9 to 12% (Very High)", "12 to 15% (Severe)", ">15% (Extreme)"))


df %>%
  filter(!is.na(single_parent_households),
         !is.na(energy_burden)) %>%
  ggplot(aes(x = factor(energy_burden_rating), y = single_parent_households)) +
  geom_boxplot(outlier.shape = NA, color = "#347FC4", fill = "#347FC4", alpha = 0.1) +
  ylim(0, 50) +
  theme_bw() +
  labs(x = "Energy Burden", y = "Single Parent Households (% of total households)")


df %>%
  filter(!is.na(single_parent_households),
         !is.na(energy_burden)) %>%
  ggplot(aes(x = factor(energy_burden_rating), y = single_parent_households, fill = demographics)) +
  geom_boxplot(outlier.shape = NA)

df %>%
  filter(!is.na(single_parent_households),
         !is.na(demographics)) %>%
  ggplot(aes(x = factor(demographics), y = single_parent_households)) +
  geom_boxplot(outlier.shape = NA)

df <- df %>%
  # Adding home energy affordability gap classification
  mutate(energy_affordability_gap_category = case_when(
    avg_shortfall_usd >= 0 & avg_shortfall_usd <= 1000 ~ "0 - $1k",
    avg_shortfall_usd > 1000 & avg_shortfall_usd <= 2000 ~ "1k - $2k",
    avg_shortfall_usd > 2000 & avg_shortfall_usd <= 3000 ~ "2k - $3k",
    avg_shortfall_usd > 3000 ~ "> $3k"
  ))

# Creating an ordering for energy burden and Majority BIPOC
df$energy_affordability_gap_category <- factor(df$energy_affordability_gap_category,
                                  c("0 - $1k", "1k - $2k", "2k - $3k", "> $3k"))


df %>%
  filter(!is.na(single_parent_households),
         !is.na(avg_shortfall_usd)) %>%
  ggplot(aes(x = factor(energy_affordability_gap_category), y = single_parent_households)) +
  geom_boxplot()
