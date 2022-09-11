
library(tidyverse)
library(sf)

rm(list = ls())

# File paths
datadir <- "data"
us_map_fp <- file.path(datadir,
                       "cb_2021_us_tract_500k_filtered_simplified",
                       "cb_2021_us_tract_500k_filtered_simplified.shp")

# Reading in EEP data
df <- read.csv(file.path(datadir, "eep_final_data.csv"))

# Filter data to just CA census tracts and relevant variables
df <- df %>%
  filter(STUSPS == "CA") %>%
  # Format GEOID as text and 0 padding
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(GEOID = case_when(
    str_length(GEOID) == 10 ~ paste("0", GEOID, sep = ""),
    TRUE ~ GEOID
  )) %>%
  select(GEOID, energy_burden, heat_wave_risk_index, seniors_living_alone, disabled)


df <- df %>%
  # Adding energy burden classification
  mutate(energy_burden_rating = case_when(
    energy_burden > 15 ~ ">15%",
    energy_burden >= 12 & energy_burden <= 15 ~ "12 to 15%",
    energy_burden >= 9 & energy_burden < 12 ~ "9 to 12%",
    energy_burden >= 6 & energy_burden < 9 ~ "6 to 9%",
    energy_burden >= 3 & energy_burden < 6 ~ "3 to 6%",
    energy_burden < 3 ~ "<3%"
  ))

seniors_quartiles <- quantile(df$seniors_living_alone, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
disabled_quartiles <- quantile(df$disabled, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

df <- df %>%
  # Breaking seniors living alone into quartiles
  mutate(seniors_living_alone_rating = case_when(
    seniors_living_alone <= seniors_quartiles[1] ~ "1st Quartile",
    seniors_living_alone > seniors_quartiles[1] & seniors_living_alone <= seniors_quartiles[2] ~ "2nd Quartile",
    seniors_living_alone > seniors_quartiles[2] & seniors_living_alone <= seniors_quartiles[3] ~ "3rd Quartile",
    seniors_living_alone > seniors_quartiles[3] ~ "4th Quartile"
  )) %>%
  # Breaking disabled population into buckets
  mutate(disabled_rating = case_when(
    disabled <= disabled_quartiles[1] ~ "1st Quartile",
    disabled > disabled_quartiles[1] & disabled <= disabled_quartiles[2] ~ "2nd Quartile",
    disabled > disabled_quartiles[2] & disabled <= disabled_quartiles[3] ~ "3rd Quartile",
    disabled > disabled_quartiles[3] ~ "4th Quartile"
  ))

# Creating an ordering for energy burden and Majority BIPOC
df$energy_burden_rating <- factor(df$energy_burden_rating,
                                  c("<3%", "3 to 6%", "6 to 9%", "9 to 12%", "12 to 15%", ">15%"))

# Creating an ordering for seniors living alone
df$seniors_living_alone_rating <- factor(df$seniors_living_alone_rating,
                                         c("1st Quartile", "2nd Quartile", "3rd Quartile", "4th Quartile"))

# Creating an ordering for disabled
df$disabled_rating <- factor(df$disabled_rating,
                             c("1st Quartile", "2nd Quartile", "3rd Quartile", "4th Quartile"))


# Getting quartile for heat wave risk
quartiles <- quantile(df$heat_wave_risk_index, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
top_quartile <- quartiles[3]

# Top most heat wave vulnerable census tracts subset
top_heat_df <- df %>%
  filter(heat_wave_risk_index >= top_quartile)

# Reading in US map
# Reading in US map (all census tracts)
us_map <- st_read(us_map_fp) %>% 
  st_transform(crs = 3857) %>%
  # Filter to only CA
  filter(STUSPS == "CA") %>%
  # Format GEOID as text and 0 padding
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(GEOID = case_when(
    str_length(GEOID) == 10 ~ paste("0", GEOID, sep = ""),
    TRUE ~ GEOID
  )) %>%
  select(c(GEOID, geometry))

# Creating Base map of California
state_base_map <- us_map %>%
  st_union() %>%
  ggplot() +
  geom_sf(fill = "white", color = "grey", alpha = 0.5)

# Creating top heat wave census tracts base map
top_heat_map <- top_heat_df %>%
  left_join(us_map,
            by = c("GEOID"))

# Energy Burden for Top 25% most heat wave vulnerable census tracts
state_base_map +
  geom_sf(data = top_heat_map %>%
            filter(!is.na(energy_burden_rating)),
          aes(fill = energy_burden_rating, geometry = geometry)) +
  scale_fill_brewer(palette = "Blues") +
  guides(fill = guide_legend(reverse = TRUE))

# Seniors living alone for Top 25% most heat wave vulnerable census tracts
state_base_map +
  geom_sf(data = top_heat_map %>%
            filter(!is.na(seniors_living_alone_rating)),
          aes(fill = seniors_living_alone_rating, geometry = geometry)) +
  scale_fill_brewer(palette = "Purples") +
  guides(fill = guide_legend(reverse = TRUE))

# Disabled for Top 25% most heat wave vulnerable census tracts
state_base_map +
  geom_sf(data = top_heat_map %>%
            filter(!is.na(disabled_rating)),
          aes(fill = disabled_rating, geometry = geometry)) +
  scale_fill_brewer(palette = "Greens") +
  guides(fill = guide_legend(reverse = TRUE))

# Census tracts are top 25% of all categories
top_vulnerable <- top_heat_df %>%
  filter(seniors_living_alone_rating == "4th Quartile" |
           seniors_living_alone_rating == "3rd Quartile",
         #disabled_rating == "4th Quartile",
         energy_burden >= 6)

# Generating map of top vulnerable census tracts
top_vulnerable_map <- top_vulnerable %>%
  left_join(us_map,
            by = c("GEOID"))

state_base_map +
  geom_sf(data = top_vulnerable_map,
          aes(fill = "red", geometry = geometry))
