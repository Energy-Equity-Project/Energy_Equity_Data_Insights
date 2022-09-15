
library(tidyverse)
library(sf)

rm(list = ls())

# File paths
datadir <- "data"
outdir <- "data_insight_2"
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


# Imperial county map
imperial_map <- st_read(us_map_fp) %>%
  st_transform(crs = 3857) %>%
  filter(STUSPS == "CA") %>%
  filter(NAMELSADCO == "Imperial County")

imperial_outline <- st_read(us_map_fp) %>%
  st_transform(crs = 3857) %>%
  filter(STUSPS == "CA") %>%
  filter(NAMELSADCO == "Imperial County") %>%
  st_union()

# Top most heat wave vulnerable census tracts subset
top_heat_df <- df %>%
  filter(heat_wave_risk_index >= top_quartile)


# Imperial County Seniors living alone in top 25% most vulnerable to heat wave
imperial_map_df <- imperial_map %>%
  left_join(top_heat_df,
            by = c("GEOID")) %>%
  mutate(seniors_living_alone_rating = case_when(
    seniors_living_alone_rating == "4th Quartile" ~ "Most Risk",
    seniors_living_alone_rating == "3rd Quartile" ~ "Moderate Risk",
    seniors_living_alone_rating == "2nd Quartile" ~ "Somewhat Risk",
    seniors_living_alone_rating == "1st Quartile" ~ "Least Risk"
  )) %>%
  mutate(disabled_rating = case_when(
    disabled_rating == "4th Quartile" ~ "Most Risk",
    disabled_rating == "3rd Quartile" ~ "Moderate Risk",
    disabled_rating == "2nd Quartile" ~ "Somewhat Risk",
    disabled_rating == "1st Quartile" ~ "Least Risk"
  ))

imperial_map_df$seniors_living_alone_rating <- factor(imperial_map_df$seniors_living_alone_rating,
                                         c("Least Risk", "Somewhat Risk", "Moderate Risk", "Most Risk"))

imperial_map_df$disabled_rating <- factor(imperial_map_df$disabled_rating,
                                                      c("Least Risk", "Somewhat Risk", "Moderate Risk", "Most Risk"))

# Imperial County Top 25% heat wave vulnerable, Seniors Living Alone
imperial_map %>%
  ggplot() +
  geom_sf(fill = "white", color = "grey") +
  geom_sf(data = imperial_map_df %>%
            filter(!is.na(seniors_living_alone_rating)),
          aes(fill = seniors_living_alone_rating)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(palette = "Purples") +
  labs(fill = "Seniors\nLiving Alone") +
  theme_bw() +
  theme(
    legend.position = c(0.92, 0.75),
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.3, "cm"),
    panel.grid.major = element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )

ggsave("imperial_heat_wave_seniors_map.png", path = file.path(outdir))

# Imperial County Top 25% heat wave vulnerable, Disabled
imperial_map %>%
  ggplot() +
  geom_sf(fill = "white", color = "grey") +
  geom_sf(data = imperial_map_df %>%
            filter(!is.na(disabled_rating)),
          aes(fill = disabled_rating)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(palette = "Greens") +
  labs(fill = "Disabled") +
  theme_bw() +
  theme(
    legend.position = c(0.92, 0.75),
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.3, "cm"),
    panel.grid.major = element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )

ggsave("imperial_heat_wave_disabled_map.png", path = file.path(outdir))

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
  geom_sf(fill = "white", color = "black", alpha = 0.5)

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
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_sf(data = imperial_outline,
          lwd = 1, color = "Red", alpha = 0, show.legend = FALSE) +
  labs(fill = "Energy Burden (%)") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        panel.grid.major = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank())


ggsave("heat_wave_energy_burden_map.png", path = file.path(outdir))
