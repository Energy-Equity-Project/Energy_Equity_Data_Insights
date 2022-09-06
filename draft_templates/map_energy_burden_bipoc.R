
# Loading Libraries
library(tidyverse)
library(ggpubr)
library(ggnewscale)
library(sf)

# File paths
datadir <- "data"
us_map_fp <- file.path(datadir,
                       "cb_2021_us_tract_500k_filtered_simplified",
                       "cb_2021_us_tract_500k_filtered_simplified.shp")


# Reading in US map (all census tracts)
us_map <- st_read(us_map_fp) %>% 
  st_transform(crs = 3857) %>%
  filter(STUSPS == "MI") %>%
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(GEOID = case_when(
    str_length(GEOID) == 10 ~ paste("0", GEOID, sep = ""),
    TRUE ~ GEOID
  )) %>%
  select(c(GEOID, geometry))

# Reading in EEP data
eep <- read.csv(file.path(datadir, "eep_final_data.csv")) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(GEOID = case_when(
    str_length(GEOID) == 10 ~ paste("0", GEOID, sep = ""),
    TRUE ~ GEOID
  )) %>%
  # Adding energy burden classification
  mutate(energy_burden_rating = case_when(
    energy_burden > 15 ~ ">15%",
    energy_burden >= 12 & energy_burden <= 15 ~ "12 to 15%",
    energy_burden >= 9 & energy_burden < 12 ~ "9 to 12%",
    energy_burden >= 6 & energy_burden < 9 ~ "6 to 9%",
    energy_burden >= 3 & energy_burden < 6 ~ "3 to 6%",
    energy_burden < 3 ~ "<3%"
  )) %>%
  # Adding Majority BIPOC classification
  mutate(majority_bipoc = case_when(
    bipoc_percent >= 50 ~ TRUE,
    TRUE ~ FALSE
  ))

# Creating an ordering for energy burden and Majority BIPOC
eep$energy_burden_rating <- factor(eep$energy_burden_rating,
                                   c(NA,
                                     "<3%",
                                     "3 to 6%",
                                     "6 to 9%",
                                     "9 to 12%",
                                     "12 to 15%",
                                     ">15%"))

# Joining EEP data with US Census tracts (NOT centroids)
us_map <- us_map %>%
  left_join(eep %>% select(c(GEOID, energy_burden, energy_burden_rating, bipoc_percent, majority_bipoc)),
            by = "GEOID")

# Creating base map - outline of US only outer boundary
us_base_map <- us_map %>%
  st_union() %>%
  ggplot() +
  geom_sf(fill = "white", color = "grey")

# Creating state borders layer
states_borders <- st_read(us_map_fp) %>% 
  st_transform(crs = 3857) %>%
  filter(STUSPS == "MI") %>%
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(GEOID = case_when(
    str_length(GEOID) == 10 ~ paste("0", GEOID, sep = ""),
    TRUE ~ GEOID
  )) %>%
  group_by(STUSPS) %>%
  summarize(geometry = st_union(geometry))

# Creating Energy Burden Map
# Base layer: US map (outer boundary only)
us_base_map +
  # Add majority Non-BIPOC energy burden map layer
  geom_sf(data = us_map %>%
            filter(!is.na(energy_burden_rating),
                   !majority_bipoc),
          aes(fill = energy_burden_rating),
          lwd = 0) +
  scale_fill_brewer(palette = "Oranges") +
  labs(fill = "Energy Burden\nNon-BIPOC") +
  guides(fill = guide_legend(reverse = TRUE, order = 1)) +
  # New scale necessary for majority BIPOC energy burden layer
  new_scale("fill") +
  # Add majority BIPOC energy burden map layer
  geom_sf(data = us_map %>%
            filter(!is.na(energy_burden_rating),
                   majority_bipoc),
          aes(fill = energy_burden_rating),
          lwd = 0) +
  scale_fill_brewer() +
  labs(fill = "Energy Burden\nBIPOC") +
  guides(fill = guide_legend(reverse = TRUE)) +
  # Add state borders map layer
  geom_sf(data = states_borders,
          color = "grey",
          fill = NA) +
  # Theme aesthetic edits
  theme_bw() +
  theme(legend.position = "right",#c(0.9, 0.27),
        legend.box = "vertical",
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm"),
        legend.text = element_text(size = 5),
        legend.margin = margin(t = 0, unit= "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())#,
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.y=element_blank())
