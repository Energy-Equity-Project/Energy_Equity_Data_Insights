
library(tidyverse)

datadir <- "data"

df <- read.csv(file.path(datadir, "eep_final_data.csv"))


df <- df %>%
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
df$energy_burden_rating <- factor(df$energy_burden_rating,
                                  c("<3%", "3 to 6%", "6 to 9%", "9 to 12%", "12 to 15%", ">15%"))

df$majority_bipoc <- factor(df$majority_bipoc,
                            c(TRUE, FALSE))

# Stacked bar chart Energy Burden on the x-axis, population on y-axis, (percent bipoc as stacking)
tmp <- df %>%
  # Remove census tracts that have missing values in energy burden and bipoc percent
  filter(!is.na(energy_burden) & !is.na(bipoc_percent)) %>%
  mutate(bipoc_pop = total_pop * (bipoc_percent / 100),
         non_bipoc_pop = total_pop * ((100 - bipoc_percent) / 100)) %>%
  # Gettting mean percent BIPOC pop by energy burden rating
  group_by(energy_burden_rating) %>%
  summarize(total_pop = sum(total_pop, na.rm = TRUE),
            bipoc_pop = sum(bipoc_pop, na.rm = TRUE),
            non_bipoc_pop = sum(non_bipoc_pop, na.rm = TRUE)) %>%
  ungroup() %>%
  select(c(energy_burden_rating, bipoc_pop, non_bipoc_pop, total_pop)) %>%
  pivot_longer(c(bipoc_pop, non_bipoc_pop), names_to = "demographics", values_to = "demographics_pop") %>%
  mutate(demographics_percent = 100 * demographics_pop / total_pop) %>%
  select(energy_burden_rating, demographics, demographics_percent) %>%
  mutate(demographics = case_when(
    demographics == "bipoc_pop" ~ "BIPOC Pop.",
    demographics == "non_bipoc_pop" ~ "Non-BIPOC Pop.",
    TRUE ~ demographics
  ))

tmp$demographics <- factor(tmp$demographics, c("Non-BIPOC Pop.", "BIPOC Pop."))

tmp %>%
  ggplot(aes(x = energy_burden_rating, y = demographics_percent, group = demographics, fill = demographics, color = demographics)) +
  geom_bar(stat = "identity", alpha = 0.5, size = 1) +
  geom_smooth(method="lm", se = FALSE,
              data = filter(tmp, demographics == "Non-BIPOC Pop."),
              color = "#083D77", show.legend = FALSE) +
  geom_smooth(method="lm", se = FALSE,
              data = filter(tmp, demographics == "BIPOC Pop."),
              color = "#B0413E", show.legend = FALSE) +
  scale_fill_manual(values = c("#3772FF", "#BC5F04"),
                    name = "Demographics",
                    breaks = c("Non-BIPOC Pop.", "BIPOC Pop.")) + 
  scale_color_manual(values = c("#3772FF", "#BC5F04"),
                     name = "Demographics",
                     breaks = c("Non-BIPOC Pop.", "BIPOC Pop.")) +
  facet_grid(rows = vars(demographics)) +
  theme_bw() +
  theme(legend.position = "none") +
  #guides(color = "none") +
  labs(x = "Energy Burden Rating", y = "Percentage of all Census Tracts (%)")

