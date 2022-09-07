
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
  ))

# Creating an ordering for energy burden and Majority BIPOC
df$energy_burden_rating <- factor(df$energy_burden_rating,
                                  c("<3%", "3 to 6%", "6 to 9%", "9 to 12%", "12 to 15%", ">15%"))


tmp <- df %>%
  # Remove census tracts that have missing values in energy burden and bipoc percent
  filter(!is.na(energy_burden) & !is.na(bipoc_percent)) %>%
  mutate(bipoc_pop = total_pop * (bipoc_percent / 100),
         non_bipoc_pop = total_pop * ((100 - bipoc_percent) / 100)) %>%
  group_by(energy_burden_rating) %>%
  summarize(bipoc_pop = sum(bipoc_pop, na.rm = TRUE),
            non_bipoc_pop = sum(non_bipoc_pop, na.rm = TRUE)) %>%
  ungroup() %>%
  select(c(energy_burden_rating, bipoc_pop, non_bipoc_pop)) %>%
  pivot_longer(-energy_burden_rating, names_to = "demographics", values_to = "pop") %>%
  mutate(demographics = case_when(
    demographics == "bipoc_pop" ~ "People of Color",
    TRUE ~ "White"
  ))

tmp$demographics <- factor(tmp$demographics, c("White", "People of Color"))

# Stacked bar chart with raw population
tmp %>%
  mutate(pop = pop / 1000000) %>%
  ggplot(aes(x = energy_burden_rating, y = pop,
             fill = demographics, color = demographics)) +
  geom_bar(stat = "identity", alpha = 0.7, size = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#3772FF", "#ff7300")) +
  scale_color_manual(values = c("#3772FF", "#ff7300")) +
  theme_light() +
  labs(x = "Energy Burden", y = "Population (millions)", fill = "Demographics", color = "Demographics") +
  theme(legend.position = c(0.85, 0.85),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
        axis.title.x = element_text(size = 15, margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(size = 1, color = "grey"))

# Stacked Bar chart with Percentage of Population
tmp <- tmp %>%
  group_by(energy_burden_rating) %>%
  mutate(total_pop = sum(pop)) %>%
  ungroup() %>%
  mutate(pop_percent = 100 * pop / total_pop)

tmp %>%
  ggplot(aes(x = energy_burden_rating, y = pop_percent,
             fill = demographics, color = demographics)) +
  geom_bar(stat = "identity", alpha = 0.7, size = 1) +
  scale_fill_manual(values = c("#3772FF", "#ff7300")) +
  scale_color_manual(values = c("#3772FF", "#ff7300")) +
  theme_light() +
  labs(x = "Energy Burden", y = "Demographic Percentage (%)",
       fill = "Demographics", color = "Demographics") +
  theme(legend.position = "top",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
        axis.title.x = element_text(size = 15, margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(size = 1, color = "grey"))

# Raw Population as side by side bar chart
tmp %>%
  mutate(pop = pop / 1000000) %>%
  ggplot(aes(x = energy_burden_rating, y = pop,
             fill = demographics, color = demographics)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, size = 1) +
  scale_fill_manual(values = c("#3772FF", "#ff7300")) +
  scale_color_manual(values = c("#3772FF", "#ff7300")) +
  theme_light() +
  labs(x = "Energy Burden", y = "Population (millions)",
       fill = "Demographics", color = "Demographics") +
  theme(legend.position = "top",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
        axis.title.x = element_text(size = 15, margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(size = 1, color = "grey"))

# Population Percentage as side by side chart
tmp %>%
  ggplot(aes(x = energy_burden_rating, y = pop_percent,
             fill = demographics, color = demographics)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, size = 1) +
  scale_fill_manual(values = c("#3772FF", "#ff7300")) +
  scale_color_manual(values = c("#3772FF", "#ff7300")) +
  theme_light() +
  labs(x = "Energy Burden", y = "Demographic Percentage (%)",
       fill = "Demographics", color = "Demographics") +
  theme(legend.position = "top",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
        axis.title.x = element_text(size = 15, margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(size = 1, color = "grey"))

# Population Percentage as line graph
tmp %>%
  ggplot(aes(x = energy_burden_rating, y = pop_percent,
             color = demographics, group = demographics)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  #scale_fill_manual(values = c("#3772FF", "#ff7300")) +
  scale_color_manual(values = c("#3772FF", "#ff7300")) +
  theme_light() +
  labs(x = "Energy Burden", y = "Demographic Percentage (%)",
       fill = "Demographics", color = "Demographics") +
  theme(legend.position = "top",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
        axis.title.x = element_text(size = 15, margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(size = 1, color = "grey"))

tmp <- df %>%
  # Remove census tracts that have missing values in energy burden and bipoc percent
  filter(!is.na(energy_burden) & !is.na(bipoc_percent)) %>%
  mutate(bipoc_pop = total_pop * (bipoc_percent / 100),
         non_bipoc_pop = total_pop * ((100 - bipoc_percent) / 100)) %>%
  select(c(energy_burden, bipoc_pop, non_bipoc_pop, total_pop)) %>%
  group_by(energy_burden) %>%
  summarize(bipoc_pop = sum(bipoc_pop, na.rm = TRUE),
            non_bipoc_pop = sum(non_bipoc_pop, na.rm = TRUE),
            total_pop = sum(total_pop, na.rm = TRUE)) %>%
  mutate(energy_burden = case_when(
    energy_burden > 15 ~ 16,
    energy_burden < 3 ~ 2,
    TRUE ~ energy_burden
  )) %>%
  ungroup() %>%
  group_by(energy_burden) %>%
  summarize(bipoc_pop = sum(bipoc_pop, na.rm = TRUE),
            non_bipoc_pop = sum(non_bipoc_pop, na.rm = TRUE),
            total_pop = sum(total_pop, na.rm = TRUE)) %>%
  mutate(energy_burden = as.character(energy_burden)) %>%
  mutate(energy_burden = case_when(
    energy_burden == "16" ~ ">15",
    energy_burden == "2" ~ "<3",
    TRUE ~ energy_burden
  )) %>%
  #mutate(energy_burden = paste(energy_burden, "%", sep = "")) %>%
  pivot_longer(c(bipoc_pop, non_bipoc_pop), names_to = "demographics", values_to = "pop") %>%
  mutate(demographics = case_when(
    demographics == "bipoc_pop" ~ "People of Color",
    TRUE ~ "White"
  ))

tmp$demographics <- factor(tmp$demographics, c("White", "People of Color"))

tmp$energy_burden <- factor(tmp$energy_burden,
                            c("<3", "3", "4", "5", "6", "7", "8", "9", "10", "11",
                              "12", "13", "14", "15", ">15"))

tmp %>%
  ggplot(aes(x = energy_burden, y = pop,
             color = demographics, group = demographics)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  #scale_y_continuous(trans = "log10") +
  #scale_fill_manual(values = c("#3772FF", "#ff7300")) +
  scale_color_manual(values = c("#3772FF", "#ff7300")) +
  theme_light() +
  labs(x = "Energy Burden (%)", y = "Population",
       fill = "Demographics", color = "Demographics") +
  theme(legend.position = "top",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
        axis.title.x = element_text(size = 15, margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(size = 1, color = "grey"))
