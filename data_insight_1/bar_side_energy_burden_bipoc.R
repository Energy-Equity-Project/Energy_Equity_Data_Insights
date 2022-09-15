
library(tidyverse)
library(showtext)
library(bbplot)

font_add_google("Montserrat", "Montserrat")

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

tmp <- tmp %>%
  group_by(energy_burden_rating) %>%
  mutate(total_pop = sum(pop)) %>%
  ungroup() %>%
  mutate(pop_percent = 100 * pop / total_pop)

# Population Percentage as side by side chart
tmp %>%
  ggplot(aes(x = energy_burden_rating, y = pop_percent,
             fill = demographics)) +
  geom_bar(stat = "identity", position="dodge", alpha = 1) +
  scale_y_continuous(breaks = c(20, 40, 60, 80), limits = c(0, 80), expand = c(0,0)) +
  scale_fill_manual(values = c("#0e26aa", "#e98101")) +
  labs(x = "Energy Burden (%)", y = "Demographics within group (%)",
       fill = "Demographics") +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(size = 14, margin = unit(c(0, 3, 0, 0), "mm")),
        axis.text = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.5),
        panel.grid.major.y = element_line(color = "grey",  size = 0.3),
        panel.background = element_rect(fill = "white"))
