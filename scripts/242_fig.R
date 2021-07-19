# Minute 242 Fig
# Martin & McCoy LLC
# keaton@martin-mccoy.com
# 2020-11-23

# packages
library(tidyverse)
library(lubridate)
library(ggpubr)
library(waterData)
library(patchwork)

# reading in big data
lc <- read_csv("./data/lc_monthly_1950_present.csv")
# reading in historic data

# Filtering, cleaning and adding better labels
mx_table_data <- lc %>%
  filter(place %in% data_list$database_label) %>%
  filter(!str_detect(measurement, "CFS")) %>%
  mutate(date = date(date)) %>%
  select(date, place, value) %>%
  left_join(data_list, by = c("place" = "database_label")) %>%
  select(date, place, decree_report_label, value)

minute_242_summary = mx_table_data %>%
  filter(decree_report_label == "Water Bypass Pursuant to IBWC Minute No. 242") %>% 
  group_by(year = year(date)) %>%
  summarize(value = sum(value)) %>%
  drop_na() %>%
  mutate(date = date(paste(year, "12", "31", sep = "-"))) %>%
  filter(year >= 2000) %>%
  summarize(mean = mean(value))

# Minute 242
minute_242 = mx_table_data %>%
  filter(decree_report_label == "Water Bypass Pursuant to IBWC Minute No. 242") %>% 
  group_by(year = year(date)) %>%
  summarize(value = sum(value)) %>%
  drop_na() %>%
  mutate(date = date(paste(year, "12", "31", sep = "-"))) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  scale_fill_discrete(name = "El Niño Event Scale") +
  scale_y_continuous(
    name = "Volume (AF)",
    labels = scales::label_comma()
  ) +
  geom_hline(yintercept = minute_242_summary %>% pull(), lty = 2) +
  scale_x_date(date_labels = "%Y", 
               date_breaks = "2 years", 
               limits = date(c("1971-01-01", "2024-08-01"))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  labs(title = "Water Bypass Pursuant to IBWC Minute No. 242") +
  annotate("text", x = date("2023-07-01"), y = 127000, label = "Average Since\n2000")

ggsave("./output/242_fig.png", minute_242)
