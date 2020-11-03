# 242 plot
# Martin Mccoy LLC
# keaton@martin-mccoy.com
# 2020-09-24

# packages
library(tidyverse)
library(lubridate)
library(ggpubr)

# reading in big data
lc <- read_csv("./data/lc_monthly_1950_present.csv")

# Filtering to get data for MX table
data_list <- tibble(
  database_label = c(
    "COLORADO RIVER AT NORTHERN INTERNATIONAL BOUNDARY",
    "MEXICO IN EXCESS OF TREATY",
    "MEXICO TREATY WATER",
    "MEXICAN BYPASS PURSUANT TO MINUTE 242",
    "MEXICO ORDER",
    "SOUTHERLY INTERNATIONAL BOUNDARY",
    "TIJUANA"
  ),
  decree_report_label = c(
    "Colorado River at the Northerly International Boundary",
    "To Mexico in Excess of Treaty",
    "Accountable Deliveries to Mexico",
    "Water Bypass Pursuant to IBWC Minute No. 242",
    "Total Deliveries to Mexico in Satisfaction of Treaty Requirements",
    "Delivery at Southern International Boundary",
    "Diversion for Delivery at Tijuana"
  )
)

# Filtering, cleaning and adding better labels
mx_table_data <- lc %>%
  filter(place %in% data_list$database_label) %>%
  filter(!str_detect(measurement, "CFS")) %>%
  select(date, place, value) %>%
  left_join(data_list, by = c("place" = "database_label")) %>%
  select(date, place, decree_report_label, value)

ibwc_242_summary = mx_table_data %>%
  filter(decree_report_label == "Water Bypass Pursuant to IBWC Minute No. 242") %>%
  drop_na() %>%
  group_by(year = year(date)) %>%
  summarize(yearly_total = sum(value))

ggplot(ibwc_242_summary, aes(x = year, y = yearly_total)) +
  geom_path() +
  scale_y_continuous(
    name = "AF",
    labels = scales::label_comma()
  ) +
  scale_x_continuous(breaks = c(1967:2021)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Water Bypass Pursuant to IBWC Minute No. 242",
       subtitle = "Total Yearly Flow",
       x = "Year")
         