# Collecting relevant BoR data from database
# Martin Mccoy LLC
# keaton@martin-mccoy.com
# 2020-09-24

# packages
library(tidyverse)
library(lubridate)

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
  mutate(date = date(date)) %>%
  select(date, place, value) %>%
  left_join(data_list, by = c("place" = "database_label")) %>%
  select(date, place, decree_report_label, value)

# plotting
# All data faceted
mx_table_data %>%
  # filter(decree_report_label == "To Mexico in Excess of Treaty") %>%
  # filter(decree_report_label == "Water Bypass Pursuant to IBWC Minute No. 242") %>%
  drop_na() %>%
  ggplot(aes(x = date, y = value)) +
  geom_path() +
  # geom_smooth(method = "lm") +
  facet_wrap(~decree_report_label, scales = "free") +
  scale_y_continuous(
    name = "AF",
    labels = scales::label_comma()
  ) +
  theme_classic() +
  xlab("Date")


# Excess Exploration - removing pre-2000 Giant spikes
mx_table_data %>%
  filter(decree_report_label == "To Mexico in Excess of Treaty") %>%
  ggplot(aes(x = date, y = value)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm") +
  theme_classic() +
  theme(plot.title = element_text(family = "Ubuntu Regular")) +
  labs(
    title = "Water to Mexico In Excess of Treaty",
    subtitle = "From January 2000 through July 2020"
  ) +
  xlim(date(c("2000-01-01", "2020-09-01"))) +
  geom_hline(yintercept = 0) +
  xlab("Date") +
  scale_y_continuous(
    name = "AF",
    labels = scales::label_comma(),
    limits = c(-5000, 100000)
  )


mx_table_data %>%
  filter(decree_report_label == "To Mexico in Excess of Treaty") %>%
  filter(value > 1500000)
  
