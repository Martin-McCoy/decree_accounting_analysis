# Paneled Overdelivery Figure
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

 lake_mead = lc %>%
  filter(place == "LAKE MEAD" & measurement == "RESERVOIR WS ELEVATION  EOP PRIMARY READING IN FEET") %>%
  filter(month(date) == 12) %>%
  mutate(year = year(date), 
         date = date(date)) %>%
  select(-SDI) %>%
  ggplot(aes(x = date, y = value)) + # meter conversion
  geom_line() +
   scale_x_date(date_labels = "%Y", 
                date_breaks = "5 years", 
                limits = date(c("1964-01-01", "2022-08-01"))) +
   theme_classic(base_size = 10) +
   scale_y_continuous(limits = c(1020, 1250), name = "End of Year Elevation (ft)", 
                      labels = scales::label_comma()) +
   geom_hline(yintercept = 1075, lty = 2) +
   geom_hline(yintercept = 1090, lty = 2) +
   ggplot2::annotate("text", x = date("2022-01-01"), y = 1110, label = "Tier 0") +
   ggplot2::annotate("text", x = date("2022-01-01"), y = 1060, label = 'Tier 1') +
   labs(title = "Lake Mead Elevation") +
   xlab("") +
   theme(axis.title.y = element_text(size = 10)) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 # Excess to MX
 excess_to_mx = mx_table_data %>%
   filter(decree_report_label == "To Mexico in Excess of Treaty") %>% 
   group_by(year = year(date)) %>%
   summarize(value = sum(value)) %>%
   drop_na() %>%
   mutate(date = date(paste(year, "12", "31", sep = "-"))) %>%
   ggplot(aes(x = date, y = value)) +
   geom_line() +
   scale_y_continuous(
     name = "Volume (AF)",
     labels = scales::label_comma()
   ) +
   scale_x_date(date_labels = "%Y", 
                date_breaks = "5 years", 
                limits = date(c("1964-01-01", "2022-08-01"))) +
   theme_classic(base_size = 10) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   xlab("") +
   labs(title = "To Mexico in Excess of Treaty") +
   theme(axis.title.y = element_text(size = 10))
 
 # Excess to AZ
 az_excess = lc %>%
   filter(place == "ARIZONA TOTALS") %>%
   group_by(year = year(date)) %>%
   summarize(annualized_total = sum(value)) %>%
   mutate(date = date(paste(year, "12", "31", sep = "-"))) %>%
   mutate(diff = (2.8e+06-annualized_total)*-1) %>%
   ggplot(aes(x = date, y = diff)) +
   geom_line() +
   scale_y_continuous(
     name = "Volume (AF)",
     labels = scales::label_comma()
   ) +
   scale_x_date(date_labels = "%Y", 
                date_breaks = "5 years", 
                limits = date(c("1964-01-01", "2022-08-01"))) +
   theme_classic(base_size = 10) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   xlab("") +
   labs(title = "Overdelivery to Arizona") +
   geom_hline(yintercept = 0, lty = 2) +
   theme(axis.title.y = element_text(size = 10))
 
 
 # Excess to AZ
 ca_excess = lc %>%
   filter(place == "CALIFORNIA TOTALS") %>%
   group_by(year = year(date)) %>%
   summarize(annualized_total = sum(value)) %>%
   mutate(date = date(paste(year, "12", "31", sep = "-"))) %>%
   mutate(diff = (4.4e+06-annualized_total)*-1) %>%
   ggplot(aes(x = date, y = diff)) +
   geom_line() +
   scale_y_continuous(
     name = "Volume (AF)",
     labels = scales::label_comma()
   ) +
   scale_x_date(date_labels = "%Y", 
                date_breaks = "5 years", 
                limits = date(c("1964-01-01", "2022-08-01"))) +
   theme_classic(base_size = 10) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   xlab("") +
   labs(title = "Overdelivery to California") +
   geom_hline(yintercept = 0, lty = 2) +
   theme(axis.title.y = element_text(size = 10))


 combined = lake_mead / excess_to_mx / az_excess / ca_excess
 combined = combined + plot_annotation(tag_levels = 'a')
ggsave("./output/combined_overdelivery_plot.png", combined, width = 8.5, height = 11, units = "in")
