# Paneled timeline figure
# Martin Mccoy LLC
# keaton@martin-mccoy.com
# 2020-09-24

# packages
library(tidyverse)
library(lubridate)
library(ggpubr)
library(waterData)

# reading in big data
lc <- read_csv("./data/lc_monthly_1950_present.csv")
# reading in historic data
historic_storage = read_csv(
  "https://raw.githubusercontent.com/johnrfleck/colorado-river/master/data/meadpowell.csv"
) %>%
  mutate(Date = date(paste(Year, "09", "30", sep = "-")),
         `Total Storage` = Mead + Powell) %>%
  pivot_longer(c(Mead:Powell, `Total Storage`),
               names_to = "Reservoir",
               values_to = "Storage") %>%
  mutate(Storage = Storage * 1000,
         Reservoir = factor(Reservoir, levels = c("Total Storage", "Mead", "Powell"), 
                            labels = c("Total Storage", "Mead Storage", "Powell Storage")))

# Lees Ferry data from waterdata
lees = importDVs("09380000", code = "00060")

# El nino patterns https://ggweather.com/enso/oni.htm
el_nino = tibble(
  date_start = date(c(
    "1982-09-01",
    "1997-09-01", 
    "2015-09-01", 
    "1972-09-01", 
    "1987-09-01", 
    "1991-09-01"
  )),
  date_end = date(c(
    "1983-04-01", 
    "1998-04-01", 
    "2016-04-01", 
    "1973-04-01", 
    "1988-04-01", 
    "1992-04-01"
  )),
  category = c(
    "Very Strong", 
    "Very Strong", 
    "Very Strong", 
    rep("Strong", 3)
  )
)

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

# Excess to MX
excess_to_mx = mx_table_data %>%
  filter(decree_report_label == "To Mexico in Excess of Treaty") %>% 
  drop_na() %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  geom_rect(data = el_nino, inherit.aes=FALSE,
            aes(xmin = date_start, xmax = date_end,
                ymin = 0, ymax = 2000000, fill = category),
            alpha = 0.5) +
  scale_fill_discrete(name = "El Niño Event Scale") +
  scale_y_continuous(
    name = "Volume (AF)",
    labels = scales::label_comma()
  ) +
  scale_x_date(date_labels = "%Y", 
               date_breaks = "2 years", 
               limits = date(c("1971-01-01", "2020-08-01"))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  labs(title = "To Mexico in Excess of Treaty")

# Minute 242
minute_242 = mx_table_data %>%
  filter(decree_report_label == "Water Bypass Pursuant to IBWC Minute No. 242") %>% 
  drop_na() %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  geom_rect(data = el_nino, inherit.aes=FALSE,
            aes(xmin = date_start, xmax = date_end,
                ymin = 0, ymax = 20000, fill = category),
            alpha = 0.5) +
  scale_fill_discrete(name = "El Niño Event Scale") +
  scale_y_continuous(
    name = "Volume (AF)",
    labels = scales::label_comma()
  ) +
  scale_x_date(date_labels = "%Y", 
               date_breaks = "2 years", 
               limits = date(c("1971-01-01", "2020-08-01"))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") +
  labs(title = "Water Bypass Pursuant to IBWC Minute No. 242")

# combined storage
combined_storage = historic_storage %>%
  filter(Reservoir == "Total Storage") %>%
  ggplot(aes(x = Date, y = Storage)) +
  geom_line() +
  scale_y_continuous(
    name = "Volume (AF)",
    labels = scales::label_comma()
  ) +
  theme_classic() +
  xlab("Date") +
  geom_rect(data = el_nino, inherit.aes=FALSE,
            aes(xmin = date_start, xmax = date_end,
                ymin = 0, ymax = 50000000, fill = category),
            alpha = 0.5) +
  scale_x_date(date_labels = "%Y", 
               date_breaks = "2 years", 
               limits = date(c("1971-01-01", "2020-08-01"))
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "El Niño Event Scale") +
  labs(title = "Combined Storage (Mead + Powell)")

# Lees Ferry
lees_ferry = lees %>%
  ggplot(aes(x = dates, y = val)) +
  geom_line(size = 0.2) +
  scale_y_continuous(
    name = "Average Flow (CFS)", 
    labels = scales::label_comma()
  ) +
  xlab("Date") +
  geom_rect(data = el_nino, inherit.aes=FALSE,
            aes(xmin = date_start, xmax = date_end,
                ymin = 0, ymax = 90000, fill = category),
            alpha = 0.5) +
  scale_x_date(date_labels = "%Y", 
               date_breaks = "2 years", 
               limits = date(c("1971-01-01", "2020-08-01"))
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "El Niño Event Scale") +
  labs(title = "Flow at Lee's Ferry")
  
ggarrange(combined_storage, 
          lees_ferry, 
          excess_to_mx,
          minute_242,
          ncol = 1, 
          nrow = 4, 
          common.legend = TRUE, 
          align = "v", 
          labels = "auto", 
          label.x = 0.95)

ggsave("./output/flows_and_climate.pdf")
