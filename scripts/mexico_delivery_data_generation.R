# Building out decree accounting data on overdeliveries
# Martin & McCoy LLC
# keaton@martin-mccoy.com
# 2020-09-23


# libraries
library(tabulizer)
library(tidyverse)

label_vector = c(
  "CO River at NIB", 
  "Limotrophe", 
  "Tijuana", 
  "SIB", 
  "Diversion Discharge", 
  "Delivery NIB", 
  "Total Delivery to MX Treaty", 
  "Water Reserve", 
  "Total to Mexico", 
  "Delivery to Water Reserve", 
  "To Mexico in Excess", 
  "Accountable Deliveries to MX", 
  "Water Bypassed 242"
)

# 2019
mx_table_2019 = extract_tables(file = "./data/decree_report_2019.pdf", 
               pages = 35, 
               output = "data.frame")[[1]]

mx_table_2019 = mx_table_2019[c(-2, -15:-21),] %>%
  mutate(labels = label_vector) %>%
  dplyr::select(-TOTAL, -X) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  mutate(year = 2019) %>%
  mutate(value = as.numeric(gsub(",", "", value)))



# 2018
mx_table_2018 = extract_tables(file = "./data/decree_report_2018.pdf", 
                              pages = 36, 
                              output = "data.frame")[[1]]

mx_table_2018 = mx_table_2018[c(-2, -15:-21),] %>%
  mutate(labels = label_vector) %>%
  dplyr::select(-TOTAL, -X) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  mutate(year = 2018) %>%
  mutate(value = as.numeric(gsub(",", "", value)))

# 2017
mx_table_2017 = extract_tables(file = "./data/decree_report_2017.pdf", 
                               pages = 35, 
                               output = "data.frame")[[1]]

mx_table_2017 = mx_table_2017[c(-2, -9, -15:-21),] %>%
  mutate(labels = c(
    "CO River at NIB", 
    "Limotrophe", 
    "SIB", 
    "Diversion Discharge", 
    "Delivery NIB", 
    "Total Delivery to MX Treaty", 
    "Water Reserve", 
    "Delivery to Water Reserve", 
    "To Mexico in Excess", 
    "Accountable Deliveries to MX", 
    "Water Bypassed 242",
    "Water Provided to US 319"
  )) %>%
  dplyr::select(-TOTAL, -X) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  mutate(year = 2017) %>%
  mutate(value = as.numeric(gsub(",", "", value)))

# 2016
mx_table_2016 = extract_tables(file = "./data/decree_report_2016.pdf", 
                               pages = 35, 
                               output = "data.frame")[[1]]

mx_table_2016 = mx_table_2016[c(-2, -14:-21),] %>%
  mutate(labels = label_vector[-3]) %>%
  dplyr::select(-TOTAL, -X) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  mutate(year = 2016) %>%
  mutate(value = as.numeric(gsub(",", "", value)))

# 2015
mx_table_2015 = extract_tables(file = "./data/decree_report_2015.pdf", 
                               pages = 33, 
                               output = "data.frame")[[1]]


mx_table_2015 = mx_table_2015[c(-2, -14:-21),] %>%
  mutate(labels = label_vector[-3]) %>%
  dplyr::select(-TOTAL, -X) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  mutate(year = 2015) %>%
  mutate(value = as.numeric(gsub(",", "", value)))

# 2014
mx_table_2014 = extract_tables(file = "./data/decree_report_2014.pdf", 
                               pages = 33, 
                               output = "data.frame")[[1]]
mx_table_2014 = mx_table_2014[c(-2, -14:-21),] %>%
  mutate(labels = label_vector[-3]) %>%
  dplyr::select(-TOTAL, -X) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  mutate(year = 2014) %>%
  mutate(value = as.numeric(gsub(",", "", value)))

# 2013
mx_table_2013 = extract_tables(file = "./data/decree_report_2013.pdf", 
                               pages = 29, 
                               output = "data.frame")[[1]]

mx_table_2013 = mx_table_2013[-2,] %>%
  mutate(labels = label_vector[-10]) %>%
  dplyr::select(-TOTAL, -X) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  mutate(year = 2013) %>%
  mutate(value = as.numeric(gsub(",", "", value)))


# 2012
mx_table_2012 = extract_tables(file = "./data/decree_report_2012.pdf", 
                               pages = 28, 
                               output = "data.frame")[[1]]

mx_table_2012 = mx_table_2012[-2,] %>%
  mutate(labels = label_vector[-10]) %>%
  dplyr::select(-TOTAL, -X) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  mutate(year = 2012) %>%
  mutate(value = as.numeric(gsub(",", "", value)))

# 2011 Labelling is weird
modified_label_vector = c(
    "CO River at NIB", 
    "Limotrophe", 
    "Tijuana", 
    "SIB", 
    "Arranged for Cienega - Mexico Portion", 
    "Arranged for Cienega - Non-Gov",
    "Diversion Discharge", 
    "Delivery NIB", 
    "Total Delivery to MX Treaty", 
    "Water Reserve", 
    "Total to Mexico", 
    "To Mexico in Excess", 
    "Accountable Deliveries to MX", 
    "Arranged for Cienega - US Portion",
    "Water Bypassed 242"
  )

# 2011
mx_table_2011 = extract_tables(file = "./data/decree_report_2011.pdf", 
                               pages = 28, 
                               output = "data.frame")[[1]]

mx_table_2011 = mx_table_2011[-2,] %>%
  mutate(labels = modified_label_vector) %>%
  dplyr::select(-TOTAL, -X) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  mutate(year = 2011) %>%
  mutate(value = as.numeric(gsub(",", "", value)))

# 2010
mx_table_2010 = extract_tables(file = "./data/decree_report_2010.pdf", 
                               pages = 28, 
                               output = "data.frame")[[1]]

mx_table_2010 = mx_table_2010[-c(2,9),] %>%
  mutate(labels = c(
    "CO River at NIB", 
    "Limotrophe", 
    "Tijuana", 
    "SIB", 
    "Arranged for Cienega - Mexico Portion", 
    "Arranged for Cienega - Non-Gov",
    "Diversion Discharge", 
    "Delivery NIB", 
    "Total Delivery to MX Treaty", 
    "Accountable Deliveries to MX", 
    "To Mexico in Excess", 
    "Arranged for Cienega - US Portion",
    "Water Bypassed 242"
  )) %>%
  dplyr::select(-TOTAL, -WATER.USER) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  mutate(year = 2010) %>%
  mutate(value = as.numeric(gsub(",", "", value)))

# 2009
mx_table_2009 = extract_tables(file = "./data/decree_report_2009.pdf", 
                               pages = 28, 
                               output = "data.frame")[[1]] %>%
  tail(-1) %>%
  mutate(labels = c(
    "CO River at NIB", 
    "Limotrophe", 
    "Tijuana", 
    "SIB", 
    "Diversion Discharge", 
    "Total Delivery to MX Treaty",
    "To Mexico in Excess", 
    "Accountable Deliveries to MX",
    "Arranged for Cienega - US Portion",
    "Water Bypassed 242"
  )) %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  mutate(year = 2009) %>%
  select(-1) %>%
  mutate(value = as.numeric(gsub(",", "", value)))


# Combining
dfs = sapply(.GlobalEnv, is.data.frame) 
data_list = mget(names(dfs)[dfs])
df = bind_rows(data_list)


# Visualization exploration
df %>%
  group_by(year, labels) %>%
  summarize(total_values = sum(value)) %>%
  filter(labels == "To Mexico in Excess") %>%
  mutate(year = as.integer(year)) %>%
  ggplot(aes(x = year, y = total_values)) +
  geom_line() +
  scale_x_continuous(name = "Year", 
                     breaks = c(2009:2019)
                     ) +
  scale_y_continuous(name = "Excess Deliveries to MX (AF)", 
                     labels = scales::label_comma()) +
  theme_classic()
  
