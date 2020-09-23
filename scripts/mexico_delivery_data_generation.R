# Building out decree accounting data on overdeliveries
# Martin & McCoy LLC
# keaton@martin-mccoy.com
# 2020-09-23


# libraries
library(tabulizer)

# 2019
mx_table_2019 = extract_tables(file = "./data/decree_report_2019.pdf", 
               pages = 35, 
               output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2019)

# 2018
mx