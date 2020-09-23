# Building out decree accounting data on overdeliveries
# Martin & McCoy LLC
# keaton@martin-mccoy.com
# 2020-09-23


# libraries
library(tabulizer)
library(tidyverse)

# 2019
mx_table_2019 = extract_tables(file = "./data/decree_report_2019.pdf", 
               pages = 35, 
               output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2019)

# 2018
mx_table_2018 = extract_tables(file = "./data/decree_report_2018.pdf", 
                              pages = 36, 
                              output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2018)

# 2017
mx_table_2017 = extract_tables(file = "./data/decree_report_2017.pdf", 
                               pages = 35, 
                               output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2017)

# 2016
mx_table_2016 = extract_tables(file = "./data/decree_report_2016.pdf", 
                               pages = 35, 
                               output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2016)

# 2015
mx_table_2015 = extract_tables(file = "./data/decree_report_2015.pdf", 
                               pages = 33, 
                               output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2015)

# 2014
mx_table_2014 = extract_tables(file = "./data/decree_report_2014.pdf", 
                               pages = 33, 
                               output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2014)

# 2015
mx_table_2015 = extract_tables(file = "./data/decree_report_2015.pdf", 
                               pages = 33, 
                               output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2015)

# 2014
mx_table_2014 = extract_tables(file = "./data/decree_report_2014.pdf", 
                               pages = 33, 
                               output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2014)

# 2013
mx_table_2013 = extract_tables(file = "./data/decree_report_2013.pdf", 
                               pages = 29, 
                               output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2013)

# 2012
mx_table_2012 = extract_tables(file = "./data/decree_report_2012.pdf", 
                               pages = 28, 
                               output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2012)

# 2013
mx_table_2013 = extract_tables(file = "./data/decree_report_2013.pdf", 
                               pages = 29, 
                               output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2013)

# 2012
mx_table_2012 = extract_tables(file = "./data/decree_report_2012.pdf", 
                               pages = 28, 
                               output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2012)

# 2011
mx_table_2011 = extract_tables(file = "./data/decree_report_2011.pdf", 
                               pages = 28, 
                               output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2011)

# 2010
mx_table_2010 = extract_tables(file = "./data/decree_report_2010.pdf", 
                               pages = 28, 
                               output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2010)

# 2009
mx_table_2009 = extract_tables(file = "./data/decree_report_2009.pdf", 
                               pages = 28, 
                               output = "data.frame")[[1]] %>%
  dplyr::select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "month") %>%
  rename(metric = X) %>% 
  mutate(year = 2009)

# Earlier gets trickier


# 2008

# 2007
mx_table_2007 = extract_tables(file = "./data/decree_report_2007.pdf", 
                               pages = 30, 
                               output = "data.frame")[[1]] %>%
  select(-X, -X.1, -TOTAL) %>%
  tail(-1) %>%
  mutate(JAN = str_split(JAN.FEB, " ", simplify = TRUE)[,1], 
         FEB = str_split(JAN.FEB, " ", simplify = TRUE)[,2]) %>%
  select(-JAN.FEB) %>%
  rename(metric = 1) %>%
  pivot_longer(cols = MAR:FEB, names_to = "month") %>%
  mutate(year = 2007) %>%
  mutate(value = as.numeric(gsub(",", "",value)))

# 2006
mx_table_2006 = extract_tables(file = "./data/decree_report_2006.pdf", 
                               pages = 30, 
                               output = "data.frame")[[1]] %>%
  select(-X, -X.1, -TOTAL) %>%
  tail(-1) %>%
  mutate(JAN = str_split(JAN.FEB, " ", simplify = TRUE)[,1], 
         FEB = str_split(JAN.FEB, " ", simplify = TRUE)[,2]) %>%
  select(-JAN.FEB) %>%
  rename(metric = 1) %>%
  pivot_longer(cols = MAR:FEB, names_to = "month") %>%
  mutate(year = 2007) %>%
  mutate(value = as.numeric(gsub(",", "",value)))


