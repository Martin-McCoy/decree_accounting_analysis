# Decree Tufte Text Figure
# Martin & McCoy LLC
# keaton@martin-mccoy.com
# 2020-09-28

# packages
library(tidytext)
library(tidyverse)
library(wordcloud)
library(widyr)
library(quanteda)
library(tm)

# loading in the list
text_list = readRDS("./data/decree_text_list.rds")
bigram_list = readRDS("./data/decree_text_list_bigram.rds")

# just lengths
dim(text_list[[1]])[1]

lengths = tibble(words = map(text_list, function(x) {dim(x)[1]}) %>% unlist(), 
                 year = 1964:2019)

ggplot(lengths, aes(x = year, y = words)) +
  geom_area() +
  theme_classic() +
  labs(y = "Number of Extracted Words", 
       x = "Year of Report") +
  geom_vline(xintercept = 2010) +
  geom_hline(yintercept = 15000)

# combining
names(text_list) = 1964:2019
text_df = text_list %>% bind_rows(.id = "year")

# wordcloud
# Everything
text_df %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# old versus new
# old
text_df %>%
  filter(year %in% 1964:1974) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# new
text_df %>%
  filter(year %in% 2009:2019) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


# Working on word comparisons
names(text_list) = 1964:2019 
text_df = bind_rows(text_list, .id = "year")

year_comparer = function(text_df, early_year = NULL, later_year = NULL){
  early_year_filtered = text_df %>%
    filter(year == early_year) %>%
    anti_join(stop_words) %>%
    select(-year)
  
  later_year_filtered = text_df %>%
    filter(year == later_year) %>%
    anti_join(stop_words) %>%
    select(-year)
  
  anti_join(later_year_filtered, early_year_filtered)
}

# We will also apply a filter to get rid of nonsense words
# 1964-1971
first_peak_diff = year_comparer(text_df, early_year = 1964, later_year = 1971)

filtered_first_peak_diff = tokens(as.list(first_peak_diff)) %>%
  tokens_select(names(data_int_syllables)) %>%
  as.list() %>% 
  bind_rows() %>%
  distinct()

#2000-2007
second_peak_diff = year_comparer(text_df, early_year = 2000, later_year = 2007)

filtered_second_peak_diff = tokens(as.list(second_peak_diff)) %>%
  tokens_select(names(data_int_syllables)) %>%
  as.list() %>% 
  bind_rows() %>%
  distinct()

#2012-2019
third_peak_diff = year_comparer(text_df, early_year = 2012, later_year = 2019)

filtered_third_peak_diff = tokens(as.list(third_peak_diff)) %>%
  tokens_select(names(data_int_syllables)) %>%
  as.list() %>% 
  bind_rows() %>%
  distinct()


#bigram search
names(bigram_list) = 1964:2019
bigram_df = bigram_list %>% bind_rows(.id = "year")

bigram_df[str_detect(bigram_df$word, "mexicos water"),] %>% View()

# Text search
text_df %>%
  filter(word == "brock")

