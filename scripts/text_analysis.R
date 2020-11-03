# Decree Tufte Text Figure
# Martin & McCoy LLC
# keaton@martin-mccoy.com
# 2020-09-28

# packages
library(tidytext)
library(tidyverse)
library(wordcloud)
library(widyr)

# loading in the list
text_list = readRDS("./data/decree_text_list.rds")

# just lengths
dim(text_list[[1]])[1]

lengths = tibble(words = map(text_list, function(x) {dim(x)[1]}) %>% unlist(), 
                 year = 1964:2019)

ggplot(lengths, aes(x = year, y = words)) +
  geom_area() +
  theme_classic() +
  labs(y = "Number of Extracted Words", 
       x = "Year of Report")

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

text_df
