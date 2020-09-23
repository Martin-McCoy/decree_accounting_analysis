# Report Scraping
# Martin & McCoy LLC
# keaton@martin-mccoy.com
# 2020-09-22

# packages
library(tidyverse)
library(pdftools)
library(tm)

# generating list of urls to scrape through and download files
# The linking logic changes at 2003
years_1 = seq(from = 1964, to = 2002, by = 1)

for(i in 1:length(years_1)) {
  url = paste0("https://www.usbr.gov/lc/region/g4000/4200Rpts/DecreeRpt/", 
          years_1[i], 
          "DecreeRpt.pdf")
  name = paste0("decree_report_", years_1[i], ".pdf")
  try(download.file(url, destfile = paste0("./data/", name)), silent = TRUE)
  print(paste("Downladed report from", years_1[i]))
}

years_2 = seq(from = 2003, to = 2019, by = 1)

for(i in 1:length(years_2)) {
  url = paste0("https://www.usbr.gov/lc/region/g4000/4200Rpts/DecreeRpt/", 
               years_2[i], 
               "/", 
               years_2[i], 
               ".pdf")
  name = paste0("decree_report_", years_2[i], ".pdf")
  try(download.file(url, destfile = paste0("./data/", name)), silent = TRUE)
  print(paste("Downladed report from", years_2[i]))
}

# Churning through pdf to text - test with most recent version
files = list.files("./data", full.names = TRUE)
decree_test = lapply(files[56], pdf_text)
corp_test = VCorpus(VectorSource(decree_test))
corp_test = tm_map(corp_test, removePunctuation, ucp = TRUE)

corp_test.tdm <- TermDocumentMatrix(corp_test, 
                                    control = 
                                      list(removePunctuation = TRUE,
                                           stopwords = TRUE,
                                           tolower = TRUE,
                                           stemming = TRUE,
                                           removeNumbers = TRUE))
inspect(corp_test.tdm[1:20,])


# What about older docs? What is the cutoff?
# Cutoff is 2003 - this is the first year where OCR seems to be in place, for 
# the rest, we'll probably have to do some tesseract OCR system. 

# Let's export this to another script. 
