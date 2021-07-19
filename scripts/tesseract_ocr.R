# Tesseract OCR on pre-2003 USBR Reports
# Martin & McCoy LLC
# keaton@martin-mccoy.com
# 2020-09-22

# packages
library(tidyverse)
library(pdftools)
library(tesseract)
library(magick)
library(tidytext)
library(quanteda)
library(tm)

# Workflow is: 1) convert pdf to an image, 2) run image through ocr, 3) get text
# Let's test it on the oldest
# 
# img_file = pdftools::pdf_convert("./data/decree_report_1971.pdf", format = 'tiff')
# text = img_file[1] %>%
#   image_read() %>%
#   image_resize("2000x") %>%
#   image_convert(type = 'Grayscale') %>%
#   image_trim(fuzz = 40) %>%
#   image_write(format = 'png', density = '300x300') %>%
#   tesseract::ocr() %>%
#   stringr::str_split("\n", simplify = TRUE) %>%
#   stringr::str_remove_all("[0-9]") %>%
#   stringr::str_replace("-", " ") %>%
#   stringr::str_remove_all("[:punct:]") %>%
#   trimws() %>%
#   stringr::str_remove_all("~") %>%
#   tolower()
# 
# text = text[text != ""]

import_old_decree = function(file = NULL, bigram = FALSE) {
 # extracting text, loops over each page

    images = pdftools::pdf_convert(file, format = 'tiff')
    
    tmp_text_list = list()
    for(i in 1:length(images)) {

      tmp_text = images[i] %>%
        image_read() %>%
        image_resize("2000x") %>%
        image_convert(type = 'Grayscale') %>%
        image_trim(fuzz = 40) %>%
        image_write(format = 'png', density = '300x300') %>%
        tesseract::ocr() %>%
        stringr::str_split("\n", simplify = TRUE) %>%
        stringr::str_remove_all("[0-9]") %>%
        stringr::str_replace("-", " ") %>%
        stringr::str_remove_all("[:punct:]") %>%
        trimws() %>%
        stringr::str_remove_all("~") %>%
        tolower()
    
      tmp_text = tmp_text[tmp_text != ""]
    
    tmp_text_list[[i]] = tmp_text
    
    }
    
    names(tmp_text_list) = 1:length(images)
    if(bigram == FALSE){
      # combining and tidying
      combined_text = tibble(un_formatted_text = Reduce(c, tmp_text_list)) %>%
        unnest_tokens(word, un_formatted_text)
      
    } else {
    
      combined_text = tibble(un_formatted_text = Reduce(c, tmp_text_list)) %>%
        unnest_tokens(word, un_formatted_text, token = "ngrams", n = 2)
      
    }
    # cleaning up images
    files = list.files("./")
    files_to_delete = files[str_detect(files, ".tiff")]
    map(files_to_delete, unlink)
    
    return(combined_text)
    
      
}

# Let's apply this to all of the pdfs
decree_files = list.files("./data/", full.names = TRUE)
decree_files = decree_files[str_detect(decree_files, ".pdf")]

big_decree_text_list = map(decree_files, import_old_decree)
saveRDS(big_decree_text_list, "./data/decree_text_list.rds")

big_decree_text_list_bigram = map(decree_files, import_old_decree, bigram = TRUE)
saveRDS(big_decree_text_list_bigram, "./data/decree_text_list_bigram.rds")
