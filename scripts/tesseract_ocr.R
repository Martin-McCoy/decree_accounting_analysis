# Tesseract OCR on pre-2003 USBR Reports
# Martin & McCoy LLC
# keaton@martin-mccoy.com
# 2020-09-22

# packages
library(tidyverse)
library(pdftools)
library(tesseract)
library(magick)

# Workflow is: 1) convert pdf to an image, 2) run image through ocr, 3) get text
# Let's test it on the oldest

img_file = pdftools::pdf_convert("./data/decree_report_1964.pdf", format = 'tiff')
text = img_file %>%
  image_read() %>%
  image_resize("2000x") %>%
  image_convert(type = 'Grayscale') %>%
  image_trim(fuzz = 40) %>%
  image_write(format = 'png', density = '300x300') %>%
  tesseract::ocr() %>%
  stringr::str_split("\n", simplify = TRUE) %>%
  stringr::str_remove_all("[0-9]") %>%
  stringr::str_remove_all("[:punct:]") %>%
  trimws() %>%
  stringr::str_remove_all("~") %>%
  tolower()

text = text[text != ""]



import_old_decree = function(file = NULL) {
 # extracting text, loops over each page

    images = image_read_pdf(file)
    
    tmp_text_list = list()
    for(i in 1:length(images)) {

      tmp_text = images[i] %>%
      image_resize("2000x") %>%
      image_trim(fuzz = 40) %>%
      magick::image_ocr(options = list(tessedit_pageseg_mode = 1)) %>%
      stringr::str_split("\n", simplify = TRUE) %>%
      stringr::str_remove_all("[0-9]") %>%
      stringr::str_remove_all("[:punct:]") %>%
      trimws() %>%
      stringr::str_remove_all("~") %>%
      tolower()
    
      tmp_text = stringr::str_remove_all(tmp_text, " *\\b[[:alpha:]]{1,2}\\b *")
      tmp_text = tmp_text[tmp_text != ""]
    
    tmp_text_list[[i]] = tmp_text
    
    }
  
    # combining and tidying
    return(tmp_text_list)
}


test = import_old_decree(file = "./data/decree_report_1964.pdf")

test %>% bind_rows(.id = "page_num")
