# Reading in tables from pdf

if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(
  tidyverse,
  pdftools,
  stringr
)

pdf_smith <- file.path('pdfs/smith_wilen_2003.pdf')
smith_text <- pdf_text(pdf_smith)

smith_df <- data.frame(text = smith_text) %>% 
  mutate(page = 1:n()) %>% 
  mutate(text_sep = str_split(text, '\\n')) %>% 
  unnest() %>% 
  group_by(page) %>% 
  mutate(line = 1:n()) %>% 
  ungroup()

col_lbls <- c('n_patches', paste0('y', 1988:1999))
table1_df <- smith_df %>% 
  filter(page == 8 & line %in% 8:18) %>% 
  separate(text_sep, col_lbls, ' +') %>% 
  select(-text, -page, -line)
  
