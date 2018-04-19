### Sentiment analysis

if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(
  tidyverse,
  pdftools,
  tidytext,
  stringr
)

got <- pdf_text('pdfs/got.pdf')

got_df <- data.frame(text = got) %>% 
  mutate(page = 1:n()) %>% 
  filter(page %in% 6:731)

main_text <- got_df %>% 
  mutate(text = str_split(text, '\\n')) %>% 
  unnest(text)

ch_list <- main_text %>% 
  group_by(page) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(ch = ifelse(str_detect(first(text), 'previous.+table of contents'),
                     nth(text, 2), NA)) %>% 
  filter(text == first(text)) %>% 
  filter(!is.na(ch)) %>% 
  ungroup() %>% 
  select(ch, page) # list of chapters and page numbers

text_w_ch <- main_text %>% 
  select(page, text) %>% 
  left_join(ch_list, by = 'page') %>% 
  fill(ch, .direction = 'down') %>% 
  mutate(text = tolower(text))

### sentiment analysis: ex. search headlines

# take text of novel, break it up by words to see positive vs. negative language

sentiments <- get_sentiments('bing')

text_words <- text_w_ch %>% 
  tidytext::unnest_tokens(output = word, input = text, token = 'words') %>% 
  inner_join(sentiments, by = 'word') # joins by matching

text_scores <- text_words %>% 
  mutate(ch = str_trim(ch)) %>% 
  count(sentiment, ch) %>% 
  spread(sentiment, n) %>% 
  mutate(raw_score = positive - negative,
         offset = mean(positive - negative),
         offset_score = raw_score - offset)
