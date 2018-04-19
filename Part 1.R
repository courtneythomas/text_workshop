### 04/18/2018

if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(
  tidyverse,
  pdftools,
  tidytext,
  rtweet,
  stringr
  )

coral_spp_narratives <- read_csv('data/iucn_narratives.csv')
coral_spp_info <- read_csv('data/coral_spp_info.csv')

coral_habs_raw <- coral_spp_narratives %>%
  left_join(coral_spp_info, by = 'iucn_sid') %>%
  select(iucn_sid, sciname, habitat)

coral_habs_raw$habitat[1]

# shift + command + M = %>% 

# text string variable

x <- "Everybody's got something to hide except for me and my monkey"

# change to lowercase and get rid of weird punctuation
# reformat text

tools::toTitleCase(x)
tolower(x) # gets ride of lowercase
str_split(x, 'hide') # creates breaks
str_replace(x, 'except for', 'including') # replace except for with including
str_replace_all(x, ' ', '_')
str_detect(x, 't') # detects the letter t
str_match(x, 't') # what part of the string matches t
str_match_all(x, 't')
str_extract(x, 't')
str_extract_all(x, 't')
str_locate(x, 't')
str_locate_all(x, 't') # where within the whole string do we have a match

coral_habs <- coral_habs_raw %>% 
  mutate(hab_cut = str_split(habitat, '.')) # split habitat everywhere there's a period
coral_habs$hab_cut[1]

# returns empty chunks..not helpful

coral_habs <- coral_habs_raw %>% 
  mutate(hab_cut = str_split(habitat, '\\. ')) # \\. tells it to look for actual periods, not just any character
coral_habs$hab_cut[1]

# string split inside a dataframe -- still keeps in original cell
# we want to 'unnest'

coral_habs <- coral_habs_raw %>% 
  mutate(hab_cut = str_split(habitat, '\\. ')) %>% 
  unnest(hab_cut) %>% 
  filter(str_detect(hab_cut, '[0-9]')) # returns strings with numbers; can also be with letters [a-z] or [A-Z] [3-7a-cA-F] etc etc
coral_habs$hab_cut[1]

coral_depth <- coral_habs %>% 
  filter(str_detect(hab_cut, '[0-9] m')) %>% 
  mutate(depth = str_extract(hab_cut, '[0-9-]+ m')) %>% 
  mutate(depth_num = str_split(depth, '[^0-9]')) %>% 
  unnest(depth_num) %>% 
  filter(depth_num != '') %>% 
  mutate(depth_num = as.numeric(depth_num))
  
years <- coral_habs %>% 
  mutate(year = str_extract(hab_cut, '[0-9]{4}'))

coral_threats <- coral_spp_narratives %>% 
  select(iucn_sid, threats) %>% 
  mutate(threats = tolower(threats),
         threats_cut = str_split(threats, '\\. ')) %>% 
  unnest(threats_cut) %>% 
  filter(str_detect(threats_cut, '^a|n$')) # anything that starts with an a or ends with an n

# str_view()

crappy_colname <- 'Per-capita income ($US) (2015 dollars)'
tolower(crappy_colname) %>% 
  str_replace_all('[^a-z0-9]+', '_') # replace anything that's not a letter or number with an underscore

# package called janitor has a 'clean_names()' function that can also do this

y <- 'one fish two fish red fish blue fish'
y %>% str_locate('(?<=two) fish') # want to pick out which fish is right after the word two; returns position within string
y %>% str_locate('fish (?=blue)')
x <- list.files('.', pattern = ) # can use this to organize files


