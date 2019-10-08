#Some code to pull my google scholar info
#Author: Brian Stacy
#Date: 10/8/2019


library(tidyverse)
library(scholar)

#define user ID for google schoalr
id<-"MACGlOYAAAAJ"

#get my basic info
basic_info<-get_profile(id)

#get my publication list
pubs<-get_publications(id)

#get citation history
cites<-get_citation_history(id)

ggplot(cites, aes(x=year, y=cites)) +
  geom_bar(stat='identity', fill="steelblue") +
  geom_text(aes(label=cites), vjust=1.6, color="white", size=3.5) +
  theme_minimal() +
  scale_y_continuous(name="Citations") +
  ggtitle("Citations For All Articles")
  

# First let's get the data, filtering to only the items tagged as
# Resume items
position_data <- read_csv('positions.csv') %>% 
  filter(in_resume) %>% 
  mutate(
    # Build some custom sections by collapsing others
    section = case_when(
      section %in% c('research_positions', 'industry_positions') ~ 'positions', 
      section %in% c('data_science_writings', 'by_me_press') ~ 'writings',
      TRUE ~ section
    )
  ) 

position_data_merge <- position_data %>%
  left_join(pubs)