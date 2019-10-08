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

