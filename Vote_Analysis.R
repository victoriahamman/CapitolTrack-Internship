
#--- Load Data Files---#
member_data <- read.csv("C:/Users/vee10/Downloads/CapitolTrack/Data/Members.csv")
vote_data <- read.csv("C:/Users/vee10/Downloads/CapitolTrack/Data/Votes.csv")

head(vote_data)
head(member_data)

#---Libraries---#
library(RSQLite)
library(tidyverse)
library(janitor)
library(stringr)

#--- Clean Data ---#

#turns all column names to lowercase and adds underscores
member_data <- member_data %>% clean_names()
vote_data <- vote_data %>% clean_names()


#remove vacancies/no member rows from member_data
member_data <- member_data %>% 
  filter(!str_detect(f_name, regex("vacancy|assigned", ignore_case=TRUE)))

# Descriptive Analysis


