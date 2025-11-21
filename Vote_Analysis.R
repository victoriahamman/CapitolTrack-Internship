
#--- Load CSV Files---#
member_data <- read.csv("C:/Users/vee10/Downloads/CapitolTrack/Data/Members.csv")
vote_data <- read.csv("C:/Users/vee10/Downloads/CapitolTrack/Data/Votes.csv")

head(vote_data)
head(member_data)


#---Libraries---#
library(RSQLite)
library(tidyverse)
library(janitor)
library(stringr)
library(DBI)


#---Connect to SQLite Database---#
db <- dbConnect(SQLite(), "vote_anaysis.db")
getwd()

#write to SQLite tables
dbWriteTable(db, "Members", member_data, overwrite=T, header=T)
dbWriteTable(db, "Votes", vote_data, overwrite=T, header=T)

dbListTables(db)
dbListFields(db, "Members")
dbListFields(db, "Votes")


#--- Clean Data ---#

#turns all column names to lowercase and adds underscores
member_data <- member_data %>% clean_names()
vote_data <- vote_data %>% clean_names()

#remove vacancies/no member rows from member_data
member_data <- member_data %>% 
  filter(!str_detect(f_name, regex("vacancy|assigned", ignore_case=T)))


#---Descriptive Analysis---#

#number of members
m_nrow <- nrow(member_data)
m_nrow
#number of votes
v_nrow <- nrow(vote_data)
v_nrow

#---Distribution of Measures---#

#total pass vs. fail
failed <- dbGetQuery(db, 'SELECT * FROM Votes WHERE outcome="F"')
num_fail <- nrow(failed)
num_fail

passed <- dbGetQuery(db, 'SELECT * FROM Votes WHERE outcome="P"')
num_pass <- nrow(passed)
num_pass

#pie chart
dist <- c(num_fail, num_pass)
pct <- round(dist/sum(dist)*100)
labels <- paste(c("Failed", "Passed"), pct)
pie(dist, labels=paste(labels, "%", sep=""), col=c('red','lightgreen'))


#---Analysis by Party---#

#make joined data by member ID
joined_data <- dbGetQuery(db, 'SELECT v.vote_date, v.originator, v.outcome,
               v.location_code, v.measure, v.author, v.member_id,
               M.l_name, M.house, M.district, M.party
               FROM Votes V LEFT JOIN Members M
               ON V.member_id = M.member_id')
dbWriteTable(db, "Joined_Data", joined_data, overwrite=T)
dbListTables(db)
dbGetQuery(db, 'SELECT * FROM Joined_Data LIMIT 10')

##fail distribution
dem_fails <- dbGetQuery(db, 'SELECT * FROM Joined_Data WHERE party="D" AND outcome="F"')
dem_fails
d_num_fails <- nrow(dem_fails)
d_num_fails

rep_fails <- dbGetQuery(db, 'SELECT * FROM Joined_Data WHERE party="R" AND outcome="F"')
rep_fails
r_num_fails <- nrow(rep_fails)
r_num_fails

#pie chart with percentages
dist <- c(d_num_fails, r_num_fails)
pct <- round(dist/sum(dist)*100)
labels <- paste(c("Democrat Fails", "Republican Fails"),pct)
pie(dist, labels=paste(labels,"%",sep=''), col=c("blue","red"))


##pass distribution
dem_pass <- dbGetQuery(db, 'SELECT * FROM Joined_Data WHERE party="D" AND outcome="P"')
d_num_pass <- nrow(dem_pass)

rep_pass <- dbGetQuery(db, 'SELECT * FROM Joined_Data WHERE party="R" AND outcome="P"')
r_num_pass <- nrow(rep_pass)

#pie chart with percentages
dist <- c(d_num_pass, r_num_pass)
pct <- round(dist/sum(dist)*100)
labels <- paste(c("Democrat Passes", "Republican Passes"),pct)
pie(dist, labels=paste(labels,"%",sep=''), col=c("blue","red"))


#---Analysis on Author---#















