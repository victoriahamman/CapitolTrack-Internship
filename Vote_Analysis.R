
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


#--- Clean Data ---#

#turns all column names to lowercase and adds underscores
member_data <- member_data %>% clean_names()
vote_data <- vote_data %>% clean_names()
dbListFields(db, "Votes")

#remove vacancies/no member rows from member_data
member_data <- member_data %>% 
  filter(!str_detect(f_name, regex("vacancy|assigned", ignore_case=T)))


#---Connect to SQLite Database---#
db <- dbConnect(SQLite(), "vote_anaysis.db")
getwd()

#write to SQLite tables
dbWriteTable(db, "Members", member_data, overwrite=T, header=T)
dbWriteTable(db, "Votes", vote_data, overwrite=T, header=T)

dbListTables(db)
dbListFields(db, "Members")
dbListFields(db, "Votes")


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
               M.f_name, M.l_name, M.house, M.district, M.party
               FROM Votes v LEFT JOIN Members M
               ON v.member_id = M.member_id')
dbWriteTable(db, "Joined_Data", joined_data, overwrite=T)
dbListTables(db)
view(dbGetQuery(db, 'SELECT * FROM Joined_Data'))

##fail distribution
dem_fails <- dbGetQuery(db, 'SELECT * FROM Joined_Data WHERE party="D" AND outcome="F"')
d_num_fails <- nrow(dem_fails)

rep_fails <- dbGetQuery(db, 'SELECT * FROM Joined_Data WHERE party="R" AND outcome="F"')
r_num_fails <- nrow(rep_fails)

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

#individual party success
party_success <- dbGetQuery(db, 'SELECT party, COUNT(*) as authored,
                           SUM(outcome="P") as passed, 
                           ROUND(SUM(outcome="P") * 1.0 / COUNT(*), 3) AS pass_rate
                           FROM Joined_Data
                           WHERE party="D" OR party="R"
                           GROUP BY party
                           ORDER BY pass_rate DESC')
party_success

#---Analysis on Author---#


#--If author is a member--#
sorted_members <- dbGetQuery(db, 'SELECT * FROM Joined_Data 
                             WHERE member_id IS NOT NULL
                             ORDER BY author')
dbWriteTable(db, "Sorted_Members", sorted_members, overwrite=T)
view(dbGetQuery(db, 'SELECT * FROM Sorted_Members'))

#top 10 members in number of measures authored
top_10_authored <- dbGetQuery(db, 'SELECT member_id, author, f_name,
                  COUNT(measure) AS n_measures FROM Sorted_Members
                  GROUP BY member_id, author
                  ORDER BY n_measures DESC LIMIT 10')
top_10_authored

#top_10 in number of measures passes
top_passed <- dbGetQuery(db, 'SELECT member_id, author, f_name,
                         COUNT(measure) as passed_measures 
                         FROM Sorted_Members
                         WHERE outcome="P"
                         GROUP BY member_id, author, f_name
                         ORDER BY passed_measures DESC 
                         LIMIT 10')
top_passed

#author success rates
author_success <- dbGetQuery(db, 'SELECT member_id, author, f_name,
                                    COUNT(*) AS authored, SUM(outcome="P") AS passed,
                                    ROUND(SUM(outcome = "P") * 1.0 / COUNT(*), 3) AS pass_rate
                                  FROM Sorted_Members
                                  GROUP BY member_id, author, f_name
                                  ORDER BY pass_rate DESC
                             ')
author_success


#--If author is a committee--#
sorted_comms <- dbGetQuery(db, 'SELECT * FROM Votes
                                WHERE member_id IS NULL
                                ORDER BY author')
dbWriteTable(db, "Sorted_Comms", sorted_comms, overwrite=T)
view(dbGetQuery(db, 'SELECT * FROM Sorted_Comms'))










