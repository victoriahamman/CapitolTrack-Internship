###Load, Clean, Exploratory Data Analysis

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
library(ggplot2)


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

ggplot(party_success, aes(x=party, y=pass_rate)) +
  geom_col() + labs(title="Porbability of Passing by Party", y="Pass Rate")


#---Analysis on Author---#


#--If author is a member--#
sorted_members <- dbGetQuery(db, 'SELECT * FROM Joined_Data 
                             WHERE member_id IS NOT NULL
                             ORDER BY author')
dbWriteTable(db, "Sorted_Members", sorted_members, overwrite=T)
view(dbGetQuery(db, 'SELECT * FROM Sorted_Members'))


#author success rates
mem_success <- dbGetQuery(db, 'SELECT member_id, author, f_name,
                  COUNT(*) AS authored, SUM(outcome="P") AS passed,
                  ROUND(SUM(outcome = "P") * 1.0 / COUNT(*), 3) AS pass_rate
                 FROM Sorted_Members
                 GROUP BY member_id, author, f_name
                 ORDER BY pass_rate DESC')
mem_success

#top 10 members in number of measures authored
top_10_mems_authored <- dbGetQuery(db, 'SELECT member_id, author, f_name,
                  COUNT(measure) AS n_measures FROM Sorted_Members
                  GROUP BY member_id, author
                  ORDER BY n_measures DESC LIMIT 10')
top_10_mems_authored

#top_10 in number of measures passed
top_mems_passed <- dbGetQuery(db, 'SELECT member_id, author, f_name,
                         COUNT(measure) as passed_measures 
                         FROM Sorted_Members
                         WHERE outcome="P"
                         GROUP BY member_id, author, f_name
                         ORDER BY passed_measures DESC 
                         LIMIT 10')
top_mems_passed


#--If author is a committee--#

sorted_comms <- dbGetQuery(db, 'SELECT * FROM Votes
                                WHERE member_id IS NULL
                                ORDER BY author')
dbWriteTable(db, "Sorted_Comms", sorted_comms, overwrite=T)
view(dbGetQuery(db, 'SELECT * FROM Sorted_Comms'))


#top 10 committees in number of measures authored
top_10_comms_authored <- dbGetQuery(db, 'SELECT author,
                  COUNT(measure) AS n_measures 
                  FROM Sorted_Comms
                  GROUP BY author
                  ORDER BY n_measures DESC LIMIT 10')
top_10_comms_authored

#top 10 committees in number of measures passed
top_comms_passed <- dbGetQuery(db, 'SELECT author,
                         COUNT(measure) as passed_measures 
                         FROM Sorted_Comms
                         WHERE outcome="P"
                         GROUP BY author
                         ORDER BY passed_measures DESC 
                         LIMIT 10')
top_comms_passed

#committee success rates
comm_success <- dbGetQuery(db, 'SELECT author,
                  COUNT(*) AS n_measures, SUM(outcome="P") AS passed,
                  ROUND(SUM(outcome = "P") * 1.0 / COUNT(*), 3) AS pass_rate
                  FROM Sorted_Comms
                  GROUP BY author
                  ORDER BY pass_rate DESC')
comm_success


#---Analysis on Location---#

#success rates for each location
loc_success <- dbGetQuery(db, 'SELECT location_code,
                  COUNT(*) AS n_measures, SUM(outcome="P") AS passed,
                  ROUND(SUM(outcome = "P") * 1.0 / COUNT(*), 3) AS pass_rate
                  FROM Joined_Data
                  GROUP BY location_code
                  ORDER BY pass_rate DESC')
loc_success

#failure rates for each location
loc_fail<- dbGetQuery(db, 'SELECT location_code,
                  COUNT(*) AS n_measures, SUM(outcome="P") AS passed,
                  ROUND(SUM(outcome = "F") * 1.0 / COUNT(*), 3) AS pass_rate
                  FROM Joined_Data
                  GROUP BY location_code
                  ORDER BY pass_rate DESC')
loc_fail


##--Probability Analyses--##


##for member authors
prob <- dbGetQuery(db, 'SELECT * FROM Joined_Data WHERE party IS NOT NULL
                    AND outcome IS NOT NULL')
prob$passed <- ifelse(prob$outcome == "P", 1, 0)


#model probability of passing for passed based on only party
model_party <- glm(passed ~ party, data=prob, family=binomial)
summary(model_party)

#model probability of passing based on party and author
model_party_author <- glm(passed ~ party + factor(author), data=prob, family=binomial)
summary(model_party_author)

#model probability of passing based on party and location
model_party_loc <- glm(passed ~ party + factor(location_code), data=prob, family=binomial)
summary(model_party_loc)

#model probability of passing based on party, author, and location
model_all <- glm(passed ~ party + factor(author) + factor(location_code), data=prob, family=binomial)
summary(model_all)



##for committee authors
prob_comm <- dbGetQuery(db, 'SELECT * FROM Joined_Data WHERE party IS NULL
                    AND outcome IS NOT NULL')
prob_comm$passed <- ifelse(prob_comm$outcome == "P", 1, 0)



#model probability of passing based on author
comm_model_author <- glm(passed ~ factor(author), data=prob_comm, family=binomial)
summary(comm_model_author)

#model probability of passing based on author and location
comm_model_author_loc <- glm(passed ~ factor(author) + factor(location_code), data=prob_comm, family=binomial)
summary(comm_model_author_loc)



