rm(list=ls(all=T))

library(ggplot2)
library(ggthemes)
library(stringr)
library(RSQLite)

############
# Parl Gov #
############
# connect to the sqlite file
con <- dbConnect(RSQLite::SQLite(), "data/parlgov-stable.db")

# get a list of all tables
alltables <- dbListTables(con)

# get the cabinets table as a data.frame
cabinets <- dbGetQuery(con,'select * from view_cabinet')

# get data for Germany
cabinetsGer <- subset(cabinets, cabinets$country_name=="Germany") # cabinets
cdu <- subset (cabinetsGer, cabinetsGer$party_name_short=="CDU")
csu <- subset (cabinetsGer, cabinetsGer$party_name_short=="CSU")


#TESTING 