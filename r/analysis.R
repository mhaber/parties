library(ggplot2)
library(ggthemes)
library(stringr)
library(RSQLite)
library(manifestoR)
library(foreign)
library(dplyr)
library(countrycode)

#####################
### Manifesto Project

mp_setapikey(key = "03bc3947326cfae8288519d7351411a8") # get from manifesto profile page
manifesto <- mp_maindataset()
parties <- manifesto %>% dplyr::mutate(iso3 = countrycode(countryname, "country.name", "iso3n")) %>% 
  select(iso3, date, party1=party, partyname, parfam)

###################
### Golder PEC Data

golder <- read.dta("data/golder.dta")
golder$iso3 <- countrycode::countrycode(golder$countryname, "country.name", "iso3n")

golder$partyname1 <- parties$partyname[match(golder$party1, parties$party1)]



pec <- dplyr::full_join(golder,parties, by="party1")


############
### Parl Gov 

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