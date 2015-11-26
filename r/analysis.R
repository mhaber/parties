library(ggplot2)
library(ggthemes)
library(stringr)
library(RSQLite)
library(manifestoR)
library(foreign)
library(readstata13)
library(dplyr)
library(countrycode)
library(lubridate)
#####################
### Manifesto Project

mp_setapikey(key = "03bc3947326cfae8288519d7351411a8") # get from manifesto profile page
manifesto <- mp_maindataset()
manifestoSample <- manifesto %>% dplyr::mutate(iso3 = countrycode(countryname, "country.name", "iso3n")) %>% 
  select(iso3, date, party, partyname, parfam, rile)

###################
### Golder PEC Data

golder <- read.dta("data/golder.dta")
golder$iso3 <- countrycode::countrycode(golder$countryname, "country.name", "iso3n")

#############################
### Chapel Hill Expert Survey 
ches <- readstata13::read.dta13("data/ches.dta")
chesSample <- ches %>%  dplyr::select(year, cmp_id, lrgen) 

############
### Parl Gov 
# connect to the sqlite file
con <- dbConnect(RSQLite::SQLite(), "data/parlgov-stable.db")

# get a list of all tables
alltables <- dbListTables(con)

# get the parlGov table as a data.frame
parlGov <- dbGetQuery(con,'select * from view_party')

################
# Merge all Data
parties <- golder

# Add year
parties$year <- lubridate::year(lubridate::ymd(parties$election+19000000))

# add party names
parties$party1Name <- manifestoSample$partyname[match(golder$party1, manifestoSample$party)]
parties$party1Fam <- manifestoSample$parfam[match(golder$party1, manifestoSample$party)]

parties$party2Name <- manifestoSample$partyname[match(golder$party2, manifestoSample$party)]
parties$party2Fam <- manifestoSample$parfam[match(golder$party2, manifestoSample$party)]

# add positions
parties %>% 
  mutate(party1Ches=chesSample$lrgen[match(golder$party1, chesSample$cmp_id)])

###########################
### Reconciliation Function
reconcile <- function(query, typeId) {
  dataUrl <- paste0("https://tools.wmflabs.org/wikidata-reconcile/?query={%22query%22%3A%22", 
                    URLencode(query), "%22%2C%22type%22%3A%22", typeId, "%22}")
  
  jsonlite::fromJSON(dataUrl)$result[1, "name"]
}

names <- parlGov %>% dplyr::group_by(country_name) %>% filter(election_id==731) %>% ungroup() %>% select(party_name_english)
sapply(names, reconcile, typeId = "Q7278")





# match party names with golder
golder %>%  group_by(country) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(partypos1 = parlGov$left_right[agrepl(parlGov$party_name_english,golder$partyname1, max.distance = .1)])


golder$partypos1 <- parlGov$left_right[agrep(parlGov$party_name_english, golder$partyname1, max.distance=0.1)]
golder$partypos1 <- sapply(parlGov$party_name_english, agrep, x=golder$partyname1, max.distance=0.1)
###########
### Testing
