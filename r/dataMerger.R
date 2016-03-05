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

manifestoLog <- read.dta13("data/manifesto.dta")

manifesto$lbml_rile <- manifestoLog$lbml_rile
manifestoSample <- manifesto %>% dplyr::mutate(iso3 = countrycode(countryname, "country.name", "iso3n")) %>%
  mutate(edate = lubridate::ymd(manifesto$edate)) %>% 
  mutate(year = lubridate::year(manifesto$edate)) %>% 
  mutate(electionId = paste0(as.character(iso3),".", as.character(edate))) %>% 
  mutate(party1=party, party2=party, party1pos = lbml_rile, party2pos = lbml_rile) %>% 
  select(iso3, electionId, year, edate, party1, party2, partyname, parfam, pervote, absseat,
         totseats, party1pos, party2pos)


#############################
# Effective Number of Parties
manifestoSample <- manifestoSample %>% group_by(iso3, edate) %>%
  mutate(enep2 = 1 / sum(pervote*pervote/10000)) %>% 
  ungroup()

############################
# Polarization (Dalton 2008)
manifestoSample <- manifestoSample %>% group_by(electionId) %>% 
  mutate(polarize =sqrt(sum((((party1pos - sum(party1pos*pervote/sum(pervote)))/100)^2)*pervote))) %>% 
  ungroup()

###################
### Golder PEC Data

golder <- read.dta("data/golder.dta")
golder$iso3 <- countrycode::countrycode(golder$countryname, "country.name", "iso3n")

#############################
### Chapel Hill Expert Survey 

ches <- readstata13::read.dta13("data/ches.dta")
chesSample <- ches %>%  dplyr::mutate(party1=cmp_id) %>% 
  dplyr::select(year, party1, lrgen) 

############
### Parl Gov 

# connect to the sqlite file
con <- dbConnect(RSQLite::SQLite(), "data/parlgov-stable.db")

# get a list of all tables
alltables <- dbListTables(con)

# get the parlGov table as a data.frame
parlGov <- dbGetQuery(con,'select * from view_party')

###################
### Create Data Set

parties <- golder

## fix election date errors in the golder data set
# Germany
parties$election[parties$election==761030] <- 761003
parties$election[parties$election==801009] <- 801005

# Portugal
parties$election[parties$election==791005] <- 791202

# add iso3
parties$iso3 <- countrycode(parties$countryname, "country.name", "iso3n") 

# add year
parties$edate <- lubridate::ymd(parties$election+19000000)
parties$year <- lubridate::year(lubridate::ymd(parties$election+19000000))

# add election id
parties <- parties %>% mutate(electionId = paste0(as.character(iso3),".",
                                                  as.character(edate)))

# add unique pec number
parties$pecId <- as.numeric(paste0(parties$party1,parties$party2))

# add party names and families
parties$party1Name <- manifestoSample$partyname[match(golder$party1, manifestoSample$party1)]
parties$party1Fam <- manifestoSample$parfam[match(golder$party1, manifestoSample$party2)]

parties$party2Name <- manifestoSample$partyname[match(golder$party2, manifestoSample$party1)]
parties$party2Fam <- manifestoSample$parfam[match(golder$party2, manifestoSample$party2)]

parties$sameFamily <- 0
parties$sameFamily[parties$party1Fam == parties$party2Fam] <- 1

# add positions from manifesto data
parties <- parties %>% left_join(manifestoSample[, c("edate","party1", "party1pos")], 
                                 by = c("edate", "party1")) 
parties <- parties %>% left_join(manifestoSample[, c("edate","party2", "party2pos")], 
                                 by = c("edate", "party2"))

# gov coalition
parties$govCoalition <- 0
parties$govCoalition[parties$in_govt1 == parties$in_govt2] <- 1

# add distance
parties$distance <- abs(parties$party1pos - parties$party2pos)

# add enep2
parties <- parties %>% 
  mutate(enep2 = manifestoSample$enep2[match(electionId, manifestoSample$electionId)])

# add polarization
parties <- parties %>% 
  mutate(polarization2 = manifestoSample$polarize[match(electionId, manifestoSample$electionId)])


# Save File
save(parties, file = "data/parties.RData")
write.csv(parties, file ="data/parties.csv", row.names = F)
