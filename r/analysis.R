library(ggplot2)
library(ggthemes)
library(stringr)
library(dplyr)
library(plm)
###########
### Testing

## Load data file
load("data/parties.RData")

# create pec subset
partiesPec <- parties %>% filter(pec==1)

test <- felm(distance ~ pec +  data = parties)

### Manifesto Distance (Log)

## PEC subset
# ols
model1 <- lm(distance ~ govCoalition,  data= partiesPec)
summary(model1)

## full data set
# ols
model2 <- lm(distance ~ pec + sameFamily + govCoalition,  data= parties)
summary(model2)

# t-test
distance1 <- parties$distance[parties$govCoalition==1]
distance2 <- parties$distance[parties$pec==1]
t.test(distance1, distance2)

## comparision pec who enter gov and pec who don't

# calculate difference in distance between two elections
partiesPec <- partiesPec %>%  group_by(pecId) %>%
  mutate(distanceChange2 = distance - lag(distance, order_by = year)) %>% 
  ungroup()

# partiesPec <- partiesPec %>%   group_by(pecId) %>%
#   arrange(year) %>% 
#   mutate(distanceChange = c(NA,diff(distance))) %>% 
#   ungroup()

# ols
model3 <- lm(distanceChange ~ govCoalition, data=partiesPec)
summary(model3)

# t-test
distanceChange1 <- partiesPec$distanceChange[partiesPec$govCoalition==1]
distanceChange2 <- partiesPec$distanceChange[partiesPec$govCoalition==0]
t.test(distanceChange1, distanceChange2)

##############
### Case Study

## Politbarometer
library(foreign)
library(lubridate)
pb <- read.dta("data/ZA2391_v5-0-0.dta")

pbVote <- pb %>% dplyr::mutate(month = match(month.abb[pb$v3],month.abb)) %>% # change month to numeric
  dplyr::mutate(willVote = ifelse(v5==2, 1, 0)) %>% 
  dplyr::select(waveId=v1, respondID=v2, month, year=v4, willVote, voteNext=v6, voteLast=v7)

voteCdu <- pbVote %>%  dplyr::group_by(year) %>% 
  dplyr::summarize(voteCdu = sum(voteNext=="CDU", na.rm=T)/n()) %>% 
  dplyr::mutate(changeVote = voteCdu - lag(voteCdu))

## CDU/CSU distance measure
load("data/sisterParties/thetas.Rdata")

distanceCduCsu <- thetas %>% dplyr::group_by(year) %>% 
  summarize(distance = abs(mean(mean[party=="CDU"])-mean(mean[party=="CSU"])))

voteDistance <- full_join(voteCdu,distanceCduCsu )

model4 <- lm(distance ~ changeVote, data=voteDistance)
summary(model4)
