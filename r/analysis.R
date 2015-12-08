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

## Politbarometer Full (West Germany only)
# library(foreign)
# library(lubridate)
# pbFull <- read.dta("data/ZA2391_v5-0-0.dta", convert.factors = F)
# 
# pbFullVote <- pbFull %>% dplyr::mutate(month = match(month.abb[pbFull$v3],month.abb)) %>% # change month to numeric
#   dplyr::mutate(willVote = ifelse(v5==1 | 2 | 5, 1, 0)) %>%
#   dplyr::mutate(date = as.numeric(paste0(month, v4))) %>%
#   dplyr::select(waveId=v1, respondID=v2, month, year=v4, date, willVote, voteNext=v6, voteLast=v7)
# 
# voteCduFull <- pbFullVote %>%  dplyr::group_by(year) %>%
#   dplyr::summarize(voteCdu = sum(voteNext==1, na.rm=T)/n()) %>%
#   dplyr::mutate(changeVote = voteCdu - lag(voteCdu, n = 1))

## Politbarometer Vote (All Germany)
library(lubridate)
pb <- read.csv("data/politbarometer.csv")
pb <- pb %>% dplyr::mutate(year = year(mdy(pb$date)))
voteCdu <- pb %>%  dplyr::group_by(year) %>% 
  summarise(voteCdu = mean(cducsu)) %>% 
  dplyr::mutate(changeVote = voteCdu - lag(voteCdu, n = 1))
  
  
## CDU/CSU distance measure
load("data/sisterParties/thetas.Rdata")

thetas$year <- as.numeric(as.character(thetas$year))
distLeader <- thetas %>% dplyr::group_by(year) %>%
  summarize(distLeader = abs(mean(mean[party=="CDU" & leader==1])- 
                               mean(mean[party=="CSU"& leader==1]))) %>% 
  dplyr::mutate(distLeaderChange = distLeader - lag(distLeader, n = 1))

distMean <- thetas %>% dplyr::group_by(year) %>%
  summarize(distMean = abs(mean(mean[party=="CDU"])-mean(mean[party=="CSU"]))) %>% 
  dplyr::mutate(distMeanChange = distMean - lag(distMean, n = 1))

distMedian <- thetas %>% dplyr::group_by(year) %>%
  summarize(distMedian = abs(mean(partyMedian[party=="CDU"]) - mean(partyMedian[party=="CSU"]))) %>% 
  dplyr::mutate(distMedianChange = distMedian - lag(distMedian, n = 1)) 

distCduCsu <- full_join(distLeader,distMean)
distCduCsu <- full_join(distCduCsu,distMedian)
distCduCsu <- full_join(distCduCsu,voteCdu)

# distLeader ~ changeVote
model4 <- lm(distLeader ~ changeVote,  data=distCduCsu)

# distMean ~ changeVote
model5 <- lm(distMean ~ changeVote,  data=distCduCsu)

# distMedian ~ changeVote
model6 <- lm(distMedian ~ changeVote,  data=distCduCsu)

# distLeader ~ voteCdu
model7 <- lm(distLeader ~ voteCdu,  data=distCduCsu)

# distMean ~ voteCdu
model8 <- lm(distMean ~ voteCdu,  data=distCduCsu)

# distMedian ~ voteCdu
model9 <- lm(distMedian ~ voteCdu,  data=distCduCsu)

# distLeaderChange ~ changeVote
model10 <- lm(distLeaderChange ~ changeVote,  data=distCduCsu)

# distMeanChange ~ changeVote
model11 <- lm(distMeanChange ~ changeVote,  data=distCduCsu)

# distMedianChange ~ changeVote
model12 <- lm(distMedianChange ~ changeVote,  data=distCduCsu)

# distLeaderChange ~ voteCdu
model13 <- lm(distLeaderChange ~ voteCdu,  data=distCduCsu)

# distMeanChange ~ voteCdu
model14 <- lm(distMeanChange ~ voteCdu,  data=distCduCsu)

# distMedianChange ~ voteCdu
model15 <- lm(distMedianChange ~ voteCdu,  data=distCduCsu)

summary(model4)
summary(model5)
summary(model6)
summary(model7)
summary(model8)
summary(model9)
summary(model10)
summary(model11)
summary(model12)
summary(model13)
summary(model14)
summary(model15)
