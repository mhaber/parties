library(ggplot2)
library(ggthemes)
library(stringr)
library(dplyr)

###########
### Testing

## Load data file
load("data/parties.RData")

# create ec subset
partiesPec <- parties %>% filter(pec==1)

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
partiesPec <- partiesPec %>%   group_by(pecId) %>%
  mutate(distanceChange = c(NA,diff(distance))) %>% 
  ungroup()

# ols
model3 <- lm(distanceChange ~ govCoalition, data=partiesPec)
summary(model3)

# t-test
distanceChange1 <- partiesPec$distanceChange[partiesPec$govCoalition==1]
distanceChange2 <- partiesPec$distanceChange[partiesPec$govCoalition==0]
t.test(distanceChange1, distanceChange2)
