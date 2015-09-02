rm(list=ls(all=T))

library(tm)
library(austin)
library(ggplot2)
library(ggthemes)
library(stringr)

######################
# Loading Data Files #
######################
dir="data/sisterParties/cduCSUCongresses"

### Load txt files into corpus
corpus=Corpus(DirSource(dir), readerControl=list(reader=readPlain, language="de", load="F")) #Adjust Languages

### Removal Word List
wordlist <- read.csv("data/stopWords.txt", stringsAsFactors=F) # wordlist.txt is list of words that you want to exclude. 
wordlist=wordlist[,1]

######################
# Text Preprocessing #
######################
corpusClean <- tm_map(corpus, gsub, pattern = "\\(.*)", replacement = "") #remove text in brackets
corpusClean <- tm_map(corpusClean, stripWhitespace) # remove extra white spaces
corpusClean <- tm_map(corpusClean, tolower) # convert words to lower case
corpusClean <- tm_map(corpusClean, removePunctuation) # remove punctuation
corpusClean <- tm_map(corpusClean, removeWords, wordlist) #remove unwanted words
#corpusClean <- tm_map(corpusClean, removeWords, c("")) # removes indiviual words if not using wordlist.txt
corpusClean <- tm_map(corpusClean, removeNumbers) # remove numbers
corpusClean <- tm_map(corpusClean, removeWords, stopwords("german")) # remove stopwords, Adjust Languages
corpusClean <- tm_map(corpusClean, stemDocument, language="german") # stem words, Adjust Languages
corpusClean <- tm_map(corpusClean, PlainTextDocument) #convert everything to plain text
names(corpusClean) <- names(corpus)

# Create object to remove frequent and sparse terms
ndocs <- length(corpusClean)
minDocFreq <- ndocs * 0.01 # keep words that occur at least in % of documents
maxDocFreq <- ndocs * 0.99 # keep words that occur at the most in % of documents

### Creating a DocumentTermMatrix
#dtm <- DocumentTermMatrix(corpusClean) # normal dtm without word bounds
dtm<- DocumentTermMatrix(corpusClean, control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))

# inspect(dtm[1:5,100:105]) # lets you inspect the first 5 documents and the 100-105 words
# findFreqTerms(dtm, 50) # list the most frequent words occuring at least 5 times
# dtm<- removeSparseTerms(dtm, 0.4) # remove sparse terms (at least 40%) to reduce size

### Creating a Word-Frequency Matrix
wfm <- wfm(dtm, word.margin = 2)

#######################
# Wordfish Estimation #
#######################

### Run Wordfish
wf.res <- wordfish(wfm, dir=c(1203, 9)) #dir=c() is used to anchor documents; i.e. one text has a lower value than another

###########
# Results #
###########

# party leader vector
leader <- rep(0,length(corpusClean)) 
leader[c(9,28,60,82,136,188,277,323,340,376,426,501,524,562,582,630,656,
         727,758,785,793,853,854,883,884,885,922,923,924,941,948,950,996,
         997,1027,1028,1038,1039,1076,1077,1113,1144,1147,1201,1203)] <- 1

# party vector
party <- rep("CDU",length(corpusClean))
party[c(grep("csu",wf.res$docs))] <- "CSU"

# party congress vector
congress <- gsub("_.*","",wf.res$docs)
congress <- gsub(" parteitag ","_",congress)
#congress <- gsub("csu ","",congress)

# combined data frame for all speakers
thetas <- data.frame(substr(wf.res$docs, 1, 4),
                     congress,
                     wf.res$docs,
                     party, 
                     leader,
                     wf.res$theta,
                     wf.res$theta - (1.96*wf.res$se.theta),
                     wf.res$theta + (1.96*wf.res$se.theta))
names(thetas)[1] <- "year"
names(thetas)[2] <- "congress"
names(thetas)[3] <- "speaker"
names(thetas)[4] <- "party"
names(thetas)[5] <- "leader"
names(thetas)[6] <- "mean"
names(thetas)[7] <- "lower"
names(thetas)[8] <- "upper"
rownames(thetas) <- NULL

# clean speaker names
thetas$speaker <- gsub("^.*_(.*).txt.*$","\\1",thetas$speaker) # keep only names
thetas$speaker <- str_trim(thetas$speaker) # remove whitespaces
thetas$speaker <- gsub("^.* (.*).*$","\\1",thetas$speaker) # keep only last names

# mean party position per congress with uncertainties
partyMean <- rep(aggregate(thetas$mean, list(thetas$congress), FUN="mean")[,2],
                 table(thetas$congress))
partyMeanLower <- rep(aggregate(thetas$lower, list(thetas$congress), FUN="mean")[,2],
                      table(thetas$congress))
partyMeanUpper <- rep(aggregate(thetas$upper, list(thetas$congress), FUN="mean")[,2],
                      table(thetas$congress))
thetas$partyMean <- partyMean
thetas$partyMeanLower <- partyMeanLower
thetas$partyMeanUpper <- partyMeanUpper

# median party position per congress with uncertainties
partyMedian <- rep(aggregate(thetas$mean, list(thetas$congress), FUN="median")[,2],
                   table(thetas$congress))
partyMedianLower <- rep(aggregate(thetas$lower, list(thetas$congress), FUN="median")[,2],
                        table(thetas$congress))
partyMedianUpper <- rep(aggregate(thetas$upper, list(thetas$congress), FUN="median")[,2],
                        table(thetas$congress))
thetas$partyMedian <- partyMedian
thetas$partyMedianLower <- partyMedianLower
thetas$partyMedianUpper <- partyMedianUpper

# 
# save data in r data file
save(thetas,file="data/sisterParties/thetas.Rda")
load("data/sisterParties/thetas.Rda")

partyDistance <- subset(thetas,leader==1)
levels(partyDistance$year) <- c(levels(partyDistance$year), "1994_2", "1998_2","2002_2")
partyDistance$year <- factor(partyDistance$year, as.character(partyDistance$year))
partyDistance$year[c(9,18,28)] <- c("1994_2", "1998_2","2002_2")
partyDistance$congress <- droplevels(partyDistance$congress)

#####################
# Distance Measures #
#####################
# 
# # distance from leader to party cdu
# meanSpeakerCDU <- subset(partyDistance$mean, partyDistance$party=="CDU")
# meanPartyCDU <- subset(partyDistance$partyMean, partyDistance$party=="CDU")
# meanSpeakerPartyCDU <- data.frame(meanSpeakerCDU,meanPartyCDU)
# #partyDistanceCDU <- apply(meanSpeakerPartyCDU, 1, var) # distance from leader to party
# 
# # distance from leader to party csu
# meanSpeakerCSU <- subset(partyDistance$mean, partyDistance$party=="CSU")
# meanPartyCSU <- subset(partyDistance$partyMean, partyDistance$party=="CSU")
# meanSpeakerPartyCSU <- data.frame(meanSpeakerCSU,meanPartyCSU)
# #partyDistanceCSU <- apply(meanSpeakerPartyCSU, 1, var) # distance from leader to party
# 
# meanSpeakerPartyCSU <- rbind(meanSpeakerPartyCSU, "5.1" = data.frame(meanSpeakerCSU = "NA", meanPartyCSU = "NA"))
# meanSpeakerPartyCSU <- rbind(meanSpeakerPartyCSU, "8.1" = data.frame(meanSpeakerCSU = "NA", meanPartyCSU = "NA"))
# meanSpeakerPartyCSU <- rbind(meanSpeakerPartyCSU, "19.1" = data.frame(meanSpeakerCSU = "NA", meanPartyCSU = "NA"))
# meanSpeakerPartyCSU <- meanSpeakerPartyCSU[order(as.numeric(row.names(meanSpeakerPartyCSU))),]
# rownames(meanSpeakerPartyCSU) <- NULL
# 
# #combine cdu and csu
# cduCsuDistance <- data.frame(meanSpeakerPartyCDU, meanSpeakerPartyCSU,
#                              "year" =subset(thetas$year,thetas$leader==1 & thetas$party=="CDU"))
# #distance between leaders
# distanceToLeader <- tapply(partyDistance$mean,partyDistance$congress,
#                            FUN=var) 
# distanceToLeader <-  as.vector(na.omit(as.vector(unlist(distanceToLeader))))
# 
# #save combined dataset
# cduCongress <- data.frame(partyDistanceCSU,distanceToLeader,
#                           subset(partyDistance$congress, partyDistance$party=="CSU"))
# names(cduCongress)[1] <- "distanceCsuCdu"
# names(cduCongress)[2] <- "distanceLeaders"
# names(cduCongress)[3] <- "congress"
# save(cduCongress,file="data/sisterParties/cduCongress.Rda")

### summary of results
#summary(wf.res)

###  Mean, Median and Variance
#mean <- mean(wf.res$theta)
#median <- median(wf.res$theta)
#variance <- var(wf.res$theta)

###  Plot results
rm(list=ls(all=T))
library(ggplot2)
library(ggthemes)

load("data/sisterParties/thetas.Rda")
load("data/sisterParties/cduCongress.Rda")

## wordfish plot sorted by estimate
# plot(wf.res)

## ggplot with nice customization

# Positions of CDU, CSU and their leaders over time
p1 <- ggplot(partyDistance, aes(x=year, y=mean, group=1), ordered=T) +
  #geom_errorbar(width=.1, aes(ymin=lower, ymax=upper)) +
  geom_point(shape=16, size=3) +
  geom_point(aes(y=partyMedian, shape=22, size=3, fill=party), show_guide = FALSE)  +
  scale_shape_identity()+
  geom_text(aes(label=speaker, color=party),hjust=.5, vjust=-.5) +
    guides(color=FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(thetas$mean), linetype = "dashed") +
  coord_flip() + theme_bw() + xlab("") + ylab("") +
  scale_color_manual(values=c("#FF9900","#0060AF")) +
  scale_fill_manual(values=c("#FF9900","#0060AF")) +
  ggtitle("Positions of CDU and CSU Parties and Leaders 1990 - 2011")
ggsave(p1, file="figures/Positions of CDU Congress and Party Leaders 1990-2011.pdf", width=12, height=10)

# Movement of Positions of CDU, CSU over time
p2 <- ggplot(subset(partyDistance, party=="CDU"), aes(x=year, y=partyMedian, group=1)) +
  geom_line(size=1, color="#FF9900") +
  geom_line(data=subset(partyDistance, party=="CSU"),aes(x=year, y=partyMedian, group=1),
                                                    color= "#0060AF", size=1,
            show_guide = FALSE)  +
  theme_bw() + xlab("") + ylab("Position") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2),
        text=element_text(size=16),
        axis.title.y=element_text(vjust=1),
        plot.title=element_text(vjust=1)) +
  geom_vline(xintercept = c(1.3,5.5,10.5,15.5,19.3,22.5), linetype = "dashed") +
  annotate("rect", 0,0,1,10.5,-2.5,2, alpha=0.1) +
  annotate("rect", 0,0,15.5,24,-2.5,2, alpha=0.1) +
  ggtitle("Median Positions of CDU (Orange) and CSU (Blue) Parties 1990 - 2011")
ggsave(p2, file="figures/Median Positions of CDU and CSU Parties 1990-2011.pdf", width=12, height=8)

# Movement of Positions of CDU, CSU and their leaders over time
p3 <- ggplot(subset(partyDistance, party=="CDU"), aes(x=year, y=mean, group=1)) +
  geom_line(size=1, color="#FF9900") +
  geom_line(data=subset(partyDistance, party=="CSU"),aes(x=year, y=mean, group=1),
            color= "#0060AF", size=1,
            show_guide = FALSE)  +
  theme_bw() + xlab("") + ylab("Position") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2),
        text=element_text(size=16),
        axis.title.y=element_text(vjust=1),
        plot.title=element_text(vjust=1)) +
  geom_vline(xintercept = c(1.3,5.5,10.5,15.5,19.3,22.5), linetype = "dashed") +
  annotate("rect", 0,0,1,10.5,-2.5,2, alpha=0.1) +
  annotate("rect", 0,0,15.5,24,-2.5,2, alpha=0.1) +
  ggtitle("Mean Positions of CDU (Orange) and CSU Leaders (Blue) 1990 - 2011")
ggsave(p3, file="figures/Mean Positions of CDU and CSU Leaders 1990-2011.pdf", width=12, height=8)
