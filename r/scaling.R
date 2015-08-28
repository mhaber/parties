rm(list=ls(all=T))

library(tm)
library(austin)
library(SnowballC)
library(ggplot2)
library(ggthemes)
library(stringr)

######################
# Loading Data Files #
######################
dir="data/sisterParties/cduCongresses"

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
corpusClean <- tm_map(corpusClean,stemDocument, language="german") # stem words, Adjust Languages
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
wf.res <- wordfish(wfm, dir=c(1581, 1608)) #dir=c() is used to anchor documents; i.e. one text has a lower value than another

###########
# Results #
###########

# party leader vector
leader <- rep(0,1657) 
leader[c(24,54,84,93,190,149,299,337,388,492,556,598,672,711,759,818,852,901,921,997,
         946,1011,1056,1058,1066,1104,1108,1165,1173,1228,1240,1288,1291,1302,1315,
         1369,1381,1431,1477,1496,1524,1581,1608)] <- 1

# party vector
party <- rep("CDU",1657)
party[c(54,93,149,337,556,711,818,921,946,1011,1066,1108,1173,1240,1291,1315,
        1381,1477,1524,1608)] <- "CSU"

# party congress vector
congress <- gsub("_.*","",wf.res$docs)
congress <- gsub(" parteitag ","_",congress)

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

# save data in r data file
save(thetas,file="data/sisterParties/thetas.Rda")

partyDistance <- subset(thetas,leader==1)
partyDistance$congress <- droplevels(partyDistance$congress)

#####################
# Distance Measures #
#####################

# distance from leader to party
meanSpeakerCDU <- subset(partyDistance$mean, partyDistance$party=="CDU")
meanPartyCDU <- subset(partyDistance$partyMean, partyDistance$party=="CDU")
partyDistanceCDU <- data.frame(meanSpeakerCDU,meanPartyCDU)
partyDistanceCDU <- apply(partyDistanceCDU, 1, var) # distance from leader to party

meanSpeakerCSU <- subset(partyDistance$mean, partyDistance$party=="CSU")
meanPartyCSU <- subset(partyDistance$partyMean, partyDistance$party=="CSU")
partyDistanceCSU <- data.frame(meanSpeakerCSU,meanPartyCSU)
partyDistanceCSU <- apply(partyDistanceCSU, 1, var) # distance from leader to party

#distance between leaders
distanceToLeader <- tapply(partyDistance$mean,partyDistance$congress,
                           FUN=var) 
distanceToLeader <-  as.vector(na.omit(as.vector(unlist(distanceToLeader))))

#save combined dataset
cduCongress <- data.frame(partyDistanceCSU,distanceToLeader,
                          subset(partyDistance$congress, partyDistance$party=="CSU"))
names(cduCongress)[1] <- "distanceCsuCdu"
names(cduCongress)[2] <- "distanceLeaders"
names(cduCongress)[3] <- "congress"
save(cduCongress,file="data/sisterParties/cduCongress.Rda")

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
library(extrafont) # for more fonts

loadfonts(device="win")
font_import("Trebuchet MS")
font_import()
load("data/sisterParties/thetas.Rda")
load("data/sisterParties/cduCongress.Rda")

## wordfish plot sorted by estimate
# plot(wf.res)

## ggplot with nice customization
p1 <- ggplot(subset(thetas,leader==1), aes(x=congress, y=mean, group=1)) +
  #geom_errorbar(width=.1, aes(ymin=lower, ymax=upper)) +
  geom_point(shape=16, size=3) +
  geom_point(aes(y=partyMean, shape=22, size=3, fill="red"), show_guide = FALSE)  +
  scale_shape_identity()+
  geom_text(aes(label=speaker, color=party),hjust=.5, vjust=-.5) +
  guides(colour=FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(thetas$mean), linetype = "dashed") +
  coord_flip() + theme_bw() + xlab("") + ylab("") +
  ggtitle("Positions of CDU Congress and Party Leaders 1990 - 2011")
ggsave(p1, file="figures/Positions of CDU Congress and Party Leaders 1990-2011.pdf", width=12, height=10)

p2 <- ggplot(cduCongress, aes(x=congress, y=distanceCsuCdu, group=1)) +
  geom_line(size=1.2) +
  geom_line(aes(y=distanceLeaders),color= "#999999", size=1.2,
            show_guide = FALSE)  +
  theme_bw() + xlab("") + ylab("Distance") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2),
        text=element_text(size=16),
        axis.title.y=element_text(vjust=1),
        plot.title=element_text(vjust=1)) +
  geom_vline(xintercept = c(1.1, 5.1, 8.1,12.1,15.1,18.5), linetype = "dashed") +
  annotate("rect", 0,0,1,8.1,0,0.8, alpha=0.2) +
  annotate("rect", 0,0,15.1,20,0,0.8, alpha=0.2) +
  ggtitle("Distance CSU-CDU Leaders (grey)\n and CSU-CDU Congress (black) 1990 - 2011")
ggsave(p2, file="figures/Distance between CSU and CDU Leaders and Party Congress 1990-2011.pdf", width=12, height=8)


# ##################################################################################
# ############################### Only Party Leaders ###############################
# ##################################################################################
# 
# ######################
# # Loading Data Files #
# ######################
# 
# ### Set Location of Files
# dir="data/sisterParties/leaders"
# 
# ### Load txt files into corpus
# corpus=Corpus(DirSource(dir), readerControl=list(reader=readPlain, language="de", load="F")) #Adjust Languages
# 
# ### Removal Word List
# wordlist <- read.csv("data/stopWords.txt", stringsAsFactors=F) # wordlist.txt is list of words that you want to exclude. 
# wordlist=wordlist[,1]
# 
# ######################
# # Text Preprocessing #
# ######################
# 
# corpusClean <- tm_map(corpus, gsub, pattern = "\\(.*)", replacement = "") #remove text in brackets
# corpusClean <- tm_map(corpusClean, stripWhitespace) # remove extra white spaces
# corpusClean <- tm_map(corpusClean, tolower) # convert words to lower case
# corpusClean <- tm_map(corpusClean, removePunctuation) # remove punctuation
# corpusClean <- tm_map(corpusClean, removeWords, wordlist) #remove unwanted words
# #corpusClean <- tm_map(corpusClean, removeWords, c("")) # removes indiviual words if not using wordlist.txt
# corpusClean <- tm_map(corpusClean, removeNumbers) # remove numbers
# corpusClean <- tm_map(corpusClean, removeWords, stopwords("german")) # remove stopwords, Adjust Languages
# corpusClean <- tm_map(corpusClean,stemDocument, language="german") # stem words, Adjust Languages
# corpusClean <- tm_map(corpusClean, PlainTextDocument) #convert everything to plain text
# names(corpusClean) <- names(corpus)
# 
# # Create object to remove frequent and sparse terms
# ndocs <- length(corpusClean)
# minDocFreq <- ndocs * 0.07 # keep words that occur at least in % of documents
# maxDocFreq <- ndocs * 0.99 # keep words that occur at the most in % of documents
# 
# ### Creating a DocumentTermMatrix
# #dtm <- DocumentTermMatrix(corpusClean) # normal dtm without word bounds
# dtm<- DocumentTermMatrix(corpusClean, control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))
#                          
# # inspect(dtm[1:5,100:105]) # lets you inspect the first 5 documents and the 100-105 words
# # findFreqTerms(dtm, 50) # list the most frequent words occuring at least 5 times
# # dtm<- removeSparseTerms(dtm, 0.4) # remove sparse terms (at least 40%) to reduce size
# 
# ### Creating a Word-Frequency Matrix
# wfm <- wfm(dtm, word.margin = 2)
# 
# #######################
# # Wordfish Estimation #
# #######################
# 
# ### Run Wordfish
# wf.res <- wordfish(wfm, dir=c(44, 2)) #dir=c() is used to anchor documents; i.e. one text has a lower value than another
# 
# ###############
# # Diagnostics #
# ###############
# 
# ### summary of results
# #summary(wf.res)
# 
# ###  Mean, Median and Variance
# #mean <- mean(wf.res$theta)
# #median <- median(wf.res$theta)
# #variance <- var(wf.res$theta)
# 
# ###  Plot results
# ## wordfish plot sorted by estimate
# # plot(wf.res) 
# 
# ## ggplot with nice customization
# thetas <- data.frame(substr(wf.res$docs, 1, 4), wf.res$docs, ifelse(grepl("Csu", wf.res$docs), "CSU","CDU"), 
#                      wf.res$theta, wf.res$theta - (1.96*wf.res$se.theta),
#                      wf.res$theta + (1.96*wf.res$se.theta))
# names(thetas)[1] <- "year"
# names(thetas)[2] <- "speaker"
# names(thetas)[3] <- "party"
# names(thetas)[4] <- "mean"
# names(thetas)[5] <- "lower"
# names(thetas)[6] <- "upper"
# rownames(thetas) <- NULL
# thetas$speaker <- gsub("^.*Cdu(.*).txt.*$","\\1",thetas$speaker) # clean speaker names
# thetas$speaker <- gsub("^.*Csu(.*).txt.*$","\\1",thetas$speaker) # clean speaker names
# 
# # To order by score not by document
# # thetas$speaker <- factor(thetas$speaker, 
# #                         levels = thetas[order(thetas$mean), "speaker"])
# 
# # Position of Parties
# p1 <- ggplot(thetas, aes(x=year, y=mean, group=1)) +
#   geom_errorbar(width=.1, aes(ymin=lower, ymax=upper)) +
#   geom_point(shape=20, size=4) + 
#   geom_text(aes(label=party, color=party),hjust=.5, vjust=-.5) +
#   guides(colour=FALSE) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   geom_hline(yintercept = mean(thetas$mean), linetype = "dashed") +
#   coord_flip() + theme_bw() + xlab("") + ylab("") +
#   ggtitle("Positions of CDU and CSU Leaders 1990-2011")
# ggsave(p1, file="figures/Positions of CDU and CSU 1990-2011.pdf", width=12, height=10)
# 
# # Position of Speakers
# p2 <- ggplot(thetas, aes(x=year, y=mean, group=1)) +
#   geom_errorbar(width=.1, aes(ymin=lower, ymax=upper)) +
#   geom_point(shape=20, size=4) + 
#   geom_text(aes(label=speaker, color=party),hjust=.5, vjust=-.5) +
#   guides(colour=FALSE) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   geom_hline(yintercept = mean(thetas$mean), linetype = "dashed") +
#   coord_flip() + theme_bw() + xlab("") + ylab("") +
#   ggtitle("Positions of CDU and CSU Leaders 1990-2011")
# ggsave(p2, file="figures/Positions of CDU and CSU Leaders 1990-2011.pdf", width=12, height=10)
# 
# ###  Plot only subsets of the data
# # wf.res$docs # look at number of documents
# # wf.res.subset <- wf.res # create subset variable 
# # wf.res.subset$docs <- wf.res.subset$docs[c(10, 19, 26, 34, 39, 42, 58)] # select documents 1 and 2 and 5
# # wf.res.subset$data <- wf.res.subset$data[,c(10, 19, 26, 34, 39, 42, 58)] # select estimates for documents 1 and 2 and 5
# # wf.res.subset$docs <- gsub("([0-9]*)", "",wf.res.subset$docs, perl=T) # remove numbers from list of speakers
# # wf.res.subset$docs <- gsub(".txt", "",wf.res.subset$docs) # remove .txt from list of speakers
# # wf.res.subset$docs <- gsub("-$", "",wf.res.subset$docs, perl=T) # remove trailing whitespaces
# # wf.res.subset$docs <- gsub("^-", "",wf.res.subset$docs, perl=T) # remove trailing whitespaces
# # wf.res.subset$docs <- gsub("-", " ",wf.res.subset$docs) # remove trailing whitespaces
# # wf.res.subset$docs <- gsub("Franois", "Francois",wf.res.subset$docs) # change name 
# # wf.res.subset$docs <- gsub("JeanLuc", "Jean-Luc",wf.res.subset$docs) # change name
# plot(wf.res, xlab="Wordfish estimates", 
#      main="Positions of CDU and CSU Leaders 1990-2011", col.main="black") # plot subset
# 
# ### Looking at individual Words
# word.coefs <- coef(wf.res, "poisson")$words # saves word coefficients in the object word.coefs
# 
# ### Plot Word coefficients
# 
# # wds <- c("entrepris", "développ", "import", "inégal", "travail", "solidar") # Select Words you wish to plot
# # word.coefs[wds, ]
# # dotchart(sort(word.coefs[wds, "beta"], decreasing = TRUE), wds, 
# #         main="Ideological Position of Words Describing the Economy",
# #         xlab = "Word Weights") 
# 
# ### Eifel Tower Plot (Word fixed effects and weights)
# 
# plot(word.coefs$beta,word.coefs$psi, type='n', 
#      xlab="Word Weights",ylab="Word Fixed Effect", 
#       main="Word Fixed Effects and Word Weights of PS Speakers at the 2000 Congress in Grenoble", cex.main = .8)
# 
#  text(word.coefs$beta, 
#      word.coefs$psi,
#      row.names(word.coefs),col="grey") # Text function is used to highlight indivual words
# 
# # selector <- 1:10 # use selector object to define words that are supposed to be highlighted
# # text(word.coefs$beta[selector],
# #     word.coefs$psi[selector],
# #     row.names(word.coefs)[selector]) 
# 
# ##################################################################################################################
# ##################################################################################################################
# ######################################### Saving Results as CSV ##################################################
# ##################################################################################################################
# ##################################################################################################################
# 
# #### Store and save estimates ####
# speaker <- gsub(".txt", "",wf.res$docs) # remove .txt from list of speakers
# speaker <- gsub(" $", "",speaker, perl=T) # remove trailing whitespaces
# out.dat <- data.frame(speaker = speaker, position = wf.res$theta, st.error= wf.res$se.theta, 
#                       mean = mean, median = median, variance = variance, stringsAsFactors = F)
# write.csv(out.dat, file ="D:/Desktop/Research/Party Congresses/PS Analysis/", quote=F, fileEncoding = "UTF8")
