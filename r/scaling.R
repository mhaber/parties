rm(list=ls(all=T))

library(tm)
library(austin)
library(SnowballC)
library(ggplot2)
library(ggthemes)

######################
# Loading Data Files #
######################

### Set Location of Files
dir="data/sisterParties"

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

### Creating a Term-Document Matrix
dtm <- DocumentTermMatrix(corpusClean) # creates the Matrix

# inspect(dtm[1:5,100:105]) # lets you inspect the first 5 documents and the 100-105 words
# findFreqTerms(dtm, 50) # list the most frequent words occuring at least 5 times
# dtm<- removeSparseTerms(dtm, 0.4) # remove sparse terms (at least 40%) to reduce size

### Creating a Word-Frequency Matrix
wfm <- wfm(dtm, word.margin = 2)

#######################
# Wordfish Estimation #
#######################

### Run Wordfish
wf.res <- wordfish(wfm, dir=c(44, 2)) #dir=c() is used to anchor documents; i.e. one text has a lower value than another

###############
# Diagnostics #
###############

### summary of results
#summary(wf.res)

###  Mean, Median and Variance
#mean <- mean(wf.res$theta)
#median <- median(wf.res$theta)
#variance <- var(wf.res$theta)

###  Plot results
## wordfish plot sorted by estimate
# plot(wf.res) 

## ggplot with nice customization 
thetas <- data.frame(wf.res$docs, wf.res$theta, wf.res$theta - (1.96*wf.res$se.theta),
                     wf.res$theta + (1.96*wf.res$se.theta))
names(thetas)[1] <- "Country"
names(thetas)[2] <- "mean"
names(thetas)[3] <- "lower"
names(thetas)[4] <- "upper"
rownames(thetas) <- NULL

# To order by score not by document
# thetas$Country <- factor(thetas$Country, 
#                         levels = thetas[order(thetas$mean), "Country"])

p1 <- ggplot(thetas, aes(x=Country, y=mean, group=1)) +
  geom_errorbar(width=.1, aes(ymin=lower, ymax=upper)) +
  geom_point(shape=20, size=4) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(thetas$mean), linetype = "dashed") +
  coord_flip() + theme_bw() + xlab("") + ylab("") +
  ggtitle("Positions of CDU and CSU Leaders 1990-2011")
ggsave(p1, file="figures/Positions of CDU and CSU Leaders 1990-2011.pdf", width=10, height=8)


###  Plot only subsets of the data
# wf.res$docs # look at number of documents
# wf.res.subset <- wf.res # create subset variable 
# wf.res.subset$docs <- wf.res.subset$docs[c(10, 19, 26, 34, 39, 42, 58)] # select documents 1 and 2 and 5
# wf.res.subset$data <- wf.res.subset$data[,c(10, 19, 26, 34, 39, 42, 58)] # select estimates for documents 1 and 2 and 5
# wf.res.subset$docs <- gsub("([0-9]*)", "",wf.res.subset$docs, perl=T) # remove .txt from list of speakers
# wf.res.subset$docs <- gsub(".txt", "",wf.res.subset$docs) # remove .txt from list of speakers
# wf.res.subset$docs <- gsub("-$", "",wf.res.subset$docs, perl=T) # remove trailing whitespaces
# wf.res.subset$docs <- gsub("^-", "",wf.res.subset$docs, perl=T) # remove trailing whitespaces
# wf.res.subset$docs <- gsub("-", " ",wf.res.subset$docs) # remove trailing whitespaces
# wf.res.subset$docs <- gsub("Franois", "Francois",wf.res.subset$docs) # change name 
# wf.res.subset$docs <- gsub("JeanLuc", "Jean-Luc",wf.res.subset$docs) # change name
plot(wf.res, xlab="Wordfish estimates", 
     main="Positions of CDU and CSU Leaders 1990-2011", col.main="black") # plot subset

### Looking at individual Words
word.coefs <- coef(wf.res, "poisson")$words # saves word coefficients in the object word.coefs

### Plot Word coefficients

# wds <- c("entrepris", "développ", "import", "inégal", "travail", "solidar") # Select Words you wish to plot
# word.coefs[wds, ]
# dotchart(sort(word.coefs[wds, "beta"], decreasing = TRUE), wds, 
#         main="Ideological Position of Words Describing the Economy",
#         xlab = "Word Weights") 

### Eifel Tower Plot (Word fixed effects and weights)

#plot(word.coefs$beta[ord],word.coefs$psi[ord], type='n', 
#     xlab="Word Weights",ylab="Word Fixed Effect", 
#      main="Word Fixed Effects and Word Weights of PS Speakers at the 2000 Congress in Grenoble", cex.main = .8)

# text(word.coefs$beta, 
#     word.coefs$psi,
#     row.names(word.coefs),col="grey") # Text function is used to highlight indivual words

# selector <- 1:10 # use selector object to define words that are supposed to be highlighted
# text(word.coefs$beta[selector],
#     word.coefs$psi[selector],
#     row.names(word.coefs)[selector]) 

##################################################################################################################
##################################################################################################################
######################################### Saving Results as CSV ##################################################
##################################################################################################################
##################################################################################################################

#### Store and save estimates ####
speaker <- gsub(".txt", "",wf.res$docs) # remove .txt from list of speakers
speaker <- gsub(" $", "",speaker, perl=T) # remove trailing whitespaces
out.dat <- data.frame(speaker = speaker, position = wf.res$theta, st.error= wf.res$se.theta, 
                      mean = mean, median = median, variance = variance, stringsAsFactors = F)
write.csv(out.dat, file ="D:/Desktop/Research/Party Congresses/PS Analysis/", quote=F, fileEncoding = "UTF8")