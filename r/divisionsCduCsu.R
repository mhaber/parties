library(ggplot2)
library(ggthemes)
library(scales)
library(RCurl)
library(XML)
library(dplyr)
library(lubridate)

## Custom Theme
theme_Publication <- function(base_size=14, base_family="sans") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
  + theme(plot.title = element_text(face = "bold",
                                    size = rel(1.2), hjust = 0.5),
          text = element_text(),
          panel.background = element_rect(colour = NA),
          plot.background = element_rect(colour = NA),
          panel.border = element_rect(colour = NA),
          axis.title = element_text(face = "bold",size = rel(1)),
          axis.title.y = element_text(angle=90,vjust =2),
          axis.title.x = element_text(vjust = -0.2),
          axis.text = element_text(hjust = 1),
          axis.line = element_line(colour="black"),
          axis.ticks = element_line(),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.size= unit(0.2, "cm"),
          legend.margin = unit(0, "cm"),
          legend.title = element_text(face="italic"),
          plot.margin=unit(c(10,5,5,5),"mm"),
          strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold")
  ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}


#################
#Public Opinion #
#################
#Politbarometer: Divisions inside CDU-CSU
year <- c(2013,2012,2009,2008,2006,2005,2004,2003,2001)
divided <- c(1059,927,1098, 758, 1948,3020,4028,1560,555)
united  <- c(575, 603, 431, 714, 1111, 1686,2100,1557,920)
divisions <- data.frame("divided" = divided / (divided+united) * 100,as.factor(year))

# plot results
p4 <- ggplot(divisions, aes(y=divided, x=year)) +
  geom_line(linetype = 2, size = 1) +
  #stat_smooth(size=1, se = F, color = "#000000") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  theme_Publication() + xlab("") + ylab("% of respondents") +
  theme(text = element_text(size = 16),
        axis.title.y=element_text(vjust=1),
        plot.title=element_text(vjust=1)) + 
  ggtitle("Respondents who Think the CDU-CSU is Internally Divided (Politbarometer)")
ggsave(p4, file="figures/Internal Divisions within CDU-CSU.pdf", width=10, height=6)
save(p4, file="figures/p4.RData")

#######################
#Discipline Bundestag #
#######################

#roll call votes
votesCDUCSUBundestag16 <- c(89,92,92,84,99,88,87,90,85,90,96,90,89,93,97,92, 75,91,90,85,90,89,79,
                            46,89,86,91,96,95,91,90,96,91,92,79,87,90,94,89,89,84,96,78,93,88,96,
                            91,89,90,85)
votesCDUCSUBundestag17 <- c( 97,97,96,97,96,95,95,95,95,93,93,95,90,98,96,92,95,93,95,92,95,92,92,94,
                             95,96,96,95,92,89,97,92,94,94,93,92,92,93,95,93,91,97,95,95,95,93,65,94,
                             89,88,90)

#get dates from abgeordnetenwatch
url1 = "http://www.abgeordnetenwatch.de/abstimmungen-346-0.html"
html1 = htmlParse(url1)
dates16 = xpathSApply(html1, "//div[@class='datum']", xmlValue)
datesBundestag16 <- as.Date(dates16, "%d.%m.%Y")

url2 = "http://www.abgeordnetenwatch.de/abstimmungen-991-0.html"
html2 = htmlParse(url2)
dates17 = xpathSApply(html2, "//div[@class='datum']", xmlValue)
datesBundestag17 <- as.Date(dates17, "%d.%m.%Y")
datesBundestag17 <- datesBundestag17[43:93]

#combine dates and votes
factionDiscipline <- data.frame("discipline" = c(votesCDUCSUBundestag16,votesCDUCSUBundestag17),
                                "date" = c(datesBundestag16, datesBundestag17))
factionDiscipline <- factionDiscipline %>% arrange(date) # order by date

#plot results

p5 <- ggplot(factionDiscipline, aes(y=discipline, x=date)) +
  #stat_smooth(se = F, color = "#FF9900") +
  geom_line(color="black") +
  scale_x_date(labels = date_format("%m/%Y"), breaks = date_breaks("3 month")) +
  geom_vline(aes(xintercept=as.numeric(date[55])),linetype = "solid",size=1,color="#999999") +
  geom_hline(yintercept=mean(factionDiscipline$discipline),linetype="dashed",color="#999999") +
  theme_Publication() + xlab("") + ylab("% of Factional Discipline") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.2),
        axis.title.y=element_text(vjust=1),
        plot.title=element_text(vjust=1)) + 
  ggtitle("Factional Discipline CDU-CSU German Bundestag 2005 - 2011")
ggsave(p5, file="figures/Factional Discipline CDU-CSU German Bundestag 2005-2011.pdf",
       width=12, height=6)
save(p5, file="figures/p5.RData")
