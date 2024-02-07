#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@REvision of the ppooled frequencieso f 4 way mate choice trial@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#5.1.14
# in preparing for my thesis presentation on 5.5.15, I noticed that
# the column of the allral goes beyond 100% frequecny--something is wrong.
# the graph needs to be remade again.

# the code below is copied from the R file "frequencies for masters thesis 4-6.R"
# within this file, there is an addendum I made on 7.28 to give the black and
# white graph. I will modifiy this code to provide the graph I need.

# For the workspace, I will upload the file titled: workspace 4.6.14
# NOTE: THE DATEFRAME COMPLETEPOOLEDDATA2 IN THE CODE HAS INCORRECT VALUES
#        FOR ALL RAL AND DOESN'T INCLUDE REVISED SHARON DATA!
# these values are revised in the data frame completepooleddata4.0 in workspace...
# code below is cleaned up but not needed...


# create new vectors of each column from the completepooleddata2 file
treatment <-as.vector(completepooleddata2$treatment,mode = "character")
date <-as.vector(completepooleddata2$date,mode = "numeric")
population <- as.vector(completepooleddata2$population,mode = "character")
Strain <- as.vector(completepooleddata2$strain,mode = "character")
total.matings <- as.vector(completepooleddata2$total.matings,mode = "numeric")
assortative.count <- as.vector(completepooleddata2$assortative.count,mode = "numeric")
disassortative.count <- as.vector(completepooleddata2$dissasortative.count,mode = "numeric")
Pattern <- as.vector(completepooleddata2$mating.direction,mode = "character")
Frequency <- as.vector(completepooleddata2$frequency,mode = "numeric")

#now build a data frame from vectors
completepooleddata4.0 = data.frame(treatment,date,Strain,population,total.matings,assortative.count,
                                   disassortative.count,Mating.pattern,frequency)

#change numbers for sharon
completepooleddata4.0[5,5] <- 385
completepooleddata4.0[6,5] <- 385
completepooleddata4.0[5,6] <- 236
completepooleddata4.0[6,6] <- 236
completepooleddata4.0[5,7] <- 149
completepooleddata4.0[6,7] <- 149
completepooleddata4.0[1,6] <- 157
completepooleddata4.0[2,6] <- 157
completepooleddata4.0[1,7] <- 145 
completepooleddata4.0[2,7] <- 145
completepooleddata4.0[1,9] <- 157/302
completepooleddata4.0[5,9] <- 236/385
completepooleddata4.0[6,9] <- 149/385

library(ggplot2)
library(plyr)
library(reshape2)
library(reshape)



#more modifications on 7.28- changing column names for legend
ggplot(aes(x=Strain,y=frequency),data=completepooleddata4.0)+
  geom_bar(stat="identity")+(aes(fill=Pattern))+
  ylab("Frequency")+
  scale_fill_manual(values= c("black","grey"))+
  theme_bw(base_size = 12, base_family = "")+
  theme(panel.grid.major.x=element_line(colour=NA))+
  theme(panel.grid.major.y=element_line(colour=NA))+
  theme(axis.title.x=element_text(size=11))+
  theme(axis.title.y=element_text(size=11))+
  theme(axis.text.x=element_text(size=9))+
  theme(axis.text.y=element_text(size=11))+
  labs(title="Frequency of mating patterns")+
  theme(title=element_text(size=11))




#@@@@@@@@@@@@@@@@@@
#@@update: 5.7.15@@
#@@@@@@@@@@@@@@@@@@
# based on the thesis defense, bill recommended I put a line at .50 freq
# to display the location of null hypothesis. Here I will use the 4 way freq.
# and add a line to it.

ggplot(aes(x=strain,y=frequency),data=completepooleddata4.0)+
  geom_bar(stat="identity")+(aes(fill=pattern))+
  ylab("Frequency")+
  scale_fill_manual(values= c("black","grey"))+
  theme_bw(base_size = 12, base_family = "")+
  theme(panel.grid.major.x=element_line(colour=NA))+
  theme(panel.grid.major.y=element_line(colour=NA))+
  theme(axis.title.x=element_text(size=11))+
  theme(axis.title.y=element_text(size=11))+
  theme(axis.text.x=element_text(size=9))+
  theme(axis.text.y=element_text(size=11))+
  labs(title="Frequency of mating patterns")+
  theme(title=element_text(size=11))+
  geom_abline(intercept=.50,colour="White",slope=0,size=1)


