#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@Revise 3-way mate choice frequency plots 5.7.15@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# the plot used for the three way mate choice is correct, I just have to 
# add a white bar across the bar plots to signify the null expectation.
# I am using code from "3-way mate choice trial data 6.26.14" and 
# using the workspace titled "workspace for 6-24-14"

# code below is copied directly from 7.28 update portion, but now include 
# a row of code for the white line at .50 mark!

three.way.data4 = read.csv("compile 3-way mate choice data 7.8.csv")

library(ggplot2)
library(plyr)
library(reshape)
library(reshape2)

t(three.way.data4)
three.way.data4 = data.frame(three.way.data4)
three.way.data4$diet <- c("starch","CMY","CMY","starch")
melt.data.frame(three.way.data4, id.var = c("mating.pattern","chooser", "total.matings",
                                            "freq.assort", "p..value", "CI", "diet"))
three.way.dataframe4 <- melt.data.frame(three.way.data4, id.var = c("mating.pattern","chooser", "total.matings",
                                                                    "freq.assort", "p..value", "CI", "diet"))


t(three.way.dataframe4)
factor(three.way.dataframe4$chooser)
levels(three.way.dataframe4$chooser)
levels(three.way.dataframe4$chooser) <- c("female","female","male","male","Female",
                                          "female","male", "male")
colnames(three.way.dataframe4)[2] <- "Chooser"    

three.way.dataframe4[5,4] = three.way.dataframe4[5,9]/three.way.dataframe4[5,3]
three.way.dataframe4[6,4] = three.way.dataframe4[6,9]/three.way.dataframe4[6,3]
three.way.dataframe4[7,4] = three.way.dataframe4[7,9]/three.way.dataframe4[7,3]
three.way.dataframe4[8,4] = three.way.dataframe4[8,9]/three.way.dataframe4[8,3]

colnames(three.way.dataframe4)[8] <- "Pattern"

levels(three.way.dataframe4$Mating.pattern) <- c("assortative","disassortative") 


ggplot(aes(x=Chooser,y=freq.assort),data=three.way.dataframe4)+
  geom_bar(stat="identity")+aes(fill=Pattern)+
  scale_fill_manual(values= c("black","grey"))+
  ylab("Frequency")+
  facet_wrap(~diet)+
  theme_bw(base_size = 12, base_family = "")+
  theme(panel.grid.major.x=element_line(colour=NA))+
  theme(panel.grid.major.y=element_line(colour=NA))+
  theme(axis.title.x=element_text(size=11))+
  theme(axis.title.y=element_text(size=11))+
  theme(axis.text.x=element_text(size=9))+
  theme(axis.text.y=element_text(size=11))+
  labs(title="Frequency of assortative matings")+
  theme(title=element_text(size=11))+
  geom_abline(intercept=.50,colour="White",slope=0,size=1)
  