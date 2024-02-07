#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@ revising box plots of latency and duration @
#@@@@@@@@   with new names 5.7.15   @@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Based on the recommendation of Bill at my committtee meeting, 
# The labelling system for arrangements 1 through 4 is difficult to read
# and keep track of so I am changing them to abbreviation of sex and diet of chooser.

# below I have take the code from R file "revised boplots of 3way and 4way for CL
# CD assort disassort 8.28.14" and will create a new workspace for this r file.
# the ggplot code come from rows 327 - 356.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#step 1: create save work space
setwd("C:/Program Files/RStudio/Man R-directory")

CLandCDforbothexp = read.csv("raw CL and CD data for three and fourway matings boxplot  8.25.14.csv")
colnames(CLandCDforbothexp)[4] <- "mating.arrangement"


#step 2: split the data to extract out only the 3way data
splitboxplot <- CLandCDforbothexp[647:1394,]


#step 3: Change the arrangement numbers to acronyms:
mode(splitboxplot$mating.arrangement)
levels(splitboxplot$mating.arrangement)

#splitboxplot$mating.arrangement <- as.character(levels(splitboxplot$mating.arrangement))[splitboxplot$mating.arrangement]

splitboxplot$newarrangement <- as.character(c(splitboxplot$mating.arrangement))
mode(splitboxplot$newarrangement)
levels(splitboxplot$newarrangement)

splitboxplot[1:89,17] <- "FS" 
splitboxplot[90:202,17] <- "FC"
splitboxplot[203:308,17] <- "MC"
splitboxplot[309:396,17] <- "MS" 

splitboxplot[397:503,17] <- "FS" 
splitboxplot[504:591,17] <- "FC" 
splitboxplot[592:670,17] <- "MC"
splitboxplot[671:748,17] <- "MS" 

#step 4: plot the 3way CL data just like before, only now have a facet wrap to put
# assort adn disassort side by side.
ggplot(aes(x=newarrangement,y=CL),data=splitboxplot)+geom_boxplot(notch = TRUE)+
  ylab("Min")+facet_wrap(~mating.pattern)+theme_bw(base_size = 12,base_family= "")+theme_bw(base_size = 12, base_family = "")+
  theme(panel.grid.major.x=element_line(colour=NA))+
  theme(panel.grid.major.y=element_line(colour=NA))+
  labs(title="Copulation latency across three-way matings")+
  xlab("mating arrangement")+
  theme(title=element_text(size=11))

#step 5: plot the 3way CD data just like before, only now have a facet wrap to put
# assort adn disassort side by side.
ggplot(aes(x=newarrangement,y=CD),data=splitboxplot)+geom_boxplot(notch = TRUE)+
  ylab("Min")+facet_wrap(~mating.pattern)+theme_bw(base_size = 12,base_family= "")+theme_bw(base_size = 12, base_family = "")+
  theme(panel.grid.major.x=element_line(colour=NA))+
  theme(panel.grid.major.y=element_line(colour=NA))+
  labs(title="Copulation duration across three-way matings")+
  xlab("mating arrangement")+
  theme(title=element_text(size=11))