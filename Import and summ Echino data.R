library(plyr)
library(ggplot2)
library(reshape)


# import count data of echinos


echino <- read.csv("~/R/NETN/Rocky-Intertidal/qryR_FlatFile_Echinoderm_Counts_wide.csv")
echino_size <- read.csv("~/R/NETN/Rocky-Intertidal/NETN_Echinoderm_Measurements.csv")

############################################################################################
#############        Setup count df's for summary            #########################
############################################################################################

############################ Echinos#############################
## 3 transects per site, 3 obs per transect; each obs is a different row but not labeled by obs

echino$Start_Date<-as.Date(echino$Start_Date, format= "%m/%d/%Y") #convert to StartDate
echino$Year<-as.factor(format(echino$Start_Date,"%Y")) #convert to Year

head(echino)
## create molten data frame (all values are represented for each site*time combination)
echino.melt<-melt(echino, id.vars=c("Site_Name", "Loc_Name" ,"Start_Date", "Year", "Plot_Name"), measure.vars=c("Count_STRDRO", "Count_HENSAN", "Count_ASTRUB", "Count_ASTFOR"))
head(echino.melt)

### rename species names
echino.melt$variable<-mapvalues(echino.melt$variable, from=c("Count_STRDRO", "Count_HENSAN", "Count_ASTRUB", "Count_ASTFOR"), 
                                               to=c("Strongylocentrotus droebachiensis", "Hemigrapsus sanguineus", "Asterias rubens", "Asterias forbesii"))
head(echino.melt)
############ Summarize species counts by site, zone, and Year
summ<-function (x) c(mean =mean(x,na.rm = TRUE),se= sd(x,na.rm = TRUE)/sqrt(length(!is.na(x))), N= length(!is.na(x)))

# aggregate data by site and year

sum.echino<-cast(echino.melt, Site_Name+ Loc_Name + Year + variable ~ . , value = "value", fun = summ, fill=NA, add.missing = TRUE)


head(sum.echino)

### export to use in R viz
write.table(sum.echino, "./Data/echino_count.csv", sep=",", row.names= FALSE)


