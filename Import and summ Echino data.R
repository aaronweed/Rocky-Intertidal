library(plyr)
library(ggplot2)
library(reshape)


# import count data of echinos


echino <- read.csv("~/R/NETN/Rocky-Intertidal/qryR_FlatFile_Echinoderm_Counts_wide.csv")
echino_size <- read.csv("~/R/NETN/Rocky-Intertidal/NETN_Echinoderm_Measurements.csv")
tlu_echino_spp <- read.csv("~/R/NETN/Rocky-Intertidal/tlu_echino_Spp.csv")

tlu_echino_sites<-unique(echino[,c("Site_Name","Loc_Name")])

############################################################################################
#############        Setup count df's for summary            #########################
############################################################################################

############################ Echinos#############################
## 3 transects per site, 3 obs per transect; each obs is a different row but not labeled by obs

echino$Start_Date<-as.Date(echino$Start_Date, format= "%m/%d/%Y") #convert to StartDate
echino$Year<-as.factor(format(echino$Start_Date,"%Y")) #convert to Year

head(echino)
## create molten data frame (all values are represented for each site*time combination)
echino.melt<-melt(echino, id.vars=c("Site_Name", "Loc_Name" ,"Start_Date", "Year", "Plot_Name", "QAQC"), measure.vars=c("Count_STRDRO", "Count_HENSAN", "Count_ASTRUB", "Count_ASTFOR"))
head(echino.melt)

#### scale counts to a per m2 basis; each transect is 20m2
echino.melt$Abundance<-echino.melt$value/20

## create logscaled values
echino.melt$logAbundance<-log(echino.melt$Abundance+1) # calc log
echino.melt$logAbundance[is.infinite(echino.melt$logAbundance)]=0

### rename species names
echino.melt$Spp_Name<-echino.melt$variable
echino.melt$Spp_Name<-mapvalues(echino.melt$Spp_Name, from=c("Count_STRDRO", "Count_HENSAN", "Count_ASTRUB", "Count_ASTFOR"), 
                                to=c("Strongylocentrotus droebachiensis", "Henricia sanguinolenta", "Asterias rubens", "Asterias forbesii"))
echino.melt$variable<-NULL
              
head(echino.melt)

## recreate molten data frame to include logAbundance (all values are represented for each site*time combination)

 echino.melt2<-melt(echino.melt, id.vars=c("Site_Name", "Loc_Name" ,"Start_Date", "Year", "Plot_Name", "QAQC","Spp_Name"), measure.vars=c("Abundance","logAbundance"))
 head(echino.melt2)             
 max(echino.melt2$QAQC)

 #### Export as raw data for R viz downloader
 # add in species names
 echino.raw<-join(echino.melt2, tlu_echino_spp, by ="Spp_Name")
 echino.raw<-echino.raw[,c("Site_Name", "Loc_Name" ,"Start_Date", "Year", "Plot_Name", "QAQC","Com_Sp", "variable","value")]
 head(echino.raw)
 
 ### export to use in R viz
 write.table(echino.raw, "./Data/echino_count_raw.csv", sep= ",", row.names= FALSE)
 
 
############ Summarize species counts by site, zone, and Year
summ<-function (x) c(mean = round(mean(x,na.rm = TRUE),2),se= round(sd(x,na.rm = TRUE)/sqrt(length(!is.na(x))),2), N= length(!is.na(x)))

# aggregate data by site and year leaving out park to fill in missing obs with 0s correctly and remove the QAQC plots 
sum.echino<-cast(echino.melt2, Loc_Name + Year +QAQC+ Spp_Name + variable ~ . , value = "value", fun = summ, fill=0, add.missing = TRUE , subset= echino.melt2$QAQC == "0")

# add back SIte_name
sum.echino<-join(sum.echino, tlu_echino_sites, by ="Loc_Name")
# add in species names
sum.echino<-join(sum.echino, tlu_echino_spp, by ="Spp_Name")

head(sum.echino)

### export to use in R viz
write.table(sum.echino, "./Data/echino_count.csv", sep= ",", row.names= FALSE)


