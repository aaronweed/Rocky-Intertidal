### Summarizes data for plotting canopy species cover along point-intercept transects
# Each site contains three of these transects that extend from the high intertidal to the low intertidal. 
# The top-most species or substrates along the transects are recorded at every 0.3 m. 
# Data collected with this SOP will allow for an across-shore estimate of canopy species cover and an estimate of the elevational range for each canopy species detected.


library(plyr)
library(ggplot2)
library(reshape)
library(reshape2)
##### convience functions for aggreagtion

  range<-function (x) c(min = min(x),max= max(x))
  summ<-function (x) c(mean =round(mean(x,na.rm = TRUE),3),sd= round(sd(x,na.rm = TRUE),3))
  

##### IMPORT DATA FOR SUMMARY ANALYSIS AND PLOTTING #########

### Species coverages from point-intercept methods (SOP 17) (Access qry)
PI_data <- read.csv("qryR_FlatFile_PointIntercept_SppDetections.csv")
  head(PI_data)
  PI_data$Start_Date<-as.Date(PI_data$Start_Date, format= "%m/%d/%Y") #convert to StartDate
  PI_data$Year<-as.factor(format(PI_data$Start_Date,"%Y")) #convert to year
  
### lookup table of species names
species <- read.csv("tlu_Point_Intercept_Species.csv")
head(species)

#### lookup tables for site names
tlu_sites<-unique(PI_data[,c("Site_Name","Site_Code","Loc_Name","Loc_Code")])
write.table(tlu_sites, "tlu_sites.csv", sep=",", row.names=F)
## Bolt elevation and placement along annualy placed transects (Access qry)

bolt_data <- read.delim("~/R/NETN/Rocky-Intertidal/qryR_FlatFile_PointIntercept_BoltDist_partc.txt")
bolt_data$Start_Date<-as.Date(bolt_data$Start_Date, format= "%m/%d/%Y") #convert to StartDate
bolt_data$Year<-as.factor(format(bolt_data$Start_Date,"%Y")) #convert to year
head(bolt_data)
bolt.m<-melt(bolt_data, id.vars=c("Site_Code", "Loc_Code" ,"QAQC","Start_Date", "Year", "Label"), measure.vars=c("Distance_m"))

###### BOLT DATA #########
# Field techs lay a measuring tape each year along the transect, but it is possible that each RTK'd bolt doesn't fall at the exact distance along the transect each year.
# This may bias the assocaition of elevation, based ont he bolts location, of each cover type over time.  
# How much does each bolt position vary along the transcet in each year?
names(bolt_data)

bolt.summ<-cast(bolt_data, Site_Code + Loc_Code + QAQC+ Label ~ . , value= "Distance_m", fun = range, subset= bolt_data$QAQC == "0")# excluding QAQC plots
bolt.summ$diff<- bolt.summ$max-bolt.summ$min

y2<-ggplot(bolt.summ, aes(diff)) + geom_histogram() + labs(y = "Number bolts", x= "Difference in m")
  
y2+ facet_wrap(~Loc_Code)

median(bolt.summ$diff)



#### view bolt 

########## SUMARIZE POINT-INTERCEPT DATA ###################
head(PI_data)

PI.m<-melt(PI_data, id.vars=c("Site_Name", "Site_Code",  "Loc_Name","Loc_Code" ,"QAQC", "Plot_Name","Start_Date", "Year", "Spp_Code"), measure.vars=c("PI_Distance"))
head(PI.m)

### export raw data for  use in R viz
### Bind species names and site names for final output
PI.raw.R<-join(PI.m,species, by= "Spp_Code")
head(PI.raw.R)
colnames(PI.raw.R)[11]<-"distance_m"
PI.raw.R$variable<-NULL
write.table(PI.raw.R, "./Data/site_transect_cover_raw.csv", sep=",", row.names= FALSE)

#################################################################################
############ SITE-LEVEL ESTIMATES ###############
#################################################################################

### P-I data typically summarized across entire transect as the number of "hits" or freq of obs of each species along transect. 
# You then calculate the porprotion of hits for each species from the total to esimtate the relative cover

## First need to sum the number of occurences of each species per transect per year at each site, adding in the 0
# observations for species not encountered along a transect

PI_spp.hits<-cast(PI.m, Loc_Code +Year+Plot_Name+ QAQC+  Spp_Code ~ . , fun = length, add.missing = TRUE, fill = 0) # number of hits of each species per transect
colnames(PI_spp.hits)[6]<-"total_hits"# rename sum column
head(PI_spp.hits)

# should be this number of rows"
length(levels(PI_data$Loc_Code))*length(levels(PI_data$Year))*length(levels(PI_data$Spp_Code))*length(levels(PI_data$Plot_Name))

# then calculate the total number of obs taken per transect
PI_obs<-cast(PI.m,Site_Name+ Site_Code +Loc_Name+  Loc_Code +Year+Plot_Name+ QAQC ~ . , fun = length) # number of total points sampled per transect
colnames(PI_obs)[8]<-"total_points"# rename sum column
head(PI_obs)

### Bind the total obs to the species hits to calculate the portional cover (# hits per species/# obs)
PI.site<-join(PI_spp.hits,PI_obs,by =c("Loc_Code", "Year", "Plot_Name", "QAQC"))
head(PI.site)

PI.site$prop_cover<-round(PI.site$total_hits/PI.site$total_points,3)
head(PI.site)

PI.site<-PI.site[PI.site$Spp_Code != "",]
### rearrange cols

PI.site<-PI.site[,c("Site_Name","Loc_Name","Year","Plot_Name","QAQC", "total_hits","total_points", "prop_cover")]


#################################################################################
#### Calc the average of all 3 transects sampled IN EACH YEAR 
#################################################################################

PI.site.yr<-cast(PI.site, Loc_Name+  Year+ QAQC+Spp_Code ~ . , value= "prop_cover", fun = summ, add.missing = TRUE, fill= NA) 
View(PI.site.yr)
PI.site.yr$se<-round(PI.site.yr$sd/sqrt(3)) # calc the SE based on 3 transects per year
# drop rows with blank species info and sort
PI.site.yr<-PI.site.yr[PI.site.yr$Spp_Code != "",]
head(PI.site.yr)
 
# drop QAQC plots
PI.site.yr<-PI.site.yr[PI.site.yr$QAQC == "0",]

# length should be 3 for each row (species*site combo)

# Bind back park names for indexing/plotting
PI.site.yr<-join(PI.site.yr,tlu_sites, by= "Loc_Name")

### What are the 10 most dominat cover types in each park?

top10ACAD<-cast(PI.site, Spp_Code ~ . , value= "prop_cover", fun = summ, subset= PI.site$Site_Code == "ACAD") 
top10ACAD<-top10ACAD[order(top10ACAD$mean, decreasing = T),]
top10ACAD<-top10ACAD[1:10,]; top10ACAD<-droplevels(top10ACAD)
top10ACAD<-unique(levels(top10ACAD$Spp_Code))

top10BOHA<-cast(PI.site, Spp_Code ~ . , value= "prop_cover", fun = summ, subset= PI.site$Site_Code == "BOHA") 
top10BOHA<-top10BOHA[order(top10BOHA$mean, decreasing = T),]
top10BOHA<-top10BOHA[1:10,]; top10BOHA<-droplevels(top10BOHA)
top10BOHA<-unique(levels(top10BOHA$Spp_Code))

### Bind species names for final plotting
PI.site.plot<-join(PI.site.yr,species, by= "Spp_Code")
PI.site.plot<-PI.site.plot[order(PI.site.plot$Site_Name,PI.site.plot$Loc_Name,PI.site.plot$Year,PI.site.plot$Common_Name),]

PI.site.plot<-PI.site.plot[!is.na(PI.site.plot$Loc_Name),]
PI.site.plot<-droplevels(PI.site.plot)
View(PI.site.plot)

#### Extract top 10 cover types for final plotting 
ACAD.temp<-PI.site.plot[PI.site.plot$Site_Code == "ACAD" & PI.site.plot$Spp_Code %in% top10ACAD,];ACAD.temp<-droplevels(ACAD.temp)
BOHA.temp<-PI.site.plot[PI.site.plot$Site_Code == "BOHA" & PI.site.plot$Spp_Code %in% top10BOHA,];BOHA.temp<-droplevels(BOHA.temp)

PI.site.plot<-rbind(ACAD.temp,BOHA.temp)

### export annual means from top 10 species/covertypes for  use in R viz
write.table(PI.site.plot, "./Data/site_transect_cover_per_year.csv", sep=",", row.names= FALSE)


# first reorder names labels for correct plotting
spp_names<-rev(levels(PI.site.plot$Common_Name))

y2<-ggplot(PI.site.plot[PI.site.plot$QAQC == 0,], aes(x=Common_Name, y= as.numeric(mean), fill= Year))+
  geom_bar(stat ="identity", position = dodge) + labs(y = "Mean cover + SE", x= "") +
  geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+scale_fill_brewer(palette="Blues")

# y2
# summary(y2)
y2<-(y2+facet_wrap(~Site_Name + Loc_Name) + 
       theme(legend.position = "top") +coord_flip()+
       theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 12 * 0.8,face="bold"))+
       theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14 * 0.8, face="bold")) +
       theme(strip.text.x= element_text(size=10, face=c("bold.italic"))) +
       theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(panel.background =  element_rect(fill="white", colour="black")) +
       theme(panel.grid.major = element_line(colour = "grey90"))+
       theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
       scale_x_discrete(limits=spp_names)+
       theme(strip.background= element_rect(size=10, color="gray" )))
y2

#################################################################################
#### Now take the average of all 3 transects sampled OVER THE ENTIRE STUDY PERIOD
#################################################################################

PI.site.sum<-cast(PI.site, Loc_Code + QAQC+Spp_Code ~ . , value= "prop_cover", fun = summ) 
head(PI.site.sum)
PI.site.N<-cast(PI.site,  Loc_Code + QAQC+Spp_Code ~ . , value= "prop_cover", fun = length) 
head(PI.site.N)
colnames(PI.site.N)[4]<-"N"# rename sum column
## Bind together (won't calc length with other metrics for some reason)
PI.site.final<-join(PI.site.sum,PI.site.N, by=c("Loc_Code", "Spp_Code", "QAQC"))
PI.site.final$se<-PI.site.final$sd/sqrt(PI.site.final$N)

# drop rows with blank species info
PI.site.final<-PI.site.final[PI.site.final$Spp_Code != "",]

#View(PI.site.final)

### Bind species names and site names for final plotting
PI.site.plot<-join(PI.site.final,species, by= "Spp_Code")
PI.site.plot<-join(PI.site.plot,tlu_sites, by= "Loc_Code")

#### Plot avg cover per site ##
# drop species that have not been observed 
PI.site.plot<-PI.site.plot[PI.site.plot$mean != 0,]
PI.site.plot<-droplevels(PI.site.plot)

### export to use in R viz
write.table(PI.site.plot, "./Data/site_transect_cover.csv", sep=",", row.names= FALSE)

# first reorder names labels for correct plotting
spp_names<-rev(levels(PI.site.plot$Common_Name))

y2<-ggplot(PI.site.plot[PI.site.plot$QAQC == 0,], aes(x=Common_Name, y= as.numeric(mean)))+
  geom_bar(stat ="identity", position = dodge) + labs(y = "Mean proportion of cover + SE", x= "") +
  geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)

# y2
# summary(y2)
y2<-(y2+facet_wrap(~Site_Name + Loc_Name) + 
       theme(legend.position = "top") +coord_flip()+
       theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 12 * 0.8,face="bold"))+
       theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14 * 0.8, face="bold")) +
       theme(strip.text.x= element_text(size=10, face=c("bold.italic"))) +
       theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(panel.background =  element_rect(fill="white", colour="black")) +
       theme(panel.grid.major = element_line(colour = "grey90"))+
       theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
       scale_x_discrete(limits=spp_names)+
       theme(strip.background= element_rect(size=10, color="gray" )))
y2

