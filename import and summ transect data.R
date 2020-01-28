### Summarizes data for plotting canopy species cover along point-intercept transects
# Each site contains three of these transects that extend from the high intertidal to the low intertidal. 
# The top-most species or substrates along the transects are recorded at every 0.3 m. 
# Data collected with this SOP will allow for an across-shore estimate of canopy species cover and an estimate of the elevational range for each canopy species detected.


library(plyr)
library(ggplot2)
library(reshape)
library(reshape2)
library(tidyverse)
library(readxl)
library(magrittr)

##### IMPORT DATA FOR SUMMARY ANALYSIS AND PLOTTING #########

### Species coverages from point-intercept methods (SOP 17) (Access qry)
PI_data <- read_excel("Data/Data from DB/2019/qryR_FlatFile_PointIntercept_SppDetections.xlsx")
  head(PI_data)
  PI_data$Start_Date<-as.Date(PI_data$Start_Date, format= "%m/%d/%Y") #convert to StartDate
  PI_data$Year<-as.factor(format(PI_data$Start_Date,"%Y")) #convert to year
  
### lookup table of species names
species <- read.csv("./Data/Look ups/tlu_Point_Intercept_Species.csv")
head(species)

#### lookup tables for site names
tlu_sites<-unique(PI_data[,c("Site_Name","Site_Code","Loc_Name","Loc_Code")])
write.table(tlu_sites, "./Data/Look ups/tlu_sites.csv", sep=",", row.names=F)

## Bolt elevation and placement along annualy placed transects (Access qry)
bolt_data <- read_excel("Data/Data from DB/2019/qryR_FlatFile_PointIntercept_BoltDist_partc.xlsx")
bolt_data$Start_Date<-as.Date(bolt_data$Start_Date, format= "%Y-%m-%d") #convert to StartDate
bolt_data$Year<-as.factor(format(bolt_data$Start_Date,"%Y")) #convert to year
head(bolt_data)

bolt.m<-bolt_data %>% 
  gather(variable, value,-Site_Code, -QAQC, -Start_Date,-Year,-Label,-Elevation_MLLW_m) %>% 
  select(Site_Code, QAQC, Start_Date,Year,Label,Elevation_MLLW_m, variable, value)

###### BOLT DATA #########
# Field techs lay a measuring tape each year along the transect, but it is possible that each RTK'd bolt doesn't fall at the exact distance along the transect each year.
# This may bias the assocaition of elevation, based ont he bolts location, of each cover type over time.  
# How much does each bolt position vary along the transcet in each year?
names(bolt_data)

### THIS CODE CHUNK HAS NOT BEEN UPDATED SINCE 2017

bolt.summ<-cast(bolt_data, Site_Code + Loc_Code + QAQC+ Label ~ . , value= "Distance_m", fun = range, subset= bolt_data$QAQC == "0")# excluding QAQC plots
bolt.summ$diff<- bolt.summ$max-bolt.summ$min

y2<-ggplot(bolt.summ, aes(diff)) + geom_histogram() + labs(y = "Number bolts", x= "Difference in m")
  
y2+ facet_wrap(~Loc_Code)

median(bolt.summ$diff)


########## SUMARIZE POINT-INTERCEPT DATA ###################
head(PI_data)

PI.m<-PI_data %>% 
  gather(variable, distance_m, -Site_Name, -Site_Code, -Loc_Name,-Loc_Code,- QAQC,-Plot_Name,-Start_Date,-Year,-Spp_Code, -Spp_Name)
head(PI.m)

### export raw data for  use in R viz
### Bind species names and site names for final output
PI.raw.R<-left_join(PI.m,species, by= "Spp_Code")
head(PI.raw.R)
PI.raw.R$variable<-NULL
write.table(PI.raw.R, "./Data/For RShiny/site_transect_cover_raw.csv", sep=",", row.names= FALSE)

#################################################################################
############ SITE-LEVEL ESTIMATES ###############
#################################################################################

### P-I data typically summarized across entire transect as the number of "hits" or freq of obs of each species along transect. 
# You then calculate the porprotion of hits for each species from the total to esimtate the relative cover
head(PI.m)

# check the number of obs per species
PI.m %>% group_by(Loc_Code,Year, QAQC,Plot_Name, Spp_Code) %>% tally() %>% View()

## First need to sum the number of occurences of each species per transect per year at each site, adding in the 0
# observations for species not encountered along a transect

PI_spp.hits<-PI.m %>% 
cast(., Loc_Code +Year+QAQC+ Plot_Name+  Spp_Code ~ . , fun = length, add.missing = TRUE, fill = 0) %>%  # number of hits of each species per transect
drop_na()
  
colnames(PI_spp.hits)[6]<-"total_hits"# rename sum column
head(PI_spp.hits)

# should be this number of rows
length(unique(PI_data$Loc_Code))*length(unique(PI_data$Year))*length(unique(PI_data$Spp_Code))*length(unique(PI_data$Plot_Name))

# then calculate the total number of obs taken per transect
PI_obs<-PI.m %>% 
  cast(.,Site_Name+ Site_Code +Loc_Name+  Loc_Code +Year+QAQC+ Plot_Name ~ . , fun = length) %>%  # number of total points sampled per transect
  mutate(QAQC= as.factor(QAQC))

colnames(PI_obs)[8]<-"total_points"# rename sum column
head(PI_obs)

### Bind the total obs to the species hits to calculate the portional cover (# hits per species/# obs) per transect

PI.site<-left_join(PI_spp.hits,PI_obs,by =c("Loc_Code", "Year", "QAQC","Plot_Name")) %>% 
  drop_na() %>% 
  mutate(prop_cover=round(total_hits/total_points,3)) %>% # calc prop cover
  mutate(Year= as.character(Year), QAQC= as.character(QAQC), Spp_Code= as.character(Spp_Code)) %>% 
  select(Site_Name,Loc_Name,Year,Plot_Name,QAQC,Spp_Code,total_hits,total_points,prop_cover)
  
PI.site<-PI.site[PI.site$Spp_Code != "",]


#################################################################################
#### Calc the average of all 3 transects sampled IN EACH YEAR 
#################################################################################

PI.site.yr<-PI.site %>% 
  group_by(Site_Name, Loc_Name, Year, QAQC, Spp_Code) %>% 
  dplyr::summarise(mean= round(mean(prop_cover, na.rm = T),1),
            sd= round(sd(prop_cover, na.rm = T),2),se = round(sd(prop_cover)/sqrt(3),3))# length should be 3 for each row (species*site combo)

### What are the 10 most dominat cover types in each park?

top10ACAD<-PI.site %>% filter(Site_Name == "Acadia NP") %>% 
  group_by(Spp_Code) %>% 
  dplyr::summarise(mean= round(mean(prop_cover, na.rm = T),1),
                   sd= round(sd(prop_cover, na.rm = T),2),se = round(sd(prop_cover)/sqrt(3),3)) %>% 
  dplyr::arrange(desc(mean)) %>% 
  slice(1:10) %>% 
  pull(Spp_Code)

top10BOHA<-PI.site %>% filter(Site_Name == "Boston Harbor NRA") %>% 
  group_by(Spp_Code) %>% 
  dplyr::summarise(mean= round(mean(prop_cover, na.rm = T),1),
                   sd= round(sd(prop_cover, na.rm = T),2),se = round(sd(prop_cover)/sqrt(3),3)) %>% 
  dplyr::arrange(desc(mean)) %>% 
  slice(1:10) %>% 
  pull(Spp_Code)


### Bind species names for final plotting
PI.site.plot<-left_join(PI.site.yr,species, by= "Spp_Code") %>% 
  arrange(Site_Name,Loc_Name, Year,QAQC,Common_Name)


#### Extract top 10 cover types for final plotting 

ACAD.temp<-PI.site.plot[PI.site.plot$Site_Name == "Acadia NP" & PI.site.plot$Spp_Code %in% top10ACAD,];ACAD.temp<-droplevels(ACAD.temp)
BOHA.temp<-PI.site.plot[PI.site.plot$Site_Name == "Boston Harbor NRA" & PI.site.plot$Spp_Code %in% top10BOHA,];BOHA.temp<-droplevels(BOHA.temp)

PI.site.plot<-rbind(ACAD.temp,BOHA.temp)

### export annual means from top 10 species/covertypes for  use in R viz
write.table(PI.site.plot, "./Data/For RShiny/site_transect_cover_per_year.csv", sep=",", row.names= FALSE)


# first reorder names labels for correct plotting
spp_names<-rev(PI.site.plot$Common_Name)

y2<-ggplot(PI.site.plot[PI.site.plot$QAQC == FALSE,], aes(x=Common_Name, y= as.numeric(mean), fill= Year))+
  geom_bar(stat ="identity") + labs(y = "Mean cover + SE", x= "") +
  geom_errorbar(aes(ymax = mean + se, ymin=mean), width=0.1)+scale_fill_brewer(palette="Blues")

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
