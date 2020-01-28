### This code summarizes moltile invert data collected as a part of NETN rocky intertidal monitoring protocol 
library(plyr)
library(reshape)
library(readxl)
library(tidyverse)


# import count data on mollusks and echinos
# counted from 50X75 cm photoplots, motiles were subsampled in 20-cm2 sub plots IN 2013/2014, but later abandoned
motile<-read_excel("Data/Data from DB/2019/qryR_FlatFile_MotileInvert_Counts.xlsx")
motile_size <-read_excel("Data/Data from DB/2019/qryR_FlatFile_MotileInvert_Measurements_wNulls_partb.xlsx")

# look up table for labelling etc
motile_spp <- read.csv("~/R/NETN/Rocky-Intertidal/Data/Look ups/tlu_motile_spp.csv")
tlu_sites<-read.csv("~/R/NETN/Rocky-Intertidal/Data/Look ups/tlu_sites.csv")


head(motile)

############################################################################################
#############        Setup count df's for summary            #########################
############################################################################################
motile$Start_Date<-as.Date(motile$Start_Date, format= "%m/%d/%Y") #convert to StartDate
motile$Year<-as.factor(format(motile$Start_Date,"%Y")) #convert to year
motile$total<-motile$Damage+motile$`No Damage` # calculate the total number of snails per plot

### adjust count for subsampling (20 cm sqaure frame or 400cm2 of 50X75 cm plot)- 9.375 mult factor if sub sampled (concern about whether we can do this but OK for now )
motile$final_total<- ifelse(motile$Subsampled == "Yes", (motile$total*9.375), motile$total)
## scale to express in m2 (X )mult by 2.667)
motile$Abundance<-round(motile$final_total*2.6666666666666667,1)
motile$logAbundance<-log(motile$Abundance) # calc log
motile$logAbundance[is.infinite(motile$logAbundance)]=0
head(motile)

#calc prop of total damaged
motile$Proportion.Damaged<-round(motile$`No Damage`/motile$Abundance,2) ### proportion damaged will be based on raw data when subsampled, should be the same anyway)
motile$Proportion.Damaged[is.na(motile$Proportion.Damaged)] = "0" # repalce NaN with 0s

head(motile)
## create molten data frame (all values are represented for each site*time combination)
motile.melt<-select(motile, Site_Name, Loc_Name,Start_Date, Year,Plot_Name,QAQC, Zone= Target_Species, Species, Abundance, logAbundance, Proportion.Damaged) %>% 
  gather(variable,value,-Site_Name, -Loc_Name,-Start_Date, -Year,-Plot_Name,-QAQC, -Zone, -Species)

head(motile.melt)

motile.melt$value<-as.numeric(as.character(motile.melt$value))# force value vector to be numeric


head(motile.melt)
##### output raw table for R viz

motile.raw.R<-left_join(motile.melt, motile_spp , by= "Species")# add species names
motile.raw.R$units<-"m2"
motile.raw.R<-motile.raw.R[,c("Site_Name", "Loc_Name" ,"Start_Date", "Year", "Species" ,"Com_Sp", "Plot_Name","QAQC" , "Zone", "variable", "units","value")]
head(motile.raw.R)
### export to use in R viz
write.table(motile.raw.R, "./Data/For RShiny/motile_count_raw.csv", sep=",", row.names= FALSE)

############ Summarize species counts by site, zone, and Year
summ<-function (x) tibble(mean =round(mean(x,na.rm = TRUE),2),se= round(sd(x,na.rm = TRUE)/sqrt(length(!is.na(x))),2), N= length(!is.na(x)))

######################## motile inverts#####################

#### Aggregate data by per zone per site per year
# Raw data includes all 5 samples per species, zone and event combo (explicit 0s). There are a few samples from Petit Manan without 5 samples, but assume these are not missing samples but <5 were taken.
# calculate plot-level mean per zone per site per year 
# Site_Name will be appended back to df after. 

sum.motile<-motile.melt%>% 
  group_by(Site_Name, Loc_Name,Year, Species, Zone, QAQC,variable) %>%
  group_modify(~summ(.$value), keep= TRUE) %>%  # applies function across df
  left_join(., motile_spp, by ="Species") %>%  # add in species names
  select(Site_Name,Loc_Name,Year,Common, Species, Spp_Name,Com_Sp, Zone,variable, QAQC, mean, se, N)

#head(sum.motile)

### append park and species names

sum.motile<-left_join(tlu_sites,sum.motile, by = c("Loc_Name","Site_Name")) ### append park name back to df for indexing in vizualizer 
#sum.motile<-left_joinjoin(sum.motile,motile_spp, by = "Species")

### export to use in R viz
write.table(sum.motile, "./Data/For RShiny/motile_count.csv", sep=",", row.names= FALSE)

#######################################
##### Plotting mean count data by species among sites in a park
#################################################
## Select park
head(sum.motile)
levels(sum.motile$Site_Name)
levels(sum.motile$variable) #"Abundance"          "logAbundance"       "Proportion.Damaged"
## "Acadia NP"               "Boston Harbor NRA"       "Maine Coast Islands NWR"

plot.df<-subset(sum.motile, Site_Name %in%"Acadia NP"  & variable %in% "logAbundance"  & Spp_Name %in% "Common periwinkle (Littorina littorea)" )
plot.df<-droplevels(plot.df)


dodge <- position_dodge(width=0.9)

### mean count

y2<-ggplot(plot.df, aes(x=Zone, y= as.numeric(mean), fill= Year))+
            geom_bar(stat ="identity", position = dodge) + labs(y = expression(paste("Mean number m"^"-2", "+ SE")), x= "Intertidal Zone") +
         geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)

## mean prop damaged

y2<-ggplot(plot.df, aes(x=Zone, y= as.numeric(mean), fill= Year))+
  geom_bar(stat ="identity", position = dodge) + labs(y = expression(paste("Mean proportion damaged m"^"-2", "+ SE")), x= "Intertidal Zone") +
  geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)

# y2
# summary(y2)
y2<-(y2+facet_grid(Loc_Name~Spp_Name) + 
       theme(legend.position = "top") +
       theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 12 * 0.8,face="bold"))+
       theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14 * 0.8, face="bold")) +
       theme(strip.text.x= element_text(size=10, face=c("bold.italic"))) +
       theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(panel.background =  element_rect(fill="white", colour="black")) +
       theme(panel.grid.major = element_line(colour = "grey90"))+
       theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
       theme(strip.background= element_rect(size=10, color="gray" )))
y2




