### This code summarizes moltile invert data collected as a part of NETN rocky intertidal monitoring protocol 



library(plyr)
library(ggplot2)
library(reshape)


# import count data on mollusks and echinos
# counted from 50X75 cm photoplots, motiles were subsampled in 20-cm2 sub plots IN 2013/2014, but later abandoned
motile<- read.csv("~/R/NETN/Rocky-Intertidal/qryR_FlatFile_MotileInvert_Counts.csv")
motile_size <- read.csv("~/R/NETN/Rocky-Intertidal/NETN_MotileInvert_Measurements.csv")

head(motile)
#### set up some design related duymmy var()
asco<-c("A1","A2","A3","A4","A5")
barn<-c("B1","B2","B3","B4","B5")
fuc<-c("F1","F2","F3","F4","F5")
muss<-c("M1","M2","M3","M4","M5")
red<-c("R1","R2","R3","R4","R5")
################################## Create new var to aggregate by zone ######
#motile
motile$Zone[motile$Plot_Name %in% asco] ="Ascophyllum" 
motile$Zone[motile$Plot_Name %in% barn] ="Barnacle" 
motile$Zone[motile$Plot_Name %in% fuc] ="Fucus" 
motile$Zone[motile$Plot_Name %in% muss] ="Mussels" 
motile$Zone[motile$Plot_Name %in% red] ="Red Algae" 

############################################################################################
#############        Setup count df's for summary            #########################
############################################################################################
motile$Start_Date<-as.Date(motile$Start_Date, format= "%m/%d/%Y") #convert to StartDate
motile$Year<-as.factor(format(motile$Start_Date,"%Y")) #convert to year
motile$total<-motile$Damage+motile$No.Damage

### adjust count for subsampling (20 cm 2 of 50X75 cm plot)- 9.375 mult factor if sub sampled (concern about whether we can do this but OK for now )
motile$final_total<- ifelse(motile$Subsampled == "Yes", (motile$total*9.375), motile$total)
## scale to express in m2 (X )mult by 2.667)
motile$Abundance<-motile$final_total*2.6666666666666667
motile$logAbundance<-log(motile$Abundance) # calc log
motile$logAbundance[is.infinite(motile$logAbundance)]=0
head(motile)

#calc prop of total damaged
motile$Proportion.Damaged<-round(motile$No.Damage/motile$Abundance,2) ### proportion damaged will be based on raw data when subsampled 9should be the same anyway)
motile$Proportion.Damaged[is.na(motile$Proportion.Damaged)] = "0" # repalce NaN with 0s

head(motile)
## create molten data frame (all values are represented for each site*time combination)
motile.melt<-melt(motile, id.vars=c("Site_Name", "Loc_Name" ,"Start_Date", "Year", "Species" ,"Plot_Name", "Zone"), measure.vars=c("Abundance","logAbundance", "Proportion.Damaged"))
head(motile.melt)

motile.melt$value<-as.numeric(as.character(motile.melt$value))# force value vector to be numeric


head(motile.melt)
##### output raw table for R viz

motile.raw.R<-join(motile.melt, motile_spp , by= "Species")
motile.raw.R<-motile.raw.R[,c("Site_Name", "Loc_Name" ,"Start_Date", "Year", "Species" ,"Com_Sp", "Plot_Name", "Zone", "variable", "value")]

### export to use in R viz
write.table(motile.raw.R, "./Data/motile_count_raw.csv", sep=",", row.names= FALSE)

############ Summarize species counts by site, zone, and Year
summ<-function (x) c(mean =round(mean(x,na.rm = TRUE),2),se= round(sd(x,na.rm = TRUE)/sqrt(length(x)),2), N= length(x))

######################## motile inverts#####################
# look up table for labelling etc
motile_spp <- read.csv("~/R/NETN/Rocky-Intertidal/tlu_motile_spp.csv")
mot_sites<-unique(motile[,c("Site_Name", "Loc_Name")])


#### Aggregate data by per zone per site per year
# raw data
# calculate plot-level mean per zone per site per year and add in missing vals combinations
# Site_Name will be appended back to df after. Adding in all vals per cate combination caused issues within plotting
# Need to add in all combos to have similar bar widths when plotting in ggplot.

sum.motile<-cast(motile.melt, Loc_Name + Year+ Species +Zone + variable~ . , value = "value", fun = summ, fill=NA, add.missing = TRUE)
head(sum.motile)

### append park and species names

sum.motile<-join(sum.motile,mot_sites, by = "Loc_Name") ### append park name back to df for indexing in vizualizer 
sum.motile<-join(sum.motile,motile_spp, by = "Species")

### export to use in R viz
write.table(sum.motile, "./Data/motile_count.csv", sep=",", row.names= FALSE)



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
            geom_bar(stat ="identity", position = dodge,) + labs(y = expression(paste("Mean number m"^"-2", "+ SE")), x= "Intertidal Zone") +
         geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)

## mean prop damaged

y2<-ggplot(plot.df, aes(x=Zone, y= as.numeric(mean), fill= Year))+
  geom_bar(stat ="identity", position = dodge,) + labs(y = expression(paste("Mean proportion damaged m"^"-2", "+ SE")), x= "Intertidal Zone") +
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




