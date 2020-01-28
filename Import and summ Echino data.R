library(plyr)
library(reshape)
library(readxl)
library(tidyverse)

# import count data of echinos

echino <- qryR_FlatFile_Echinoderm_Counts_wide <- read_excel("Data/Data from DB/2019/qryR_FlatFile_Echinoderm_Counts_wide.xlsx")
echino_size <-read_excel("Data/Data from DB/2019/qryR_FlatFile_MotileInvert_Measurements_wNulls_partb.xlsx")
tlu_echino_spp <- read.csv("Data/Look ups/tlu_echino_Spp.csv")

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
echino.melt<-echino %>% select(-State_Code, -Target_Species) %>%  
        gather(variable,value ,-Site_Name,-Loc_Name, -Start_Date, -Year, -Plot_Name, -QAQC)

head(echino.melt)

#### scale counts to a per m2 basis; each transect is 20m2
echino.melt$Abundance<-echino.melt$value/20

## create log-scaled values
echino.melt$logAbundance<-log(echino.melt$Abundance+1) # calc log
echino.melt$logAbundance[is.infinite(echino.melt$logAbundance)]=0

### rename species names
echino.melt$Spp_Name<-echino.melt$variable
echino.melt$Spp_Name<-mapvalues(echino.melt$Spp_Name, from=c("Count_STRDRO", "Count_HENSAN", "Count_ASTRUB", "Count_ASTFOR"), 
                                to=c("Strongylocentrotus droebachiensis", "Henricia sanguinolenta", "Asterias rubens", "Asterias forbesii"))
echino.melt$variable<-NULL
              
head(echino.melt)

## recreate molten data frame to include logAbundance (all values are represented for each site*time combination)

 echino.melt2<-echino.melt %>% select(-value) %>% gather(variable, value, -Site_Name,-Loc_Name,-Start_Date,-Year,-Plot_Name,-QAQC, -Spp_Name)
 
head(echino.melt2)

 #### Export as raw data for R viz downloader
 # add in species names
 echino.raw<-join(echino.melt2, tlu_echino_spp, by ="Spp_Name") %>% filter(QAQC == "FALSE")
 echino.raw<-echino.raw[,c("Site_Name", "Loc_Name" ,"Start_Date", "Year", "Plot_Name", "Com_Sp", "variable","value")]
 head(echino.raw)
 
 ### export to use in R viz
 write.table(echino.raw, "./Data/For RShiny/echino_count_raw.csv", sep= ",", row.names= FALSE)
 
 
############ Summarize species counts by site, zone, and Year
 # imported data inlcudes zeroes for all species, even when missing from a plot
summ<-function (x) tibble(mean = round(mean(x,na.rm = TRUE),2),se= round(sd(x,na.rm = TRUE)/sqrt(length(!is.na(x))),2), N= length(!is.na(x)))

# aggregate data by site and year, data imported from 2019 includes all 0 counts so no need to complete matrix for correct calculations
# in 2014 only 2 plots were sampled in Green and Petit Manan; did they not do 3?

sum.echino<-echino.melt2 %>% 
        group_by(Site_Name, Loc_Name,Year, Spp_Name, variable, QAQC) %>% 
        group_modify(~summ(.$value), keep= TRUE) %>%  # applies function across df
        left_join(., tlu_echino_spp, by ="Spp_Name") %>%  # add in species names
        select(Site_Name,Loc_Name,Year,Common, Spp_Name, Spp_Code,Com_Sp, variable, QAQC, mean, se, N)

head(sum.echino)

### export to use in R viz
write.table(sum.echino, "./Data/For RShiny/echino_count.csv", sep= ",", row.names= FALSE)


