

### Import data tables and setup lookup tables for selection and plotting

## transect data

transect<-read.csv("./Data/site_transect_cover.csv")
transect_yr<-read.csv("./Data/site_transect_cover_per_year.csv")

transect_raw<-read.csv("./Data/site_transect_cover_raw.csv")

ParkList_trans<-unique(levels(transect$Site_Name))

## Mollusks

motile<-read.csv("./Data/motile_count.csv")

motile_spp <- read.csv("./Data/motile_spp.csv")

ParkList<-unique(levels(motile$Site_Name))

SiteList<-unique(levels(motile$Loc_Name))

SppList<-unique(levels(motile$Com_Sp))

VarList<-unique(levels(motile$variable))





### Echinoderms
echino<-read.csv("./Data/echino_count.csv")

SeaStarList<-unique(levels(echino$variable))

# ACAD_var<- unique(levels(df$Local.Characteristic.Name[df$ParkCode == "ACAD"]))
# MABI_var<- unique(levels(df$Local.Characteristic.Name[df$ParkCode == "MABI"]))
