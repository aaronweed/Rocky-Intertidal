

### Import data tables and setup lookup tables for selection and plotting


## Mollusks

motile<-read.csv("./Data/motile_count.csv")

ParkList<-unique(levels(motile$Site_Name))

SiteList<-unique(levels(motile$Loc_Name))

SppList<-unique(levels(motile$Spp_Name))

VarList<-unique(levels(motile$variable))

### Echinoderms
#SeaStarList

# ACAD_var<- unique(levels(df$Local.Characteristic.Name[df$ParkCode == "ACAD"]))
# MABI_var<- unique(levels(df$Local.Characteristic.Name[df$ParkCode == "MABI"]))
