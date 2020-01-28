

### Import data tables and setup lookup tables for selection and plotting

######## Vertical transect data ###########

transect<-read.csv("./Data/For RShiny/site_transect_cover.csv") ## summarized point intercept data by site from 'import and summ transect data.R'

transect_yr<-read.csv("./Data/For RShiny/site_transect_cover_per_year.csv") ## summarized point intercept data by site AND YEAR from 'import and summ transect data.R'
## data only includes non-QAQC plots

transect_raw<-read.csv("./Data/For RShiny/site_transect_cover_raw.csv") ## raw point intercept data by transect from 'import and summ transect data.R' for downloading
# data includes QAQC plots

ParkList_trans<-unique(levels(transect$Site_Name)) ## Pak lookup table for UI

######## Mollusks #########

motile<-read.csv("./Data/For RShiny/motile_count.csv") # summarized count data by site and year from 'import and summ motile invert data.R'
## data only includes non-QAQC plots
 
motile_spp <- read.csv("./Data/Look ups/tlu_motile_spp.csv") # lookup table for species names

motile_raw<-read.csv("./Data/For RShiny/motile_count_raw.csv") # raw plot-level data with labels from from 'import and summ motile invert data.R' for downloading


ParkList<-unique(levels(motile$Site_Name))

SiteList<-unique(levels(motile$Loc_Name))

SppList<-unique(levels(motile$Com_Sp))

VarList<-unique(levels(motile$variable))


###### Tidepools #########

echino<-read.csv("./Data/For RShiny/echino_count.csv") ## summarized count data by site and year from 'import and summ echino data.R' 
## data only includes non-QAQC plots

echino_raw<-read.csv("./Data/For RShiny/echino_count_raw.csv")# raw plot-level data with labels from from 'import and summ echino data.R' for downloading
## data  includes QAQC plots


SeaStarList<-unique(levels(echino$variable)) # lookup table for species names

# ACAD_var<- unique(levels(df$Local.Characteristic.Name[df$ParkCode == "ACAD"]))
# MABI_var<- unique(levels(df$Local.Characteristic.Name[df$ParkCode == "MABI"]))
