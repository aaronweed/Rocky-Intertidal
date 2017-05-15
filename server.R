#The server.R script contains the instructions that your computer needs to build your app

library(shiny)
library(leaflet)
library(reshape)
library(rgdal)
library(shinyjs)
library(jsonlite,pos=100)
library(httr)
library(dplyr)
library(DT)
library(ggplot2)
library(RColorBrewer)

## bring in data
# df<-read.csv("./Data/NETN_Water_Data_RViz.csv")
# units<-read.csv("./Data/tlu_Units.csv") ## table with units for lableing plots


##### Begin Server Function ####

shinyServer(function(input,output){

  ###################### Create set of reactive selection boxes in UI  ####################
  ### select site based on park
  output$SiteResultsA <- renderUI({ 
    
    df_sub<-subset(transect_yr, Site_Name %in% input$park)
    df_sub<-droplevels(df_sub)
    
    selectInput(inputId='site', label='Select Site',   unique(levels(df_sub$Loc_Name)))
  })
  
  
  #################### Transect data plots planel ##############################################
  ### 5/12/17: currently only plots per site over the entire observation period
  
  output$plot1 <- renderPlot({
    
    ############## DATA MANIPULATION ##############################        
    if(input$many == "All sites"){
      
      plot.df<-subset(transect_yr, Site_Name %in% input$park & QAQC == 0) # select by park and drop the QAQC plots
      plot.df<-droplevels(plot.df)
      plot.df$Year<-as.factor(plot.df$Year)
      
      
    }else{
    
    ## SUBSET BY SITE 
    
    
      plot.df<-subset(transect_yr, Loc_Name %in% input$site & QAQC == 0)# select by site and drop the QAQC plots
      plot.df<-droplevels(plot.df)
      plot.df$Year<-as.factor(plot.df$Year)
      
    }
      ############## PLOT mean over time  ##############################
      y2<-ggplot(plot.df[plot.df$QAQC == 0,], aes(x=Common_Name, y= as.numeric(mean), fill= Year))+
        geom_bar(stat ="identity", position = dodge,colour="black") + labs(y = "Mean proportion of cover + SE", x= "") +
        geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+scale_fill_brewer(palette="Blues")
      
      y2<-(y2+facet_wrap(~Loc_Name) + 
             theme(legend.position = "top", legend.text = element_text(size = 16), legend.title = element_text(size =16)) +
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
             theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 14 , face="bold")) +
             theme(strip.text.x= element_text(size=16, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 0, debug=F))+
             theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90"))+
             theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
             theme(strip.background= element_rect(size=10, color="gray" )))
      #scale_x_discrete(limits=rev(levels(plot.df$Common_Name)))+ # use with coord_flip
      
      print(y2)
  }
  
  , height = 800, width = 1000)
  
  
  #################### Single site plots planel ##############################################
###################### Create set of reactive selection boxes in UI  ####################
  ### select site based on park
  # output$SiteResultsA <- renderUI({ 
  #   
  #   df_sub<-subset(df, SiteName %in% input$park)
  #   df_sub<-droplevels(df_sub)
  #   
  #   selectInput(inputId='site', label='Select Site',  unique(levels(df_sub$Loc_)))
  # })
  
  

### Summary plot by site and intertidal zone
   
      output$plot <- renderPlot({

        
        
############## DATA MANIPULATION ##############################        
## SUBSET MOTILE DF BY PARK, SPECIES, VARIABLE
    
    if(input$logscale == FALSE){    
        
        if(input$SPP =="All species"){
          plot.df<-subset(motile, Site_Name %in% input$park  & variable %in% input$variable )
          plot.df<-droplevels(plot.df)
          plot.df$Year<-as.factor(plot.df$Year)
          

        }else{
        
        plot.df<-subset(motile, Site_Name %in% input$park  & variable %in% input$variable  & Spp_Name %in% input$species)
        plot.df<-droplevels(plot.df)
        plot.df$Year<-as.factor(plot.df$Year)
         
        }
        
    }else{
      
      if(input$SPP =="All species"){
        plot.df<-subset(motile, Site_Name %in% input$park  & variable %in% "logAbundance" )
        plot.df<-droplevels(plot.df)
        plot.df$Year<-as.factor(plot.df$Year)
        
        
      }else{
        
        plot.df<-subset(motile, Site_Name %in% input$park  & variable %in% "logAbundance"  & Spp_Name %in% input$species)
        plot.df<-droplevels(plot.df)
        plot.df$Year<-as.factor(plot.df$Year)
        
      }
    
      
    }
      
      
        if(nrow(plot.df)== 0){
          stop("Sorry, this species has not been collected at this site.")
        }
   
        
################ BEGIN PLOTTING FUNCTIONS ####################             
      
        if(input$variable == "Abundance"){
     
        dodge <- position_dodge(width=0.9)
        
      y2<-ggplot(plot.df, aes(x=Zone, y= as.numeric(mean), fill= Year))+
        geom_bar(stat ="identity",colour="black", position = dodge) + 
        labs(y = expression(paste("Mean number m"^"2", "+ SE")), x= "Intertidal Zone") +
        geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+ scale_fill_brewer(palette="Blues")
      
      }else{
        
      ## plot mean prop damaged
        dodge <- position_dodge(width=0.9)
        
      y2<-ggplot(plot.df, aes(x=Zone, y= as.numeric(mean), fill= Year))+
        geom_bar(stat ="identity", colour="black",position = dodge) + 
        labs(y = expression(paste("Mean proportion damaged m"^"2", "+ SE")), x= "Intertidal Zone") +
        geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+ scale_fill_brewer(palette="Blues")
      }
        
      if(input$logscale == TRUE){
      
        dodge <- position_dodge(width=0.9)
        
        y2<-ggplot(plot.df, aes(x=Zone, y= as.numeric(mean), fill= Year))+
          geom_bar(stat ="identity",colour="black", position = dodge) + labs(y = expression(paste("log Mean number m"^"2", "+ SE")), x= "Intertidal Zone") +
          geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+ scale_fill_brewer(palette="Blues")
        
      }
        if(input$free_y == FALSE){
     
        y2<-(y2+facet_wrap(~Loc_Name, ncol =1, scales="free_y") +
               theme(legend.position = "top") +
               theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
               theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14 , face="bold")) +
               theme(strip.text.x= element_text(size=14, face=c("bold.italic"))) +
               theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
               theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
               theme(panel.background =  element_rect(fill="white", colour="black")) +
               theme(panel.grid.major = element_line(colour = "grey90"))+
               theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
               theme(strip.background= element_rect(size=10, color="gray" )))
        
        }else{
          
          y2<-(y2+facet_wrap(~Loc_Name, ncol =1, scales="fixed") +
                 theme(legend.position = "top") +
                 theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
                 theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14, face="bold")) +
                 theme(strip.text.x= element_text(size=14, face=c("bold.italic"))) +
                 theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                 theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                 theme(panel.background =  element_rect(fill="white", colour="black")) +
                 theme(panel.grid.major = element_line(colour = "grey90"))+
                 theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
                 theme(strip.background= element_rect(size=10, color="gray" )))
        }
       
        if(input$SPP =="All species"){
          y2<-(y2+facet_grid(Loc_Name~Spp_Name) +
                 theme(legend.position = "top") +
                 theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
                 theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 12, face="bold")) +
                 theme(strip.text.x= element_text(size=11, face=c("bold.italic"))) +
                 theme(strip.text.y= element_text(size=12, face=c("bold")))+
                 theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                 theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                 theme(panel.background =  element_rect(fill="white", colour="black")) +
                 theme(panel.grid.major = element_line(colour = "grey90"))+
                 theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
                 theme(strip.background= element_rect(size=10, color="gray" )))
        }
  
        
 if(input$compare == TRUE){      
        
   if(input$variable == "Abundance"){
     
     dodge <- position_dodge(width=0.9)
     
     y2<-ggplot(plot.df, aes(x=Loc_Name, y= as.numeric(mean), fill= Year))+
       geom_bar(stat ="identity",colour="black", position = dodge) + 
       labs(y = expression(paste("Mean number m"^"2", "+ SE")), x= "Site Name") +
       geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+ scale_fill_brewer(palette="Blues")
     
   }else{
     
     ## plot mean prop damaged
     dodge <- position_dodge(width=0.9)
     
     y2<-ggplot(plot.df, aes(x=Loc_Name, y= as.numeric(mean), fill= Year))+
       geom_bar(stat ="identity", colour="black",position = dodge) + 
       labs(y = expression(paste("Mean proportion damaged m"^"2", "+ SE")), x= "Site Name") +
       geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+ scale_fill_brewer(palette="Blues")
   }
        
        if(input$logscale == TRUE){
          
          dodge <- position_dodge(width=0.9)
          
          y2<-ggplot(plot.df, aes(x=Loc_Name, y= as.numeric(mean), fill= Year))+
            geom_bar(stat ="identity",colour="black", position = dodge) + labs(y = expression(paste("log Mean number m"^"2", "+ SE")), x= "Site Name") +
            geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+ scale_fill_brewer(palette="Blues")
          
        }
        if(input$free_y == FALSE){
          
          y2<-(y2+facet_wrap(~Zone, ncol =1, scales="free_y") +
                 theme(legend.position = "top") +
                 theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
                 theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14 , face="bold")) +
                 theme(strip.text.x= element_text(size=14, face=c("bold.italic"))) +
                 theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                 theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                 theme(panel.background =  element_rect(fill="white", colour="black")) +
                 theme(panel.grid.major = element_line(colour = "grey90"))+
                 theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
                 theme(strip.background= element_rect(size=10, color="gray" )))
        }else{
          
          y2<-(y2+facet_wrap(~Zone, ncol =1, scales="fixed") +
                 theme(legend.position = "top") +
                 theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
                 theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14, face="bold")) +
                 theme(strip.text.x= element_text(size=14, face=c("bold.italic"))) +
                 theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                 theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                 theme(panel.background =  element_rect(fill="white", colour="black")) +
                 theme(panel.grid.major = element_line(colour = "grey90"))+
                 theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
                 theme(strip.background= element_rect(size=10, color="gray" )))
        }
         
   if(input$SPP =="All species"){
     y2<-(y2+facet_grid(Zone ~ Spp_Name+common) +
            theme(legend.position = "top") +
            theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
            theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 12, face="bold")) +
            theme(strip.text.x= element_text(size=11, face=c("bold.italic"), panel.margin = unit(0.25, "cm"))) +
            theme(strip.text.y= element_text(size=12, face=c("bold")))+
            theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
            theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
            theme(panel.background =  element_rect(fill="white", colour="black")) +
            theme(panel.grid.major = element_line(colour = "grey90"))+
            theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
            theme(strip.background= element_rect(size=10, color="gray" )))
   }
   
   
 }
      
          print(y2)
      }
        
     , height = 800, width = 1000)

      
      #####################################################  SEASTARS plots       
      
      
      
}) ## end shiny serverfunc    


#####################################################  
### download data table

# output$downloadData <- downloadHandler(
#   
#   filename = function() { 
#     paste(input$site,input$parm, '.csv', sep='') 
#   },
#   content = function(file) {
#     write.csv(data, file)
#   }
# )
# 
#######################################################   

