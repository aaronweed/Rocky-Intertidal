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
  ### 5/12/17: currently only plots per site in each year. 
  
  output$plot1 <- renderPlot({
    
    ############## DATA MANIPULATION ##############################        
    if(input$many == "All sites"){
      
      plot.df<-subset(transect_yr, Site_Name %in% input$park & QAQC == 0) # select by park and drop the QAQC plots
      plot.df<-droplevels(plot.df)
      plot.df$Year<-as.factor(plot.df$Year)
      #plot.df$Year<- ordered(plot.df$Year, levels = c("2016", "2015", "2014", "2013"))
      
    }else{
    
    ## SUBSET BY SITE 
    
    
      plot.df<-subset(transect_yr, Loc_Name %in% input$site & QAQC == 0)# select by site and drop the QAQC plots
      plot.df<-droplevels(plot.df)
      plot.df$Year<-as.factor(plot.df$Year)
      plot.df$Year<- ordered(plot.df$Year, levels = c("2016", "2015", "2014", "2013"))
    }
      ############## PLOT mean over time  ##############################
      dodge <- position_dodge(width=0.9)
    
        if(input$compare == "Cover types within a site"){
      
      y2<-ggplot(plot.df[plot.df$QAQC == 0,], aes(x=Common_Name, y= as.numeric(mean), fill= Year))+
        geom_bar(stat ="identity", position = dodge,colour="black") + labs(y = "Mean proportion of cover + SE", x= "") +

        geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+scale_fill_brewer(palette="Blues")
      
      y2<-(y2+facet_wrap(~Loc_Name) + coord_flip()+
             theme(legend.position = "right", legend.text = element_text(size = 16), legend.title = element_text(size =16)) +
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14 , face="bold")) +
             theme(strip.text.x= element_text(size=16, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 0, debug=F))+
             theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90"))+
             theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
             scale_x_discrete(limits=rev(levels(plot.df$Common_Name)))+ # use with coord_flip
             theme(strip.background= element_rect(size=10, color="gray" )))
        }else{
          
          y2<-ggplot(plot.df[plot.df$QAQC == 0,], aes(x=Loc_Name, y= as.numeric(mean), fill= Year))+
            geom_bar(stat ="identity", position = dodge,colour="black") + labs(y = "Mean proportion of cover + SE", x= "") +
            
            geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+scale_fill_brewer(palette="Blues")
          
          y2<-(y2+facet_wrap(~Common_Name) + coord_flip()+
                 theme(legend.position = "right", legend.text = element_text(size = 16), legend.title = element_text(size =16)) +
                 theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
                 theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14 , face="bold")) +
                 theme(strip.text.x= element_text(size=16, face=c("bold.italic"))) +
                 theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 0, debug=F))+
                 theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                 theme(panel.background =  element_rect(fill="white", colour="black")) +
                 theme(panel.grid.major = element_line(colour = "grey90"))+
                 theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
                 scale_x_discrete(limits=rev(levels(plot.df$Loc_Name)))+ # use with coord_flip
                 theme(strip.background= element_rect(size=10, color="gray" )))
          
        }
      
      
      print(y2)
  }
  
  , height = 800, width = 1000)
  
  
  ###Setup dynamic caption
  
  output$captionVertTrans <- renderText({
    " Average annual cover of the 10 most abundant species/cover types estimated by point-intercept sampling along three parallel transects."
  })
  
  ####################  Mollusk data  panel ##############################################

### Summary plot by site and intertidal zone
   
      output$plot <- renderPlot({
        
  ####### Reactive list of sites ####################      
        output$SiteResultsMoll <- renderUI({ 
          
          df_sub<-subset(motile, Site_Name %in% input$parkMoll)
          df_sub<-droplevels(df_sub)
          
          selectInput(inputId='siteMoll', label='Select Site',   unique(levels(df_sub$Loc_Name)))
        })
        
        
  ############## DATA MANIPULATION ##############################        
## SUBSET MOTILE DF BY PARK, SPECIES, VARIABLE
    
    if(input$manyMoll == "All sites"){
        if(input$logscale == FALSE){    
        
        if(input$SPP =="All species"){
          
          plot.df<-subset(motile, Site_Name %in% input$parkMoll  & variable %in% input$variable )

        }else{
        
        plot.df<-subset(motile, Site_Name %in% input$parkMoll  & variable %in% input$variable  & Com_Sp %in% input$species)
         
        }
        
    }else{
      
      if(input$SPP =="All species"){
        
        plot.df<-subset(motile, Site_Name %in% input$parkMoll  & variable %in% "logAbundance" )

      }else{
        
        plot.df<-subset(motile, Site_Name %in% input$parkMoll  & variable %in% "logAbundance"  & Com_Sp %in% input$species)

      }
    
    }
        
    }else{
      
      if(input$logscale == FALSE){    
        
        if(input$SPP =="All species"){
          
          plot.df<-subset(motile, Loc_Name %in% input$siteMoll  & variable %in% input$variable )
          
        }else{
          
          plot.df<-subset(motile, Loc_Name %in% input$siteMoll  & variable %in% input$variable  & Com_Sp %in% input$species)
          
        }
        
      }else{
        
        if(input$SPP =="All species"){
          
          plot.df<-subset(motile, Loc_Name %in% input$siteMoll  & variable %in% "logAbundance" )
          
        }else{
          
          plot.df<-subset(motile, Loc_Name %in% input$siteMoll & variable %in% "logAbundance"  & Com_Sp %in% input$species)
          
        }
        
      }
  
      
    }
        #Define ordering of Zones for plotting
        
        plot.df$Zone<- ordered(plot.df$Zone, levels = c("Red Algae", "Mussels", "Ascophyllum", "Fucus", "Barnacle"))
        
        ## clean up dataframe 
        
        plot.df<-droplevels(plot.df)
        plot.df$Year<-as.factor(plot.df$Year)
        
        if(nrow(plot.df)== 0){
          stop("Sorry, this species has not been collected at this site.")
        }
        
        
       
        
      
      
              
################ BEGIN PLOTTING FUNCTIONS ####################             
  ###Setup dynamic caption
  
  output$captionMoll <- renderText({
    if(input$SPP == "Single"){
    if(input$variable =="Abundance" | input$variable =="logAbundance"){
      
      if(input$compareMoll == TRUE){
        paste0("Average annual abundance of ",input$species, " among sites in each intertidal zone (panels) .",sep =" ")
      }else{
        paste0("Average annual abundance of ",input$species, " among intertidal zones.", sep =" ")
      
      }
      }else{
        if(input$compareMoll == TRUE){
          paste0("Average annual proportion ",input$species, " damaged by predators among sites in each intertidal zone (panels).", sep=" ")
        }else{
        paste0("Average annual proportion of ",input$species, " damaged by predators among intertidal zones with each site (panels).", sep =" ")
        }
      }
    }else{
      if(input$variable =="Abundance"| input$variable =="logAbundance"){
        paste0("Average annual abundance of motile invertebrates among intertidal zones.", sep =" ")
      }else{
      paste0("Average annual proportion of motile invertebrates damaged by predators among intertidal zones.", sep =" ")
      
      }
    }
    
  })    
  
  
        if(input$variable == "Abundance"){
     
          # setup some graphic arguments
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
               theme(legend.position = "top", legend.text = element_text(size = 16), legend.title = element_text(size =16)) +
               theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
               theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14 , face="bold")) +
               theme(strip.text.x= element_text(size=14, face=c("bold.italic"))) +
               theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
               theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
               theme(panel.background =  element_rect(fill="white", colour="black")) +
               theme(panel.grid.major = element_line(colour = "grey90"))+
               scale_x_discrete(limits=rev(levels(plot.df$Zone)))+
               theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
               theme(strip.background= element_rect(size=10, color="gray" )))
        
        }else{
          
          y2<-(y2+facet_wrap(~Loc_Name, ncol =1, scales="fixed") +
                 theme(legend.position = "top", legend.text = element_text(size = 16), legend.title = element_text(size =16)) +
                 theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
                 theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14, face="bold")) +
                 theme(strip.text.x= element_text(size=14, face=c("bold.italic"))) +
                 theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                 theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                 theme(panel.background =  element_rect(fill="white", colour="black")) +
                 theme(panel.grid.major = element_line(colour = "grey90"))+
                 scale_x_discrete(limits=rev(levels(plot.df$Zone)))+
                 theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
                 theme(strip.background= element_rect(size=10, color="gray" )))
        }
       
  if(input$SPP =="All species"){
    if(input$manyMoll == "One site"){
      y2<-(y2+facet_wrap(~Com_Sp, ncol=1) + theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16, face="bold")) +
        theme(strip.text.x= element_text(size=16, face=c("bold.italic"))) )
      
    }else{
      y2<-(y2+facet_grid(Loc_Name ~ Common +Spp_Name)+ 
            theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 16, face="bold"))+ 
           theme(strip.text.x= element_text(size=11, face=c("bold.italic"))))
    } 
    
    y2<-(y2 + theme(legend.position = "top", legend.text = element_text(size = 16), legend.title = element_text(size =16)) +
      theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
   
      theme(strip.text.y= element_text(size=12, face=c("bold")))+
      theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
      theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
      theme(panel.background =  element_rect(fill="white", colour="black")) +
      theme(panel.grid.major = element_line(colour = "grey90"))+
        scale_x_discrete(limits=rev(levels(plot.df$Zone)))+
        theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
      theme(strip.background= element_rect(size=10, color="gray" )))
  }
  ####### Compare data among sites within an intertidal zone ############
        ### Still in progress
        
 if(input$compareMoll == TRUE){      
        
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
                 theme(legend.position = "top", legend.text = element_text(size = 16), legend.title = element_text(size =16)) +
                 theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
                 theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14 , face="bold")) +
                 theme(strip.text.x= element_text(size=14, face=c("bold.italic"))) +
                 theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                 theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                 theme(panel.background =  element_rect(fill="white", colour="black")) +
                 scale_x_discrete(limits=rev(levels(plot.df$Zone)))+
                 theme(panel.grid.major = element_line(colour = "grey90"))+
                 theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
                 theme(strip.background= element_rect(size=10, color="gray" )))
        }else{
          
          y2<-(y2+facet_wrap(~Zone, ncol =1, scales="fixed") +
                 theme(legend.position = "top", legend.text = element_text(size = 16), legend.title = element_text(size =16)) +
                 theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
                 theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14, face="bold")) +
                 theme(strip.text.x= element_text(size=14, face=c("bold.italic"))) +
                 theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                 theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
                 scale_x_discrete(limits=rev(levels(plot.df$Zone)))+
                 theme(panel.background =  element_rect(fill="white", colour="black")) +
                 theme(panel.grid.major = element_line(colour = "grey90"))+
                 theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
                 theme(strip.background= element_rect(size=10, color="gray" )))
        }
         
   if(input$SPP =="All species"){
     y2<-(y2+facet_grid(Zone ~ Spp_Name+common) +
            theme(legend.position = "top", legend.text = element_text(size = 16), legend.title = element_text(size =16)) +
            theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
            theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 12, face="bold")) +
            theme(strip.text.x= element_text(size=11, face=c("bold.italic"), panel.margin = unit(0.25, "cm"))) +
            theme(strip.text.y= element_text(size=12, face=c("bold")))+
            scale_x_discrete(limits=rev(levels(plot.df$Zone)))+
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

  
      
      #####################################################  tidepools plots  panel  #####################################################        
  ###################### Create set of reactive selection boxes in UI  ####################
  ### select site based on park
  output$SiteResultsSS <- renderUI({ 
    
    df_sub<-subset(echino, Site_Name %in% input$parkSS)
    df_sub<-droplevels(df_sub)
    
    selectInput(inputId='siteSS', label='Select Site',   unique(levels(df_sub$Loc_Name)))
  })    
  
  
  output$plot2 <- renderPlot({
    
    ############## DATA MANIPULATION ##############################        
    if(input$logscaleSS == FALSE){
      if(input$manySS == "All sites"){
      plot.df<-subset(echino, Site_Name %in% input$parkSS & variable == "value") # select by park and variable ("value" is the raw data, "logAbundance the log transformed)
      }else{
        plot.df<-subset(echino, Loc_Name %in% input$siteSS & variable == "value") # select by site and variable ("value" is the raw data, "logAbundance the log transformed)
      }
     
    }else{
      if(input$manySS == "All sites"){
        plot.df<-subset(echino, Site_Name %in% input$parkSS & variable == "logAbundance") # select by park and variable ("value" is the raw data, "logAbundance the log transformed)
      }else{
        plot.df<-subset(echino, Loc_Name %in% input$siteSS & variable == "logAbundance") # select by site and variable ("value" is the raw data, "logAbundance the log transformed)
      }
    }
    
    plot.df<-droplevels(plot.df)
    plot.df$Year<-as.factor(plot.df$Year)
    plot.df$Year<- ordered(plot.df$Year, levels = c("2016", "2015", "2014", "2013"))
    
    
    ############## PLOT mean over time  ##############################
    dodge <- position_dodge(width=0.9)
    
    if(input$compSS == FALSE){      
      if(input$logscaleSS ==FALSE){
        
        y2<-ggplot(plot.df, aes(x=Common, y= as.numeric(mean), fill= Year))+ geom_bar(stat ="identity", position = dodge,colour="black") +
          geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+scale_fill_brewer(palette="Blues")+ labs(y = "Mean number  + SE", x= "")
      }else {
        y2<-ggplot(plot.df, aes(x=Common, y= as.numeric(mean), fill= Year))+ geom_bar(stat ="identity", position = dodge,colour="black") +
          geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+scale_fill_brewer(palette="Blues")+ labs(y = "log Mean number + SE", x= "")
      }
    }else{
      if(input$logscaleSS ==FALSE){
        
        y2<-ggplot(plot.df, aes(x=Loc_Name, y= as.numeric(mean), fill= Year))+ geom_bar(stat ="identity", position = dodge,colour="black") +
          geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+scale_fill_brewer(palette="Blues")+ labs(y = "Mean number  + SE", x= "")
      }else {
        y2<-ggplot(plot.df, aes(x=Loc_Name, y= as.numeric(mean), fill= Year))+ geom_bar(stat ="identity", position = dodge,colour="black") +
          geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+scale_fill_brewer(palette="Blues")+ labs(y = "log Mean number + SE", x= "")
      }
      
    }
    
    if(input$compSS == FALSE){ 
    y2<-(y2+facet_wrap(~Loc_Name))
         
      }else{
    
        y2<-(y2+facet_wrap(~Com_Sp))
      
    }
         
         
    y2<-(y2+ coord_flip()+
           theme(legend.position = "right", legend.text = element_text(size = 16), legend.title = element_text(size =16)) +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold.italic"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14 , face="bold")) +
           theme(strip.text.x= element_text(size=16, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 0, debug=F))+
           theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+
           theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
           scale_x_discrete(limits=rev(levels(plot.df$Common_Name)))+ # use with coord_flip
           theme(strip.background= element_rect(size=10, color="gray" )))
    
    
    print(y2)
  }
  
  , height = 800, width = 1000)
  
  ###Setup dynamic caption
  
  output$captionSS <- renderText({
      "Average annual abundance of invertebrates observed within tide pools."
  
  })
  
  
      
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

