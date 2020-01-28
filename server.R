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
  
  
  
  ####################VERTICAL TRANSECT DATA ####################################
  
  ##### plOTTING PANEL ####
  
  ### select site based on park
  output$SiteResultsA <- renderUI({ 
    
    df_sub<-subset(transect_yr, Site_Name %in% input$park)
    df_sub<-droplevels(df_sub)
    
    selectInput(inputId='site', label='Select Site',   unique(levels(df_sub$Loc_Name)))
  })
  
  #################### Transect data plots ##############################################
  ### 5/12/17: currently only plots per site in each year. 
  
  output$plot1 <- renderPlot({
    
    ############## DATA MANIPULATION ##############################        
    if(input$many == "All sites"){
      
      plot.df<-subset(transect_yr, Site_Name %in% input$park & QAQC == FALSE) # select by park and drop the QAQC plots
      plot.df<-droplevels(plot.df)
      plot.df$Year<-as.factor(plot.df$Year)
      #plot.df$Year<- ordered(plot.df$Year, levels = c("2016", "2015", "2014", "2013"))
      
    }else{
      
      ## SUBSET BY SITE 
      
      
      plot.df<-subset(transect_yr, Loc_Name %in% input$site & QAQC == FALSE)# select by site and drop the QAQC plots
      plot.df<-droplevels(plot.df)
      plot.df$Year<-as.factor(plot.df$Year)
      #plot.df$Year<- ordered(plot.df$Year, levels = c("2016", "2015", "2014", "2013"))
    }
    ############## PLOT mean over time  ##############################
    dodge <- position_dodge(width=0.9)
    
    if(input$many == "All sites"){
      if(input$compare == "Cover types within a site" ){
        
        y2<-ggplot(plot.df[plot.df$QAQC == FALSE,], aes(x=Common_Name, y= as.numeric(mean), fill= Year))+
          geom_bar(stat ="identity", position = dodge,colour="black") + labs(y = "Average proportion of cover + SE", x= "") +
          
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
        
        y2<-ggplot(plot.df[plot.df$QAQC == FALSE,], aes(x=Loc_Name, y= as.numeric(mean), fill= Year))+
          geom_bar(stat ="identity", position = dodge,colour="black") + labs(y = "Average proportion of cover + SE", x= "") +
          
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
    }else{
      y2<-ggplot(plot.df[plot.df$QAQC == FALSE,], aes(x=Common_Name, y= as.numeric(mean), fill= Year))+
        geom_bar(stat ="identity", position = dodge,colour="black") + labs(y = "Average proportion of cover + SE", x= "") +
        
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
      
    }
    
    
    print(y2)
  }
  
  , height = 800, width = 1000)
  
  ###Setup dynamic plot caption
  
  output$captionVertTrans <- renderText({
    "Average site-level annual cover of the 10 most abundant species/cover types in the park estimated by point-intercept sampling."
  })
  
  ######## VERTICAL TRANSECT TABULAR VIEW ################
  
  ###### Create set of reactive selection boxes in UI for TABLE  #####
  
  
  ### select site based on park
  output$SiteResultsB <- renderUI({ 
    
    df_sub<-subset(transect_yr, Site_Name %in% input$park)
    df_sub<-droplevels(df_sub)
    
    selectInput(inputId='siteb', label='Select Site',   unique(levels(df_sub$Loc_Name)))
  })
  
  
  #### Create summary data for tabular presentation and downloading
  datasetInput <- reactive({
    if(input$manyB == "All sites"){
      
      table.df<-subset(transect_yr, Site_Name %in% input$park & QAQC == FALSE) # select by park and drop the QAQC plots
      table.df<-table.df[!is.na(table.df$mean),]
      table.df<-droplevels(table.df)
      
    }else{
      
      ## SUBSET BY SITE
      table.df<-subset(transect_yr, Loc_Name %in% input$siteb & QAQC == FALSE)# select by site and drop the QAQC plots
      table.df<-table.df[!is.na(table.df$mean),]
      table.df<-droplevels(table.df)
    }
    
    table.df<-as.data.frame(table.df[,c("Loc_Name","Year","Common_Name",  "mean"   ,"se")])
  })
  
  ### show Data table
  output$Transectsumtable <-DT::renderDataTable(DT::datatable({
    
    datasetInput()
    
    
  }, rownames = FALSE, colnames = c("Site Name","Year","Species or Cover type",  "Average proportion of cover" ,"St. Error"),
  caption = 'Average annual cover per site of the 10 most abundant species/cover types within the park estimated by point-intercept sampling along three parallel transects.',
  filter = 'bottom', class = 'cell-border stripe'
  ))
  
  output$downloadsumData <- downloadHandler(
    filename = function() { 
      paste('NETN_Rocky_Intertidal_Summ_Transect_Data.csv') 
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = F)
    }
  )
  
  ####   #### Dowload raw data   ####   #### 
  datasetInputRaw <- reactive({
    if(input$manyB == "All sites"){
      
      out.df<-subset(transect_raw, Site_Name %in% input$park & QAQC == FALSE) # select by park and drop the QAQC plots
      out.df<-droplevels(out.df)
      
    }else{
      
      ## SUBSET BY SITE
      out.df<-subset(transect_raw, Loc_Name %in% input$siteb & QAQC == FALSE)# select by site and drop the QAQC plots
      out.df<-droplevels(out.df)
    }
    
    out.df
  })
  
  
  output$downloadDataRaw <- downloadHandler(
    filename = function() { 
      paste('NETN_Rocky_Intertidal_Raw_Transect_Data.csv') 
    },
    content = function(file) {
      write.csv(datasetInputRaw(), file, row.names = F)
    }
  )
  
  ####################  Mollusk Plot  panel ##############################################
  ####### Reactive list of sites ####################      
  output$SiteResultsMoll <- renderUI({ 
    
    df_sub<-subset(motile, Site_Name %in% input$parkMoll)
    df_sub<-droplevels(df_sub)
    
    selectInput(inputId='siteMoll', label='Select Site', unique(levels(df_sub$Loc_Name)))
  })
  ### Summary plot by site and intertidal zone
  
  output$plot <- renderPlot({
    
    
    ############## DATA MANIPULATION ##############################        
    ## SUBSET MOTILE DF BY PARK, SPECIES, VARIABLE
    ## data only have non-QAQC plots
    #  data file 'motile' from 'import and summ motile invert data.R'
    
    if(input$variable == "Abundance"){
      if(input$manyMoll == "All sites"){
        if(input$SPP =="All species"){
          if(input$logscale == FALSE){ 
            
            plot.df<-subset(motile, Site_Name %in% input$parkMoll  & variable %in% "Abundance" )
            
          }else{
            plot.df<-subset(motile, Site_Name %in% input$parkMoll  & variable %in% "logAbundance" )
          }
        }else{
          if(input$logscale == FALSE){ 
            
            plot.df<-subset(motile, Site_Name %in% input$parkMoll  & variable %in% "Abundance" & Com_Sp %in% input$species)
          }else{
            
            plot.df<-subset(motile, Site_Name %in% input$parkMoll  & variable %in% "logAbundance" & Com_Sp %in% input$species)
          }
          
        }
        
      }else{
        
        if(input$SPP =="All species"){
          if(input$logscale == FALSE){ 
            
            plot.df<-subset(motile, Loc_Name %in% input$siteMoll  & variable %in% "Abundance" )
            
          }else{
            plot.df<-subset(motile, Loc_Name %in% input$siteMoll  & variable %in% "logAbundance" )
          }
        }else{
          if(input$logscale == FALSE){ 
            
            plot.df<-subset(motile, Loc_Name %in% input$siteMoll  & variable %in% "Abundance" & Com_Sp %in% input$species)
          }else{
            
            plot.df<-subset(motile, Loc_Name %in% input$siteMoll  & variable %in% "logAbundance" & Com_Sp %in% input$species)
          }
          
        }
      }
    }
    
    if(input$variable == "Proportion.Damaged"){
      if(input$manyMoll == "All sites"){
        if(input$SPP =="All species"){
          plot.df<-subset(motile, Site_Name %in% input$parkMoll  & variable %in% "Proportion.Damaged" )
          
        }else{
          plot.df<-subset(motile, Site_Name %in% input$parkMoll  & variable %in% "Proportion.Damaged" & Com_Sp %in% input$species)
        }
        
      }else{
        
        if(input$SPP =="All species"){
          plot.df<-subset(motile, Loc_Name %in% input$siteMoll  & variable %in% "Proportion.Damaged" )
          
        }else{
          plot.df<-subset(motile, Loc_Name %in% input$siteMoll  & variable %in% "Proportion.Damaged" & Com_Sp %in% input$species)
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
          
          if(input$compareMoll == "Compare among sites"){
            paste0("Average annual abundance of ",input$species, " among sites in each intertidal zone (panels).",sep =" ")
          }else{
            paste0("Average annual abundance of ",input$species, " among intertidal zones.", sep =" ")
            
          }
        }else{
          if(input$compareMoll == "Compare among sites"){
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
    
    #### SETUP PLOTS #####
    
    # setup some graphic arguments
    dodge <- position_dodge(width=0.9)
    
    
    if(input$compareMoll == "Compare within a site"){
      ####### Compare data among intertidal zones within a site ############
      if(input$variable == "Abundance"){
        if(input$logscale == TRUE){
          
          y2<-ggplot(plot.df, aes(x=Zone, y= as.numeric(mean), fill= Year))+
            geom_bar(stat ="identity",colour="black", position = dodge) + labs(y = expression(paste("log Average number m"^"-2", "+ SE")), x= "Intertidal Zone") +
            geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+ scale_fill_brewer(palette="Blues")
          
        }else{
          
          y2<-ggplot(plot.df, aes(x=Zone, y= as.numeric(mean), fill= Year))+
            geom_bar(stat ="identity",colour="black", position = dodge) + 
            labs(y = expression(paste("Average number m"^"-2", "+ SE")), x= "Intertidal Zone") +
            geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+ scale_fill_brewer(palette="Blues")
          
        }
      }else{
        
        ## plot mean prop damaged
        
        y2<-ggplot(plot.df, aes(x=Zone, y= as.numeric(mean), fill= Year))+
          geom_bar(stat ="identity", colour="black",position = dodge) + 
          labs(y = expression(paste("Average proportion damaged m"^"-2", "+ SE")), x= "Intertidal Zone") +
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
    }else{ 
      ####### Compare data among sites within an intertidal zone ############
      
      if(input$variable == "Abundance"){
        if(input$logscale == TRUE){
          
          dodge <- position_dodge(width=0.9)
          
          y2<-ggplot(plot.df, aes(x=Loc_Name, y= as.numeric(mean), fill= Year))+
            geom_bar(stat ="identity",colour="black", position = dodge) + labs(y = expression(paste("log Average number m"^"-2", "+ SE")), x= "Site Name") +
            geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+ scale_fill_brewer(palette="Blues")
          
        } else{
          dodge <- position_dodge(width=0.9)
          
          y2<-ggplot(plot.df, aes(x=Loc_Name, y= as.numeric(mean), fill= Year))+
            geom_bar(stat ="identity",colour="black", position = dodge) + 
            labs(y = expression(paste("Average number m"^"-2", "+ SE")), x= "Site Name") +
            geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+ scale_fill_brewer(palette="Blues")
          
        }
      }else{
        
        ## plot mean prop damaged
        dodge <- position_dodge(width=0.9)
        
        y2<-ggplot(plot.df, aes(x=Loc_Name, y= as.numeric(mean), fill= Year))+
          geom_bar(stat ="identity", colour="black",position = dodge) + 
          labs(y = expression(paste("Average proportion damaged m"^"-2", "+ SE")), x= "Site Name") +
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
               #scale_x_discrete(limits=rev(levels(plot.df$Loc_Name)))+
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
               #scale_x_discrete(limits=rev(levels(plot.df$Loc_Name)))+
               theme(panel.background =  element_rect(fill="white", colour="black")) +
               theme(panel.grid.major = element_line(colour = "grey90"))+
               theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
               theme(strip.background= element_rect(size=10, color="gray" )))
      }
      
      if(input$SPP =="All species"){
        y2<-(y2+facet_grid(Zone ~ Spp_Name+Common) +
               theme(legend.position = "top", legend.text = element_text(size = 16), legend.title = element_text(size =16)) +
               theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold"))+
               theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 12, face="bold")) +
               #theme(strip.text.x= element_text(size=11, face=c("bold.italic"), panel.margin = unit(0.25, "cm"))) +
               theme(strip.text.y= element_text(size=12, face=c("bold")))+
               # scale_x_discrete(limits=rev(levels(plot.df$Loc_Name)))+
               theme(axis.title.x =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
               theme(axis.title.y =element_text(size = 16, face ="bold", vjust= 1, debug=F))+
               theme(panel.background =  element_rect(fill="white", colour="black")) +
               theme(panel.grid.major = element_line(colour = "grey90"))+
               theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
               theme(strip.background= element_rect(size=10, color="gray" )))
      }
      
      
    }
    print(y2)     
    
  } # end Mollusks RenderPlot
  
  , height = 800, width = 1000)
  
  ######## MOLLUSK TABULAR VIEW ################
  ###### Create set of reactive selection boxes in UI for TABLE  #####
  ### select site based on park
  
  output$SiteResultsMollTab <- renderUI({ 
    
    df_sub<-subset(motile, Site_Name %in% input$parkMollTable)
    df_sub<-droplevels(df_sub)
    
    selectInput(inputId='siteMollTab', label='Select Site',   unique(levels(df_sub$Loc_Name)))
  })
  
  #### Create reactive summary data for presentation and downloading
  datasetInputMoll <- reactive({
    ## SUBSET MOTILE DF BY PARK, SPECIES, VARIABLE
    ## data only have non-QAQC plots
    #  data file from 'import and summ motile invert data.R'
    
    if(input$variableTab == "Abundance"){
      if(input$manyMoll_Tab == "All sites"){
        if(input$SPP_MOLL_TAB =="All species"){
          if(input$logscaleTab == FALSE){ 
            
            table.df<-subset(motile, Site_Name %in% input$parkMollTable  & variable %in% "Abundance" )
            
          }else{
            table.df<-subset(motile, Site_Name %in% input$parkMollTable  & variable %in% "logAbundance" )
          }
        }else{
          if(input$logscaleTab == FALSE){ 
            
            table.df<-subset(motile, Site_Name %in% input$parkMollTable  & variable %in% "Abundance" & Com_Sp %in% input$species)
          }else{
            
            table.df<-subset(motile, Site_Name %in% input$parkMollTable  & variable %in% "logAbundance" & Com_Sp %in% input$species)
          }
          
        }
        
      }else{
        
        if(input$SPP_MOLL_TAB =="All species"){
          if(input$logscaleTab == FALSE){ 
            
            table.df<-subset(motile, Loc_Name %in% input$siteMollTab  & variable %in% "Abundance" )
            
          }else{
            table.df<-subset(motile, Loc_Name %in% input$siteMollTab  & variable %in% "logAbundance" )
          }
        }else{
          if(input$logscaleTab == FALSE){ 
            
            table.df<-subset(motile, Loc_Name %in% input$siteMollTab  & variable %in% "Abundance" & Com_Sp %in% input$species)
          }else{
            
            table.df<-subset(motile, Loc_Name %in% input$siteMollTab  & variable %in% "logAbundance" & Com_Sp %in% input$species)
          }
          
        }
      }
    }
    
    if(input$variableTab == "Proportion.Damaged"){
      if(input$manyMoll_Tab == "All sites"){
        if(input$SPP_MOLL_TAB =="All species"){
          table.df<-subset(motile, Site_Name %in% input$parkMollTable  & variable %in% "Proportion.Damaged" )
          
        }else{
          table.df<-subset(motile, Site_Name %in% input$parkMollTable  & variable %in% "Proportion.Damaged" & Com_Sp %in% input$species)
        }
        
      }else{
        
        if(input$SPP =="All species"){
          table.df<-subset(motile, Loc_Name %in% input$siteMollTab  & variable %in% "Proportion.Damaged" )
          
        }else{
          table.df<-subset(motile, Loc_Name %in% input$siteMollTab  & variable %in% "Proportion.Damaged" & Com_Sp %in% input$species)
        }
      }
    }
    
    #Define ordering of Zones for plotting
    
    table.df$Zone<- ordered(table.df$Zone, levels = c("Red Algae", "Mussels", "Ascophyllum", "Fucus", "Barnacle"))
    
    ## clean up dataframe 
    
    table.df<-droplevels(table.df)
    table.df$Year<-as.factor(table.df$Year)
    
    table.df<-table.df[,c("Loc_Name","Year","Com_Sp", "Zone", "mean"   ,"se")]
    
  })
  
  
  ### show table
  output$TransectsumtableMoll <-DT::renderDataTable(DT::datatable({
    
    datasetInputMoll()}
    , 
    rownames = FALSE, 
    colnames = c("Site Name","Year","Species", "Intertidal Zone",paste0("Average ",input$variableTab,sep= " ") ,"St. Error"),
    caption = 
      if(input$variableTab == "Abundance"){
        "Average annual abundance (per m2) of motile invertbrates in each intertidal zone."
      }else{
        'Average annual proportion of motile invertebrates depredated on in each intertidal zone.'
      }
    ,
    filter = 'bottom', class = 'cell-border stripe'
  ))
  
  
  
  output$downloadsumDataMoll <- downloadHandler(
    filename = function() { 
      paste('NETN_Rocky_Intertidal_Mollusk_Summ_Data.csv') 
    },
    content = function(file) {
      write.csv(datasetInputMoll(), file, row.names = F)
    }
  )
  
  ####   #### Dowload raw data   ####   #### 
  datasetInputRawMoll <- reactive({
    # raw data file from 'import and summ motile invert data.R'; has QAQC plots so below codes removes those
    
    
    if(input$variableTab == "Abundance"){
      if(input$manyMoll_Tab == "All sites"){
        if(input$SPP_MOLL_TAB =="All species"){
          if(input$logscaleTab == FALSE){ 
            
            table.df<-subset(motile_raw, Site_Name %in% input$parkMollTable  & variable %in% "Abundance" & QAQC == 0 )
            
          }else{
            table.df<-subset(motile_raw, Site_Name %in% input$parkMollTable  & variable %in% "logAbundance" & QAQC == 0 )
          }
        }else{
          if(input$logscaleTab == FALSE){ 
            
            table.df<-subset(motile_raw, Site_Name %in% input$parkMollTable  & variable %in% "Abundance" & Com_Sp %in% input$species & QAQC == 0 )
          }else{
            
            table.df<-subset(motile_raw, Site_Name %in% input$parkMollTable  & variable %in% "logAbundance" & Com_Sp %in% input$species & QAQC == 0 )
          }
          
        }
        
      }else{
        
        if(input$SPP_MOLL_TAB =="All species"){
          if(input$logscaleTab == FALSE){ 
            
            table.df<-subset(motile_raw, Loc_Name %in% input$siteMollTab  & variable %in% "Abundance" & QAQC == 0 )
            
          }else{
            table.df<-subset(motile_raw, Loc_Name %in% input$siteMollTab  & variable %in% "logAbundance" & QAQC == 0 )
          }
        }else{
          if(input$logscaleTab == FALSE){ 
            
            table.df<-subset(motile_raw, Loc_Name %in% input$siteMollTab  & variable %in% "Abundance" & Com_Sp %in% input$species & QAQC == 0 )
          }else{
            
            table.df<-subset(motile_raw, Loc_Name %in% input$siteMollTab  & variable %in% "logAbundance" & Com_Sp %in% input$species & QAQC == 0 )
          }
          
        }
      }
    }
    
    if(input$variableTab == "Proportion.Damaged"){
      if(input$manyMoll_Tab == "All sites"){
        if(input$SPP_MOLL_TAB =="All species"){
          table.df<-subset(motile_raw, Site_Name %in% input$parkMollTable  & variable %in% "Proportion.Damaged"& QAQC == 0  )
          
        }else{
          table.df<-subset(motile_raw, Site_Name %in% input$parkMollTable  & variable %in% "Proportion.Damaged" & Com_Sp %in% input$species & QAQC == 0 )
        }
        
      }else{
        
        if(input$SPP =="All species"){
          table.df<-subset(motile_raw, Loc_Name %in% input$siteMollTab  & variable %in% "Proportion.Damaged" & QAQC == 0 )
          
        }else{
          table.df<-subset(motile_raw, Loc_Name %in% input$siteMollTab  & variable %in% "Proportion.Damaged" & Com_Sp %in% input$species & QAQC == 0 )
        }
      }
    }
    
    motile_raw<-droplevels(motile_raw)
  })
  
  
  output$downloadDataRawMOll <- downloadHandler(
    filename = function() { 
      paste('NETN_Rocky_Intertidal_Raw_Mollusks_Data.csv') 
    },
    content = function(file) {
      write.csv(datasetInputRawMoll(), file, row.names = F)
    }
  )
  
  
  #####################################################  tidepools plots  panel  #####################################################        
  ###################### Create set of reactive selection boxes in UI  ####################
  ### select site based on park
  output$SiteResultsSS <- renderUI({ 
    
    df_sub<-subset(echino, Site_Name %in% input$parkSS & QAQC == "FALSE")
    df_sub<-droplevels(df_sub)
    
    selectInput(inputId='siteSS', label='Select Site',   unique(levels(df_sub$Loc_Name)))
  })    
  
  #### Plot summary data ##### 
  
  output$plot2 <- renderPlot({
    
    ############## DATA MANIPULATION ##############################        
    if(input$logscaleSS == FALSE){
      if(input$manySS == "All sites"){
        plot.df<-subset(echino, Site_Name %in% input$parkSS & variable == "Abundance" & QAQC == "FALSE") # select by park and variable ("value" is the raw data, "logAbundance the log transformed)
      }else{
        plot.df<-subset(echino, Loc_Name %in% input$siteSS & variable == "Abundance"& QAQC == "FALSE") # select by site and variable ("value" is the raw data, "logAbundance the log transformed)
      }
      
    }else{
      if(input$manySS == "All sites"){
        plot.df<-subset(echino, Site_Name %in% input$parkSS & variable == "logAbundance"& QAQC == "FALSE") # select by park and variable ("value" is the raw data, "logAbundance the log transformed)
      }else{
        plot.df<-subset(echino, Loc_Name %in% input$siteSS & variable == "logAbundance"& QAQC == "FALSE") # select by site and variable ("value" is the raw data, "logAbundance the log transformed)
      }
    }
    
    plot.df<-droplevels(plot.df)
    plot.df$Year<-as.factor(plot.df$Year)
    #plot.df$Year<- ordered(plot.df$Year, levels = c("2016", "2015", "2014", "2013"))
    
    
    ############## PLOT mean over time  ##############################
    dodge <- position_dodge(width=0.9)
    
    if(input$manySS == "One site"){
      if(input$logscaleSS ==FALSE){
        
        y2<-ggplot(plot.df, aes(x=Common, y= as.numeric(mean), fill= Year))+ geom_bar(stat ="identity", position = dodge,colour="black") +
          geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+scale_fill_brewer(palette="Blues")+ labs(y = expression(paste("Average number m"^"-2", "+ SE")), x= "")+
          theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14 , face="bold"))
      }else {
        y2<-ggplot(plot.df, aes(x=Common, y= as.numeric(mean), fill= Year))+ geom_bar(stat ="identity", position = dodge,colour="black") +
          geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+scale_fill_brewer(palette="Blues")+ labs(y = expression(paste(" log Average number m"^"-2", "+ SE")), x= "") +
          theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 14 , face="bold"))
        
      }
      
    }else{
      
    
    if(input$compSS == FALSE){      
      if(input$logscaleSS ==FALSE){
        
        y2<-ggplot(plot.df, aes(x=Common, y= as.numeric(mean), fill= Year))+ geom_bar(stat ="identity", position = dodge,colour="black") +facet_wrap(~Loc_Name) +
          geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+scale_fill_brewer(palette="Blues")+ labs(y = expression(paste("Average number m"^"-2", "+ SE")), x= "") +
          theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 14 , face="bold")) 
      }else {
        y2<-ggplot(plot.df, aes(x=Common, y= as.numeric(mean), fill= Year))+ geom_bar(stat ="identity", position = dodge,colour="black") + facet_wrap(~Loc_Name) +
          geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+scale_fill_brewer(palette="Blues")+ labs(y = expression(paste("log Average number m"^"-2", "+ SE")), x= "") +
          theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 14 , face="bold"))
      }
    }else{
      if(input$logscaleSS ==FALSE){
        
        y2<-ggplot(plot.df, aes(x=Loc_Name, y= as.numeric(mean), fill= Year))+ geom_bar(stat ="identity", position = dodge,colour="black") +facet_wrap(~Common + Spp_Name) +
          geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+scale_fill_brewer(palette="Blues")+ labs(y = expression(paste("Average number m"^"-2", "+ SE")), x= "Site Name") +
          theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 14 , face="bold"))
      }else {
        y2<-ggplot(plot.df, aes(x=Loc_Name, y= as.numeric(mean), fill= Year))+ geom_bar(stat ="identity", position = dodge,colour="black") + facet_wrap(~Common + Spp_Name)+
          geom_errorbar(aes(ymax = mean + se, ymin=mean), position=dodge, width=0.1)+scale_fill_brewer(palette="Blues")+ labs(y = expression(paste("log Average number m"^"-2", "+ SE")), x= "Site Name")+
          theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 14 , face="bold"))
      }
      
    }
    }
    
  
    y2<-(y2+ 
           theme(legend.position = "right", legend.text = element_text(size = 16), legend.title = element_text(size =16)) +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 13,face="bold.italic"))+
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
  
 ######### TIDEPOOL TABULAR VIEW ############
  ### select site based on park
  output$SiteResultsSSTab <- renderUI({ 
    
    df_sub<-subset(echino, Site_Name %in% input$parkSSTab & QAQC == "FALSE")
    df_sub<-droplevels(df_sub)
    
    selectInput(inputId='siteSSTab', label='Select Site',   unique(levels(df_sub$Loc_Name)))
  })   
  
  #### Create reactive summary data for presentation and downloading
  datasetInputTidepool <- reactive({

    if(input$logscaleSSTab == FALSE){
      if(input$manySSTab == "All sites"){
        table.df<-subset(echino, Site_Name %in% input$parkSSTab & variable == "Abundance"& QAQC == "FALSE") # select by park and variable ("value" is the raw data, "logAbundance the log transformed)
      }else{
        table.df<-subset(echino, Loc_Name %in% input$siteSSTab & variable == "Abundance"& QAQC == "FALSE") # select by site and variable ("value" is the raw data, "logAbundance the log transformed)
      }
      
    }else{
      if(input$manySSTab == "All sites"){
        table.df<-subset(echino, Site_Name %in% input$parkSSTab & variable == "logAbundance"& QAQC == "FALSE") # select by park and variable ("value" is the raw data, "logAbundance the log transformed)
      }else{
        table.df<-subset(echino, Loc_Name %in% input$siteSSTab & variable == "logAbundance"& QAQC == "FALSE") # select by site and variable ("value" is the raw data, "logAbundance the log transformed)
      }
    }
    
    table.df<-droplevels(table.df)
    table.df$Year<-as.factor(table.df$Year)
    table.df<-table.df[,c("Loc_Name","Year","Com_Sp",  "mean"   ,"se")]
    
  })
  
  
  ### show table
  output$TransectsumtableTidePool <-DT::renderDataTable(DT::datatable({
    
    datasetInputTidepool()}
    , 
    rownames = FALSE, 
    colnames = c("Site Name","Year","Species","Average number per m2" ,"St. Error"),
    caption = 'Average annual site abundance per 1m2 of tidepool invertebrates. Counts are made within in 3, 2 X 10m transects per site.',
    filter = 'bottom', class = 'cell-border stripe'
  )) 
  
  output$downloadsumDataTidePool <- downloadHandler(
    filename = function() { 
      paste('NETN_Rocky_Intertidal_TidePool_Summ_Data.csv') 
    },
    content = function(file) {
      write.csv(datasetInputTidepool(), file, row.names = F)
    }
  )
  
  ####   #### create raw data for download   ####   #### 
  datasetInputRawTidePool <- reactive({
    
    if(input$logscaleSSTab == FALSE){
      if(input$manySSTab == "All sites"){
        table.df<-subset(echino_raw, Site_Name %in% input$parkSSTab & variable == "Abundance") # select by park and variable ("value" is the raw data, "logAbundance the log transformed)
      }else{
        table.df<-subset(echino_raw, Loc_Name %in% input$siteSSTab & variable == "Abundance" ) # select by site and variable ("value" is the raw data, "logAbundance the log transformed)
      }
      
    }else{
      if(input$manySSTab == "All sites"){
        table.df<-subset(echino_raw, Site_Name %in% input$parkSSTab & variable == "logAbundance" ) # select by park and variable ("value" is the raw data, "logAbundance the log transformed)
      }else{
        table.df<-subset(echino_raw, Loc_Name %in% input$siteSSTab & variable == "logAbundance") # select by site and variable ("value" is the raw data, "logAbundance the log transformed)
      }
    }
    
    table.df<-droplevels(table.df)
    table.df$Year<-as.factor(table.df$Year)
   
    
  })
  
  
  output$downloadDataRawTP <- downloadHandler(
    filename = function() { 
      paste('NETN_Rocky_Intertidal_Raw_TidePool_Data.csv') 
    },
    content = function(file) {
      write.csv(datasetInputRawTidePool(), file, row.names = F)
    }
  )
  
  
  
  
}) ## end shiny serverfunc    



