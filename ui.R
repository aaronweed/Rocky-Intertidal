
# The user-interface (ui) script controls the layout and appearance of your app. 

library(shiny)
library(leaflet)
library(shinyjs)
library(DT)


shinyUI(navbarPage(title=HTML("<div> <a href='https://science.nature.nps.gov/im/units/netn/'> <img src='ah_small_black.gif',
          alt='WQ Visualizer'> </a> NETN Rocky Intertidal Community Visualizer</div>"),position = "static-top", inverse=TRUE, collapsible = FALSE, fluid=TRUE, 
           windowTitle = "NETN Rocky Intertidal Community Visualizer", id="MainNavBar",
     
######################################### Vertical transect data Panel ####################################################################
tabPanel(title="Vertical transect data",
         #style="padding: 0",
         useShinyjs(),
         div(class="outer",
             #tags$head(includeCSS("./www/mapstyles.css") ), # defines css file
             tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />'))
             #puts up icon on tab
             #, tags$head(includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"))#,
         ),
         
         fluidPage(
           sidebarPanel(
             h1(""),
             h3("Plot average cover of species or substrate types over time."),
             br(),
             #Park selection
             tags$div(title="Choose a park",selectInput(inputId='park', label='Select Park', choices= ParkList_trans, selectize = TRUE,selected ="Acadia NP")),
             br(),
             
             
             # Selection to plot single or multple sites
             tags$div(title="Choose between plotting one or multiple sites",radioButtons(inputId='many', label='Do you want to plot data from one or multiple sites?', choices= c("One site","All sites"), selected = "All sites")),
             
             
             # Site selection
             conditionalPanel(condition = "input.many == 'One site'", uiOutput("SiteResultsA")),
             br(),
             
             ## comparisons when selecting multiple sites
             tags$div(title="Compare cover types",conditionalPanel(condition = "input.many == 'All sites'",radioButtons(inputId='compare', label='How do you want compare cover data?', choices= c("Cover types within a site","Cover type among sites"), selected = "Cover types within a site"))),
             
             
             # plot combined years or by year
            # tags$div(title="Plot abundance on log-scale ", checkboxInput(inputId='logscale', label='Plot across all years', value=FALSE))),
             
             # tags$div(title="Toggle y-scale ", conditionalPanel(condition = "input.SPP == 'Single'", checkboxInput(inputId='free_y', label='Make scale of y-axes the same', value=FALSE))),
             # 
             # tags$div(title="Compare sites within same zone", checkboxInput(inputId='compare', label='Compare data among sites within an intertidal zone', value=FALSE)),
             # 
             br(),
             
             #downloadButton('downloadData', 'Download Data'),
             img(src = "transects.jpg", height = 280, width = 360),
            h6("Photo: Ed Sharron, NPS"),
             br(),
             br(),
             p("For further information about the objectives and methods of this sampling protocol, visit the ", 
               a("NETN Rocky Intertidal Community protocol page.", href= "https://science.nature.nps.gov/im/units/netn/monitor/programs/rockyIntertidal/rockyIntertidal.cfm")),
             br()
           ),
           
           mainPanel(h3(textOutput("captionVertTrans",  container = span)),
                     plotOutput("plot1",  width = "100%")
                     
                     
           )
           
         )
), #end navbarPage

tabPanel(title="Mollusks",
         #style="padding: 0",
                    useShinyjs(),
         div(class="outer",
             #tags$head(includeCSS("./www/mapstyles.css") ), # defines css file
             tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />'))
              #puts up icon on tab
            #, tags$head(includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"))#,
         ),
         
fluidPage(
  sidebarPanel(
    h1(""),
    h3("Plot average abundance and predation damage of motile invertebrates within rocky intertidal zones."),
    br(),
    #Park selection
    tags$div(title="Choose the park you want to work with",selectInput(inputId='parkMoll', label='Select Park', choices= ParkList_trans, selectize = TRUE)),
    
    # Selection to plot single or multple sites
    tags$div(title="Choose between plotting data from one or multiple sites",radioButtons(inputId='manyMoll', label='Do you want to plot data from one or multiple sites?', choices= c("One site","All sites"), selected = "All sites")),
    
    tags$div(title="Compare among sites within same intertidal zone", conditionalPanel(condition = "input.manyMoll == 'All sites'", checkboxInput(inputId='compareMoll', label='Compare data among sites within an intertidal zone', value=FALSE))),
    
    
    # Site selection
    conditionalPanel(condition = "input.manyMoll == 'One site'", uiOutput("SiteResultsMoll")),
    br(),
    
    # Selection to plot single or multple species
    tags$div(title="HOw many species data do  you want to plot?",radioButtons(inputId='SPP', label='Do you want to plot single or multiple species?', choices= c("Single","All species"), selected = "Single")),
    
    # Species selection
    tags$div(title="Choose the species abundance data you want to plot",conditionalPanel(condition = "input.SPP == 'Single'", selectInput(inputId='species', label='Select species to plot', choices=SppList)),
    
    # Variable selection
    tags$div(title="Choose the data you want to plot",selectInput(inputId='variable', label='Select type of plot', selected = "Abundance" ,choices=  c("Abundance", "Proportion.Damaged")))),
    
    
    ##Add in options for plots
    
   tags$div(title="Plot abundance on log-scale ", conditionalPanel(condition = "input.variable == 'Abundance'", checkboxInput(inputId='logscale', label='Convert abundance to log-scale', value=FALSE))),
    tags$div(title="Toggle y-scale ", conditionalPanel(condition = "input.SPP == 'Single'", checkboxInput(inputId='free_y', label='Make scale of y-axes the same', value=FALSE))),
    
  br(),
   
    #downloadButton('downloadData', 'Download Data'),
    #img(src = "BMI_sampling.jpg", height = 140, width = 180),
    br(),
    br(),
    p("For further information about the objectives and methods of this sampling protocol, visit the ", 
    a("NETN Rocky Intertidal Community protocol page.", href= "https://science.nature.nps.gov/im/units/netn/monitor/programs/rockyIntertidal/rockyIntertidal.cfm")),
    br()
    ),
   #img(src = "transects.jpg", height = 280, width = 360),
      
  mainPanel(h3(textOutput("captionMoll",  container = span)),
      plotOutput("plot",  width = "100%")
    
                        )
    
  )
  ), #end navbarPage

######################################### Tidepool Plot Panel ####################################################################

tabPanel(title="Tidepool invertebrate surveys",
         #style="padding: 0",
         useShinyjs(),
         div(class="outer",
             tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />'))
               ),
         
         fluidPage(
           sidebarPanel(
             h1(""),
             h3("Plot average abundance of tidal pools within the rocky intertidal."),
             br(),
             #Park selection
             tags$div(title="Choose the park you want to work with",selectInput(inputId='parkSS', label='Select Park', choices= ParkList, selectize = TRUE)),
             
             
             # Selection to plot single or multple sites
             tags$div(title="Choose between plotting one or multiple sites",radioButtons(inputId='manySS', label='Do you want to plot data from one or multiple sites?', choices= c("One site","All sites"), selected = "All sites")),
             
             
             # Site selection
             conditionalPanel(condition = "input.manySS == 'One site'", uiOutput("SiteResultsSS")),
             
             ##Add in options
             tags$div(title="Plot abundance on log-scale ", checkboxInput(inputId='logscaleSS', label='Convert abundance to log-scale', value=FALSE)),
             tags$div(title="Compare species abundance among sites.", conditionalPanel(condition = "input.manySS == 'All sites'", checkboxInput(inputId='compSS', label='Compare by species among sites', value=FALSE))),
             
             
             br(),
             
             #downloadButton('downloadData', 'Download Data'),
            
             br(),
             img(src = "seastars.jpg", height = 280, width = 360),
             br(),
             p("For further information about the objectives and methods of this sampling protocol, visit the ", 
               a("NETN Rocky Intertidal Community protocol page.", href= "https://science.nature.nps.gov/im/units/netn/monitor/programs/rockyIntertidal/rockyIntertidal.cfm")),
             br()
           ),
          
           
           mainPanel(h3(textOutput("captionSS",  container = span)),
                     plotOutput("plot2",  width = "100%")
                     
                     
           )
  
           
         )
)#end navbarPage

)
)