
# The user-interface (ui) script controls the layout and appearance of your app. 

library(shiny)
library(leaflet)
library(shinyjs)
library(DT)


shinyUI(navbarPage(title=HTML("<div> <a href='https://science.nature.nps.gov/im/units/netn/'> <img src='ah_small_black.gif',
          alt='WQ Visualizer'> </a> NETN Rocky Intertidal Community Visualizer</div>"),position = "static-top", inverse=TRUE, collapsible = FALSE, fluid=TRUE, 
           windowTitle = "NETN Rocky Intertidal Community Visualizer", id="MainNavBar",
     
######################################### Plot per site Panel ####################################################################
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
             h3("Plot mean annual cover of species or substrate type over time at rocky intertidal sites."),
             br(),
             #Park selection
             tags$div(title="Choose a park",selectInput(inputId='park', label='Select Park', choices= ParkList_trans, selectize = TRUE,selected ="Acadia NP")),
             br(),
             
             
             # Selection to plot single or multple sites
             tags$div(title="Choose between plotting one or multiple sites",radioButtons(inputId='many', label='Do you want to plot data from one or multiple sites?', choices= c("One site","All sites"), selected = "All sites")),
             
             
             # Site selection
             conditionalPanel(condition = "input.many == 'One site'", uiOutput("SiteResultsA")),
             br(),
             h4("Plot diplays the mean annual cover of the 10 most abundant species/cover types estimated from point-intercept sampling along three, parallel transects."),
             
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
           
           mainPanel(plotOutput("plot1",  width = "100%")
                     
                     
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
    h3("Plot mean abundance of motile invertebrate species within zones of the rocky intertidal."),
    br(),
    #Park selection
    tags$div(title="Choose the park you want to work with",selectInput(inputId='park', label='Select Park', choices= ParkList, selectize = TRUE)),
    
    # Selection to plot single or multple species
    tags$div(title="Choose the park you want to work with",radioButtons(inputId='SPP', label='Do you want to plot single or multiple species?', choices= c("Single","All species"), selected = "Single")),
    
    # Species selection
    tags$div(title="Choose the species abundance data you want to plot",conditionalPanel(condition = "input.SPP == 'Single'", selectInput(inputId='species', label='Select species to plot', choices=SppList)),
    
    # Variable selection
    tags$div(title="Choose the data you want to plot",selectInput(inputId='variable', label='Select type of plot', selected = "Abundance" ,choices=  c("Abundance", "Proportion.Damaged")))),
    
    
    ##Add in options for plots
    
   tags$div(title="Plot abundance on log-scale ", conditionalPanel(condition = "input.variable == 'Abundance'", checkboxInput(inputId='logscale', label='Convert abundance to log-scale', value=FALSE))),
    tags$div(title="Toggle y-scale ", conditionalPanel(condition = "input.SPP == 'Single'", checkboxInput(inputId='free_y', label='Make scale of y-axes the same', value=FALSE))),
    
    tags$div(title="Compare sites within same zone", checkboxInput(inputId='compare', label='Compare data among sites within an intertidal zone', value=FALSE)),
    
    br(),

    #downloadButton('downloadData', 'Download Data'),
    #img(src = "BMI_sampling.jpg", height = 140, width = 180),
    br(),
    br(),
    p("For further information about the objectives and methods of this sampling protocol, visit the ", 
    a("NETN Rocky Intertidal Community protocol page.", href= "https://science.nature.nps.gov/im/units/netn/monitor/programs/rockyIntertidal/rockyIntertidal.cfm")),
    br()
    ),
    
    mainPanel(plotOutput("plot",  width = "100%")
    
              
                        )
    
  )
  ), #end navbarPage

######################################### Seastars Plot Panel ####################################################################

tabPanel(title="Seastars",
         #style="padding: 0",
         useShinyjs(),
         div(class="outer",
             tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />'))
               ),
         
         fluidPage(
           sidebarPanel(
             h1(""),
             h3("Plot mean abundance of seastars counted in tidal pools within the rocky intertidal."),
             br(),
             #Park selection
             tags$div(title="Choose the park you want to work with",selectInput(inputId='park', label='Select Park', choices= ParkList, selectize = TRUE)),
             
             # Site selection
             #uiOutput("SiteResultsA"),
             
             # Species selection
            # tags$div(title="Choose the species abundance data you want to plot",selectInput(inputId='species', label='Select species to plot', choices=SeaStarList),
                      
                      # Variable selection
                  #    tags$div(title="Choose the data you want to plot",selectInput(inputId='variable', label='Select species to plot', selected = "Abundance" ,choices=  c("Abundance","Proportion.Damaged")))),
             
             
             ##Add in options
             
             # tags$div(title="Plot Type ",selectInput(inputId='plottype', label='Plot type',choices=c("Time Series", "Histogram", "Box Plot (monthly)"), selected = "Time Series")),
             # tags$div(title="Add a trend line ", conditionalPanel(condition = "input.plottype == 'Time Series'",checkboxInput(inputId='trend', label='Add Linear trend line', value=FALSE))),
             # tags$div(title="Binwidth control ", conditionalPanel(condition = "input.plottype == 'Histogram'",sliderInput(inputId='binwidth', label='Binwidth', value=.1, min= 0, max= 1, step = .10))),
             tags$div(title="Plot abundance on log-scale ", conditionalPanel(condition = "input.variable == 'Abundance'", checkboxInput(inputId='logscale', label='Convert abundance to log-scale', value=FALSE))),
             tags$div(title="Toggle y-scale ", conditionalPanel(condition = "input.variable == 'Abundance'", checkboxInput(inputId='free_y', label='Make y-axis scale the same', value=FALSE))),
             
             
             br(),
             
             #downloadButton('downloadData', 'Download Data'),
             #img(src = "BMI_sampling.jpg", height = 140, width = 180),
             br(),
             br(),
             p("For further information about the objectives and methods of this sampling protocol, visit the ", 
               a("NETN Rocky Intertidal Community protocol page.", href= "https://science.nature.nps.gov/im/units/netn/monitor/programs/rockyIntertidal/rockyIntertidal.cfm")),
             br()
           ),
           
           mainPanel(plotOutput("plot2",  width = "100%")
                     
                     
           )
  
           
         )
)#end navbarPage

)
)