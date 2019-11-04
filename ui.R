dput(df)

ui <- fluidPage( #column for inputs
  fluidRow(
    tags$img(style = "display: block; margin-left: auto; margin-right: auto;
             ", #; float: right
             height = 250,
             width  = 350, 
             src    = "HBRC_RGB_screen.png"#, 
             #align = "right"
             ),
    tags$h1(
      tags$strong("Stream Depletion Calculator"), 
            style = "color:DarkBlue;font-family:Tahoma;font-size:300%;text-align:center;")
    #titlePanel(
     ),
  tags$hr(),
  fluidRow(
    column(2,
           downloadButton("downloadpdfreport_ae", 
                          "Download pdf report"),
           checkboxInput("include_wells",
                         "Include wells in the report?",
                         value = T),
           #added 
           checkboxInput("include_effect_streams",
                         "Include effects for different streams?",
                         value = T),
           radioButtons(inputId = "pump_in_type",
                        label = "pumping input type",
                        choices = c("single point","upload csv","historical data"),
                        selected = "single point"),
           conditionalPanel("input.pump_in_type == 'historical data'",
                            checkboxGroupInput("abstr_type",
                                               "abstraction type",
                                               choices = list("Irrigation" = "irr",
                                                              "Public Water Supply" = "PWS",
                                                              "Industrial"= "IND"),
                                               selected = "PWS"),
                            radioButtons(inputId = "period_type",
                                         label = "Select averaging period",
                                         choices = c("dry summer 2012-2013","date range"),
                                         selected = "dry summer 2012-2013"),
                            conditionalPanel("input.period_type == 'date range'",
                                             dateRangeInput(inputId = "date_range_hist",
                                                            label = "enter date range between 1/7/1980 and 1/6/2015",
                                                            start = "2012-11-01",
                                                            end = "2013-03-01",
                                                            min = "1980-07-01",
                                                            max = "2015-06-01",
                                                            startview = "decade"))),
           conditionalPanel("input.pump_in_type == 'single point'",
                            numericInput(inputId = "E",
                                         label = "Easting:",
                                         value = 1930000,
                                         step = 500),
                            
                            numericInput(inputId = "N",
                                         label = "Northing:",
                                         value = 5605000,
                                         step = 500),
                            
                            numericInput(inputId = "Q",
                                         label = "Pumping rate L/s:",
                                         value = 50)
           ),
           
           conditionalPanel("input.pump_in_type == 'upload csv'",
                            downloadButton("downloadData_bore_template", "Download csv template"),
                            fileInput("file1", "Choose CSV File",
                                      accept = c(
                                        "text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                            )
                            
           ),
           radioButtons(inputId = "zone",
                        label = "Select stream",
                        choiceNames = rivers$descr,
                        choiceValues = rivers$river,
                        selected = "ALLZN"
           ),
           radioButtons(inputId = "time",
                        label = "Select time (days)",
                        choices = c("7","30","60","90","150"),
                        selected = "150"),
           radioButtons(inputId = "Layer",
                        label = "Select Layer",
                        choices = c("1","2"),
                        selected = "1")
    ),
    #column(1),
    column(10, #column for results
           
           fluidRow( #row for text results
             fixedRow(
               h3("Results"),
               h4("for selected stream:",strong(textOutput("zone2"))),
               h4("at selected time inteval",strong(textOutput("time2"))),
               h4("for selected pumping location, rate and duration"),
               h4(strong(textOutput("total_Q"))),
               tags$br(),
               tags$br(),
               tags$br(),
               withSpinner(dataTableOutput("results_table")),
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br()
             )
             
           ),
           fluidRow( #row for map
             tabsetPanel(type = "tabs",
                         selected = "Map",
                         tabPanel("Info",
                                  h1("Stream Depletion Calculator"),
                                  h2("for Heretaunga Aquifer"),
                                  h3("Description"),
                                  p("This calculator allows for stochastic estimation of impact of groundwater pumping on surface water flows (stream depletion)"),
                                  h3("User input"),
                                  p("User can provide pumping location by:"),
                                  p("- providing coordinates in NZTM or by clicking on a map"),
                                  p("- uploading a file with coordinates for multiple points"),
                                  p("- using historical data for selected abstraction type and pumping interval"),
                                  h3("Outputs"),
                                  p("outputs include:"),
                                  p("- a table with results, including total stream depletion for a selected pumping location, zone (river), and time interval "),
                                  p("--- if single pumping location is selected, output includes stream depletion ratio (understood as sensitivity to pumping at a selected location) "),
                                  p("--- the results are stochastic and are reported as most likely value (Q50) and minimum (Q10) and maximum (Q90) range"),
                                  p("- a map showing location of pumping"),
                                  p("- a downloadable table showing breakdown of stream depletion (SD) and stream depletion ration (SRD) per each bore"),
                                  p("- a bar chart and a downloadable table with stream depletion breakdown per stream, for selected pumping and time interval"),
                                  p("- a line chart and a downloadable table with stream depletion vs time for selected pumping and stream "),
                                  p("- full results can be downloaded as a pdf report"),
                                  h3("Methodology"),
                                  p("The calculator is based on outputs of groundwater model developed for Heretaunga Aquifer in 2018 by HBRC."),
                                  p(" Model development is documented in Heretaunga Aquifer Groundwater Model Development Report"),
                                  p(em("Rakowski P., Knowling M. (2018) 4997 Heretaunga Aquifer Groundwater Model Development Report. HBRC")),
                                  a(href = "https://www.hbrc.govt.nz/assets/Document-Library/Publications-Database/4999-Executive-summary-of-Heretaunga-Aquifer-Groundwater-Model-Development-Report1.pdf","executive summary"),
                                  a(href ="https://www.hbrc.govt.nz/assets/Document-Library/Publications-Database/4997-Heretaunga-Model-Groundwater-Development-Report.pdf","full report"),
                                  p("The methodology behind the calculator is based on",em("response function method")),
                                  p("Full documentation is provided in a report:", em("Rakowski P. (2019) Heretaunga Aquifer Stream Depletion Assessment Stochastic Stream Depletion Distribution, Zone Delineation and Response Function Methodology. HBRC."),"(in prep)"),
                                  p("this application was developed by Pawel Rakowski")#,
                                  #p("click below to reveal email"),
                                  #recaptchaUI("test", sitekey = "6LeIxAcTAAAAAJcZVRqyHh71UMIEGNQ_MXjiZKhI"),
                                  #uiOutput("humansOnly")
                                   ),
                         tabPanel("Map", 
                                  tags$br(),
                                  tags$br(),
                                  withSpinner(leafletOutput("map")),
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  withSpinner(dataTableOutput("wells_tab")),
                                  tags$br(),
                                  tags$br(),
                                  #withSpinner(dataTableOutput("selected_zone_tab")),
                                  tags$br(),
                                  tags$br(),
                                  downloadButton("downloadData_bore", "Download Data"),
                                  tags$br(),
                                  tags$br(),
                                  p("Download effect for different streams"),
                                  downloadButton("downloadData_effectzone", "Download Data")),
                         
                         tabPanel("Bar chart", 
                                  tags$br(),
                                  tags$br(),
                                  fluidRow(
                                    column(width =3),
                                    column(width = 6,
                                                  withSpinner(plotOutput("bar_chart", width = "75%",  height = 550))
                                    ), 
                                    column(width = 2)
                                  ), 
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  withSpinner(dataTableOutput("zones_tab")),
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  downloadButton("downloadData_river", "Download Data")),
                         tabPanel("Line chart", 
                                  tags$br(),
                                  tags$br(),
                                  withSpinner(plotOutput("line_chart", height = 450)),
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  tags$br(),
                                  withSpinner(dataTableOutput("times_tab")),
                                  tags$br(),
                                  downloadButton("downloadData_time", "Download Data")
                         )
                         
                         
             )
           )
    )
  )
)
