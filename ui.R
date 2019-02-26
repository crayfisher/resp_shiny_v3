

ui <- fluidPage( #column for inputs
  titlePanel("Stream depletion calculator"),
  fluidRow(
    column(2,
           downloadButton('downloadReport',"Download pdf report"),
           checkboxInput("include_wells",
                         "Include wells in the report?",
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
                                         label = "select averaging period",
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
                        label = "select stream",
                        choiceNames = rivers$descr,
                        choiceValues = rivers$river,
                        selected = "ALLZN"
           ),
           radioButtons(inputId = "time",
                        label = "select time (days)",
                        choices = c("7","30","60","90","150"),
                        selected = "150"),
           radioButtons(inputId = "Layer",
                        label = "select Layer",
                        choices = c("1","2"),
                        selected = "1")
    ),
    column(10, #column for results
           
           fluidRow( #row for text results
             fixedRow(
               h3("RESULTS"),
               h4("for selected stream:",strong(textOutput("zone2"))),
               h4("at selected time inteval",strong(textOutput("time2"))),
               h4("for selected pumping location, rate and duration"),
               h4(strong(textOutput("total_Q"))),
               dataTableOutput("results_table")
               
               
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
                                  p("this application was developed by Pawel Rakowski"),
                                  p("click below to reveal email"),
                                  recaptchaUI("test", sitekey = "6LeIxAcTAAAAAJcZVRqyHh71UMIEGNQ_MXjiZKhI"),
                                  uiOutput("humansOnly")
                                  
                         ),
                         tabPanel("Map", 
                                  leafletOutput("map"),
                                  downloadButton("downloadData_bore", "Download Data"),
                                  dataTableOutput("wells_tab")),
                         tabPanel("bar chart", 
                                  plotlyOutput("bar_chart"),
                                  downloadButton("downloadData_river", "Download Data"),
                                  dataTableOutput("zones_tab")),
                         tabPanel("line chart", 
                                  plotlyOutput("line_chart"),
                                  downloadButton("downloadData_time", "Download Data"),
                                  dataTableOutput("times_tab")
                         )
                         
                         
             )
           )
    )
  )
)
