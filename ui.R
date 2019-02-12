

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
               h4("for selected stream"),
               h4("at selected time inteval" ),
               h4("for selected pumping location, rate and duration"),
               dataTableOutput("results_table")
               
               
             )
             
           ),
           fluidRow( #row for map
             tabsetPanel(type = "tabs",
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