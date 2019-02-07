library(shiny)
library(tidyverse)
library(raster)
library(leaflet)
library(RColorBrewer)
library(sf)
library(rgdal)
library(plotly)
library(DT)

source("proc.R")
folder <- "data/raster/"
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
nztm <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"
load(file = "data/params/params1.rdata") # this contains definition of zones and times available
pal <- rev(rainbow(5,start = 0,end = .5))
at <-seq(0,100,10)
cb <- colorBin(pal,bins=at,domain = at)
perc_s <- c("Q50","Q10","Q90")
wells_dummy <-data.frame(x = c(0,0),
                         y = c(0,0),
                         Q = c(0,0),
                         L = c(1,2))  

#####
ui <- fluidPage( #column for inputs
  titlePanel("Stream depletion calculator"),
  fluidRow(
    column(2,
           downloadButton('downloadReport'),
           radioButtons(inputId = "pump_in_type",
                        label = "pumping input type",
                        choices = c("single point","upload csv"),
                        selected = "single point"),
           radioButtons(inputId = "what_display",
                        label = "what to display",
                        choices = c("map","bar chart","line chart"),
                        selected = "map"),
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
               
               
               column(7,
                      h3(textOutput("tot_effect_out_label"))
               ),
               column(3,
                      strong(h1(textOutput("tot_effect_out")))
               )
               
               
             ),
             fixedRow(conditionalPanel("input.pump_in_type == 'single point'",
                                       column(7,
                                              h3(textOutput("SDZ_out_label"))
                                       ),
                                       column(3,
                                              strong(h1(textOutput("SDZ_out")))#,
                                              #strong(h1(textOutput("test1")))
                                       )
             ))
             
           ),
           fluidRow( #row for map
             leafletOutput("map")
           ),
           fluidRow( # row for 2 charts at the bottom
             column(5,
                    plotlyOutput("bar_chart"),
                    verbatimTextOutput("event")
             ),
             column(5,
                    plotlyOutput("line_chart")
             )
           ),
           fluidRow( dataTableOutput("wells_tab"))
    )
  )
)
#######
server <- function(input, output,session) {
  
  
  lf <- reactive({
    leaflet() %>% 
    addTiles((urlTemplate = "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png") ) %>%
    fitBounds(176.4991,-39.7270,177.0382,-39.4487)
  })
  
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    lf()
  })
  #observe map click and update input vith coordinates
  observe({
    if(input$pump_in_type == "single point"){
      #input$map_clic
      click <- input$map_shape_click
      if(is.null(click))
        return()
      click_xy <- data.frame (lat = as.numeric(click$lat),
                              lon = as.numeric(click$lng)) 
      #transfrm coordinates to nztm
      click_xy <- st_as_sf(click_xy,coords = c("lon", "lat"),crs = 4326)
      click_xy <- st_transform(click_xy,2193)
      click_xy$E <- st_coordinates(click_xy)[,1]
      click_xy$N <- st_coordinates(click_xy)[,2]
      click_xy <- st_set_geometry(click_xy, NULL)
      click_xy_txt <- paste(click_xy[1,1],click_xy[1,2])
      E <- click_xy[1,1]
      N <- click_xy[1,2]
      updateNumericInput(session,
                         inputId = "E",
                         label = "Easting:",
                         value = E
      )
      updateNumericInput(session,
                         inputId = "N",
                         label = "Northing:",
                         value = N
      )  
    }
    
  })
  
  wells_csv <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read_csv(inFile$datapath)
    
    
  })
  
  well_csv_row <- reactive({
    NROW(wells_csv())
  })
  
  
  
  well_csv_ready <- reactive({
    #if(exists ("wells_csv()" )==T){
    if( well_csv_row()>0){
      T
      # }
    }else{F}
  })
  
  #generate data frame from input
  wells <- reactive({
    #input
    if (well_csv_ready() == T &
        input$pump_in_type == 'upload csv') {
      #impot csv
      #wells <- read_csv("data/wells.csv")
      wells <-  wells_csv()
      colnames(wells) <- c("x","y","Q","L")
      wells <- wells[,1:4] 
    }else{
      #generate well from input
      wells <- data.frame(x = input$E,
                          y= input$N,
                          Q= input$Q,
                          L=1) 
    }
    
    wells
  })
  
  
  
  #convert wells to spatial and transform (for ploting locations)
  wells1 <- reactive({
    #####
    #create a point for the selected location
    #convert wells to lat long as set as spatial object for mapping in leaflet
    wells1 <- st_as_sf(wells(),coords = c("x", "y"),crs = 2193)
    wells1 <- st_transform(wells1,4326)
    wells1 <- as(wells1, 'Spatial')
    wells1
  })
  
  #get wells for sp (exptracting rasters etc)
  wells_sp <- reactive({
    #this is to add dummy row to avoid emty df
    
    wells_b <- bind_rows(wells(),wells_dummy)
    
    wells1_1 <- wells_b %>% 
      filter(L==1) %>% 
      st_as_sf(coords = c("x", "y"),crs = 2193,remove = F)
    wells_sp1 <- as(wells1_1, 'Spatial')
    
    wells1_2 <- wells_b %>% 
      filter(L==2) %>% 
      st_as_sf(coords = c("x", "y"),crs = 2193,remove = F)
    wells_sp2 <- as(wells1_2, 'Spatial')
    
    wells_sp <-list(wells_sp1,wells_sp2)
    wells_sp
  })
  
  #this is convenience fucntion to gather sp and data frmes into a list as argument tot_eff2 functon
  wells_list <- reactive( {
    wells1_1 <- wells() %>% 
      filter(L==1)
    wells1_2 <- wells() %>% 
      filter(L==2)
    wells_sp1 <- wells_sp()[[1]]
    wells_sp2 <- wells_sp()[[2]]
    wells_list <- list(wells_sp1,wells_sp2,wells1_1,wells1_2)
    wells_list
  })
  
  
  # #generate raster from pumping
  # commented out as not needed anymore
  #  raster_pumping_comb <- reactive({
  #    
  #    #####
  #    #generate pumping rater
  #    
  #    #function to process wells
  #    raster_pumping_comb <- pump2rast(wells())
  #  })
  
  
  #generate a map with selected wells
  map_point <- observe({
    #add a marker to the map
    proxy <- leafletProxy("map")
    proxy %>% 
      clearGroup(group = "markers") %>% 
      #removeMarker() %>% #clear old marker
      addCircleMarkers(data = wells1(),  #add marker
                       radius = 4,
                       fillColor = "red",
                       stroke = F,
                       fillOpacity = 1,
                       group = "markers"
      )
  })
  
  time <- reactive({
    time <- input$time
    time <- as.integer(time)
    df <- data.frame(comm_time=c(time)) %>% 
      left_join(times) %>% 
      select(id)
    time <- unlist(df)
    
  })
  zone <- reactive({
    input$zone
    # zone <- input$zone
    # zone = "ALLZN"
    # df<-data.frame(id = c(zone)) %>% 
    #   left_join(rivers,by = c("id"="river")) %>% 
    #   select(descr)
    # zone <- unlist(df)
  })
  #layer <- input$Layer
  
  #generate 
  
  
  #generate a map with selected raster (time, layer, zone)
  map_raster <- observe({
    
    perc <- "Q50"
    #function to load raster
    RF_comb <- SDZ_imp(zone(),time(),perc)
    #output
    RF_L1 <- RF_comb[[1]]
    RF_L2 <- RF_comb[[2]]
    
    #projectRaster
    # 
    # crs(RF_L1) <- nztm
    # crs(RF_L2) <- nztm
    
    #add raster to the map
    
    if(as.numeric(input$Layer) == 1){
      RF <- RF_L1
    }  
    else{
      RF <- RF_L2
    }
    RF_poly <- rasterToPolygons(RF,na.rm=T) %>% 
      st_as_sf() %>% 
      st_set_crs(value= 2193) %>% 
      st_transform(crs = 4326)
    names(RF_poly) <- c("SD","geometry")   
    labels <- as.character(paste("stream depletion",round(RF_poly$SD*100,2),"%",sep=" "))
    
    
    proxy <- leafletProxy("map")
    proxy %>% 
      clearGroup(group = "poly") %>% 
      addPolygons(group = "poly",
                  data=RF_poly,
                  opacity = 1,
                  fillOpacity = 0.7,
                  stroke = F,
                  fillColor = ~cb(SD*100),
                  label= labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto",
                    clickable = F)
      ) %>% 
      addLegend(layerId = "legend",
                title = "Stream Depletion %",
                pal = cb,
                values= at ) 
    
    
    # test <- as.character(crs(RF))
    #  test
  })
  
  
  
  #generate dat frame for all zones
  tot_effect_zones_df <- reactive({
    
    #####
    #input
    #time <- as.character(input$time)
    
    #loop through the zones to create a dataframe for plotting
    for (z in 1: NROW(rivers)){
      
      zone_z <- unlist(rivers$river[z])
      
      for (p in 1:3){
        perc_p <- perc_s[p] 
        res <- tot_eff2(wells_list(),zone_z,time(),perc_p)%>% 
          mutate(zone = zone_z,
                 perc = perc_p )
        if(z==1 & p==1) {
          res1 <- res
        } else {
          res1 <- bind_rows(res1,res)
        }
      }
    }
    
    res1 <- res1 %>% 
      select(-SD) %>% 
      spread(perc,SD_tot)
    res1
  })
  
  
  #generate dat frame for all times
  tot_effect_times_df <- reactive({
    
    #####
    #input
    zone <- zone()
    
    #loop through the zones to create a dataframe for plotting
    for (t in 1: NROW(times)){
      
      time_t <- unlist(times[t,1])
      for (p in 1:3){
        perc_p <- perc_s[p] 
        res <- data.frame(SD_tot =tot_eff2_sum(tot_eff2(wells_list(),zone,time_t,perc_p))) %>% 
          mutate(time = time_t,
                 perc = perc_p )
        if(t==1 & p==1) {
          res1 <- res
        } else {
          res1 <- bind_rows(res1,res) 
        }
      }
    }
    
    res2 <- res1 %>% 
      spread(perc,SD_tot)
    res2
  })
 
  
  
  SDZ_perc <- reactive({
    SDZ_perc <- round(sd_extract(wells1(),zone(),time(),as.numeric(input$Layer))*100,2)
  })
  
  tot_effect_df <- reactive({
    perc = "Q50"
    tot_effect_df <- tot_eff2(wells_list(),zone(),time(),perc)
    tot_effect_df
  })
  
  
  tot_effect <- reactive({
    # df <- tot_eff_calc(input$zone,input$time,raster_pumping_comb()) %>% 
    #   select(tot_effect)
    # df <- round(unlist(df),2)
    perc = "Q50"
    df <- tot_eff2_sum(tot_effect_df())
    df <- round(df,1)
    df
  })
  
  tot_effect_10 <- reactive({
    # df <- tot_eff_calc(input$zone,input$time,raster_pumping_comb()) %>% 
    #   select(tot_effect)
    # df <- round(unlist(df),2)
    perc = "Q10"
    df <- tot_eff2_sum(tot_eff2(wells_list(),zone(),time(),perc))
    df <- round(df,1)
    df
  })
  
  tot_effect_90 <- reactive({
    # df <- tot_eff_calc(input$zone,input$time,raster_pumping_comb()) %>% 
    #   select(tot_effect)
    # df <- round(unlist(df),2)
    perc = "Q90"
    df <- tot_eff2_sum(tot_eff2(wells_list(),zone(),time(),perc))
    df <- round(df,1)
    df
  })
  
  
  
  
  
  output$tot_effect_out <- renderText({
    paste( tot_effect(),"(",tot_effect_10()," - ",tot_effect_90(),")","L/s")
  })
  
  
  output$SDZ_out <- renderText({
    paste( SDZ_perc(),"%")
  }) 
  
  output$tot_effect_out_label <- renderText({
    "Total effect on selected stream from pumping for specified location, rate and duration:"
  })
  output$SDZ_out_label <- renderText({
    "Stream depletion as % of pumping:"
  }) 
  chart_bar1 <- reactive({
    chart <- tot_effect_zones_df() %>% 
      ggplot(aes(zone,Q50,fill = zone))+
      geom_bar(stat = "identity")+
      geom_errorbar(aes(ymin = Q10, ymax = Q90),
                    width = .2)+
      theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1))+
      ggtitle("Effect vs stream (for selected time) ")+
      ylab("Effect L/s")+
      xlab("Streams")
    chart
    #+ theme(legend.position = "none"))
  })
  
  output$bar_chart <- renderPlotly({
    
    ggplotly(chart_bar1())
    #ggplot(tot_effect_df(),aes(zone_z,tot_effect))+geom_bar(stat = "identity")
  })
  
  
  chart_line1 <- reactive({
    chart <- ggplot(data =tot_effect_times_df(),aes(x=time,y=Q50))+
      geom_ribbon(aes(ymin=Q10, ymax=Q90),fill = "gray70")+
      geom_line(col = "red")+
      theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1))+
      ggtitle("Effect vs time (for selected stream) ")+
      ylab("Effect L/s")+
      xlab("Time days")
    chart
  })
  
  output$line_chart <- renderPlotly({
    
    #chart
    #+ theme(legend.position = "none"))
    ggplotly(chart_line1())
    #ggplot(tot_effect_df(),aes(zone_z,tot_effect))+geom_bar(stat = "identity")
  })
  output$test1 <- renderText(map_raster())
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else round(d$y,1)
  })
  
  output$wells_tab <- renderDataTable(tot_effect_zones_df())
  
  output$downloadReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(chart_line = chart_line1(),
                     chart_bar = chart_bar1(),
                     map = lf())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())#,
                        #output_format = "pdf_document",
                        #output_file = "report.pdf"
      )
    }
  )
  
}

shinyApp(ui = ui, server = server)

