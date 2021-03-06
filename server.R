#App was crashing when 60 days is selected because there was no 60 days in times dataframe. Replace all times df with times_up df to fix 60d issue (5 replacements in total) in (time/time2/tot_effect_times_df)

#instead of showing the duration as time (which ends by 147, I left joined times_up1, then used comm_time instead of duration 
#for table in tot_effect_times_df_print


server <- function(input, output,session) {
 
  #leaflet map is create here, because it is used in both the app and in the report
  lf <- reactive({
    leaflet() %>% 
      addTiles((urlTemplate = "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png") ) %>%
      fitBounds(176.4991,-39.7270,177.0382,-39.4487) %>% 
      addMapPane(name = "raster", zIndex = 410) %>% 
      addMapPane(name = "wells", zIndex = 420)
  })
  

  output$map <- renderLeaflet({

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
    if (well_csv_ready() == T & input$pump_in_type == 'upload csv') {
      #impot csv
      #wells <- read_csv("data/wells.csv")
      wells <-  wells_csv() %>% 
        mutate(type = NA) %>% 
        select(x,y,Q,L,bore,type)
      
    } else if (input$pump_in_type == "historical data" &  length(input$abstr_type >0) ){
      load("data/wells/wells_hist.rdata")
      wells <- wells_hist %>% 
        filter(type %in%  input$abstr_type) %>% 
        mutate(Q=-Q) %>% 
        select(x,y,Q,L,bore,type,date)
      
      if (input$period_type == "dry summer 2012-2013"){
        dates_range <- parse_date_time (c("1/11/2012","1/03/2013"),"dmy")
      }
      else if (input$period_type =="date range"){
        dates_range <- as_datetime(as_date(input$date_range_hist,origin = "1970-01-01"))
        
      }
      wells <- wells %>% 
        filter(between(date,dates_range[1],dates_range[2]))  %>%  
        filter(type %in%  input$abstr_type) %>%
        group_by(x,y,L,bore,type) %>%
        summarise(Q = mean(Q))
      
      
    }
    else{
      #generate well from input
      wells <- data.frame(x = input$E,
                          y= input$N,
                          Q= input$Q,
                          L=1,
                          bore = NA,
                          type= NA) 
    }
    
    wells
  })
  
  wells_total <- reactive({
    round(sum(wells()$Q),digits = 1)
  })
  
  
  
  
  #convert wells to spatial and transform (for ploting locations)
  wells1 <- reactive({
    #####
    #create a point for the selected location
    #convert wells to lat long as set as spatial object for mapping in leaflet
    wells1 <- st_as_sf(wells(),coords = c("x", "y"),crs = 2193)
    wells1 <- st_transform(wells1,4326)
    #wells1 <- as(wells1, 'Spatial')
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
  
  vals <- c("PWS","IND","irr")
  pal <- colorFactor(
    palette = 'Spectral',
    domain = vals)
  
  #generate a map with selected wells
  map_point <- observe({
    #add a marker to the map
    proxy <- leafletProxy("map")
    proxy %>% 
      clearGroup(group = "markers") %>% 
      #removeMarker() %>% #clear old marker
      addCircleMarkers(data = wells1(),  #add marker
                       radius = ~Q/5,#4
                       fillColor = ~pal(type),
                       stroke = T,
                       color = "black",
                       weight = 1,
                       fillOpacity = .7,
                       group = "markers",
                       options = leafletOptions(pane = "wells")
                       
      )%>% 
      addLegend(layerId = "legend2",
                title = "abstraction type",
                pal = pal,
                values= vals ) 
  })
  
  time <- reactive({
    time <- input$time
    time <- as.integer(time)
    df <- data.frame(comm_time=c(time)) %>% 
      left_join(times_up) %>% 
      select(id)
    time <- unlist(df)
    
  })
  
  time2 <- reactive({
    time2 <- times_up %>% 
      filter(id == time()) %>% 
      select(comm_time) 
    time2 <- unlist(time2)
    time2 <- unname(time2)
    time2
  })
  output$time2 <- renderText(time2())
  
  zone <- reactive({
    input$zone
    # zone <- input$zone
    # zone = "ALLZN"
    # df<-data.frame(id = c(zone)) %>% 
    #   left_join(rivers,by = c("id"="river")) %>% 
    #   select(descr)
    # zone <- unlist(df)
  })
  
  
  zone2 <- reactive({
    zone2 <- rivers %>% 
      filter(river == zone()) %>% 
      select(descr) 
    zone2 <- unlist(zone2)
    zone2 <- unname(zone2)
    zone2
  })
  
  output$zone2 <- renderText(zone2())
  
  
  #layer <- input$Layer
  
  #generate 
  
  perc <- "Q50"
  RF_poly0 <- reactive({
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
    crs(RF) <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"
    names(RF) <- "SDR"
    RF

  })
  
  RF_poly1 <- reactive({
    RF_poly <- rasterToPolygons(RF_poly0(),na.rm=T) %>% 
      st_as_sf() %>% 
      st_set_crs(value= 2193) %>% 
      st_transform(crs = 4326)
    names(RF_poly) <- c("SD","geometry")
    RF_poly
  })
  
  
  
  labels1 <- reactive({
    labels <- as.character(paste("stream depletion",round(RF_poly1()$SD*100,2),"%",sep=" "))
    labels
  })
  
  
  
  
  
  #generate a map with selected raster (time, layer, zone)
  map_raster <- observe({
    
    
    proxy <- leafletProxy("map")
    proxy %>% 
      clearGroup(group = "poly") %>% 
      addPolygons(group = "poly",
                  data=RF_poly1(),
                  opacity = 1,
                  fillOpacity = 0.7,
                  stroke = F,
                  fillColor = ~cb(SD*100),
                  label= labels1(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto",
                    clickable = F),
                  options = leafletOptions(pane = "raster")
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
      river_z <- unlist(rivers$descr[z])
      
      for (p in 1:3){
        perc_p <- perc_s[p] 
        res <- data.frame(SD_tot =tot_eff2_sum(tot_eff2(wells_list(),zone_z,time(),perc_p)))%>% 
          mutate(zone = zone_z,
                 river = river_z,
                 perc = perc_p )
        if(z==1 & p==1) {
          res1 <- res
        } else {
          res1 <- bind_rows(res1,res) 
        }
      }
    }
    res1 <- res1 %>% 
      #select(-SD) %>% 
      spread(perc,SD_tot) %>% 
      mutate(river1 = str_wrap(river,width = 10))
    res1
  })
  
  
  #to generate user_perc for each zone table 
  tot_effect_per_each_zone_df <- reactive({
    
    
    perc_s <- c("Q50","Q10","Q90")
    
    #loop through the zones to create a dataframe for plotting
    for (z in 1: NROW(rivers)){
      
      zone_z <- unlist(rivers$river[z]) #abbreviation
      river_z <- unlist(rivers$descr[z]) #description
      
      for (p in 1:3){
        perc_p <- perc_s[p] 
        res_zone_trial <- data.frame(tot_eff2(wells_list(),zone_z, time() ,perc_p))%>% 
          mutate(zone = zone_z,
                 river = river_z,
                 perc = perc_p )
        if(z==1 & p==1) {
          res1_zone_trial <- res_zone_trial
        } else {
          res1_zone_trial <- bind_rows(res1_zone_trial,res_zone_trial) 
        }
      }
    }
    

    tot_per_zone <-  res1_zone_trial  %>% 
      dplyr::group_by(zone, perc) %>% 
      dplyr::mutate(SD_litsec_tot = sum(SD_tot,na.rm = T)) %>% 
      dplyr::mutate(user_percent = (SD_tot/SD_litsec_tot)*100) %>% 
      dplyr::mutate(tot_perc = sum(user_percent)) %>% 
      dplyr::mutate(user_percent = round(user_percent, 3)) %>% 
      ungroup() %>% 
      dplyr::rename(quantile = perc) %>% 
      dplyr::select(x, y, L, bore, type, Q, SD, SD_tot, quantile, SD_litsec_tot, user_percent, zone) %>% 
      dplyr::rename(SD_ls = SD_tot,
                    SD_tot = SD_litsec_tot, 
                    SDR =SD, 
                    stream = zone) %>% 
      dplyr::mutate_at(vars(Q, SDR, SD_ls, SD_tot, user_percent),funs(round(.,2))) 
    
      tot_per_zone
    
  })
  

  
  tot_effect_per_selected_zone_df <-  reactive({
    
    #river <- zone()
    
    df_per_selected_zone <- tot_effect_per_each_zone_df() %>% 
      dplyr::filter(stream == zone()) %>% 
      dplyr::select(x, y, L, bore, type, Q, SDR, SD_ls, quantile, SD_tot, user_percent, stream) %>% 
      dplyr::mutate_at(vars(Q, SDR, SD_ls, SD_tot, user_percent),funs(round(.,2))) %>% 
      dplyr::mutate(L=factor(L),
                    quantile =factor(quantile))
    

  })
  
  

  
  
  tot_effect_zones_df_print <- reactive({
    df <- tot_effect_zones_df() %>% 
      select(- river1) %>% 
      mutate_at(vars(Q10,Q50,Q90),funs(round(.,2))) %>% 
      dplyr::mutate(river = factor(river))
  })
  
  
  tot_effect_zones_df_print_ae <- reactive({
    df <- tot_effect_zones_df() %>% 
      dplyr::select(- river1) %>% 
      dplyr::mutate_at(vars(Q10,Q50,Q90),funs(round(.,2)))
    
    df1 <- tableGrob(df)
  })
  
  #generate dat frame for all times
  tot_effect_times_df <- reactive({
    
    #####
    #input
    zone <- zone()
    
    #loop through the zones to create a dataframe for plotting
    for (t in 1: NROW(times_up)){
      
      time_t <- unlist(times_up[t,1])
      time_t2 <- unlist(times_up[t,3])
      for (p in 1:3){
        perc_p <- perc_s[p] 
        res <- data.frame(SD_tot =tot_eff2_sum(tot_eff2(wells_list(),zone,time_t,perc_p))) %>% 
          #ae rename time to be time_id to show it has the id info
          mutate(time_id = time_t,
                 perc = perc_p,
                 #ae rename time2 as time_duration
                 time_duration = time_t2)
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
  
  tot_effect_times_df_print <-  reactive({
    df <- tot_effect_times_df() %>% 
      dplyr::select(-time_id) %>% 
      #ae left join so it will show comm_time instead of duration
      dplyr::left_join(., times_up1) %>% 
      dplyr::select(-time_duration) %>% 
      rename(time = comm_time) %>% 
      dplyr::select(time, everything()) %>% 
      mutate_all(funs(round(.,2)))
    
    df
  })
  
 
  tot_effect_times_df_print_ae <-  reactive({
    df <- tot_effect_times_df() %>% 
      dplyr::select(-time_id) %>% 
      #ae left join so it will show comm_time instead of duration
      dplyr::left_join(., times_up1) %>% 
      dplyr::select(-time_duration) %>% 
      rename(time = comm_time) %>% 
      dplyr::select(time, everything()) %>% 
      mutate_all(funs(round(.,2)))
    
    df1 <- tableGrob(df)
  }) 
  
  
  SDZ_perc <- reactive({
    SDZ_perc <- round(sd_extract(wells1(),zone(),time(),as.numeric(input$Layer))*100,2)
  })
  
  tot_effect_df <- reactive({
    perc = "Q50"
    tot_effect_df <- tot_eff2(wells_list(),zone(),time(),perc) %>% 
      mutate(perc = "Q50")
    tot_effect_df
  })
  tot_effect10_df <- reactive({
    perc = "Q10"
    tot_effect_df <- tot_eff2(wells_list(),zone(),time(),perc) %>% 
      mutate(perc = "Q10")
    tot_effect_df
  })
  tot_effect90_df <- reactive({
    perc = "Q90"
    tot_effect_df <- tot_eff2(wells_list(),zone(),time(),perc)%>% 
      mutate(perc = "Q90")
    tot_effect_df
  })
  
  
  tot_effect_df_print <- reactive({
    df <- bind_rows(tot_effect_df(),tot_effect10_df(),tot_effect90_df()) %>% 
      dplyr::rename(SDR = SD,
                    SD_litsec_user = SD_tot,
                    quantile = perc) %>% 
      dplyr::group_by(#river, 
                      quantile) %>% 
      dplyr::mutate(SD_litsec_total = sum(SD_litsec_user,na.rm = T)) %>% 
      dplyr::mutate(user_percent = (SD_litsec_user/SD_litsec_total)*100) %>% 
      dplyr::mutate(tot_perc = sum(user_percent)) %>% 
      dplyr::mutate(user_percent = round(user_percent, 3)) %>% 
      ungroup() %>% 
      #dplyr::rename(quantile = perc) %>% 
      dplyr::select(x, y, L, bore, type, Q, SDR, SD_litsec_user, #river, 
                    quantile, SD_litsec_total, user_percent) %>% 
      dplyr::mutate_at(vars(Q, SDR, SD_litsec_user,SD_litsec_total, user_percent),funs(round(.,2))) %>% 
      #dplyr::select(x,y,L,bore,type,Q,SDR,SD,quantile) %>% 
      dplyr::mutate(L=factor(L),
                    quantile =factor(quantile)) %>% 
      dplyr::mutate(stream = zone()) %>% 
      dplyr::rename(SD_tot = SD_litsec_total, 
                    user_perc = user_percent,
                    SD_ls = SD_litsec_user)
    
    
  })
  
  

  tot_effect_df_print_ae <- reactive({
    df <- bind_rows(tot_effect_df(),tot_effect10_df(),tot_effect90_df()) %>% 
      rename(SDR = SD,
             SD = SD_tot,
             quantile = perc) %>% 
      mutate_at(vars(Q, SD,SDR),funs(round(.,2))) %>% 
      select(x,y,L,bore,type,Q,SDR,SD,quantile)
    
    df1 <-tableGrob(df)
    
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
  
  tot_effect_all <- reactive({data.frame(`Stream Depletion` = c(tot_effect(),tot_effect_10(),tot_effect_90()),
                                         row.names = c("mean Q50","minimum Q10","maximum Q90"))})
  SDR_all <- reactive({data.frame(`Stream Depletion Ratio` = SDZ_perc(),
                        row.names = c("mean Q50","minimum Q10","maximum Q90"))})
  
  results_df <- reactive({if(input$pump_in_type == "single point"){ 
    results_df <- bind_cols(tot_effect_all(),SDR_all())
    results_df
  }else{
    results_df <-  tot_effect_all()
    results_df
  }
    #changed it from Mean to Median 
    row.names(results_df) <- c("Most Likely Median: (Q50)","Minimum (Q10)","Maximum (Q90)")
    results_df
    })
  
  dt_col_names <- reactive({
    if(input$pump_in_type == "single point"){ 
      c("Stream Depletion (L/s)","Stream Depletion Ratio (%)")
      }else{
      c("Stream Depletion (L/s)")
      }  
    }) 
  
  output$results_table <- DT::renderDataTable(datatable(results_df(),
                                                        #ae changed it from Mean to Median 
                                                        rownames = c("Most Likely Median: (Q50)","Minimum (Q10)","Maximum (Q90)"),
                                                        colnames = dt_col_names()))
  

  chart_bar1 <- reactive({
    chart <- tot_effect_zones_df() %>% 
      ggplot(aes(zone,Q50,fill = river))+
      geom_bar(stat = "identity")+
      geom_errorbar(aes(ymin = Q10, ymax = Q90),
                    width = .2)+
      theme_bw()+
      theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1))+
      ggtitle("Effect vs stream (for selected time) ")+
      ylab("Effect L/s")+
      xlab("Streams")
    
    chart

  })
  
  
  
  output$total_Q <- (renderText(paste("Total pumping rate:",wells_total(),"L/s")))
  
  
  

  
  output$bar_chart <- renderPlot({

    chart_allzones <- tot_effect_zones_df() %>% 
      dplyr::mutate(selected_zone = ifelse(zone == "ALLZN", "all zones", zone)) %>% 
      dplyr::filter(selected_zone %in% c("all zones", zone()))

    
    ggplot(chart_allzones, aes(selected_zone, Q50,fill = selected_zone))+
      geom_bar(stat = "identity", width = 0.25)+
      geom_errorbar(aes(ymin = Q10, ymax = Q90),
                    width = .05, size=0.15)+
      theme_bw()+
       facet_wrap(~selected_zone, ncol = 2, scales ="free") +
      ggtitle("Effect vs stream (for selected time) ")+
      labs(x=expression(Streams),
           y=expression(Effect~~(L~s^{-1})))+ 
      theme(legend.position = "none",
            panel.spacing = unit(2, "lines"),
            plot.title = element_text(color="black", face = "bold", size = 20), 
            axis.title.x = element_blank(), 
            axis.title.y= element_text(color="black", face="bold", size = 18, margin=margin(0,10,0,0)),
            strip.text = element_text(color="black", size= 14, face="bold"),
            axis.text.x=element_blank(), 
            axis.text.y=element_text(size = 14),
            panel.grid.major.y = element_line(size=1),
            panel.grid.minor.y = element_line(size=0.5))

  })
  # 
  # 
  # output$barcharttrial <- renderChart2({
  #   df <- tot_effect_zones_df() %>% 
  #     tidyr::gather("quantile", "value", 3:5)
  # 
  #   
  #   n2 <- nPlot(value ~ zone, 
  #               group = "quantile", 
  #               data = df, 
  #               type = "multiBarChart") #multiBarChart
  #   #multiBarHorizontalChart
  #   #n2$params$width <- 700
  #   #n2$params$height <- 400
  #   #n2$yAxis( axisLabel = "Load (ton/yr)")
  #   #n2$chart(margin = list(left = 140))
  #   #n2$xAxis( axisLabel = "Site", width = 40 )
  #   n2$xAxis(axisLabel = 'Streams')
  #   n2$yAxis(axisLabel = 'Effect L/s')
  #   
  #   #n2$xAxis(
  #   #  axisLabel = 'Site'
  #   #  ,tickFormat = "#!function(d){return d + "      " }!#"   
  #   #  ,rotateLabels= 90                                         
  #   #)
  #   
  #   n2$setTemplate(afterScript = '<script>
  #                  var css = document.createElement("style");
  #                  css.type = "text/css";
  #                  css.innerHTML = ".nv-x .nv-axislabel { font-size: 40px; }";
  #                  document.body.appendChild(css);
  #                  css = document.createElement("style");
  #                  css.type = "text/css";
  #                  css.innerHTML = ".nv-y .nv-axislabel { font-size: 40px; }";
  #                  document.body.appendChild(css);
  #                  </script>'
  #   )
  #   n2$chart(margin=list(left=100,bottom=100))
  #   n2$addParams(dom="barcharttrial")
  #   n2$params$width <- 2700
  #   n2$params$height <- 1000
  #   
  #   n2
  #   
  #   
  # })
  
  
  chart_line1 <- reactive({
    #ae I changed x=time2 to x = time_duration 
    plot_tot_effect_times_df <- tot_effect_times_df() %>% 
      dplyr::left_join(., times_up1) %>% 
      dplyr::select(-time_duration)
      
    chart <- ggplot(data =plot_tot_effect_times_df,aes(x=comm_time,y=Q50))+
      geom_ribbon(aes(ymin=Q10, ymax=Q90),fill = "gray70")+
      geom_line(col = "red")+
      #ae I added it 
      theme_bw()+
      theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1))+
      ggtitle("Effect vs time (for selected stream) ")+
      ylab("Effect L/s")+
      xlab("Time days")
    chart
  })
  

  output$line_chart <- renderPlot({
    
    plot_tot_effect_times_df <- tot_effect_times_df() %>% 
      dplyr::left_join(., times_up1) %>% 
      dplyr::select(-time_duration)
    
    ggplot(data =plot_tot_effect_times_df,aes(x=comm_time,y=Q50))+
      geom_ribbon(aes(ymin=Q10, ymax=Q90),fill = "gray70")+
      geom_line(col = "red")+
      scale_x_continuous(breaks=seq(0, 150, 30))+
      ggtitle("Effect vs time (for selected stream) ")+
      labs(x=expression(Time~(days)),
           y=expression(Effect~~(L~s^{-1})))+ 
      theme_minimal()+
      theme(legend.position = "none",
            plot.title = element_text(color="black", face = "bold", size = 23), 
            axis.title.x = element_text(color="black", face="bold", size = 20, margin=margin(10,0,0,0)),
            axis.title.y= element_text(color="black", face="bold", size = 20, margin=margin(0,10,0,0)),
            axis.text.x=element_text(size = 14 #, angle=65#, vjust=1, hjust=1
                                     ),
            axis.text.y=element_text(size = 14),
            panel.grid.major.y = element_line(size=1),
            panel.grid.minor.y = element_line(size=0.5))
    

  })
  
  output$test1 <- renderText(map_raster())
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else round(d$y,1)
  })
  
  
  output$selected_zone_tab <- DT::renderDataTable(tot_effect_per_selected_zone_df(),
                                          extensions = c('Buttons', 
                                                         'ColReorder'
                                          ), options = list(
                                            colReorder = TRUE,
                                            dom = 'Bfrtip',
                                            buttons = c(#'copy', 
                                              #'csv', 
                                              #'excel', 
                                              #'pdf', 
                                              #'print', 
                                              I('colvis')
                                            )
                                          ), 
                                          
                                          filter = "top")
  
  
  
  output$wells_tab <- DT::renderDataTable(tot_effect_df_print(),
                                          extensions = c('Buttons', 
                                                         'ColReorder'
                                                         ), options = list(
                                            colReorder = TRUE,
                                            dom = 'Bfrtip',
                                            buttons = c(#'copy', 
                                              #'csv', 
                                              #'excel', 
                                              #'pdf', 
                                              #'print', 
                                              I('colvis')
                                            )
                                          ), 
                                          
                                          filter = "top")
  
  output$zones_tab <- renderDataTable(tot_effect_zones_df_print(),
                                      extensions = c('Buttons', 
                                                     'ColReorder'
                                      ), options = list(
                                        colReorder = TRUE,
                                        dom = 'Bfrtip',
                                        buttons = c(#'copy', 
                                          #'csv', 
                                          #'excel', 
                                          #'pdf', 
                                          #'print', 
                                          I('colvis')
                                        )
                                      ), 
                                      
                                      filter = "top")
  
  output$times_tab <- renderDataTable(tot_effect_times_df_print(),
                                      extensions = c('Buttons', 
                                                     'ColReorder'
                                      ), options = list(
                                        colReorder = TRUE,
                                        dom = 'Bfrtip',
                                        buttons = c(#'copy', 
                                          #'csv', 
                                          #'excel', 
                                          #'pdf', 
                                          #'print', 
                                          I('colvis')
                                        )
                                      ), 
                                      
                                      filter = "top")
  
  

    
    
  output$downloadpdfreport_ae <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(chart_line = chart_line1(),
                     chart_bar = chart_bar1(),
                     #map = lf(),
                     wells = wells1(),
                     #raster = RF_poly0(),
                     labels = labels1(),
                     zone = zone2(),
                     time  = time2(),
                     pumping_type = input$pump_in_type,
                     abstr_type = input$abstr_type,
                     period_type = input$period_type,
                     date_range_hist = input$date_range_hist,
                     results_df = results_df(),
                     SD_per_bore = tot_effect_df_print(),
                     SD_per_river = tot_effect_zones_df_print(),
                     SD_per_times = tot_effect_times_df_print(),
                     SD_per_streams = tot_effect_per_each_zone_df(),
                     include_wells = input$include_wells,
                     include_effect_streams = input$include_effect_streams,
                     total_pump = wells_total()
      )
      
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
  
  output$downloadData_bore_template <- downloadHandler(
    
      filename = "bore_template.csv",
      content = function(file) {
          file.copy("data/wells/wells.csv", file)
    }
  )
  
  
  output$downloadData_bore <- downloadHandler(
    filename = "SD_per_bore.csv",
    content = function(file) {
      write.csv(tot_effect_df_print() , file = file, row.names = FALSE)
    }
  )
  
  output$downloadData_river <- downloadHandler(
    filename = "SD_per_river.csv",
    content = function(file) {
      write.csv(tot_effect_zones_df_print() , file = file, row.names = FALSE)
    }
  )
  
  output$downloadData_time <- downloadHandler(
    filename = "SD_per_times.csv",
    content = function(file) {
      write.csv(tot_effect_times_df_print() , file = file, row.names = FALSE)
    }
  )
  
  output$downloadData_effectzone <- downloadHandler(
    filename = "effect_per_zone.csv",
    content = function(file) {
      write.csv(tot_effect_per_each_zone_df() , file = file, row.names = FALSE)
    }
  )
  
  #this is for captchta email
  result <- callModule(recaptcha, "test", secret = "6LeIxAcTAAAAAGG-vFI1TnRWxMZNFuojJ4WifJWe")
  
  output$humansOnly <- renderUI({
    req(result()$success)
    tags$p("prakowski@gmail.com")
  })
}