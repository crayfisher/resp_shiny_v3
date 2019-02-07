library(raster)

#function to read in SDZ rasters
# this is temporaty for testing
folder <- "data/raster/"

SDZ_imp <- function(zone_i,time_i,perc){
    #load SDZ raster
  fnameL1 <- paste("RF",zone_i,time_i,1,perc,sep = "_")
  fnameL1 <- paste(folder,fnameL1,".tif",sep = "")
  fnameL2 <- paste("RF",zone_i,time_i,2,perc,sep = "_")
  fnameL2 <- paste(folder,fnameL2,".tif",sep = "")
  RF1 <- raster(fnameL1)
  RF2 <- raster(fnameL2)
  RF_comb <- list(RF1,RF2)
  return(RF_comb)
}

#function to  
tot_eff2 <- function(wells_list,zone,time,perc){
  RF_comb <- SDZ_imp(zone,time,perc)
  #output
  RF_L1 <- RF_comb[[1]]
  RF_L2 <- RF_comb[[2]]
  
 
 wells_sp1 <- wells_list[[1]]
 wells_sp2 <-  wells_list[[2]]
 wells1_1 <-   wells_list[[3]] 
 wells1_2 <-   wells_list[[4]] 
 if(NROW(wells1_1) >=1) {
   ef1 <- raster::extract(RF_L1,wells_sp1,method ="bilinear",fun = sum) 
   df1 <- data.frame(SD = ef1) %>% 
     dplyr::slice(1:(n()-1)) %>% 
     bind_cols(wells1_1) 
 }
 if(NROW(wells1_2) >=1) {
   ef2 <- raster::extract(RF_L2,wells_sp2,method ="bilinear",fun = sum) 
   df2 <- data.frame(SD =  ef2) %>%
     dplyr::slice(1:(n()-1)) %>% 
     bind_cols(wells1_2)
 }
 if(NROW(wells1_1) >=1 & NROW(wells1_2) >=1){
   tot_effect <- bind_rows(df1,df2)
 }else if(NROW(wells1_1) == 0){
    tot_effect <- df2
    }else if(NROW(wells1_2) == 0){tot_effect <- df1}
 tot_effect <- tot_effect %>% 
    filter(is.na(SD) != T) %>% 
    mutate(SD_tot = Q*SD)
  
  return(tot_effect)
}

sd_extract <- function(wells1,zone_i,time_i,layer){
  
  SD50R<- SDZ_imp(zone_i,time_i,"Q50")[[layer]]
  SD10R <- SDZ_imp(zone_i,time_i,"Q10")[[layer]]
  SD90R <- SDZ_imp(zone_i,time_i,"Q90")[[layer]]
  SD50 <- raster::extract(SD50R,wells1,method = "bilinear",fun = mean)
  SD10 <- raster::extract(SD10R,wells1,method = "bilinear",fun = mean)
  SD90 <- raster::extract(SD90R,wells1,method = "bilinear",fun = mean)
  effect <- c(SD50,SD10,SD90)
  effect
  
  
  
}

#summarise to one value
tot_eff2_sum <- function(df){
  tot_eff2_sum <- df %>% 
    group_by() %>% 
    summarise(SD_tot= sum(SD_tot,na.rm = T))
  return(unlist(tot_eff2_sum))
}

#SDZ_imp("ALLZN","49","Q50")


#function to convert wells data frame to rasters per layer
# pump2rast <- function(wells){
# #generate well from input
# 
# zero_row <- wells %>% 
#     slice(0) %>% 
#     add_row() %>% 
#     mutate(Q=0, 
#            bore = "dummy",
#            x=0,
#            y=0)  # dummy row to be added to avoid empty matrix  
#   
#   
# 
# #wells need to be divided to L1 and L2 because sdz are for layers 1 and 2
# wells_L1 <- wells %>% 
#   filter(L==1) %>%
#   bind_rows(zero_row) #dummy row added just in case its's empty
# 
# wells_L2 <- wells %>% 
#   filter(L==2)%>%
#   bind_rows(zero_row) #dummy row added just in case its's empty
# 
# #create emplty raster
# raster_blnak <- raster(nrows = 58,ncol = 98, xmn = 1892760, xmx = 1941760 , ymn =  5594200, ymx  = 5623200)
# 
# #rasterrise pumping data set
# coordinates(wells_L1) <- ~ x + y
# coordinates(wells_L2) <- ~ x + y
# raster_pumping_L1 <- rasterize(wells_L1,raster_blnak,wells_L1$Q,fun=sum)
# raster_pumping_L2 <- rasterize(wells_L2,raster_blnak,wells_L2$Q,fun=sum)
# raster_pumping_comb <- list(raster_pumping_L1,raster_pumping_L2)
# return(raster_pumping_comb)
# }

###### this sction is for testing
# RF1 <- raster(paste(folder,"RF_ALLZN_49_1_Q50.tif",sep=""))
# RF2 <- raster(paste(folder,"RF_ALLZN_49_2_Q50.tif",sep=""))
# wells <- read_csv("data/wells/wells.csv") %>%
#   select(E:L)
#   colnames(wells) <- c("x","y","Q","L")
# 
# 
#     #create a point for the selected location
#     #convert wells to lat long as set as spatial object for mapping in leaflet
#     wells1_1 <- wells %>% 
#       filter(L==1) %>% 
#       st_as_sf(coords = c("x", "y"),crs = 2193)
#     
#     #st_write(wells1_1,"wells1_1.shp")
#     wells_sp1 <- as(wells1_1, 'Spatial')
#     
#     wells1_2 <- wells %>% 
#       filter(L==2) %>% 
#       st_as_sf(coords = c("x", "y"),crs = 2193)
#     wells_sp2 <- as(wells1_2, 'Spatial')
# 
#   plot(wells_sp1)
#   
#   ef1 <- raster::extract(RF1,wells_sp1,method ="bilinear",fun = sum)
#   df1 <- data.frame(effect = ef1) %>% 
#     bind_cols(wells1_1) %>% 
#     mutate(L=1)
####### 

#test this
#test <- tot_eff2_sum(tot_eff2(wells,"ALLZN","49","Q50"))


########
# #inner function to calclulate the total effect
# tot_eff <- function(raster_pumping_L1,
#                     raster_pumping_L2,
#                     RF_L1,
#                     RF_L2){
#   RF_tot_effect_L1 <- raster_pumping_L1 *RF_L1/100/3600/24*(-1000)
#   RF_tot_effect_L2 <- raster_pumping_L2 *RF_L2/100/3600/24*(-1000)
#   tot_effect_L1 <- cellStats(RF_tot_effect_L1, sum)
#   tot_effect_L2 <- cellStats(RF_tot_effect_L2, sum)
#   tot_effect <- tot_effect_L1 + tot_effect_L2
#   return(tot_effect)
# }




#outer function to calclulate pumping effect 
# tot_eff_calc <- function(zone,time,raster_pumping_comb){
#   
#   RF_comb <- SDZ_imp(zone,time)
#   #output
#   RF_L1 <- RF_comb[[1]]
#   RF_L2 <- RF_comb[[2]]
#   
#   #####
#   #function to calc combined effect 
#   tot_effect <- tot_eff(raster_pumping_comb[[1]],
#                         raster_pumping_comb[[2]],
#                         RF_L1,
#                         RF_L2)
#   res1 <- data.frame(zone = zone,
#                               time = time,
#                               tot_effect=tot_effect,
#                               stringsAsFactors = F)
# 
#   # #
#   # RFL <- if(input$Layer==1){RF_L1}else{RF_L2}
#   # crs(RFL) <- nztm
#   # res1[z,4] <- extract(RFL,wells1()) #this extracts %stream depletion from raster
#   #
#   return(res1)
#   
# }
