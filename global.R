load(file = "data/params/params1.rdata") # this contains definition of zones and times available
#require(devtools)
#install_github('rCharts', 'ramnathv')

#install.packages("downloader")
#library(downloader)
#download("https://github.com/ramnathv/rCharts/archive/master.tar.gz", "rCharts.tar.gz")
#install.packages("rCharts.tar.gz", repos = NULL, type = "source")

library(shiny)
library(tidyverse)
library(raster)
library(leaflet)
library(RColorBrewer)
library(sf)
library(rgdal)
library(plotly)
library(DT)
library(lubridate)
library(gridExtra)
library(knitr)
library(tmap)
library(tmaptools)
library(OpenStreetMap)
library(lubridate)
library(shinycssloaders)
#library(rCharts)

#2019_09_05
#library(pdftools) #2019_09_06
library(shinyCAPTCHA)
#devtools::install_github("CannaData/shinyCAPTCHA", force = TRUE)

source("proc.R")
folder <- "data/raster/"
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
nztm <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"

#I changed the times df to fix 60 days issue in the app (app was crashing when 60 days is chosen)
#I will create a new df, I will call it times_up in which I will change 61 to 60 in comm_time 
#after that, in server.R, I will replcae all times with times_up
times_up <- times %>% 
  dplyr::mutate(comm_time= replace(comm_time, id== 20, 60 ))

times_up1 <- times_up %>% 
  dplyr::select(duration, comm_time) %>% 
  dplyr::rename(time_duration =duration)

pal <- rev(rainbow(5,start = 0,end = .5))
at <-seq(0,100,10)
cb <- colorBin(pal,bins=at,domain = at)
perc_s <- c("Q50","Q10","Q90")
wells_dummy <-data.frame(x = c(0,0),
                         y = c(0,0),
                         Q = c(0,0),
                         L = c(1,2))  

