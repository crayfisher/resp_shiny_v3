load(file = "data/params/params1.rdata") # this contains definition of zones and times available

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


source("proc.R")
folder <- "data/raster/"
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
nztm <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"

pal <- rev(rainbow(5,start = 0,end = .5))
at <-seq(0,100,10)
cb <- colorBin(pal,bins=at,domain = at)
perc_s <- c("Q50","Q10","Q90")
wells_dummy <-data.frame(x = c(0,0),
                         y = c(0,0),
                         Q = c(0,0),
                         L = c(1,2))  
