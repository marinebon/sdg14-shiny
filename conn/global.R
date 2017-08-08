library(tidyverse)
library(stringr)
library(shiny)
library(sf)
library(rgdal)
library(raster)
library(leaflet)
library(leaflet.extras)
library(geojsonio)
select = dplyr::select

dir_root = switch(
  Sys.info()[['sysname']],
  'Darwin'  = '/Volumes/Best HD/mbon_data_big/connectivity', # BB's Mac
  'Windows' = 'P:',                                          # constance.bren.ucsb.edu
  'Linux'   = '/mbon/data_big/connectivity')                 # mbon.marine.usf.edu

# sanctuaries = c('fknms','mbnms')
# yrs = 2009:2011
# plds = c(10,30,50)
# bufs = c(300,500)
# cellsizes = c(27,45)
# dates = c('01_25_2009','04_24_2009','07_21_2009','10_17_2009')
# 
# # get file paths
# rm('dpaths')
# for (sanctuary in sanctuaries){ # sanctuary='mbnms'
#   for (buf in bufs){
#     for (cellsize in cellsizes){
#       
#       pid_tif = sprintf(
#         '%s/habitats/%s_%dbuf-%dkm_patchid.tif',
#         dir_root, sanctuary, buf, cellsize)
#       
#       for (yr in yrs){
#         for (pld in plds){
#           for (date in dates){ # date = '01_25_2009'
#             conn_csv = sprintf(
#               '%s/%s_%d/%dday_%dbuf-%dkm/%s_%s_%dday_%dbuf-%dkm_results/connectivity.csv',
#               dir_root,sanctuary, yr, pld, buf, cellsize, date, sanctuary, pld, buf, cellsize)
#             
#             d1 = frame_data(
#               ~sanctuary, ~yr, ~pld, ~buf, ~cellsize, ~date, ~conn_csv, ~conn_csv_exists, ~pid_tif,         ~pid_tif_exists,
#               sanctuary,  yr,  pld,  buf,  cellsize,  date, conn_csv, file.exists(conn_csv), pid_tif, file.exists(pid_tif))
#             
#             if (!exists('dpaths')){
#               dpaths = d1
#             } else {
#               dpaths = bind_rows(dpaths, d1)
#             }
#           }
#         }
#       }
#     }
#   }
# }

# dpaths %>%
#   filter(yr==2009, conn_csv_exists, pid_tif_exists) %>%
#   group_by(sanctuary,pld,buf,cellsize,date) %>%
#   summarize(n=n()) %>%
#   View()
