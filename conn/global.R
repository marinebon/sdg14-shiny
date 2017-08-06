library(tidyverse)
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
  
dir_results = file.path(dir_root, 'mbnms_2009/10day_300buf-27km/01_25_2009_mbnms_10day_300buf-27km_results')
tif_pid     = file.path(dir_root, 'habitats/mbnms_300buf-27km_patchid.tif')

r_pid = raster(tif_pid)

#study_sf = readOGR('H:/MBON/study_area.gdb','mbnms_300buf_mol') %>%
#  st_as_sf()
