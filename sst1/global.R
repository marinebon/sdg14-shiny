# load packages
library(tidyverse)
library(stringr)
library(leaflet)
library(shiny)
library(xml2)

dir_wd = 'sst1'
if (basename(getwd())!=dir_wd) setwd(dir_wd)

dir_root = switch(
  Sys.info()[['sysname']],
  'Darwin'  = '/Volumes/Best HD/mbon_data_big', # BB's Mac
  'Windows' = 'P:',                                          # constance.bren.ucsb.edu
  'Linux'   = '/mbon/data_big')                 # mbon.marine.usf.edu

sst_dir   = file.path(dir_root, 'satellite/sst4/anom_27km')
sst_files = list.files(sst_dir, 'sst_.*\\.tif$')
lyr = 'satellite:sst_monthly_mean_27km'

# get dates from WMS layer time list
wms_url = 'http://mbon.marine.usf.edu:8080/geoserver/ows?service=wms&version=1.3.0&request=GetCapabilities'
dates = read_xml(wms_url) %>%
  xml_find_first("//d1:Layer/d1:Layer[contains(d1:Name,'satellite:sst_monthly_mean_27km')]/d1:Dimension") %>%
  xml_text() %>%
  str_split(',') %>%
  .[[1]] %>%
  str_sub(1,10) %>%
  sort(decreasing=T) %>%
  as.Date()