# load packages
library(tidyverse)
library(stringr)
library(lubridate)
library(leaflet)
library(sf)
library(shiny)
library(shinydashboard)
library(xml2)
library(RColorBrewer)
library(dygraphs)
library(xts)
library(streamgraph) # devtools::install_github('hrbrmstr/streamgraph')

# debug ----
# https://shiny.rstudio.com/reference/shiny/latest/shiny-options.html
options(
  shiny.sanitize.errors = F, shiny.autoreload=T,
  shiny.fullstacktrace=T, shiny.stacktraceoffset=T,
  shiny.trace=F, shiny.testmode=F, shiny.minified=T,
  shiny.deprecation.messages=T,
  shiny.reactlog=F)

msg = function(txt, debug=F){
  if (debug)
    cat(sprintf('%s ~ %s\n', txt, Sys.time()), file=stderr())
}

# paths ----
dir_wd = 'env3'
if (basename(getwd())!=dir_wd) setwd(dir_wd)

dir_local = switch(
  Sys.info()[['sysname']],
  'Darwin'  = '/Volumes/Best HD/mbon_data_big/local', # BB's Mac
  'Windows' = 'P:',                                          # constance.bren.ucsb.edu
  'Linux'   = '/mbon-local')                 # mbon.marine.usf.edu

#eez_s005_shp    = file.path(dir_local, 'boundaries/eez_s005.shp')
eez_s005005_shp = file.path(dir_local, 'boundaries/eez_s005005.shp')

# eez ----
eez_sf = read_sf(eez_s005005_shp)  %>%
  filter(Pol_type=='200NM') %>% # not 'Disputed','Joint regime'
  mutate(
    n_sov = n(),
    sov_ter = ifelse(
      Sovereign1 == Territory1,
      Sovereign1,
      sprintf('%s - %s', Sovereign1, Territory1))) %>% 
  ungroup() %>%
  arrange(sov_ter)

eez_labels = sprintf(
  '<strong>%s</strong>',
  eez_sf$Territory1) %>% 
  lapply(HTML)

# env layers ----
# get dates from WMS layer time list
wms_url = 'http://mbon.marine.usf.edu:8080/geoserver/ows?service=wms&version=1.3.0&request=GetCapabilities'
xml = read_xml(wms_url)

get_wms_dates = function(xml, lyr){
  # lyr = 'gl_sst_curr_09km_mo'
  xpath = sprintf("//d1:Layer/d1:Layer[contains(d1:Name,'satellite:%s')]/d1:Dimension", lyr)
  xml %>%
    xml_find_first(xpath) %>%
    xml_text() %>%
    str_split(',') %>%
    .[[1]] %>%
    str_sub(1,10) %>%
    sort(decreasing=T) %>%
    as.Date()
}

env_vars = list(
  chl     = list(
    legend     = 'Chl (mg/m<sup>3</sup>)',
    values     = seq(-6.91, 4.61, length.out = 7),
    pal        = colorNumeric('Greens', seq(-6.91, 4.61, length.out = 7), na.color='transparent'),
    transform  = function(x){ round(exp(x),2) },
    curr_lyr   = 'gl_chl_curr_09km_mo',
    curr_dates = get_wms_dates(xml, 'gl_chl_curr_09km_mo'),
    curr_eez   = read_csv(sprintf('%s/raster-extract/eez_%s.csv', dir_local, 'gl_chl_curr_09km_mo'))),
  seascape=list(
    legend     = 'Class',
    values     = 1:14,
    pal        = colorFactor(colorRampPalette(brewer.pal(11,'Spectral'))(14), 1:14, na.color='transparent'),
    transform = function(x){ x },
    curr_lyr   = 'gl_sea_curr_09km_mo',
    curr_dates = get_wms_dates(xml, 'gl_sea_curr_09km_mo'),
    curr_eez   = read_csv(sprintf('%s/raster-extract/eez_%s.csv', dir_local, 'gl_sea_curr_09km_mo'))),
  sst     = list(
    legend     = 'SST (°C)',
    values     = seq(-4, 36, length.out = 7),
    pal        = colorNumeric('Reds', seq(-4, 36, length.out = 7), na.color='transparent'),
    transform = function(x){ x },
    curr_lyr   = 'gl_sst_curr_09km_mo',
    curr_dates = get_wms_dates(xml, 'gl_sst_curr_09km_mo'),
    curr_eez   = read_csv(sprintf('%s/raster-extract/eez_%s.csv', dir_local, 'gl_sst_curr_09km_mo'))))

# default layer
var   = 'sst'
dates = env_vars[[var]][['curr_dates']] %>% sort()


# TODO: extract timeseries by EEZ

# TODO: other env w/ clim: chl, sst 9km
# [Using the ImageMosaic extension — GeoServer 2.13.x User Manual](http://docs.geoserver.org/latest/en/user/data/raster/imagemosaic/tutorial.html)

# TODO: WMS legend
# http://mbon.marine.usf.edu:8080/geoserver/wms?REQUEST=GetLegendGraphic&VERSION=1.3.0&FORMAT=image/png&WIDTH=20&HEIGHT=20&LAYER=satellite:sst_monthly_mean_27km
# https://github.com/bhaskarvk/leaflet.extras/blob/master/inst/examples/wmsLegend.R

# TODO: optimize caching params
# http://docs.geoserver.org/stable/en/user/geowebcache/webadmin/defaults.html

# TODO: dynamic GeoServer layer creation/deletion
# [CRAN - Package geosapi](https://cran.r-project.org/web/packages/geosapi/index.html)

# TODO: update password for postgis docker, geoserver admin

# TODO: vector tile EEZ
# [Vector tiles tutorial — GeoServer 2.13.x User Manual](http://docs.geoserver.org/latest/en/user/extensions/vectortiles/tutorial.html)