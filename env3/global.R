# load packages
library(tidyverse)
library(stringr)
library(leaflet)
library(shiny)
library(shinydashboard)
library(xml2)
library(RColorBrewer)

# https://shiny.rstudio.com/reference/shiny/latest/shiny-options.html
options(
  shiny.sanitize.errors = F, shiny.autoreload=T,
  shiny.fullstacktrace=T, shiny.stacktraceoffset=T,
  shiny.trace=F, shiny.testmode=F, shiny.minified=T,
  shiny.deprecation.messages=T,
  shiny.reactlog=F)

dir_wd = 'env3'
if (basename(getwd())!=dir_wd) setwd(dir_wd)

dir_root = switch(
  Sys.info()[['sysname']],
  'Darwin'  = '/Volumes/Best HD/mbon_data_big', # BB's Mac
  'Windows' = 'P:',                                          # constance.bren.ucsb.edu
  'Linux'   = '/mbon/data_big')                 # mbon.marine.usf.edu

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

var_colors = list(
  seascape = list(
    type   = 'interval',
    colors = 'Spectral',
    min    = 1,
    max    = 14,
    n      = 14),
  chl = list(
    type   = 'ramp',
    colors = 'Greens',
    min    = -6.91,
    max    = 4.61,
    n      = 7),
  sst = list(
    type   = 'ramp',
    colors = 'Reds',
    min    = -4,
    max    = 36,
    n      = 7))

env_vars = list(
  chl     = list(
    legend     = 'Chl (mg/m<sup>3</sup>)',
    values     = seq(-6.91, 4.61, length.out = 7),
    pal        = colorNumeric('Greens', seq(-6.91, 4.61, length.out = 7), na.color='transparent'),
    transform  = function(x){ round(exp(x),2) },
    curr_lyr   = 'gl_chl_curr_09km_mo',
    curr_dates = get_wms_dates(xml, 'gl_chl_curr_09km_mo')),
  seascape=list(
    legend     = 'Class',
    values     = 1:14,
    pal        = colorFactor(colorRampPalette(brewer.pal(11,'Spectral'))(14), 1:14, na.color='transparent'),
    transform = function(x){ x },
    curr_lyr   = 'gl_sea_curr_09km_mo',
    curr_dates = get_wms_dates(xml, 'gl_sea_curr_09km_mo')),
  sst     = list(
    legend     = 'SST (°C)',
    values     = seq(-4, 36, length.out = 7),
    pal        = colorNumeric('Reds', seq(-4, 36, length.out = 7), na.color='transparent'),
    transform = function(x){ x },
    curr_lyr   = 'gl_sst_curr_09km_mo',
    curr_dates = get_wms_dates(xml, 'gl_sst_curr_09km_mo')))

# default layer
var   = 'sst'
dates = env_vars[[var]][['curr_dates']]

msg = function(txt, debug=F){
  if (debug)
    cat(sprintf('%s ~ %s\n', txt, Sys.time()), file=stderr())
}

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