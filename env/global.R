# load packages, installing if needed ----
packages = c(
  # general data science
  'tidyverse','stringr','tools','lubridate',
  # spatial
  'sf','leaflet','htmltools', 'bhaskarvk/leaflet.extras','rmapshaper','geojsonio','ncdf4','raster',
  # time-series plot
  'xts','dygraphs','hrbrmstr/streamgraph',
  # shiny
  'shiny')
for (pkg in packages){ # pkg= packages[1] # pkg = 'r-spatial/mapview@develop' # pkg='ropensci/plotly' # pkg='rmapshaper'
  github_pkg = grepl('/', pkg)
  p = ifelse(github_pkg, sub('([-0-9A-Za-z]*)/([-0-9A-Za-z]*)@?([-0-9A-Za-z]*)', '\\2', pkg), pkg)
  if (!require(p, character.only=T)){
    if (github_pkg){
      if (!require(devtools)) install.packages('devtools')
      devtools::install_github(pkg)
    } else {
      install.packages(p)
    }
    library(p, character.only=T)
  }
}
# override namespace with preferred function calls
select    = dplyr::select
addLegend = leaflet::addLegend


if (basename(getwd())!='env') setwd('env')

eez_s005_shp    = '../info-gl/data/eez_s005.shp'
eez_s005005_shp = '../info-gl/data/eez_s005005.shp'

if (!file.exists(eez_s005005_shp)){
  library(geojsonio)
  eez_s005 = read_sf(eez_s005_shp) # plot(eez_s005['Territory1'])
  #object.size(eez_s005) %>% print(units='Mb') # 11.1 Mb

  # simplify eez
  eez_s005005 = eez_s005 %>%
    geojson_json() %>% # convert to geojson for faster ms_simplify; 69.6 sec
    ms_simplify(keep=0.05, keep_shapes=T, explode=F) %>% # simplify; 11.9 minutes
    geojson_sp() %>% st_as_sf() # convert back to simple features
  # plot(eez_s005005['Territory1'])

  write_sf(eez_s005005, eez_s005005_shp)
}
eez_sf = read_sf(eez_s005005_shp) # plot(eez['Territory1'])

# process data
eez_sf = eez_sf %>% # eez$Pol_type %>% table()
  filter(Pol_type=='200NM') %>% # 'Disputed','Joint regime'
  group_by(Sovereign1) %>%
  mutate(
    n_sov = n(),
    sov_ter = ifelse(
      Sovereign1 == Territory1,
      Sovereign1,
      sprintf('%s - %s', Sovereign1, Territory1))) %>% 
  ungroup() %>%
  arrange(sov_ter) # View(eez)


dir_root = switch(
  Sys.info()[['sysname']],
  'Darwin'  = '/Volumes/Best HD/mbon_data_big', # BB's Mac
  'Windows' = 'P:',                                          # constance.bren.ucsb.edu
  'Linux'   = '/mbon/data_big')                 # mbon.marine.usf.edu

chl_dir = file.path(dir_root, 'satellite/chlor_a/clim_27km')
chl_files = list.files(chl_dir, '.*_leaflet\\.grd$')
chl_paths = file.path(chl_dir, chl_files)
chl_names = str_sub(chl_files, 1, nchar(chl_files)-12)

grd_choices = list(
  `Seascapes` = c('GLOBE14_I90VAR3_9k'=file.path(dir_root, 'satellite/seascapes/gl/GLOBE14_I90VAR3_9k_leaflet.grd')),
  `Chl` = setNames(chl_paths, chl_names))