# load packages, installing if needed ----
packages = c(
  # general data science
  'tidyverse','stringr',
  # spatial
  'sf','leaflet','htmltools', # 'leaflet.extras',
  # shiny
  'shiny')
for (pkg in packages){ # pkg= packages[1] # pkg = 'r-spatial/mapview@develop' # pkg='ropensci/plotly'
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

# # set vars
# #setwd('info-gl')
# countries_geo = 'data/countries.geojson'
# eez_shp = 'data/eez_s005.shp'
# crs_mol = '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs'
# 
# # prep data
# if (!file.exists(countries_geo)){
#   download.file(
#     'https://github.com/datasets/geo-boundaries-world-110m/raw/master/countries.geojson', 
#     countries_geo)}
# 
# # read data
# eez = read_sf(eez_shp)
# countries = read_sf(countries_geo)

eez_s005005_shp = '../info-gl/data/eez_s005005.shp'
eez = read_sf(eez_s005005_shp) # plot(eez['Territory1'])

# process data
eez = eez %>% # eez$Pol_type %>% table()
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