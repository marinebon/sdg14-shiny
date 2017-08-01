library(tidyverse)
library(RPostgreSQL)

db = list(
  host = read_delim('/etc/hosts', '\t', col_names=c('ip','host')) %>%
    separate(host, c('name','image'), extra='merge', fill='left') %>%
    filter(name=='postgis') %>%
    .$ip,
  port = 5432,
  user = 'docker',
  pass = readLines('/mbon/.pg_pass_docker'),
  name = 'mbon')
db$dsn=sprintf(
  "PG:dbname=%s host=%s port=%s user=%s password=%s", 
  db$name, db$host, db$port, db$user, db$pass)

con = dbConnect(
  dbDriver('PostgreSQL'), 
  dbname=db$name, 
  user=db$user, password=db$pass, host=db$host, port=db$port)

dbListTables(con)
dbExistsTable(con, "obis_occ")

dbGetQuery(con, 'select * from geometry_columns')
dbGetQuery(con, 'select * from geography_columns')