## code to prepare `latest_covid` dataset goes here
library(dplyr)
provinces <-  read.csv('data-raw/provinces.csv')

latest = read.csv(
  'http://opendata-geohive.hub.arcgis.com/datasets/d9be85b30d7748b5b7c09450b8aede63_0.csv',
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate(Date = as.Date(TimeStamp)) %>% 
  dplyr::add_count(CountyName) %>% 
  dplyr::filter(n == max(n)) %>% 
  dplyr::left_join(provinces, by = c('CountyName' = 'County'))

load('data/latest_covid.rda')
if(identical(latest_covid, latest)) {
  usethis::ui_done("Nothing to update")
} else {
  latest_covid <- latest
  usethis::use_data(latest_covid, overwrite = TRUE)
  usethis::ui_done("dataset updated!")
  deploy_app <- 1
} 
