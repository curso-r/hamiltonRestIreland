## code to prepare `latest_covid` dataset goes here
library(dplyr)
provinces <-  read.csv('data-raw/provinces.csv')

latest_covid = read.csv(
  'http://opendata-geohive.hub.arcgis.com/datasets/d9be85b30d7748b5b7c09450b8aede63_0.csv',
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate(Date = as.Date(TimeStamp)) %>% 
  dplyr::add_count(CountyName) %>% 
  dplyr::filter(n == max(n)) %>% 
  dplyr::left_join(provinces, by = c('CountyName' = 'County'))

old_irish_county_data = load('data/latest_covid.rda')
if(identical(latest_covid, old_irish_county_data)) {
  usethis::ui_done("Nothing to update")
} else {
  usethis::use_data(latest_covid, overwrite = TRUE)
  usethis::ui_done("dataset updated!")
  deploy_app <- 1
} 
