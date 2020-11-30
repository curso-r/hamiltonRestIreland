## code to prepare `latest_covid` dataset goes here
provinces <-  read.csv('data-raw/provinces.csv')

latest_covid = read.csv(
  'http://opendata-geohive.hub.arcgis.com/datasets/d9be85b30d7748b5b7c09450b8aede63_0.csv',
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate(Date = as.Date(TimeStamp)) %>% 
  dplyr::add_count(CountyName) %>% 
  dplyr::filter(n == max(n)) %>% 
  dplyr::left_join(provinces, by = c('CountyName' = 'County'))

old_irish_county_data = readRDS('data-raw/latest_irish_county_data.rds')
if(nrow(latest_covid) > nrow(old_irish_county_data)) {
  saveRDS(latest_covid, file = 'latest_irish_county_data.rds')  
} else {
  latest_covid = readRDS(file = 'latest_irish_county_data.rds')
}
usethis::use_data(latest_covid, overwrite = TRUE)
