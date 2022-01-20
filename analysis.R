## Import modules
library(sf)
library(tmap)
library(janitor)
library(dplyr)
library(reshape2)
source('R/os_hub_api_key_for_hackday20Jan2022.R')

## Options
tmap_mode('view')

## Import data
care_homes = read.csv('../data/carehome_point_locations.csv') %>%
  st_as_sf(wkt = 'lat_long_coordinates', crs = 4326) %>%
  clean_names() 

care_homes$cqclocationid_nformat = paste0('cqc_',gsub('-', '_',care_homes$cqclocationid))

care_home_metadata = read.csv('../data/carehome-meta-data.csv') %>%
  clean_names()

care_homes = care_homes %>% 
  left_join(care_home_metadata, 
            by = c('cqclocationid' = 'i_location_id'))

merseyside_lsoa_data = read.csv('../data/output tables/agg_results_merseyside_10km_25km.csv') %>%
  mutate(good_or_oustanding_10km = rating_good_10km	+ rating_outstanding_10km)

lsoa_pop_weighted_centroids = st_read("../data/LSOA_pwc_points/Lower_Layer_Super_Output_Areas_(December_2011)_Population_Weighted_Centroids.geojson")

merseyside_lsoa_data = lsoa_pop_weighted_centroids %>%
  right_join(merseyside_lsoa_data, by = "lsoa11cd")

merseyside_population = read.csv('../data/merseyside_lsoa_populations.csv') %>%
  clean_names()

lsoa_care_home_matrix = read.csv('../data/output tables/merseyside_results_table_10km.csv')

## reshape care home - lsoa matrix
lsoa_care_homes_lookup = lsoa_care_home_matrix %>% 
  melt(id.vars = 'lsoa11cd') %>%
  filter(! is.na(value))

## find care homes within 25km drive of merseyside
merseyside_care_homes_10km = care_homes %>%
  inner_join(lsoa_care_homes_lookup, by =c('cqclocationid_nformat' = 'variable')) 

# some are missing
lsoa_care_homes_lookup %>%
  filter(! variable %in% merseyside_care_homes_10km$cqclocationid_nformat) %>%
  arrange(variable) %>%
  head()

care_homes %>%
  filter(! cqclocationid_nformat %in% lsoa_care_homes_lookup$variable) %>%
  arrange(cqclocationid_nformat) %>%
  select(cqclocationid_nformat) %>%
  head()
# not sure what is going on

agg_by_lsoa_10km = merseyside_care_homes_10km %>% 
  group_by(lsoa11cd) %>%
  summarise(n_care_homes = length(unique(cqclocationid)),
            n_beds = sum(care_homes_beds)) %>%
  ungroup()

## adjusting beds
pop_served =  merseyside_care_homes_10km %>%
  filter(over_65 == 'Y') %>%
  full_join(merseyside_population, by = 'lsoa11cd') %>%
  group_by(cqclocationid) %>%
  summarise(pop_served = sum(aged_65))

care_homes_beds = care_homes[, c('cqclocationid', 'care_homes_beds')] %>%
  st_drop_geometry()

adjusted_beds = pop_served %>%
  left_join(care_homes_beds, by = "cqclocationid") %>%
  mutate(adjusted_beds = care_homes_beds/pop_served,
         cqclocationid = paste0('cqc_',gsub('-', '_',cqclocationid))) %>%
  st_drop_geometry() %>%
  left_join(lsoa_care_homes_lookup, by =c('cqclocationid' = 'variable')) %>%
  left_join(merseyside_population, by = 'lsoa11cd') %>%
  mutate(pop_adjusted_beds = adjusted_beds*aged_65) %>%
  group_by(lsoa11cd) %>%
  summarise(pop_adjusted_beds = sum(pop_adjusted_beds))

## plot adjusted beds:
adjusted_beds = lsoa_pop_weighted_centroids %>%
  right_join(adjusted_beds, by = "lsoa11cd")

tm_shape(adjusted_beds) +
  tm_dots(col = 'pop_adjusted_beds', breaks = c(0,5, 10,25,50,100,500))



agg_by_lsoa_10km %>% 
  tm_shape() +
  tm_dots(col = 'n_beds')

## divide by population
agg_by_lsoa_10km = agg_by_lsoa_10km %>%
  left_join(merseyside_population, by = 'lsoa11cd') %>%
  mutate(beds_per_person = n_beds/sum(aged_65))




agg_by_lsoa_10km %>% 
  tm_shape() +
  tm_dots(col = 'beds_per_person')

agg_by_lsoa_10km = agg_by_lsoa_10km %>%
  mutate(beds_per_person_grouped = case_when(beds_per_person < 25 ~ '0-24',
                                             beds_per_person < 50 ~ '25-49',
                                             beds_per_person >= 50 ~ '50+'
                                            ))

agg_by_lsoa_10km %>% 
  tm_shape() +
  tm_dots(col = 'beds_per_person_grouped')


hist(agg_by_lsoa_10km$beds_per_person)


## Filter care homes to merseyside
merseyside_ccgs = c('E38000068',
                    'E38000091',
                    'E38000101',
                    'E38000161',
                    'E38000170',
                    'E38000172',
                    'E38000208')

merseyside_care_homes = care_homes %>%
  filter(location_onspd_ccg_code %in% merseyside_ccgs) %>%
  st_as_sf()



########################################
# Plot 
########################################

# care homes
tm_shape(merseyside_care_homes) +
  tm_dots()

# lsoas with aggregated data
tm_shape(merseyside_lsoa_data) +
  tm_bubbles('no_carehomes_10km', id = 'no_carehomes_10km') +
  tm_shape(merseyside_care_homes) +
  tm_dots()

# lsoas with aggregated data
tm_shape(merseyside_lsoa_data) +
  tm_bubbles('no_carehomes_10km', id = 'no_carehomes_10km') +
  tm_shape(merseyside_care_homes) +
  tm_dots()

# lsoas with aggregated data, good or oustanding
tm_shape(merseyside_lsoa_data) +
  tm_bubbles('good_or_oustanding_10km', id = 'good_or_oustanding_10km ') +
  tm_shape(merseyside_care_homes) +
  tm_dots()


tm_shape(merseyside_lsoa_data) +
  tm_dots(col= 'good_or_oustanding_10km', id = 'good_or_oustanding_10km ') +
  tm_shape(merseyside_care_homes) +
  tm_dots()





