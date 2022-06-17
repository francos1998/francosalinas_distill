library(tidycensus)
library(tidyverse)
library(sf)
library(data.table)

options(tigris_use_cache = TRUE)
census_api_key("19609eb52d1c1d8a6000e796ad5ee4027040605f", overwrite = TRUE) # to get your own data, you need a Census API key (https://walker-data.com/tidycensus/articles/basic-usage.html)

v19 <-load_variables(2019, "acs5", cache = TRUE)  


VARS <- c(Income = "B19013_001", Pop = 'B01003_001', Age = "B01002_001", HouseValue = "B25077_001",HouseholdSize = "B25010_001",NumHouse = "B25003_001") # We can get more variables if we need

# House Value is median value dollars

# Get census data for Bay Area

BAY_AREA_COUNTY_NAMES = c("San Francisco", "San Mateo", "Santa Clara", "Sonoma", "Napa", "Solano", "Marin", "Contra Costa", "Alameda")

bay_area_data_2019 <- get_acs(state = "CA", geography = "county", 
                   variables = VARS, geometry = TRUE, output='wide', survey = "acs5", year = 2009)

# bay_area_data_2019  %>%
#   ggplot() +
  # geom_sf(aes(fill = HouseValueE))


TECH_BAY_AREA_COUNTY_NAMES = c("San Francisco", "San Mateo", "Santa Clara", "Marin", "Contra Costa", "Alameda")


bay_area_data_2009_2019_acs5 <- cbind(get_acs(state = "CA", county = TECH_BAY_AREA_COUNTY_NAMES, geography = "tract", 
                               variables = VARS, geometry = TRUE, output='wide', year = 2009), year = 2009) %>% # 2009
  bind_rows(cbind(get_acs(state = "CA", county = TECH_BAY_AREA_COUNTY_NAMES, geography = "tract", variables = VARS, geometry = TRUE, output='wide', year = 2010), year = 2010)) %>% # 2010
  bind_rows(cbind(get_acs(state = "CA", county = TECH_BAY_AREA_COUNTY_NAMES, geography = "tract", variables = VARS, geometry = TRUE, output='wide', year = 2011), year = 2011)) %>% # 2011
  bind_rows(cbind(get_acs(state = "CA", county = TECH_BAY_AREA_COUNTY_NAMES, geography = "tract", variables = VARS, geometry = TRUE, output='wide', year = 2012), year = 2012)) %>% # 2012
  bind_rows(cbind(get_acs(state = "CA", county = TECH_BAY_AREA_COUNTY_NAMES, geography = "tract", variables = VARS, geometry = TRUE, output='wide', year = 2013), year = 2013)) %>% # 2013
  bind_rows(cbind(get_acs(state = "CA", county = TECH_BAY_AREA_COUNTY_NAMES, geography = "tract", variables = VARS, geometry = TRUE, output='wide', year = 2014), year = 2014)) %>% # 2014
  bind_rows(cbind(get_acs(state = "CA", county = TECH_BAY_AREA_COUNTY_NAMES, geography = "tract", variables = VARS, geometry = TRUE, output='wide', year = 2015), year = 2015)) %>% # 2015
  bind_rows(cbind(get_acs(state = "CA", county = TECH_BAY_AREA_COUNTY_NAMES, geography = "tract", variables = VARS, geometry = TRUE, output='wide', year = 2016), year = 2016)) %>% # 2016
  bind_rows(cbind(get_acs(state = "CA", county = TECH_BAY_AREA_COUNTY_NAMES, geography = "tract", variables = VARS, geometry = TRUE, output='wide', year = 2017), year = 2017)) %>% # 2017
  bind_rows(cbind(get_acs(state = "CA", county = TECH_BAY_AREA_COUNTY_NAMES, geography = "tract", variables = VARS, geometry = TRUE, output='wide', year = 2018), year = 2018)) %>% # 2018
  bind_rows(cbind(get_acs(state = "CA", county = TECH_BAY_AREA_COUNTY_NAMES, geography = "tract", variables = VARS, geometry = TRUE, output='wide', year = 2019), year = 2019)) # 2019


# bay_area_data_test_18 <- get_acs(state = "CA", county = TECH_BAY_AREA_COUNTY_NAMES, geography = "tract", 
                         # variables = VARS, geometry = TRUE, output='wide', year = 2018)

bay_area_data_2009_2019_acs5 <- bay_area_data_2009_2019_acs5 %>% select(-ends_with('M'))


get_bay_area_data <- function(V,V1,NAMES){
  bay_area_cat <- get_acs(state = "CA", county = TECH_BAY_AREA_COUNTY_NAMES, geography = "tract", 
                        variables = V, geometry = FALSE, output='wide',summary_var = V1)
  
  bay_area_cat %>% 
    select(-ends_with('M')) %>%
    mutate(Total = select(.,-c(GEOID,NAME,summary_est,summary_moe)) %>% rowSums(na.rm=TRUE)) %>%
    mutate(across(-c(GEOID,NAME,summary_est,summary_moe), ~.x/bay_area_cat$summary_est)) %>%
    select(-c(summary_est,summary_moe,Total))
}


V1 <- "C24070_001"
V <- paste0("C24070_0",str_pad(c(2:14),2,"0",side='left'))
NAMES <- v19 %>% filter(name %in% V) %>% pull(label) %>% str_replace('Estimate\\!\\!Total\\:\\!\\!','')
industry = get_bay_area_data(V,V1,NAMES)
names(industry) = c('GEOID','NAME',paste0('Industry_',str_sub(NAMES,0,5)))
NMS <- c(VARS,V)

V1 <- "B06001_001"
V <- paste0("B06001_0",str_pad(c(13,25,37,49),2,"0",side='left'))
NAMES <- v19 %>% filter(name %in% V) %>% pull(label) %>% str_replace('Estimate\\!\\!Total\\:\\!\\!','')
birthplace = get_bay_area_data(V,V1,NAMES)
names(birthplace) = c('GEOID','NAME',trimws(paste0('BirthPlace_',str_sub(NAMES,0,20))))
NMS <- c(NMS,V)

V1 <- "B25003_001"
V <- paste0("B25003_0",str_pad(c(2:3),2,"0",side='left'))
NAMES <- v19 %>% filter(name %in% V) %>% pull(label) %>% str_replace('Estimate\\!\\!Total\\:\\!\\!','')
housetype = get_bay_area_data(V,V1,NAMES)
names(housetype) = c('GEOID','NAME',trimws(paste0('HouseType_',str_sub(NAMES,0,20))))
NMS <- c(NMS,V)


V1 <- "B02001_001"
V <- paste0("B02001_0",str_pad(c(2:8),2,"0",side='left'))
NAMES <- v19 %>% filter(name %in% V) %>% pull(label) %>% str_replace('Estimate\\!\\!Total\\:\\!\\!','')
race = get_bay_area_data(V,V1,NAMES)
names(race) = c('GEOID','NAME',trimws(paste0('Race_',str_sub(NAMES,0,26))))
NMS <- c(NMS,V)

bay_area_data_2009_2019_acs5_data <- bay_area_data_2009_2019_acs5 %>% left_join(birthplace) %>% left_join(industry) %>% left_join(housetype) %>% left_join(race)

bay_area_data_2009_2019_acs5_data$AREA = st_area(bay_area_data_2009_2019_acs5_data) %>% as.vector()


bay_area_data_2009_2019_acs5_data %>% View()

save(bay_area_data_2009_2019_acs5_data,file = 'CapstoneData.RData')
