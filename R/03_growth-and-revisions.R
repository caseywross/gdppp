## GDPplus-Related Revisions 03 - Growth and revision calculation script ----

# This script will calculate annualized growth rates and revisions in growth 
# for GDPplus, real GDP, and real GDI.

## Setup ----
library(timetk)

## Testing ----

# Calculate growth rates by vintage
output_data_clean$rgdp %>%
  
  filter_by_time(date_obs_dt, "2021-03-31", "2021-09-30") %>% 
  
  group_by(vintage_dt) %>% 
  mutate(growth_rt = ((value/lag(value, 1))^4-1)*100) %>% 
  ungroup() %>% 
  
#calculcate revision in growth rates by observation and flag vintages with revisions
  drop_na(growth_rt) %>% 
  
  group_by(date_obs_dt) %>% 
  mutate(rev_growth = growth_rt - lag(growth_rt)) %>% 
  mutate(is_revised = if_else(rev_growth > 0, TRUE, FALSE, missing = FALSE)) %>% 
  ungroup() %>% print(n = 41)

## Functions ----

ann_growth_rt <- function(data){
  
  data %>%
    group_by(vintage_dt) %>% 
    mutate(growth_rt = ((value/lag(value, 1))^4-1)*100) %>% 
    ungroup()
  }
  
revision_calc <- function(data) {
  data %>% 
    group_by(date_obs_dt) %>% 
    mutate(rev_growth = growth_rt - lag(growth_rt)) %>% 
    mutate(is_revised = if_else(rev_growth != 0, 
                                TRUE, 
                                FALSE, 
                                missing = FALSE)) %>% 
    ungroup()
}

## Apply functions ----

#only need to map to rgdp and rgdi since gdpplus is already in growth rates
convert_to_growth <- names(output_data)[-3]

### map to each dataset ----
output_growth_revised <- output_data_clean %>%
  
  # growth rates for the datasets in levels
  map_at(.at = convert_to_growth, ann_growth_rt) %>% 
  
  # copy the GDPplus value column to a new growth_rt column for consistency
  map_at("gdpplus", 
         ~.x %>% mutate(growth_rt = value)) %>% 
  map(revision_calc)

### test result ----
output_growth_revised$rgdp %>% 
  filter_by_time(date_obs_dt, "2021-09-30", "2021-12-31") %>% 
  print(n = 41)


  