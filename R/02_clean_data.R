## GDPplus-Related Revisions 02 - Data clean script ----

# This script will read in real time datasets for GDPplus, real GDP, and real GDI.

# list of each output dataset so we can operate on all three at once with purrr functions
output_data <- list(rgdp, rgdi, gdpplus) %>% set_names(c("rgdp", "rgdi", "gdpplus"))


### Pivot long ----
output_data_long <- 
output_data %>% 
  
  map(function (x) {
    x %>%
      rename_with(function(colname) {return("date_obs")}, 1) %>% 
      
      # pivot to long format with date of the observation as the key variable
      pivot_longer(-date_obs) %>% 
      
      #split the vintage column into a variable and vintage date column
      separate_wider_regex(
        cols = name,
        patterns = c(dataset = "[:alpha:]*", 
                     vintage = ".*")
      )
    } 
      )

### GDPplus special ----

# gdpplus needs additional fixing unique to that dataset
output_data_long$gdpplus <- output_data_long$gdpplus %>% 
  
  #remove underscore from vintage
  mutate(vintage = str_remove(vintage, "_")) 

### Real GDP Special ----

# rgdp needs 19 or 20 prepended to data vintages
output_data_long$rgdp <- output_data_long$rgdp %>% 
  separate_wider_delim(vintage, names = c("vintg_yr", "vintg_mnth"), delim = "M") %>% 
  mutate(vintg_yr = case_when(as.numeric(vintg_yr) %>% between(65, 99) ~ paste0("19", vintg_yr),
                              TRUE                                     ~ paste0("20", vintg_yr))) %>% 
  unite(vintage, vintg_yr, vintg_mnth, sep = "M")

### Date coercion ----

output_data_long_dt <- 
output_data_long %>% 
  
  map(function (x) {
    x %>% 
      
      #coerce the quarterly observation to last date of quarter
      mutate(date_obs_dt = date_obs %>% yq() %>% ceiling_date("quarter") %>% rollback()) %>% 
      
      #coerce the vintage to a mdy for gdpplus and md (first of month) for NIPA
      mutate(vintage_dt = case_when(
        str_detect(vintage, "M") ~ vintage %>% ym(quiet = TRUE),
        TRUE                     ~ vintage %>% mdy(quiet = TRUE)
        )
      )
  
  })

### Assign release number ----

output_data_long_dt_rel <- 

  output_data_long_dt %>% 
  
  map(function (x){
    x %>%
      
      #group by the observations
      group_by(date_obs_dt) %>% 
      
      #arrange by the vintage date
      arrange(vintage_dt, .by_group = TRUE) %>% 
      
      #drop the NA values so they aren't assigned releases
      drop_na(value) %>% 
      
      #assign a release number
      mutate(release_no = row_number()) %>% 
      
      ungroup()
  })

### Final cleaned dataset ----

output_data_clean <- 
output_data_long_dt_rel %>% 
  
  map(function (x) {
    x %>% select(dataset, date_obs_dt, vintage_dt, release_no, value)
  })

output_data_clean
