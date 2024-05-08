## GDPplus-Related Revisions 01 - Data intake script ----

# This script will read in real time datasets for GDPplus, real GDP, and real GDI.


###  Libraries and setup ----

library(tidyverse)
library(readxl)
library(lubridate)
library(rvest)
library(fs)

if (!dir_exists("data")) {dir_create("data")}
update_gdpplus <- FALSE
update_rgdp <- FALSE

###  Data Import ----

#### GDPplus ----

gdpplus_link <- "https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/gdpplus"

# download

if (update_gdpplus){
read_html(gdpplus_link) %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  str_subset("gdpplus_vintages.xlsx") %>% 
  str_c(gdpplus_link, .) %>% 
  download.file(destfile = "data/gdpplus.xlsx", mode = "wb")
}
# read into R

gdpplus <- read_xlsx("data/gdpplus.xlsx")

gdpplus


#### Real GDI ----

rgdi <- read_xlsx("data/rgdi.xlsx") %>% 
  
  # coerce vintages with no data to be numeric
  mutate(across(where(is.logical), as.numeric))

rgdi %>% glimpse()

#### Real GDP ----

rgdp_link <- "https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/routput"

# download

if (update_rgdp){
  read_html(rgdp_link) %>% 
    html_elements("a") %>% 
    html_attr("href") %>% 
    str_subset("routputmvqd.xlsx") %>% 
    str_c(rgdp_link, .) %>% 
    download.file(destfile = "data/routput.xlsx", mode = "wb")
}
# read into R

# Seems to be reading in some of the columns as character, so we need to specify how many numeric columns there should be (all but the first)
number_cols <- ncol(read_xlsx("data/routput.xlsx")) - 1

# read in, specifying one text column and the rest numeric
rgdp <- read_xlsx("data/routput.xlsx", col_types = c("text", rep("numeric", number_cols)), na = "#N/A")


rgdp
