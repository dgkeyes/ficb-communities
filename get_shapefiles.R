library(tigris)
library(tidyverse)
library(sf)
library(janitor)

or_ca_map <- counties(cb = T) %>% 
     st_as_sf() %>% 
     clean_names() %>% 
     filter(statefp == "41" | statefp == "06") %>% 
     filter(statefp == "41" | name == "Siskiyou")
     
