# Packages ----------------------------------------------------------------

library(tidyverse)
library(tidycensus)
library(googlesheets)
library(readxl)
library(janitor)
library(hrbrmisc)
library(hrbrthemes)
library(scales)
library(skimr)
library(zipcode)


# Functions ---------------------------------------------------------------

clean_community_name <- function(.data) {
     .data %>% 
          mutate(name = str_remove(name, c("California"))) %>% 
          mutate(name = str_remove(name, c("Oregon"))) %>% 
          mutate(name = str_remove(name, c("CDP"))) %>% 
          mutate(name = str_remove(name, c("city"))) %>% 
          mutate(name = str_remove(name, c(" ,"))) %>% 
          mutate(name = str_remove(name, c("town,"))) %>% 
          mutate(name = str_trim(name)) 
}


# See all ACS vars --------------------------------------------------------

vars <- load_variables(2016, "acs5", cache = TRUE)


# Communities -------------------------------------------------------------

# gs_title("FICB map data") %>% 
#      gs_download(to = "data/data.xlsx",
#                  overwrite = T)

# Get spreadsheet with all geocodes

# download.file("https://www2.census.gov/programs-surveys/popest/geographies/2016/all-geocodes-v2016.xlsx",
#               destfile = "data/geocodes.xlsx")

geocodes <- read_excel("data/geocodes.xlsx",
                       skip = 4) %>% 
     clean_names() %>% 
     filter(state_code_fips %in% c("41", "06")) %>% 
     rename(place_name = area_name_including_legal_statistical_area_description) %>% 
     filter(str_detect(place_name, "city")) %>% 
     mutate(place_name = str_remove(place_name, " city")) %>% 
     select(c(place_name,
              state_code_fips,
              place_code_fips)) %>% 
     mutate(state = case_when(
          state_code_fips == "06" ~ "CA",
          TRUE ~ "OR"
     )) %>% 
     mutate(full_address = str_glue("{place_name} {state}"))

communities <- read_excel("data/data.xlsx",
                          sheet = "Community details") %>% 
     clean_names() %>% 
     select(c(community, state, longitude, latitude)) %>% 
     rename(name = community) %>% 
     mutate(name = str_replace(name, "Mt.", "Mount")) %>% 
     mutate(full_address = str_glue("{name} {state}"))
     # left_join(geocodes, by = "full_address") 
     # select(c(name.x, state.x, full_address, geoid, longitude, latitude)) %>% 
     # set_names(c("name", "state", "full_address", "geoid", "lon", "lat"))


write_csv(communities, "temp/communities.csv",
          na = "")



# Core county -------------------------------------------------------------

core_county <- read_excel("data/data.xlsx",
                          sheet = "Community details") %>% 
     clean_names() %>% 
     select(c(community, state, core_co, longitude, latitude)) %>% 
     rename(name = community) %>% 
     mutate(core = case_when(
          core_co == 0 ~ "Not core county",
          TRUE ~ "Core county"
     ))

# Population size ---------------------------------------------------------

pop_size <- get_acs(geography = "place", 
                    variables = c(medincome = "B01003_001"), 
                    state = c("OR", "CA"),
                    cache = T) %>% 
     clean_names() %>% 
     mutate(state = case_when(
          str_detect(name, "California") ~ "CA",
          TRUE ~ "OR"
     )) %>% 
     clean_community_name() %>% 
     mutate(full_address = str_glue("{name} {state}")) %>% 
     # Need to fix because only getting 52 communities so some matching is off
     inner_join(communities, by = "full_address") %>% 
     mutate(moe_pct = moe/estimate) 



# Median income -----------------------------------------------------------

median_income <- get_acs(geography = "place", 
                         variables = c(medincome = "B19013_001"), 
                         state = c("OR", "CA"),
                         cache = T) %>% 
     clean_names() %>% 
     mutate(state = case_when(
          str_detect(name, "California") ~ "CA",
          TRUE ~ "OR"
     )) %>% 
     clean_community_name() %>% 
     mutate(full_address = str_glue("{name} {state}")) %>% 
     # Need to fix because only getting 52 communities so some matching is off
     inner_join(communities, by = "full_address") %>% 
     mutate(moe_pct = moe/estimate) 




# Median home value -------------------------------------------------------


median_home_value <- get_acs(geography = "place", 
                      variables = c(medincome = "B25077_001"), 
                      state = c("OR", "CA"),
                      cache = T) %>% 
     clean_names() %>% 
     mutate(state = case_when(
          str_detect(name, "California") ~ "CA",
          TRUE ~ "OR"
     )) %>% 
     clean_community_name() %>% 
     mutate(full_address = str_glue("{name} {state}")) %>% 
     # Need to fix because only getting 52 communities so some matching is off
     inner_join(communities, by = "full_address") %>% 
     mutate(moe_pct = moe/estimate) 




# FAR -----------------------------------------

# Get FAR data

# download.file("https://www.ers.usda.gov/webdocs/DataFiles/51020/FARcodesZIPdata2010WithAKandHI.xlsx?v=0",
#               destfile = "data/far.xlsx")

far_zips <- read_excel("data/far.xlsx",
                  sheet = "FAR ZIP Code Data") %>% 
     clean_names() %>% 
     filter(state %in% c("CA", "OR")) %>% 
     select(name, state, zip, far1:far4) %>% 
     mutate(far_level = far1 + far2 + far3 + far4) %>% 
     select(-(far1:far4))

# Get zipcode data

data(zipcode)

us_zips <- zipcode %>% 
     mutate(full_address = str_glue("{city} {state}"))

far <- communities %>% 
     left_join(us_zips, by = "full_address") %>% 
     select(name, state.x, longitude.x, latitude.x, zip) %>% 
     set_names(c("name", "state", "lon", "lat", "zip")) %>% 
     left_join(far_zips, by = "zip") %>% 
     select(-c(name.y:state.y)) %>% 
     set_names(c("name", "state", "lon", "lat", "zip", "far_level")) %>% 
     group_by(name) %>% 
     top_n(1)


# Percent of Native Am only --------------------------

native_americans <- get_acs(geography = "place", 
                             variables = c(medincome = "B02001_004"), 
                             state = c("OR", "CA"),
                             cache = T) %>% 
     clean_names() %>% 
     mutate(state = case_when(
          str_detect(name, "California") ~ "CA",
          TRUE ~ "OR"
     )) %>% 
     clean_community_name() %>% 
     mutate(full_address = str_glue("{name} {state}")) %>% 
     # Need to fix because only getting 52 communities so some matching is off
     inner_join(communities, by = "full_address") %>% 
     mutate(moe_pct = moe/estimate) %>% 
     left_join(pop_size, by = "geoid") %>% 
     mutate(pct = estimate.x / estimate.y)

skim(native_americans$pct)

# Percent of Latinx ------------------------------------------------------

latinx <- get_acs(geography = "place", 
                            variables = c(medincome = "B01001I_001"), 
                            state = c("OR", "CA"),
                            cache = T) %>% 
     clean_names() %>% 
     mutate(state = case_when(
          str_detect(name, "California") ~ "CA",
          TRUE ~ "OR"
     )) %>% 
     clean_community_name() %>% 
     mutate(full_address = str_glue("{name} {state}")) %>% 
     # Need to fix because only getting 52 communities so some matching is off
     inner_join(communities, by = "full_address") %>% 
     mutate(moe_pct = moe/estimate) %>% 
     left_join(pop_size, by = "geoid") %>% 
     mutate(pct = estimate.x / estimate.y)
