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



# Get data ----------------------------------------------------------------

# gs_title("FICB map data") %>%
#      gs_download(to = "data/data.xlsx",
#                  overwrite = T)

# Communities -------------------------------------------------------------



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



# Communities not in ACS --------------------------------------------------

communities_not_in_acs <- get_acs(geography = "place", 
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
     right_join(communities, by = "full_address") %>% 
     filter(is.na(name.x)) %>% 
     pull(name.y)


# Core county -------------------------------------------------------------

core_county <- read_excel("data/data.xlsx",
                          sheet = "Community details") %>% 
     clean_names() %>% 
     select(c(community, state, core_co, longitude, latitude)) %>% 
     rename(name = community) %>% 
     mutate(core = case_when(
          core_co == 0 ~ "No",
          TRUE ~ "Yes"
     ))

core_county_cats <- core_county %>% 
     group_by(core) %>% 
     count() %>% 
     ungroup()

# Incorporation status ----------------------------------------------------

incorporation_status <- read_excel("data/data.xlsx",
                                   sheet = "Community details") %>% 
     clean_names() %>% 
     select(c(community, state, incorporation_status, longitude, latitude)) %>% 
     rename(name = community) %>% 
     mutate(incorporation_status_dichotomous = case_when(
          str_detect(incorporation_status, "incorp") ~ "No",
          TRUE ~ "Yes"
     )) %>% 
     mutate(incorporation_status_dichotomous = fct_inorder(incorporation_status_dichotomous))

skim(incorporation_status$incorporation_status_dichotomous)

incorporation_status_cats <- incorporation_status %>% 
     group_by(incorporation_status_dichotomous) %>% 
     count() %>% 
     ungroup() 

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
     inner_join(communities, by = "full_address") %>% 
     mutate(moe_pct = moe/estimate) %>% 
     add_row(name.x = "Applegate",
             state.x = "OR",
             estimate = 1441) %>% 
     add_row(name.x = "Blue River",
             state.x = "OR",
             estimate = 986) %>% 
     add_row(name.x = "McKenzie Bridge",
             state.x = "OR",
             estimate = 493) %>% 
     add_row(name.x = "Vida",
             state.x = "OR",
             estimate = 1096) %>% 
     add_row(name.x = "Deadwood",
             state.x = "OR",
             estimate = 343) %>% 
     add_row(name.x = "Mapleton",
             state.x = "OR",
             estimate = 731) %>% 
     add_row(name.x = "Swisshome",
             state.x = "OR",
             estimate = 341) 

skim(pop_size$estimate)


pop_size_cats <- pop_size %>% 
     mutate(pop_cat = case_when(
          estimate > 20000 ~ "20,000+",
          estimate > 10000 ~ "10,000-20,000",
          estimate > 5000 ~ "5,000-10,000",
          estimate > 2500 ~ "2,500-5,000",
          estimate > 1000 ~ "1,000-2,500",
          estimate > 500 ~ "500-1,000",
          TRUE ~ "<500"
     )) %>% 
     group_by(pop_cat) %>% 
     count() %>% 
     ungroup() %>% 
     mutate(pop_cat = factor(pop_cat, levels = c("<500",
                                                 "500-1,000",
                                                 "1,000-2,500",
                                                 "2,500-5,000",
                                                 "5,000-10,000",
                                                 "10,000-20,000",
                                                 "20,000+"))) %>% 
     # mutate(pop_cat = fct_rev(pop_cat)) %>%
     mutate(pct = prop.table(n))

get_acs(geography = "place", 
        variables = c(medincome = "B01003_001"), 
        state = "OR",
        cache = T) %>% 
     arrange(-estimate) %>% 
     slice(1:5)

get_acs(geography = "place", 
        variables = c(medincome = "B01003_001"), 
        state = "OR",
        cache = T) %>% 
     arrange(estimate) %>% 
     slice(1:5)

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
     mutate(moe_pct = moe/estimate) %>% 
     mutate(state = case_when(
          str_detect(state.x, "CA") ~ "California",
          TRUE ~ "Oregon"
     )) 

median_income %>% 
     nrow()

# Between poverty line and Oregon median

median_income %>% 
     filter(estimate < median_income_comparisons$estimate[1]) %>% 
     filter(estimate > median_income_comparisons$estimate[2]) %>% 
     nrow()

# Below poverty line

median_income %>% 
     filter(estimate < median_income_comparisons$estimate[2]) %>% 
     nrow()

6/53
     
skim(median_income$estimate)

median_income_comparisons <- get_acs(geography = "state", 
                                     variables = c(medincome = "B19013_001"), 
                                     state = c("OR"),
                                     cache = T) %>% 
     clean_names() %>% 
     rename("state.x" = "name") %>% 
     mutate(state.x = str_glue("{state.x} Median Income")) %>% 
     select(c("state.x", "estimate")) %>% 
     # add_row(state.x = "United Way Survival Budget", 
     #           estimate = 62484) %>% 
     add_row(state.x = "Federal Poverty Line",
             estimate = 28290)


median_income_cats <- median_income %>%
     mutate(median_income_cat = case_when(
          estimate > 60000 ~ "$60,000+",
          estimate > 50000 ~ "$50,000-$60,000",
          estimate > 40000 ~ "$40,000-$50,000",
          estimate > 30000 ~ "$30,000-$40,000",
          estimate > 20000 ~ "$20,000-$30,000",
          TRUE ~ "<$20,000"
     )) %>%
     group_by(median_income_cat) %>%
     count() %>%
     ungroup() %>%
     mutate(median_income_cat = factor(median_income_cat, levels = c("$60,000+",
                                                                     "$50,000-$60,000",
                                                                     "$40,000-$50,000",
                                                                     "$30,000-$40,000",
                                                                     "$20,000-$30,000",
                                                                     "<$20,000"))) %>%
     mutate(pct = prop.table(n))

get_acs(geography = "place", 
        variables = c(medincome = "B19013_001"), 
        state = "OR",
        cache = T) %>% 
     arrange(-estimate) %>% 
     slice(1:5)

get_acs(geography = "place", 
        variables = c(medincome = "B19013_001"), 
        state = "OR",
        cache = T) %>% 
     arrange(estimate) %>% 
     slice(1:5)

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


median_home_value_oregon <- get_acs(geography = "state", 
                                    variables = c(medincome = "B25077_001"), 
                                    state = c("OR"),
                                    cache = T) %>% 
     clean_names() %>% 
     pull(estimate) 


median_home_value %>% 
     filter(estimate < median_home_value_oregon) %>% 
     nrow()

42/53

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

# us_zips <- zipcode %>% 
#      mutate(full_address = str_glue("{city} {state}"))

communities_zips <- read_excel("data/data.xlsx",
                               sheet = "Community details") %>% 
     clean_names() %>% 
     select(c(community, state, latitude, longitude, main_zip_code, additional_zip)) %>% 
     mutate(main_zip_code = as.character(main_zip_code)) %>% 
     mutate(additional_zip = na_if(additional_zip, "n/a")) %>% 
     mutate(additional_zip = str_remove(additional_zip, "\\.0")) %>% 
     mutate(additional_zip = as.character(additional_zip)) %>% 
     rename(name = community) %>% 
     mutate(name = str_replace(name, "Mt.", "Mount")) %>% 
     mutate(full_address = str_glue("{name} {state}")) %>% 
     gather(type, zip, -c(name, state, latitude, longitude, full_address)) %>% 
     select(-type) %>% 
     filter(!is.na(zip))

far <- communities_zips %>% 
     # left_join(us_zips, by = "full_address") %>% 
     # select(name, state.x, longitude.x, latitude.x, zip) %>% 
     # set_names(c("name", "state", "lon", "lat", "zip")) %>% 
     left_join(far_zips, by = "zip") %>% 
     select(-c(name.y:state.y)) %>% 
     set_names(c("name", "state", "lat", "lon", "full_address", "zip", "far_level")) %>% 
     group_by(full_address) %>% 
     add_count() %>%
     # arrange(full_address, n) %>% 
     slice(1) %>% 
     mutate(far_level = na_if(far_level, "0")) %>% 
     mutate(far_level_categ = as.character(far_level)) %>% 
     mutate(far_level_categ = as.integer(far_level_categ))



far_categ <- far %>% 
     group_by(far_level_categ) %>% 
     count() %>% 
     ungroup() %>% 
     mutate(far_level_categ = replace_na(far_level_categ, "0")) %>% 
     mutate(far_level_categ = as.numeric(far_level_categ)) %>% 
     rename("n" = "nn")


str(far_categ$far_level_categ)

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
     mutate(pct = estimate.x / estimate.y) %>% 
     select(name.x.x, longitude.x, latitude.x, pct) %>% 
     mutate(group = "Native American")



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
     mutate(pct = estimate.x / estimate.y) %>% 
     select(name.x.x, longitude.x, latitude.x, pct) %>% 
     mutate(group = "Latinx")



# Merge Native Am and Latinx ----------------------------------------------

native_am_latinx <- bind_rows(native_americans, latinx)
