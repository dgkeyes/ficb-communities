library(tidyverse)
library(janitor)
library(readxl)
library(scales)
library(formattable)

source("get_data.R")



# Themes ------------------------------------------------------------------


tfff.dark.green <- "#265142"
tfff.light.green <- "#B5CC8E"
tfff.orange <- "#e65100"
tfff.yellow <- "#FBC02D"
tfff.blue <- "#283593"
tfff.red <- "#B71C1C"
tfff.brown <- "#51261C"
tfff.dark.gray <- "#545454"
tfff.medium.gray <- "#a8a8a8"
tfff.light.gray <- "#eeeeee"

# Table -------------------------------------------------------------------



final_table <- read_excel("data/data.xlsx",
                          sheet = "Community details") %>% 
     clean_names() %>% 
     select(community, region) %>% 
     rename("name" = "community") %>% 
     # Add core county
     left_join(core_county, by = "name") %>% 
     select(name, region, core) %>% 
     mutate(name = str_replace(name, "Mt.", "Mount")) %>% 
     mutate(core = case_when(
            name == "Mount Shasta" ~ "Yes",
            TRUE ~ core
            )) %>% 
     # Add incorporation status 
     left_join(incorporation_status, by = "name") %>% 
     select(name, region, core, incorporation_status_dichotomous) %>% 
     # Add population size
     left_join(pop_size, by = c("name" = "name.x")) %>% 
     select(name, region, core, incorporation_status_dichotomous, estimate) %>% 
     rename("population" = "estimate") %>% 
     # Add median income
     left_join(median_income, by = c("name" = "name.x")) %>% 
     select(name, region, core, incorporation_status_dichotomous, population, estimate) %>% 
     rename("median_income" = "estimate") %>% 
     # Add median home value
     left_join(median_home_value, by = c("name" = "name.x")) %>% 
     select(name, region, core, incorporation_status_dichotomous, population, median_income, estimate) %>% 
     rename("median_home_value" = "estimate") %>% 
     # FAR
     left_join(far, by = "name") %>% 
     # Native American 
     left_join(native_americans, by = c("name" = "name.x.x")) %>% 
     select(name:median_home_value, pct, far_level) %>% 
     rename("native_american_pct" = "pct") %>% 
     # Latinx
     left_join(latinx, by = c("name" = "name.x.x")) %>% 
     select(name:native_american_pct, pct, far_level) %>% 
     rename("latinx_pct" = "pct")


# Formatted ---------------------------------------------------------------

final_table_formatted <- final_table %>% 
     mutate(core = case_when(
          core == "Yes" ~ "Y",
          TRUE ~ ""
     )) %>% 
     mutate(incorporation_status_dichotomous = case_when(
          incorporation_status_dichotomous == "Yes" ~ "Y",
          TRUE ~ ""
     )) %>% 
     mutate(population = comma(population, 0)) %>% 
     mutate(median_income = dollar(median_income, 1)) %>% 
     mutate(median_home_value = dollar(median_home_value, 1)) %>% 
     mutate(latinx_pct = percent(latinx_pct, .1)) %>% 
     mutate(native_american_pct = percent(native_american_pct, .1)) %>% 
     mutate(far_level = replace_na(far_level, 0)) %>% 
     # mutate(population = replace_na(population, "")) %>% 
     # mutate(median_income = replace_na(median_income, "")) %>%
     # mutate(median_home_value = replace_na(median_home_value, "")) %>%
     # mutate(latinx_pct = replace_na(latinx_pct, "")) %>%
     # mutate(native_american_pct = replace_na(native_american_pct, "")) %>%
     na_if("$NA") %>% 
     arrange(region, name) %>% 
     select(region, 
            name, 
            core, 
            incorporation_status_dichotomous, 
            population,
            median_income, 
            median_home_value,
            far_level, 
            latinx_pct,
            native_american_pct) %>% 
     set_names(c("Region", 
                 "Community", 
                 "Core County", 
                 "Incorporated",
                 "Population", 
                 "Median Income", 
                 "Median Home Value",
                 "FAR Level",
                 "Latinx Percent",
                 "Native American Percent")) 


write_csv(final_table_formatted, 
          "table/table.csv")



# Make table --------------------------------------------------------------

formattable(final_table_formatted, 
            align = "l",
            list(far_level = color_tile(tfff.light.green, tfff.dark.green),
                 median_income = color_tile(tfff.light.green, tfff.dark.green),
                 native_american_pct = color_tile(tfff.light.green, tfff.dark.green)))

