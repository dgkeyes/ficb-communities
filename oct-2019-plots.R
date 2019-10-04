
# Packages ----------------------------------------------------------------

library(tidyverse)
library(tfff)
library(googlesheets)
library(janitor)
library(gghighlight)
library(hrbrthemes)
library(ggbeeswarm)
library(ggtext)
library(readxl)


# Get Data ------------------------------------------------------------------

# gs_title("FICB map data") %>%
#      gs_download(to = "data/ficb-map-data.xlsx",
#                  overwrite = T)

ficb_communities <- read_excel("data/ficb-map-data.xlsx") %>% 
        clean_names() %>% 
        mutate(ficb_community = "Yes")




# Population --------------------------------------------------------------

population <- tfff_oregon_siskiyou_communities %>% 
        left_join(ficb_communities, by = c("city" = "community")) %>% 
        select(-c(state.y:x4c_focus)) %>% 
        rename("state" = "state.x") %>% 
        drop_na(city) %>% 
        mutate(ficb_community = fct_explicit_na(ficb_community, 
                                                na_level = "No")) %>% 
        mutate(ficb_community = fct_rev(ficb_community))

population %>% 
        ggplot(aes(census_count, 
                   fill = ficb_community)) +
        geom_histogram(bins = 50) +
        gghighlight() +
        tfff_theme() +
        scale_x_comma() +
        scale_fill_manual(values = c(tfff_colors("Light Green"), tfff_colors("Dark Green"))) +
        # geom_curve(x = 100000, xend = 10000,
        #            y = 50, yend = 40,
        #            curvature = -0.1,
        #            color = tfff_colors("Dark Green")) +
        labs(y = "Number of Communities",
             x = "Population",
             title = "The Vast Majority of <span style = color:#265142>FICB Communities</span>, 
             Like <span style = color:#B5CC8E>All Communities</span> in Oregon and Siskiyou County,
             <br>Have Very Small Populations") +
        theme(
                plot.title = element_markdown(lineheight = 1.1,
                                              color = tfff_colors("Medium Gray")),
                plot.subtitle = element_markdown(lineheight = 1.1)
        )

ggsave("sample.png")

# Median Household Income -------------------------------------------------



# Frontier and Remote -----------------------------------------------------

far_zips <- tfff_far_zips

communities_geocoded <- read_csv("data/communities.csv") %>% 
        left_join(far_zips, by = "name")

