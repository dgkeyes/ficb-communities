
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
library(hrbrmisc)
library(tinter)
library(scales)
library(ggchicklet)

# Get Data ------------------------------------------------------------------

# gs_title("FICB map data") %>%
#      gs_download(to = "data/ficb-map-data.xlsx",
#                  overwrite = T)

ficb_communities <- read_excel("data/ficb-map-data.xlsx") %>% 
        clean_names() %>% 
        mutate(ficb_community = "Yes")




# Population --------------------------------------------------------------

population <- tfff_oregon_siskiyou_communities %>% 
        left_join(ficb_communities, by = c("name" = "community")) %>% 
        select(-c(state.y:x4c_focus)) %>% 
        rename("state" = "state.x") %>% 
        filter(census_count <= 35000) %>% 
        drop_na(name) %>% 
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
             Like <span style = color:#B5CC8E>All Communities</span> 
             <br>in Oregon and Siskiyou County, Have Very Small Populations") +
        theme(
                plot.title = element_markdown(lineheight = 1.1,
                                              color = tfff_colors("Medium Gray")),
                plot.subtitle = element_markdown(lineheight = 1.1)
        )



# Population Categorical --------------------------------------------------


population_categorical <- population %>% 
        mutate(pop_cat = case_when(
                census_count > 20000 ~ "20,000+",
                census_count > 10000 ~ "10,000-20,000",
                census_count > 5000 ~ "5,000-10,000",
                census_count > 2500 ~ "2,500-5,000",
                census_count > 1000 ~ "1,000-2,500",
                census_count > 500 ~ "500-1,000",
                TRUE ~ "<500"
        )) %>% 
        count(pop_cat, ficb_community) %>% 
        mutate(pop_cat = factor(pop_cat, levels = c("<500",
                                                    "500-1,000",
                                                    "1,000-2,500",
                                                    "2,500-5,000",
                                                    "5,000-10,000",
                                                    "10,000-20,000",
                                                    "20,000+"))) %>% 
        group_by(ficb_community) %>% 
        mutate(pct = prop.table(n))


# Dodged bars

population_categorical %>% 
        ggplot(aes(pop_cat, pct,
                   fill = ficb_community)) +
        geom_col(position = position_dodge()) +
        geom_text(aes(label = percent(pct, 1)),
                  position = position_dodge(width = 1),
                  hjust = -.35) +
        coord_flip() +
        theme_ipsum(base_family = "Calibri",
                    grid = FALSE,
                    axis = FALSE,
                    axis_title_size = 0) +
        scale_y_continuous(limit = c(0, .37)) +
        scale_fill_manual(values = c(tfff_colors("Light Green"), tfff_colors("Dark Green"))) +
        labs(y = "Number of Communities",
             x = "Population",
             title = "Population of <span style = color:#265142>FICB Communities</span> and  
             <span style = color:#B5CC8E>All Oregon Communities Under 35,000</span>") +
        theme(axis.text.x = element_blank(),
              legend.position = "none",
              plot.title = element_markdown(lineheight = 1.1,
                                            color = tfff_colors("Medium Gray")),
              plot.subtitle = element_markdown(lineheight = 1.1))


# Stacked bars

tfff_continuous_green_palette <- tinter(tfff_colors("Dark Green"), 
                                        steps = 7,
                                        direction = "both")

population_categorical_labels <- population_categorical %>% 
        filter(ficb_community == "Yes")

population_categorical %>% 
        ungroup() %>% 
        mutate(pop_cat = fct_rev(pop_cat)) %>% 
        mutate(ficb_community = case_when(
                ficb_community == "Yes" ~ "FICB Communities",
                ficb_community == "No" ~ "All Communities Under 35,000 Population"
        )) %>% 
        mutate(ficb_community = str_wrap(ficb_community, 25)) %>% 
        ggplot(aes(1, pct,
                   fill = pop_cat)) +
        geom_col() +
        # geom_text(data = population_categorical_labels,
        #           aes(3, pct,
        #               label = pop_cat)) +
        facet_wrap(~ficb_community, 
                   nrow = 1) +
        geom_text(aes(label = percent(pct, 1)),
                  position = position_stack(vjust = 0.5)) +
        # coord_flip() +
        theme_ipsum(base_family = "Calibri",
                    grid = FALSE,
                    axis = FALSE,
                    axis_text_size = 0,
                    axis_title_size = 0) +
        # guides(fill = guide_legend(reverse = TRUE,
        #                            ncol = 1)) +
        labs(fill = "Population") +
        theme(axis.text.x = element_blank(),
              # legend.position = "bottom",
              plot.title = element_markdown(lineheight = 1.1,
                                            color = tfff_colors("Medium Gray")),
              plot.subtitle = element_markdown(lineheight = 1.1))

# Bullet chart


ggplot() +
        geom_col(data = filter(population_categorical, ficb_community == "No"),
                 aes(pop_cat, pct),
                 width = 0.75,
                 # alpha = 0.5,
                 fill = tfff_colors("Light Green")) +
        geom_col(data = filter(population_categorical, ficb_community == "Yes"),
                 aes(pop_cat, pct),
                 width = 0.5,
                 fill = tfff_colors("Dark Green")) +
        geom_text(data = filter(population_categorical, ficb_community == "Yes"),
                  aes(pop_cat, pct,
                      label = percent(pct, 1)),
                  family = "Calibri",
                  hjust = 1.25,
                  color = "white") +
        coord_flip() +
        theme_ipsum(base_family = "Calibri",
                    # grid = FALSE,
                    axis = FALSE,
                    axis_title_size = 0) +
        scale_y_continuous(labels = percent_format(1)) +
        scale_fill_manual(values = c(tfff_colors("Light Green"), tfff_colors("Dark Green"))) +
        labs(y = "Number of Communities",
             x = "Population",
             title = "Population of <span style = color:#265142>FICB Communities</span> and  
             <span style = color:#B5CC8E>All Oregon Communities Under 35,000</span>") +
        theme(
                plot.title = element_markdown(lineheight = 1.1,
                                              color = tfff_colors("Medium Gray")),
                plot.subtitle = element_markdown(lineheight = 1.1))




# Median Household Income -------------------------------------------------

median_income <- get_acs(geography = "place", 
                         variables = c(medincome = "B19013_001"), 
                         state = c("OR", "CA"),
                         cache = TRUE) %>% 
        clean_names() %>% 
        mutate(state = case_when(
                str_detect(name, "California") ~ "CA",
                TRUE ~ "OR"
        )) %>% 
        clean_community_name() %>% 
        mutate(full_address = str_glue("{name} {state}"))


# Need to fix because only getting 52 communities so some matching is off
inner_join(communities, by = "full_address") %>% 
        mutate(moe_pct = moe/estimate) %>% 
        mutate(state = case_when(
                str_detect(state.x, "CA") ~ "California",
                TRUE ~ "Oregon"
        )) 



# Frontier and Remote -----------------------------------------------------

far_zips <- tfff_far_zips

oregon_siskiyou_communities_far <- tfff_oregon_siskiyou_communities %>% 
        left_join(far_zips, by = "name") %>% 
        rename("state" = "state.x") %>% 
        select(-state.y) %>% 
        replace_na(list(far_level = 0)) %>% 
        left_join(ficb_communities, c("name" = "community")) %>% 
        replace_na(list(ficb_community = "No"))

far_levels_summary <- oregon_siskiyou_communities_far %>% 
        count(far_level, ficb_community) %>% 
        group_by(ficb_community) %>% 
        mutate(pct = prop.table(n)) %>% 
        mutate(far_level_factor = factor(far_level)) %>% 
        mutate(far_level_factor = fct_rev(far_level_factor)) %>% 
        mutate(ficb_community_description = case_when(
                ficb_community == "Yes" ~ "FICB Communities",
                ficb_community == "No" ~ "All Communities"
        ))

tfff_continuous_palette <- tinter(tfff_colors("Dark Green"),
                                  steps = 5,
                                  crop = 3)

# Stacked Bar

far_levels_summary %>% 
        ggplot(aes(1, pct,
                   fill = far_level_factor)) +
        geom_col() +
        geom_text(aes(label = percent(pct, 1)),
                  position = position_stack(vjust = 0.5),
                  color = c("black", rep("white", 4),
                            "black", rep("white", 4))) +
        coord_flip() +
        facet_wrap(~ficb_community_description,
                   ncol = 1) +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_fill_manual(values = rev(c(tfff_colors("Light Gray"),
                                         tfff_colors("Orange"),
                                         tfff_colors("Yellow"),
                                         tfff_colors("Light Green"),
                                         tfff_colors("Dark Green")))) +
        theme_ipsum(base_family = "Calibri",
                    grid = FALSE,
                    axis = FALSE,
                    axis_text_size = 0,
                    axis_title_size = 0) +
        theme(legend.position = "bottom") +
        labs(fill = "FAR Level")


# Dodged Bar

far_levels_summary %>% 
        filter(far_level_factor != "0") %>% 
        ggplot(aes(far_level_factor, pct,
                   fill = ficb_community,
                   color = ficb_community)) +
        geom_col() +
        geom_text(aes(label = percent(pct, 1)),
                  hjust = -.35) +
        coord_flip() +
        facet_wrap(~ficb_community_description,
                   ncol = 1) +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_fill_manual(values = c(tfff_colors("Dark Green"),
                                     tfff_colors("Light Green"))) +
        scale_color_manual(values = c(tfff_colors("Dark Green"),
                                     tfff_colors("Light Green"))) +
        theme_ipsum(base_family = "Calibri",
                    grid = FALSE,
                    axis_title_size = 0) +
        theme(legend.position = "none",
              axis.text.x = element_blank()) +
        labs(x = "FAR Level",
             y = NULL) +
        scale_y_continuous(labels = percent_format(1),
                           limits = c(0, .15))
