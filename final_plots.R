
# Packages ----------------------------------------------------------------

library(tidyverse)
library(tidycensus)
library(googlesheets)
library(readxl)
library(janitor)
library(hrbrmisc)
library(hrbrthemes)
library(scales)
library(ggthemes)
library(patchwork)


# Source ------------------------------------------------------------------

source("get_data.R")
source("get_shapefiles.R")



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


dk_theme <- theme_minimal(base_size = 10) +
     theme(plot.title = element_text(face = "bold"),
           plot.caption = element_text(color = tfff.dark.gray),
           plot.margin = margin(.5, .5, .5, .5, "cm"),)

theme_set(dk_theme)


# Functions ---------------------------------------------------------------

dk_or_ca_map <- function() {
     ggplot(or_ca_map) +
          geom_sf(fill = "white") +
          coord_sf(datum = NA,
                   expand = F) +
          theme(axis.title = element_blank())
}



# Core county -------------------------------------------------------------


plot_core_county <- ggplot(core_county_cats, aes(x = "", y = n, fill = core)) +
     geom_bar(stat = "identity") +
     coord_polar("y") +
     geom_text(aes(label = str_glue("{core}\n{n}")), position = position_stack(vjust = 0.5),
               color = "white") +
     theme_void() +
     theme(legend.position = "none",
           plot.title = element_text(hjust = 0.5,
                                     face = "bold"),
           plot.margin=unit(c(.5, .5, .5, .5),"cm"),
           plot.caption = element_text(color = "#505050"),
           strip.text = element_text(face = "bold")) +
     scale_fill_manual(values = c(tfff.light.green,
                                  tfff.dark.green)) +
     labs(title = "Core County")


# Incorporation status -------------------------------------------------------------

# The labels are reverse below and I'm not quite sure why

plot_incorporation_status <- ggplot(incorporation_status_cats, aes(x = "", 
                                                                   y = n, 
                                                                   fill = incorporation_status_dichotomous)) +
     geom_bar(width = 1, stat = "identity") +
     coord_polar("y") +
     geom_text(aes(label = str_glue("{incorporation_status_dichotomous}\n{n}")), 
               position = position_stack(vjust = 0.5),
               color = "white") +
     theme_void() +
     theme(legend.position = "none",
           plot.title = element_text(hjust = 0.5,
                                     face = "bold"),
           plot.margin=unit(c(.5, .5, .5, .5),"cm"),
           plot.caption = element_text(color = "#505050"),
           strip.text = element_text(face = "bold")) +
     scale_fill_manual(values = rev(c(tfff.light.green,
                                      tfff.dark.green))) +
     labs(title = "Incorporated")



# Pop size ----------------------------------------------------------------


plot_pop_size <- ggplot(pop_size_cats, aes(1, pct,
                                           fill = pop_cat)) +
     geom_col(color = "white") +
     geom_text(aes(label = str_glue("{pop_cat} ({percent(pct, 0)})")),
               position = position_stack(vjust = .5),
               color = "white") +
     theme_void() +
     theme(legend.position = "none",
           plot.title = element_text(hjust = 0.5,
                                     face = "bold")) +
     scale_fill_manual(values = c(tfff.dark.green, tfff.light.green,
                                  tfff.dark.green, tfff.light.green,
                                  tfff.dark.green, tfff.light.green,
                                  tfff.dark.green)) +
     labs(title = "Population")



# Median income -----------------------------------------------------------


plot_median_income <- ggplot(median_income, aes(1, estimate)) +
     geom_jitter(alpha = 0.5,
                 color = tfff.dark.green,
                 size = 3) +
     scale_y_continuous(labels = dollar_format(),
                        limits = c(0, 75000),
                        breaks = seq(0, 75000, 25000)) +
     scale_x_continuous(limits = c(1, 1.65)) +
     coord_cartesian(clip = "off") +
     geom_text(data = median_income_comparisons,
               aes(1.5,
                   median_income_comparisons$estimate + 1700,
                   label = median_income_comparisons$state.x,
                   hjust = 0),
               color = c(tfff.orange, tfff.blue, tfff.red)) +
     geom_hline(yintercept = median_income_comparisons$estimate,
                alpha = 0.5,
                color = c(tfff.orange, tfff.blue, tfff.red),
                linetype = "dashed") +
     theme(axis.text.x = element_blank(),
           axis.text.y = element_text(color = tfff.medium.gray),
           axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           legend.position = "bottom",
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.minor.y = element_blank()) +
     labs(color = "",
          title = "Median Income",
          subtitle = "Each dot represents one community")



# Median home value -------------------------------------------------------


plot_median_home_value <- ggplot(median_home_value_cats, aes(1, pct,
                                                             fill = median_home_value_cat)) +
     geom_col(color = "white") +
     geom_text(aes(label = str_glue("{median_home_value_cat} ({percent(pct, 0)})")),
               position = position_stack(vjust = .5),
               color = "white") +
     theme_void() +
     theme(legend.position = "none",
           plot.title = element_text(hjust = 0.5,
                                     face = "bold")) +
     scale_fill_manual(values = c(tfff.dark.green, tfff.light.green,
                                  tfff.dark.green, tfff.light.green,
                                  tfff.dark.green)) +
     labs(title = "Median Home Value")


# Latinx + Native Am pops -------------------------------------------------

plot_latinx_native_am <- dk_or_ca_map() +
     geom_point(data = native_am_latinx, aes(longitude.x, latitude.x,
                                             size = pct,
                                             color = pct),
                alpha = 0.8) +
     labs(color = "") +
     scale_size(guide = "none") +
     scale_color_gradientn(colors = c(tfff.light.green, tfff.dark.green),
                           labels = percent_format(accuracy = 1),
                           breaks = seq(0, 1, by = .25)) +
     facet_wrap(~group, ncol = 2) +
     theme(plot.title = element_text(hjust = 0.5,
                                     face = "bold")) +
     labs(title = "Latinx and Native American Populations")




# FAR ---------------------------------------------------------------------

plot_far <- dk_or_ca_map() +
     geom_point(data = far, aes(lon, lat,
                                size = far_level,
                                color = far_level),
                alpha = 0.8) +
     labs(color = "") +
     geom_text(data = far, aes(lon, lat, label = far_level),
               color = "white") +
     scale_size(guide = "none",
                range = c(5, 8)) +
     scale_color_gradientn(colors = c(tfff.light.green, tfff.dark.green),
                           guide = "none") +
     labs(title = "Frontier and Remote Level") +
     theme(plot.title = element_text(hjust = 0.5))



# Put it all together -----------------------------------------------------

plots_pies <- plot_core_county + plot_incorporation_status + plot_layout(ncol = 1)

plots_stacked_bars <- (plot_pop_size | plot_median_home_value)

plots_top_left <- (plots_pies | plots_stacked_bars) / plot_median_income

plots_maps <- plot_far  / plot_latinx_native_am

plot_final <- (plots_top_left | plots_maps)

ggsave("plots/ficb_communities.pdf", 
       plot_final,
       width = 16,
       height = 10)

