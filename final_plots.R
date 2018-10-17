
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





# Pop size ----------------------------------------------------------------

ggplot(pop_size_cats, aes(1, pct,
                          fill = pop_cat)) +
     geom_col(color = "white") +
     theme_void() +
     coord_flip() +
     theme(legend.position = "none",
           plot.title = element_text(hjust = 0.5,
                                     face = "bold")) 

ggplot(pop_size_cats, aes(pop_cat, n,
                          alpha = n)) +
     geom_col(fill = tfff.blue) +
     geom_text(aes(label = n),
               hjust = -1,
               color = tfff.blue,
               alpha = 1) +
     coord_flip() +
     scale_y_continuous(limits = c(0, 15)) +
     theme(axis.title = element_blank(),
           legend.position = "none",
           axis.text.x = element_blank(),
           panel.grid = element_blank())

ggsave("plots/pop-size.pdf",
       height = 3,
       width = 4)

mean(pop_size$estimate)

# Median income -----------------------------------------------------------


ggplot(median_income, aes(1, estimate)) +
     geom_hline(yintercept = median_income_comparisons$estimate,
                alpha = 1,
                size = 1,
                color = tfff.yellow,
                linetype = "dashed") +
     geom_jitter(alpha = 0.85,
                 color = tfff.dark.green,
                 size = 3) +
     scale_y_continuous(labels = dollar_format(),
                        limits = c(0, 75000),
                        breaks = seq(0, 75000, 25000)) +
     coord_cartesian(clip = "off") +
     theme(axis.text.x = element_blank(),
           axis.text.y = element_text(color = tfff.medium.gray),
           axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           legend.position = "bottom",
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.minor.y = element_blank())


ggsave("plots/median-income.pdf",
       height = 3,
       width = 3)

# Median home value -------------------------------------------------------


ggplot(median_home_value, aes(estimate)) +
     geom_histogram(fill = tfff.light.green) +
     geom_vline(xintercept = median_home_value_oregon,
                alpha = 1,
                color = tfff.yellow,
                linetype = "dashed",
                size = 1) +
     scale_x_continuous(labels = dollar_format(),
                        limits = c(0, 350000)) +
     scale_y_continuous(limits = c(0, 12),
                        breaks = pretty_breaks(n = 5)) +
     theme(panel.grid.minor.x = element_blank(),
           panel.grid.minor.y = element_blank(),
           axis.title = element_blank())

ggsave("plots/median-home-value.pdf",
       height = 3,
       width = 4)




# Communities -------------------------------------------------------------

library(ggthemes)

dk_or_ca_map() +
     geom_point(data = communities, 
                aes(longitude, latitude),
                alpha = 0.8,
                # shape = 21,
                fill = "white",
                color = "black") +
     theme_map()

ggsave("plots/communities.pdf",
       height = 3,
       width = 4)

# Native Americans ------------------------------------------------------------------



dk_or_ca_map() +
     geom_point(data = filter(native_am_latinx, group == "Native American"), aes(longitude.x, latitude.x,
                                                                                 size = pct,
                                                                                 color = pct),
                alpha = 0.8) +
     labs(color = "") +
     scale_size(guide = "none") +
     scale_color_gradientn(colors = c("#FADFD0", tfff.orange),
                           labels = percent_format(accuracy = 1),
                           breaks = seq(0, 1, by = .25)) 

ggsave("plots/native-am.pdf",
       height = 3,
       width = 4)

# Latinx ------------------------------------------------------------------



dk_or_ca_map() +
     geom_point(data = filter(native_am_latinx, group == "Latinx"), aes(longitude.x, latitude.x,
                                                                        size = pct,
                                                                        color = pct),
                alpha = 0.8) +
     labs(color = "") +
     scale_size(guide = "none") +
     scale_color_gradientn(colors = c("#F1D5D5", tfff.red),
                           labels = percent_format(accuracy = 1),
                           breaks = seq(0, 1, by = .25))

ggsave("plots/latinx.pdf",
       height = 3,
       width = 4)


# FAR ---------------------------------------------------------------------



# ggplot(far_categ, aes(far_level_categ, n,
#                           alpha = far_level_categ)) +
#      geom_col(fill = tfff.brown) +
#      geom_text(aes(label = n),
#                hjust = -1,
#                color = tfff.blue,
#                alpha = 1) +
#      coord_flip() +
#      # scale_y_continuous(limits = c(0, 15)) +
#      theme(axis.title = element_blank(),
#            legend.position = "none",
#            axis.text.x = element_blank(),
#            panel.grid = element_blank())

dk_or_ca_map() +
     geom_jitter(data = far, aes(lon, lat,
                                 size = far_level_categ,
                                 color = far_level_categ),
                 alpha = 0.8) +
     labs(color = "") +
     geom_text(data = far, aes(lon, lat, label = far_level_categ),
               color = "white") +
     scale_size(guide = "none",
                range = c(5, 8)) +
     scale_color_gradientn(colors = c("#BFB0AC", tfff.brown),
                           guide = "none")

ggsave("plots/far.pdf",
       height = 3,
       width = 4)

