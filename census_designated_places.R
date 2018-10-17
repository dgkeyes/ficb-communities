# Was just trying to figure out why not all places are showing up

library(tigris)
library(tidyverse)
library(sf)

tfff_cdps <- places(state = c("OR", "CA"),
                    cache = T) %>% 
     st_as_sf()

ggplot(tfff_cdps) +
     geom_sf(fill = "white") +
     coord_sf(datum = NA) +
     theme(axis.title = element_blank())
