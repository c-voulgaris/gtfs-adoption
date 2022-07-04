#### The purpose of this file is to visualize the assembled GTFS data.

# 1. Set up the environment
library(tidyverse)
library(ggplot2)
library(tigris)
library(sf)
library(here)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(cartogram)
library(ggthemes)

# 2. Visualisation 
## (1) To present agencies as points on map
agency_location <- here("assembled-data",
                        "agency-location.csv") %>%
  read_csv() %>%
  select(Company_Nm, agency, date, lon, lat) %>%
  mutate(adopted_year = as.numeric(str_sub(date, -2, -1)) + 2000) %>%
  filter(lon > -150 & lat < 60)

agency_pt <- st_as_sf(agency_location, coords = c("lon", "lat"), crs = 4326)

US_states <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name != "Alaska",
         name != "Hawaii")

ggplot() +
  geom_sf(data = US_states, fill = NA) +
  geom_sf(data = agency_pt,
          aes(color = adopted_year),
          size = 1) +
  theme_map()

## (2) 
### The percentage increase of GTFS adoption rate from 2005 to 2020
gtfs_data <- here("assembled-data",
                  "final-data.csv") %>%
  read_csv() %>%
  select(-X1)

ggplot(gtfs_data, 
       aes(x = year, y = `Percent adoption of GTFS data standard`)) +
  geom_smooth() +
  annotate("segment",
           x = 2008, xend = 2008,
           y = 0.085, yend = 0.135,
           color = "gray") +
  annotate("text", 
           x = 2008, 
           y = 0.15,
           label = "8.60%",
           size = 5) +
  annotate("segment",
           x = 2018, xend = 2018,
           y = 0.655, yend = 0.685,
           color = "gray") +
  annotate("text",
           x = 2018,
           y = 0.7,
           label = "65.29%",
           size = 5)

### The number increase of GTFS adoption agencies from 2005 to 2020 & The number change of total agencies from 2005 to 2020
ggplot(gtfs_data) +
  geom_point(aes(x = year, y = num_adopted)) +
  geom_point(aes(x = year, y = num_agencies)) 

## (3) Use Tigris package to get urbanized area shapefile. But it is too big and does not plot on my computer. 
options(tigris_class = "sf")
options(tigris_cache_dir = TRUE)

uza <- urban_areas()

uza_ie <- uza %>%
  filter(NAME10 == "Dixon, IL" | NAME10 == "Escanaba, MI")

ggplot(uza_ie) +
  geom_sf()
