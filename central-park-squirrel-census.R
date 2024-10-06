library(tidyverse)
library(janitor)
library(osmdata)
library(showtext)
library(sf)

font_add_google("Fira Code", "font")
showtext_auto()

brown <- "#AB6F4C"
gray <- "#7B888E"
black <- "#2F1F1C"

city_bb <- c(-73.9902, 40.7633, -73.9482, 40.8021)

city_roads <- city_bb %>%
  opq() %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

city_roads_lines <- city_roads$osm_lines %>%
  st_transform(4326)

city_water <- city_bb %>%
  opq() %>%
  add_osm_feature(key = "water") %>%
  osmdata_sf()

city_water_polygons <- city_water$osm_polygons %>%
  st_transform(4326)

squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv') %>%
  clean_names() %>%
  mutate(date = mdy(date)) %>%
  filter(!is.na(primary_fur_color))

ggplot() +
  geom_sf(data = city_water_polygons, fill = "#7fc0ff") +
  geom_sf(data = city_roads_lines, colour = "#6a5a00", linewidth = 0.2, alpha = 0.2) +
  geom_point(data = squirrel_data, aes(x = x, y = y, colour = primary_fur_color), size = 0.7) +
  scale_colour_manual(values = c(black, brown, gray)) +
  guides(colour = guide_legend(title = NULL, keywidth = 1, keyheight = 1, override.aes = list(size = 5))) +
  labs(caption = "NYC CENTRAL PARK SQUIRREL CENSUS") +
  coord_sf(xlim = c(-73.95, -73.982), ylim = c(40.765, 40.801), expand = FALSE, datum = NA) +
  theme_void() +
  theme(
    text = element_text(size = 10),
    plot.title = element_text(colour = "black", size = 16, hjust = 0.5),
    plot.caption = element_text(colour = "black", size = 12, hjust = 0.5),
    legend.position = c(0.85, 0.1),
    legend.background = element_rect(fill = "#f5f2f2", colour = "#f5f2f2"),
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.margin = margin(3, 3, 3, 3),
    plot.margin = margin(20, 20, 20, 20),
    panel.background = element_rect(fill = "#f5f2f2", colour = NA)
  )

