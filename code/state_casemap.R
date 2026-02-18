mpb1 <- sf::read_sf("data/ADS_data/shapefiles/mpb_allyears_fp.shp")
mpb2 <- sf::read_sf("data/ADS_data/shapefiles/MPB2_malh_fp.shp")
mpb3 <- sf::read_sf("data/ADS_data/shapefiles/crla_mpb_fp.shp")

psme <- sf::read_sf("data/ADS_data/shapefiles/psme_fp.shp")

ocho <- sf::read_sf("data/ADS_data/shapefiles/ocho_fir2022.shp")

or <- sf::read_sf("/Users/DanielPerret/Box/01. daniel.perret Workspace/base_spatialdata/state_boundaries/state_boundaries.shp") %>% filter(STATE=="OR")


# ext <- sf::st_bbox(or %>% sf::st_as_sf() %>%  sf::st_transform(crs=CRS(old.proj)))
# names(ext) <- c("left","bottom","right","top")
# or.map <- get_stadiamap(bbox = ext, maptype = "stamen_terrain_labels",zoom=6)
# 
# ggmap(or.map) +
#   # geom_sf(data = or,
#   #         fill = "gray95") +
#   geom_sf(data = mpb1,
#           aes(fill = "MPB"),
#           inherit.aes=F) +
#   geom_sf(data = mpb2,
#           aes(fill = "MPB"),
#           inherit.aes=F) +
#   geom_sf(data = mpb3,
#           aes(fill = "MPB"),
#           inherit.aes=F) +
#   geom_sf(data = psme,
#           aes(fill = "FFB (PSME)"),
#           inherit.aes=F) + 
#   geom_sf(data = ocho,
#           aes(fill = "FE (Abies sp.)"),
#           inherit.aes=F) +
#   scale_color_manual(name = "Case study",
#                      values = c("MPB" = "gold2",
#                                 "FFB (PSME)" = "dodgerblue3",
#                                 "FE (Abies sp.)" = "firebrick3"),
#                      aesthetics = "fill")


ggplot() +
  geom_sf(data = or,
          fill = "gray95") +
  geom_sf(data = mpb1,
          aes(fill = "MPB"),
          inherit.aes=F) +
  geom_sf(data = mpb2,
          aes(fill = "MPB"),
          inherit.aes=F) +
  geom_sf(data = mpb3,
          aes(fill = "MPB"),
          inherit.aes=F) +
  geom_sf(data = psme,
          aes(fill = "FFB (PSME)"),
          inherit.aes=F) +
  geom_sf(data = ocho,
          aes(fill = "FE (Abies sp.)"),
          inherit.aes=F) +
  scale_color_manual(name = "Case study",
                     values = c("MPB" = "gold2",
                                "FFB (PSME)" = "dodgerblue3",
                                "FE (Abies sp.)" = "firebrick3"),
                     aesthetics = "fill")

