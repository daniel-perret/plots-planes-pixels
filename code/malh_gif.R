#d0a45b35-73bc-47d3-a8c7-8a0be7f39dc6
library(ggmap)
ext <- sf::st_bbox(malh.fp %>% sf::st_as_sf() %>%  sf::st_transform(crs=CRS(old.proj)))
names(ext) <- c("left","bottom","right","top")

malh.all <- sf::read_sf("data/ADS_data/shapefiles/MPB2_malh_2011_2019.shp")

register_stadiamaps("d0a45b35-73bc-47d3-a8c7-8a0be7f39dc6")
malh.map <- get_stadiamap(bbox = ext, mapytpe = "stamen_terrain",)


for(i in 1:length(names(malh.pred))){
  
  year <- substr(names(malh.pred)[i],start = 2,stop = 5)
  
  p <- ggmap(malh.map) +
    geom_sf(data = malh.fp %>% 
              sf::st_as_sf() %>%  
              sf::st_transform(crs=CRS(old.proj)),
            inherit.aes = F,
            fill=NA,
            col = "black",
            lwd=1.5) +
    geom_raster(data = malh.pred[[i]] %>%
                  raster::projectRaster(.,crs=CRS(old.proj)) %>% 
                  as.data.frame(.,xy=T) %>% 
                  rename(val = 3),
                aes(x = x, y = y, fill = val)) +
    scale_fill_gradient2(low = "indianred2",mid="white",high = "dodgerblue2",
                         midpoint=0,
                         aesthetics = "fill", 
                         guide = "legend", 
                         limits = c(-20,20),
                         n.breaks = 9,
                         na.value="transparent",
                         name="BAH\n(m2/ha)") +
    labs(x = "Longitude", y = "Latitude",title = year) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  
  
  ggsave(filename = paste0("figures/malh_bah_gif/colorblind/",year,".jpg"), 
         plot = p,
         width = 12,
         height = 9,
         units = "in",
         dpi = 150)
  
}




for(i in 1:length(names(malh.pred))){
  
  year <- substr(names(malh.pred)[i],start = 2,stop = 5)
  
  p <- ggmap(malh.map) +
    geom_sf(data = malh.fp %>% 
              sf::st_as_sf(),
            inherit.aes = F,
            fill=NA,
            col = "black",
            lwd=1.5) +
    geom_sf(data = malh.all %>% 
              filter(SURVEY_YEA == year),
            inherit.aes=F,
            fill = "black",
            col = NA,
            alpha = 0.7) +
    labs(x = "Longitude", y = "Latitude",title = year) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  
  
  ggsave(filename = paste0("figures/malh_ads_gif/",year,".jpg"), 
         plot = p,
         width = 12,
         height = 9,
         units = "in",
         dpi = 150)
  
}



#---------
p <- ggmap(malh.map) +
  geom_sf(data = malh.fp %>% 
            sf::st_as_sf() %>% 
            sf::st_transform(crs=CRS(old.proj)),
          inherit.aes = F,
          fill=NA,
          col = "black",
          lwd=1.5)

for(i in 1:length(names(malh.pred))){
  
  year <- substr(names(malh.pred)[i],start = 2,stop = 5)
  
  p <- p +
    geom_raster(data = malh.pred[[i]] %>%
                  raster::projectRaster(.,crs=CRS(old.proj)) %>% 
                  as.data.frame(.,xy=T) %>% 
                  rename(val = 3),
                aes(x = x, y = y, fill = val)) +
    scale_fill_gradient2(low = "indianred2",mid="white",high = "green3",
                         midpoint=0,
                         aesthetics = "fill", 
                         guide = "legend", 
                         limits = c(-20,20),
                         n.breaks = 9,
                         na.value="transparent",
                         name="BAH\n(m2/ha)") +
    labs(x = "Longitude", y = "Latitude",title = year) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))

}
  
  
ggsave(filename = "figures/malh_allbah.jpg", 
         plot = p,
         width = 12,
         height = 9,
         units = "in",
         dpi = 150)

## ------

p <- ggmap(malh.map) +
  geom_sf(data = malh.fp %>% 
            sf::st_as_sf() %>% 
            sf::st_transform(crs=CRS(old.proj)),
          inherit.aes = F,
          fill=NA,
          col = "black",
          lwd=1.5) +
  geom_sf(data = malh.all %>% 
            arrange(LEGACY_TPA),
          inherit.aes=F,
          #aes(fill = (LEGACY_TPA)),
          fill="black",
          col = NA,
          alpha = 0.5) +
  labs(x = "Longitude", y = "Latitude",title = year) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_fill_binned(low="white",high="firebrick3",
                    breaks=c(0,10,20,30,40,50))

ggsave(filename = "figures/malh_allads_nocol.jpg", 
       plot = p,
       width = 12,
       height = 9,
       units = "in",
       dpi = 150)

  