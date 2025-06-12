library(ggmap)

klam.fp <- klam.fp%>% sf::st_as_sf() %>%  sf::st_transform(crs=CRS(old.proj))

ext <- sf::st_bbox(klam.fp) 

names(ext) <- c("left","bottom","right","top")

klam.all <- sf::read_sf("data/ADS_data/shapefiles/MPB_casestudy_2000_2016.shp")

klam.map <- get_stadiamap(bbox = ext, mapytpe = "stamen_terrain")


for(i in 1:length(names(klam.pred))){
  
  year <- substr(names(klam.pred)[i],start = 2,stop = 5)
  
  p <- ggmap(klam.map) +
    geom_sf(data = klam.fp,
            inherit.aes = F,
            fill=NA,
            col = "black",
            lwd=1.5) +
    geom_raster(data = klam.pred[[i]] %>%
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
  
  
  ggsave(filename = paste0("figures/klam_bah_gif/colorblind2/",year,".jpg"), 
         plot = p,
         width = 12,
         height = 9,
         units = "in",
         dpi = 150)
  
}




for(i in 1:length(names(klam.pred))){
  
  year <- substr(names(klam.pred)[i],start = 2,stop = 5)
  
  p <- ggmap(klam.map) +
    geom_sf(data = klam.fp %>% 
              sf::st_as_sf(),
            inherit.aes = F,
            fill=NA,
            col = "black",
            lwd=1.5) +
    geom_sf(data = klam.all %>% 
              filter(SURVEY_YEA == year),
            inherit.aes=F,
            fill = "black",
            col = NA,
            alpha = 0.7) +
    labs(x = "Longitude", y = "Latitude",title = year) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  
  
  ggsave(filename = paste0("figures/klam_ads_gif/",year,".jpg"), 
         plot = p,
         width = 12,
         height = 9,
         units = "in",
         dpi = 150)
  
}



#---------
p <- ggmap(klam.map) +
  geom_sf(data = klam.fp,
          inherit.aes = F,
          fill=NA,
          col = "black",
          lwd=1.5)

for(i in 1:length(names(klam.pred))){
  
  year <- substr(names(klam.pred)[i],start = 2,stop = 5)
  
  p <- p +
    geom_raster(data = klam.pred[[i]] %>%
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


ggsave(filename = "figures/klam_allbah.jpg", 
       plot = p,
       width = 12,
       height = 9,
       units = "in",
       dpi = 150)

## ------

p <- ggmap(klam.map) +
  geom_sf(data = klam.fp %>% 
            sf::st_as_sf() %>% 
            sf::st_transform(crs=CRS(old.proj)),
          inherit.aes = F,
          fill=NA,
          col = "black",
          lwd=1.5) +
  geom_sf(data = klam.all %>% 
            arrange(LEGACY_TPA),
          inherit.aes=F,
          aes(fill = (LEGACY_TPA)),
          #fill="black",
          col = NA,
          alpha = 0.5) +
  labs(x = "Longitude", y = "Latitude",title = year) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_fill_binned(low="white",high="firebrick3",
                    breaks=c(0,10,20,30,40,50))

ggsave(filename = "figures/klam_allads_col.jpg", 
       plot = p,
       width = 12,
       height = 9,
       units = "in",
       dpi = 150)

