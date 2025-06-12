#### leaflet map attempt
#### 
library(leaflet)
library(leafem)
library(htmlwidgets)


pred.pal <- colorBin(c("firebrick3",
                       "gray105",
                       "dodgerblue2"), domain = values(psme.pred), bins = 11, na.color = "transparent")
#err.pal <- colorBin(c("white","firebrick2"), domain = values(psme.err), bins = 10, na.color = "transparent")

#psme.fp <- spTransform(psme.fp, CRSobj = CRS(old.proj))

psme.fp <- sf::st_transform(psme.fp, crs=CRS(old.proj))

# need to make this layer!
psme.all <- sf::read_sf("/Users/DanielPerret/Box/01. daniel.perret Workspace/PROJECTS/plots-planes-pixels/data/ADS_data/shapefiles/psme_all.shp") %>% 
  sf::st_transform(., crs=CRS(old.proj))
  


leafmap <- leaflet() %>% 
  # basemap groups
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Stadia.StamenTonerLite, group = "Light") %>% 
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>% 
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>% 
  addPolygons(data = psme.fp,
              fill = F,
              stroke = T, col = "black", weight = 2.5)

for(i in 1:length(names(psme.pred))){
  
  year <- substr(names(psme.pred)[i],start = 2,stop = 5)
  
  leafmap <- leafmap %>% 
    addRasterImage(x = psme.pred[[i]],
                   opacity=0.9,
                   project=T,
                   colors = pred.pal,
                   group=paste0("BAH ", year),
                   maxBytes = Inf) #%>% 
    # addRasterImage(x = psme.err[[i]],
    #                opacity=0.9,
    #                project=T,
    #                colors = err.pal,
    #                group=paste0("SE ", year),
    #                maxBytes = Inf) %>% 
    # addPolygons(data = psme.all %>% 
    #               filter(SURVEY_YEA == year),
    #             fillColor = "black", opacity = 0.8,
    #             stroke = T, col = "black", weight = 2.5,
    #             group = paste0("ADS ", year))
}

leafmap <- leafmap %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Light", "Dark", "Topo", "Imagery"),
    overlayGroups = c(paste0(c("BAH "#,
                               #"SE ", 
                               #"ADS "
                               ), rep(years,each=1))),
    options = layersControlOptions(collapsed = FALSE,
                                   position = "topleft")
  ) %>% 
  addLegend(pal = pred.pal, values = values(psme.pred), 
            title = "BAH CHANGE", position = "topright") %>% 
  # addLegend(pal = err.pal, values = values(psme.pred), 
  #           title = "PREDICTION SE", position = "topright") %>% 
  hideGroup(c(paste0(c("BAH "
                       #,"SE ", "ADS "
                       ), rep(years,each=1)))) %>% 
  addMouseCoordinates()

leafmap

#"C:/Users/DanielPerret/Box/01. daniel.perret Workspace/PROJECTS/plots-planes-pixels"

setwd("C:/Users/DanielPerret/Box/01. daniel.perret Workspace/PROJECTS/plots-planes-pixels/leaflet_maps/")
saveWidget(widget = leafmap,
           file = "psme_mpb_102424.html",
           selfcontained = TRUE)

