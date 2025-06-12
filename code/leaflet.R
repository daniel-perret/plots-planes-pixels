#### leaflet map attempt
#### 
library(leaflet)
library(leafem)
library(htmlwidgets)


pred.pal <- colorBin(c("firebrick2","gray85","green3"), domain = values(malh.pred), bins = 11, na.color = "transparent")
err.pal <- colorBin(c("white","firebrick2"), domain = values(malh.err), bins = 10, na.color = "transparent")

malh.fp <- spTransform(malh.fp, CRSobj = CRS(old.proj))

malh.all <- readOGR(dsn="/Users/DanielPerret/Box/01. daniel.perret Workspace/PROJECTS/plots-planes-pixels/data/ADS_data/shapefiles/",
                   layer = "MPB2_malh_2011_2019",
                   verbose=F) %>% 
  spTransform(., CRSobj = CRS(old.proj)) %>% 
  as(.,"sf")
  

leafmap <- leaflet() %>% 
  # basemap groups
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Stadia.StamenTonerLite, group = "Light") %>% 
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>% 
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>% 
  addPolygons(data = malh.fp,
              fill = F,
              stroke = T, col = "black", weight = 2.5)

for(i in 1:length(names(malh.pred))){
  
  year <- substr(names(malh.pred)[i],start = 2,stop = 5)
  
  leafmap <- leafmap %>% 
    addRasterImage(x = malh.pred[[i]],
                   opacity=0.9,
                   project=T,
                   colors = pred.pal,
                   group=paste0("BAH ", year),
                   maxBytes = Inf) %>% 
    addRasterImage(x = malh.err[[i]],
                   opacity=0.9,
                   project=T,
                   colors = err.pal,
                   group=paste0("SE ", year),
                   maxBytes = Inf) %>% 
    addPolygons(data = malh.all %>% 
                  filter(SURVEY_YEA == year),
            fillColor = "black", opacity = 0.8,
            stroke = T, col = "black", weight = 2.5,
            group = paste0("ADS ", year))
}

leafmap <- leafmap %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Light", "Dark", "Topo", "Imagery"),
    overlayGroups = c(paste0(c("BAH ","SE ", "ADS "), rep(years,each=3))),
    options = layersControlOptions(collapsed = FALSE,
                                   position = "topleft")
  ) %>% 
  addLegend(pal = pred.pal, values = values(malh.pred), 
            title = "BAH CHANGE", position = "topright") %>% 
  # addLegend(pal = err.pal, values = values(malh.pred), 
  #           title = "PREDICTION SE", position = "topright") %>% 
  hideGroup(c(paste0(c("BAH ","SE ", "ADS "), rep(years,each=3)))) %>% 
  addMouseCoordinates()

leafmap

#"C:/Users/DanielPerret/Box/01. daniel.perret Workspace/PROJECTS/plots-planes-pixels"

setwd("C:/Users/DanielPerret/Box/01. daniel.perret Workspace/PROJECTS/plots-planes-pixels/leaflet_maps/")
saveWidget(widget = leafmap,
           file = "malh_mpb_102424.html",
           selfcontained = TRUE)

