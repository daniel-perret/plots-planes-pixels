#### leaflet map attempt
#### 
library(leaflet)
library(leafem)
library(htmlwidgets)


pred.pal <- colorBin(c("firebrick2","gray85","green3"), domain = values(klam.pred), bins = 11, na.color = "transparent")
err.pal <- colorBin(c("white","firebrick2"), domain = values(klam.err), bins = 10, na.color = "transparent")

klam.fp <- spTransform(klam.fp, CRSobj = CRS(old.proj))

klam.all <- readOGR(dsn="/Users/DanielPerret/Box/01. daniel.perret Workspace/PROJECTS/plots-planes-pixels/data/ADS_data/shapefiles/",
                    layer = "MPB_casestudy_2000_2016",
                    verbose=F) %>% 
  spTransform(., CRSobj = CRS(old.proj)) %>% 
  as(.,"sf")


leafmap <- leaflet() %>% 
  # basemap groups
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>% 
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>% 
  addPolygons(data = klam.fp,
              fill = F,
              stroke = T, col = "black", weight = 2.5)

for(i in 1:length(names(klam.pred))){
  
  year <- substr(names(klam.pred)[i],start = 2,stop = 5)
  
  leafmap <- leafmap %>% 
    addRasterImage(x = klam.pred[[i]],
                   opacity=0.9,
                   project=T,
                   colors = pred.pal,
                   group=paste0("BAH ", year),
                   maxBytes = Inf) %>% 
    addRasterImage(x = klam.err[[i]],
                   opacity=0.9,
                   project=T,
                   colors = err.pal,
                   group=paste0("SE ", year),
                   maxBytes = Inf) %>% 
    addPolygons(data = klam.all %>% 
                  filter(SURVEY_YEA == year),
                fillColor = "black", opacity = 0.8,
                stroke = T, col = "black", weight = 2.5,
                group = paste0("ADS ", year))
}

leafmap <- leafmap %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Dark", "Topo", "Imagery"),
    overlayGroups = c(paste0(c("BAH ","SE ", "ADS "), rep(years,each=3))),
    options = layersControlOptions(collapsed = FALSE,
                                   position = "topleft")
  ) %>% 
  addLegend(pal = pred.pal, values = values(klam.pred), 
            title = "BAH CHANGE", position = "topright") %>% 
  addLegend(pal = err.pal, values = values(klam.pred), 
            title = "PREDICTION SE", position = "topright") %>% 
  hideGroup(c(paste0(c("BAH ","SE ", "ADS "), rep(years,each=3)))) %>% 
  addMouseCoordinates()

leafmap

#"C:/Users/DanielPerret/Box/01. daniel.perret Workspace/PROJECTS/plots-planes-pixels"

setwd("C:/Users/DanielPerret/Box/01. daniel.perret Workspace/PROJECTS/plots-planes-pixels/leaflet_maps/")
saveWidget(widget = leafmap,
           file = "klam_mpb_102524.html",
           selfcontained = TRUE)

### making jpeg maps to turn into a gif













