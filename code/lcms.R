### LCMS data
### 
### 

malh.cause <- stack("/Users/DanielPerret/Box/PlotsPlanesPixels/data/LCMS/malh_cause_of_change_annual.tif")

for(i in 1:length(names(malh.cause))){
  malh.cause[[i]][!malh.cause[[i]][]%in%15:16] <- NA
}

klam.cause <- stack("/Users/DanielPerret/Box/PlotsPlanesPixels/data/LCMS/klam_cause_of_change_annual.tif")
for(i in 1:length(names(klam.cause))){
  klam.cause[[i]][!klam.cause[[i]][]%in%15:16] <- NA
}

psme.cause <- stack("/Users/DanielPerret/Box/PlotsPlanesPixels/data/LCMS/psme_causeofchange_annual.tif")
for(i in 1:length(names(psme.cause))){
  psme.cause[[i]][!psme.cause[[i]][]%in%10:12] <- NA
}

ocho.cause <- raster::stack("/Users/DanielPerret/Box/PlotsPlanesPixels/data/LCMS/ocho_causeofchange_annual.tif")
for(i in 1:length(names(ocho.cause))){
  ocho.cause[[i]][!ocho.cause[[i]][]%in%10:12] <- NA
}

