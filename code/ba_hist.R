klam.cum <- klam.pred.lcms %>%
  terra::subset(., paste0(2003:2016)) %>% 
  terra::app(., fun=sum, na.rm=T)

klam.ba.lcms <- ifel(is.na(klam.cum),NA,prop.ba.klam)
hist(values(klam.ba.lcms))
mean(values(klam.ba.lcms),na.rm=T)

plot(klam.ba.lcms)
plot(prop.ba.klam)


malh.cum <- malh.pred.lcms %>%
  terra::subset(., paste0(2013:2019)) %>% 
  terra::app(., fun=sum, na.rm=T)

malh.ba.lcms <- ifel(is.na(malh.cum),NA,prop.ba.malh)
hist(values(malh.ba.lcms))
mean(values(malh.ba.lcms),na.rm=T)

plot(malh.ba.lcms)
plot(prop.ba.malh)


ffb.cum <- psme.pred.lcms %>%
  terra::subset(., paste0(2015:2023)) %>% 
  terra::app(., fun=sum, na.rm=T)

ffb.ba.lcms <- ifel(is.na(ffb.cum),NA,prop.ba.psme)
hist(values(ffb.ba.lcms))
mean(values(ffb.ba.lcms),na.rm=T)

plot(ffb.ba.lcms)
plot(prop.ba.psme)


fe.cum <- ocho.pred.lcms %>%
  terra::subset(., paste0(2021:2023)) %>% 
  terra::app(., fun=sum, na.rm=T)

fe.ba.lcms <- ifel(is.na(fe.cum),NA,prop.ba.ocho)
hist(values(fe.ba.lcms))
mean(values(fe.ba.lcms),na.rm=T)

plot(fe.ba.lcms)
plot(prop.ba.ocho)
