### Douglas fir case study, Medford area 2015-2019
### 
### 

library(rFIA)
library(tidyverse)
library(sf)
library(lme4)
library(ggmap)
library(ggplot2)
library(sp)

# setting my preferred ggplot2 theme
theme_set(theme_bw())
theme_update(text = element_text(size=16, color = "black"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             panel.border=element_rect(size=1.5))


## (1) PULL IN FIA DATA ----

fia <- readFIA(dir = "/Users/DanielPerret/Box/01. daniel.perret Workspace/PROJECTS/plots-planes-pixels/data/OR_FIA_041825/",
               common=T, states=c("OR"))

exact.coords <- read.csv("D:/coord_update.csv",header=T) %>% 
  select(STATECD,
         PLOT = PLOT_FIADB,
         LAT_EXACT = PC_LAT_Y,
         LON_EXACT = PC_LON_X)

#creating some fields in various tables

fia$PLOT <- fia$PLOT %>% 
  mutate(pltID = paste(UNITCD,STATECD,COUNTYCD,PLOT,sep="_"),
         PLT_CN = CN) %>% 
  group_by(pltID) %>% 
  mutate(most.recent = ifelse(MEASYEAR==max(MEASYEAR),
                              "yes","no")) %>% 
  ungroup() %>% 
  left_join(exact.coords,
            by=c("STATECD","PLOT")) %>% 
  mutate(LON = LON_EXACT,
         LAT = LAT_EXACT)

fia$COND <- fia$COND %>% 
  left_join(fia$PLOT %>% 
              select(PLT_CN,most.recent),
            by="PLT_CN")

fia$TREE <- fia$TREE %>% 
  left_join(fia$PLOT %>% 
              select(PLT_CN,most.recent),
            by="PLT_CN") %>% 
  mutate(TRE_CN = CN,
         #DIA = DIA*2.54,
         #PREVDIA = PREVDIA*2.54,
         agent_key = case_when(STATUSCD==2 & AGENTCD %in% c(00,70) ~ "unknown1",
                               STATUSCD==2 & AGENTCD == 10 ~ "insect",
                               STATUSCD==2 & AGENTCD == 20 ~ "disease",
                               STATUSCD==2 & AGENTCD == 30 ~ "fire",
                               STATUSCD==2 & AGENTCD == 40 ~ "animal",
                               STATUSCD==2 & AGENTCD == 50 ~ "weather",
                               STATUSCD==2 & AGENTCD == 60 ~ "competition",
                               STATUSCD==2 & AGENTCD == 80 ~ "land use",
                               STATUSCD==2 & is.na(AGENTCD) & 
                                 (PREV_STATUS_CD==1 | 
                                    is.na(PREV_STATUS_CD)) ~ "unknown2"),
         insect.damage = case_when(DAMAGE_AGENT_CD1 >= 10000 &
                                     DAMAGE_AGENT_CD1 < 19000 ~ 1,
                                   DAMAGE_AGENT_CD2 >= 10000 &
                                     DAMAGE_AGENT_CD2 < 19000 ~ 1,
                                   DAMAGE_AGENT_CD3 >= 10000 &
                                     DAMAGE_AGENT_CD3 < 19000 ~ 1,
                                   TRUE ~ 0),
         disease.damage = case_when(DAMAGE_AGENT_CD1 >= 20000 &
                                      DAMAGE_AGENT_CD1 < 30000 ~ 1,
                                    DAMAGE_AGENT_CD2 >= 20000 &
                                      DAMAGE_AGENT_CD2 < 30000 ~ 1,
                                    DAMAGE_AGENT_CD3 >= 20000 &
                                      DAMAGE_AGENT_CD3 < 30000 ~ 1,
                                    TRUE ~ 0),
         other.damage = case_when(DAMAGE_AGENT_CD1 > 30000 ~ 1,
                                  DAMAGE_AGENT_CD2 > 30000 ~ 1,
                                  DAMAGE_AGENT_CD3 > 30000 ~ 1,
                                  TRUE ~ 0)) %>% 
  left_join(.,
            fia$TREE %>% 
              select(PREV_TRE_CN, SPCD) %>% 
              rename(LATER_SPCD=SPCD),
            by=c("TRE_CN"="PREV_TRE_CN")) %>% 
  mutate(SPCD = case_when(SPCD!=LATER_SPCD & !is.na(LATER_SPCD) ~ LATER_SPCD,
                          is.na(LATER_SPCD) ~ SPCD,
                          TRUE ~ SPCD)) %>% 
  left_join(.,
            fia$PLOT %>% 
              select(PLT_CN, MEASYEAR),
            by="PLT_CN")

## (2) PULL IN RASTER AND MISC SPATIAL DATA ----

psme.nbr <- raster::stack("/Users/DanielPerret/Box/PlotsPlanesPixels/data/FIR_casestudy/psme_nbr.tif")
psme.ndvi <- raster::stack("/Users/DanielPerret/Box/PlotsPlanesPixels/data/FIR_casestudy/psme_ndvi.tif")

old.proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#### Albers Equal Area; centered in western US
base.proj <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

psme.fp <- sf::read_sf("data/ADS_data/shapefiles/psme_fp.shp") %>% sf::st_transform(base.proj)

sp.fia <- fia$PLOT %>% 
  filter(!is.na(LON_EXACT)) %>% 
  SpatialPointsDataFrame(coords = .[,c("LON_EXACT","LAT_EXACT")],
                         data = .,
                         proj4string = CRS(old.proj)) %>% 
  spTransform(.,
              CRSobj = CRS(base.proj))

psme.fia <- sp.fia[psme.fp %>% sf::as_Spatial(),]

## (3) EXTRACT TIMESERIES FOR PLOT LOCATIONS ----

psme.extract <- raster::extract(psme.nbr, psme.fia, df=T, sp=T, buffer=45,fun=mean)

psme.extract2 <- psme.extract %>% 
  sf::st_as_sf() %>%
  mutate(YEAR2 = MEASYEAR,
         YEAR1 = as.integer(round(MEASYEAR-REMPER))) %>%
  select(PLT_CN,YEAR1,YEAR2,REMPER,
         contains("X")) %>% 
  pivot_longer(cols = c(contains("X")),
               names_to = "year",
               values_to = "nbri") %>% 
  mutate(year = as.integer(substr(year, start=2, stop = 5))) %>% 
  filter(!is.na(REMPER)) %>% sf::st_drop_geometry() %>% 
  ungroup() %>% 
  rowwise() %>% 
  filter(year %in% YEAR1:YEAR2) %>% 
  group_by(PLT_CN) %>%   
  mutate(d_nbri = nbri - lag(nbri),
         di_nbri = nbri - first(nbri)) %>%
  ungroup() %>% 
  mutate(nbri_t2 = ifelse(year == YEAR2,nbri,0),
         nbri_t1 = ifelse(year == YEAR1,nbri,0),
         d_nbri_ann = ifelse(year == YEAR2,d_nbri,0)) %>% 
  group_by(PLT_CN,YEAR1,YEAR2,REMPER) %>% 
  summarise(nbri_t2 = sum(nbri_t2),
            nbri_t1 = sum(nbri_t1),
            d_nbri_tot = sum(d_nbri, na.rm=T),
            d_nbri_abs = sum(abs(d_nbri), na.rm=T),
            di_nbri_tot = sum(di_nbri, na.rm=T)) %>% 
  ungroup()

## (4) CHANGE ESTIMATION ---------

# all species mortality
# psme.est.bah <- rFIA::growMort(db = fia,
#                                polys = psme.fp,
#                                byPlot = T,
#                                returnSpatial = F,
#                                treeDomain = DIA>5,
#                                stateVar = "BAA",
#                                totals = T) %>% 
#   group_by(pltID) %>% 
#   filter(YEAR == max(YEAR))

# just PSME mortality here
est <- rFIA::growMort(db = fia,
                               polys = psme.fp,
                               byPlot = T, bySpecies = T,
                               returnSpatial = F,
                               treeDomain = DIA>5,
                               stateVar = "BAA",
                               totals = T) %>% 
  group_by(pltID) %>% 
  filter(YEAR == max(YEAR)) %>% 
  ungroup()

psme.est.bah <- est %>% 
  filter(SPCD == 202) %>% 
  left_join(est %>% 
              group_by(pltID) %>% 
              mutate(psme = ifelse(SPCD==202,1,0)) %>% 
              summarise(PSME.prop = sum(PREV_BAA*psme)/sum(PREV_BAA),
                        other.BAA = sum(PREV_BAA)-sum(PREV_BAA*psme),
                        all.BAA = sum(PREV_BAA),
                        all.BAA.curr = sum(CURR_BAA),
                        all.BAA.prev = sum(PREV_BAA),
                        all.CHNG_BAA = sum(CHNG_BAA))) %>% 
  mutate(CHNG_PERC_psme = (CURR_BAA-PREV_BAA)/(all.BAA))




# The condition-class filter gives very few hits, probably because the mortality is consistently at a pretty low level
filt.cond <- fia$COND %>% 
  filter(PLT_CN %in% psme.est.bah$PLT_CN,
         DSTRBCD1 %in% 10:12 |
         DSTRBCD2 %in% 10:12|
         DSTRBCD3 %in% 10:12) %>% 
  pull(PLT_CN) %>% unique()

filt.cond <- fia$COND %>% 
  filter(PLT_CN %in% psme.est.bah$PLT_CN,
         TRTCD1 == 10 |
           TRTCD2 == 10 |
           TRTCD3 == 10 |
           DSTRBCD1 %in% 30:32 |
           DSTRBCD2 %in% 30:32 |
           DSTRBCD3 %in% 30:32) %>% 
  pull(PLT_CN) %>% unique()

# filter for insect-killed doug fir
filt.tree <- fia$TREE %>% 
  filter(PLT_CN %in% psme.est.bah$PLT_CN,
         SPCD == 202,
         agent_key == "insect") %>%
  pull(PLT_CN) %>% unique()

# filter for insect-damaged doug fir --> DAMTYP IS NOT RECORDED :(
filt.dam <- fia$TREE %>% 
  filter(PLT_CN %in% psme.est.bah$PLT_CN,
         SPCD == 202,
         DAMTYP1 %in% c(21,24,25) |
         DAMTYP2 %in% c(21,24,25)) %>% 
  pull(PLT_CN) %>% unique()

## (5) MODEL EXPLORATION -----------

psme.est.bah %>% 
  left_join(psme.extract2, by = "PLT_CN") %>% 
  left_join(fia$PLOT %>% 
              select(PLT_CN, ELEV, REMPER)) %>% 
  ungroup() %>% 
  mutate(ELEV.class = cut(ELEV,breaks = 5)) %>% 
  mutate(dist = ifelse(PLT_CN %in% filt.cond, 1, 0)) %>% 
  #filter(dist==1) %>% 
  ggplot(.,
         aes(x = d_nbri_tot,#CURR_BAA-PREV_BAA,
             y = CHNG_BAA#,
             #col = ELEV.class
             )) +
  geom_point(pch=19,
             size = 5,
             alpha = 0.4) +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0) #+
 # facet_wrap(facets = ~ELEV.class)

psme.est.bah %>%
  left_join(psme.extract2, by = "PLT_CN") %>% 
  lm(CHNG_BAA ~ d_nbri_tot, data = .) %>% 
  summary()

# a couple simple model variants that might improve fit somewhat

d1 <- psme.est.bah %>% 
  left_join(psme.extract2 %>% 
              select(-REMPER), by = "PLT_CN") %>% 
  left_join(fia$PLOT %>% 
              select(PLT_CN,ELEV)) %>% 
  left_join(fia$COND %>% 
              filter(CONDID==1) %>% 
              select(PLT_CN, FORTYPCD)) %>% 
  as.data.frame() %>% 
  mutate(FORTYPGRP = substr(FORTYPCD, 0 ,2),
         CHNG_BAA_NET = CHNG_BAA*REMPER)

m1 <- lme4::lmer(data = d1 %>% filter(YEAR>2010,PSME.prop>0.5),
                 formula = CHNG_BAA_NET ~ d_nbri_tot)
summary(m1)
performance::r2(m1)

d1 %>% 
  filter(YEAR>2010,
         PSME.prop>0.50) %>%
  #filter(!PLT_CN %in% filt.cond) %>% 
  #mutate(psme.class = cut(PSME.prop,breaks=c(0,0.25,0.5,0.75,1))) %>% 
  #filter(FORTYPGRP == 20) %>% 
  #filter(PLT_CN %in% filt.tree) %>% 
  ggplot(.,
         aes(x = d_nbri_tot,#CURR_BAA-PREV_BAA,
             y = CHNG_BAA*REMPER,
#             y = (MORT_BAA*REMPER)/all.BAA,
             col = ELEV
         )) +
  geom_point(pch=19,
             size = 5,
             alpha = 0.7) +
  #lims(y = c(-1,1))
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)# +
    #facet_wrap(facets = ~psme.class)
  

m0 <- lm(CHNG_BAA*REMPER ~ d_nbri_tot, data = d1 %>% filter(YEAR>2010,PSME.prop>0.50))
summary(m0)

d1 %>% 
  ggplot(.,
         aes(x = FORTYPGRP,
             y = PSME.prop)) +
  geom_boxplot()

# putting spatial layers together for BA change prediction

#LCMS insect/disease/drought cause classes
psme.cause <- raster::stack("/Users/DanielPerret/Box/PlotsPlanesPixels/data/LCMS/psme_causeofchange_annual.tif")
for(i in 1:length(names(psme.cause))){
  psme.cause[[i]][!psme.cause[[i]][]%in%10:12] <- NA
}

# prediction frames


psme.nbr.calc <- psme.nbr %>% 
  rasterToPoints() %>% 
  as.data.frame() %>% 
  mutate(X1995_d10y = X1995 - X1985,
         X1996_d10y = X1996 - X1986,
         X1997_d10y = X1997 - X1987,
         X1998_d10y = X1998 - X1988,
         X1999_d10y = X1999 - X1989,
         X2000_d10y = X2000 - X1990,
         X2001_d10y = X2001 - X1991,
         X2002_d10y = X2002 - X1992,
         X2003_d10y = X2003 - X1993,
         X2004_d10y = X2004 - X1994,
         X2005_d10y = X2005 - X1995,
         X2006_d10y = X2006 - X1996,
         X2007_d10y = X2007 - X1997,
         X2008_d10y = X2008 - X1998,
         X2009_d10y = X2009 - X1999,
         X2010_d10y = X2010 - X2000,
         X2011_d10y = X2011 - X2001,
         X2012_d10y = X2012 - X2002,
         X2013_d10y = X2013 - X2003,
         X2014_d10y = X2014 - X2004,
         X2015_d10y = X2015 - X2005,
         X2016_d10y = X2016 - X2006,
         X2017_d10y = X2017 - X2007,
         X2018_d10y = X2018 - X2008,
         X2019_d10y = X2019 - X2009,
         X2020_d10y = X2020 - X2010,
         X2021_d10y = X2021 - X2011,
         X2022_d10y = X2022 - X2012,
         X2023_d10y = X2023 - X2013)

years <- 2010:2020
psme.pred <- psme.nbr[[which(names(psme.nbr) %in% paste0("X",years,sep=""))]]
names(psme.pred) <- paste0(years,"_dbah")

for(i in 1:length(names(psme.pred))){
  
  year <- years[i]
  
  psme.pred[[i]][!is.na(psme.pred[[i]][])] <- 
    predict(m0,
            newdata = psme.nbr.calc %>% 
              select(d_nbri_tot = paste0("X",year,"_d10y")) %>% 
              mutate(case="psme"))
  
  psme.pred[[i]][is.na(psme.cause[[paste0("cause_",year)]][])] <- NA
  
}

#NOTE: this is the SE of the marginal prediction; i.e., does not include variance of the random intercept

malh.err <- psme.nbr[[which(names(psme.nbr) %in% paste0("X",years,"_nbri"))]]
names(malh.err) <- paste0(years,"_se")
for(i in 1:length(names(malh.err))){
  
  year <- years[i]
  
  newdat <- psme.nbr %>% 
    select(d_nbri_tot = paste0("X",year,"_nbri_d10y")) %>% 
    mutate(case="malh", CHNG_BAH=NA)
  malh.err[[i]][!is.na(malh.err[[i]][])] <- 
    predict(m0,
            newdata = newdat,
            se.fit = T,
            re.form = NA) %>% 
    as.data.frame() %>% 
    pull(se.fit)
  
  malh.err[[i]][is.na(malh.cause[[paste0("cause_",year)]][])] <- NA
  
}












































