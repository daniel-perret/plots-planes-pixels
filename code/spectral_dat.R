## initial wrangling of spectral timeseries put together by HZ
## We have two indices -- NDVI and NBR, annual timeseries from 1985 to 2013

## processing steps will be:

## (1) pull in raster stacks

malh.ndvi <- stack("/Users/DanielPerret/Box/PlotsPlanesPixels/data/MPB_casestudy/malheur/malhNDVI19852023.tif")
names(malh.ndvi) <- paste0(names(malh.ndvi), "_ndvi")
klam.ndvi <- stack("/Users/DanielPerret/Box/PlotsPlanesPixels/data/MPB_casestudy/klamath/klamNDVI19852023.tif")
names(klam.ndvi) <- paste0(names(klam.ndvi), "_ndvi")
malh.nbri <- stack("/Users/DanielPerret/Box/PlotsPlanesPixels/data/MPB_casestudy/malheur/malhNBR19852023.tif")
names(malh.nbri) <- paste0(names(malh.nbri), "_nbri")
klam.nbri <- stack("/Users/DanielPerret/Box/PlotsPlanesPixels/data/MPB_casestudy/klamath/klamNBR19852023.tif")
names(klam.nbri) <- paste0(names(klam.nbri), "_nbri")

## (2) extract timeseries for every FIA plot in footprint

### FIA data

fia <- readFIA(dir = "/Users/DanielPerret/Box/01. daniel.perret Workspace/PROJECTS/plots-planes-pixels/data/FIA/",
               common=T, states=c("OR"))

fia <- readFIA(dir = "/Users/DanielPerret/Box/01. daniel.perret Workspace/fia_data_082123/",
               common=T, states=c("OR"))

exact.coords <- read.csv("D:/coords_format.csv",header=T) %>% 
  select(PLT_CN = ID1,
         LAT_EXACT = lat,
         LON_EXACT = long)

#creating some fields in various tables

fia$PLOT <- fia$PLOT %>% 
  mutate(pltID = paste(UNITCD,STATECD,COUNTYCD,PLOT,sep="_"),
         PLT_CN = CN,
         ECOSUBCD = trimws(ECOSUBCD)) %>% 
  group_by(pltID) %>% 
  mutate(most.recent = ifelse(MEASYEAR==max(MEASYEAR),
                              "yes","no")) %>% 
  ungroup() %>% 
  left_join(exact.coords,
            by="PLT_CN")

fia$COND <- fia$COND %>% 
  left_join(fia$PLOT %>% 
              select(PLT_CN,most.recent),
            by="PLT_CN")

fia$TREE <- fia$TREE %>% 
  left_join(fia$PLOT %>% 
              select(PLT_CN,most.recent),
            by="PLT_CN") %>% 
  mutate(TRE_CN = CN,
         DIA = DIA*2.54,
         PREVDIA = PREVDIA*2.54,
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

spcd.ref <- read.csv("/Users/DanielPerret/Box/01. daniel.perret Workspace/FIADB_REFERENCE/REF_SPECIES.csv")

fia$TREE <- fia$TREE %>% 
  left_join(., spcd.ref %>% select(SPCD,COMMON_NAME))


### reading shapefiles for case study AOIs

#### WGS84
old.proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#### Albers Equal Area; centered in western US
base.proj <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#### shps

malh.fp <- sf::read_sf("/Users/DanielPerret/Box/PlotsPlanesPixels/data/MPB_casestudy/malheur/MPB2_malh_fp.shp") %>% sf::st_transform(base.proj)

klam.fp <- sf::read_sf("/Users/DanielPerret/Box/PlotsPlanesPixels/data/MPB_casestudy/klamath/mpb_allyears_fp.shp") %>% sf::st_transform(base.proj)

### making dataframe of FIA plots for each case study

#### get FIA data read in from the 'mpb_casestudy' document
sp.fia <- fia$PLOT %>% 
  filter(!is.na(LON_EXACT)) %>% 
  SpatialPointsDataFrame(coords = .[,c("LON_EXACT","LAT_EXACT")],
                         data = .,
                         proj4string = CRS(old.proj)) %>% 
  spTransform(.,
              CRSobj = CRS(base.proj))

malh.fia <- sp.fia[malh.fp,]
klam.fia <- sp.fia[klam.fp,]

fia$PLOT$LON <- fia$PLOT$LON_EXACT
fia$PLOT$LAT <- fia$PLOT$LAT_EXACT

### extractions

load("data/malh_extract_22oct.Rdata")
load("data/klam_extract22oct.Rdata")

# 
malh.extract <-  raster::extract(malh.ndvi, malh.fia, df=T, sp=T, buffer=45,fun=mean) %>%
  sf::st_as_sf() %>%
  left_join(raster::extract(malh.nbri, malh.fia, df=T, sp=T, buffer=45,fun=mean) %>%
              sf::st_as_sf() %>%
              sf::st_drop_geometry() %>%
              select(PLT_CN,contains("nbri")),
            by = "PLT_CN")
# 
# klam.extract <-  raster::extract(klam.ndvi, klam.fia, df=T, sp=T, buffer=45,fun=mean) %>% 
#   sf::st_as_sf() %>% 
#   left_join(raster::extract(klam.nbri, klam.fia, df=T, sp=T, buffer=45,fun=mean) %>% 
#               sf::st_as_sf() %>% 
#               sf::st_drop_geometry() %>% 
#               select(PLT_CN,contains("nbri")),
#             by = "PLT_CN") 


## (3) calculate annual difference for year prior to FIA plot remeasurement
## (4) calculate cumulative difference for all years during remeasurement period

malh.extract2 <- malh.extract %>% 
  mutate(YEAR2 = MEASYEAR,
         YEAR1 = as.integer(round(MEASYEAR-REMPER))) %>%
  select(PLT_CN,YEAR1,YEAR2,REMPER,
         contains("ndvi")) %>% 
  pivot_longer(cols = c(contains("ndvi")),
               names_to = "year",
               values_to = "ndvi") %>% 
  mutate(year = as.integer(substr(year, start=2, stop = 5))) %>% 
  left_join(malh.extract %>% 
              sf::st_drop_geometry() %>% 
              mutate(YEAR2 = MEASYEAR,
                     YEAR1 = as.integer(round(MEASYEAR-REMPER))) %>%
              select(PLT_CN,YEAR1,YEAR2,REMPER,
                     contains("nbri")) %>% 
              pivot_longer(cols = c(contains("nbri")),
                           names_to = "year",
                           values_to = "nbri") %>% 
              mutate(year = as.integer(substr(year, start=2, stop = 5))),
            by=c("PLT_CN","REMPER","YEAR1","YEAR2","year")) %>% 
  filter(!is.na(REMPER)) %>% sf::st_drop_geometry() %>% 
  ungroup() %>% 
  rowwise() %>% 
  filter(year %in% YEAR1:YEAR2) %>% 
  group_by(PLT_CN) %>%   
  mutate(d_ndvi = ndvi - lag(ndvi),
         d_nbri = nbri - lag(nbri),
         di_ndvi = ndvi - first(ndvi),
         di_nbri = nbri - first(nbri)) %>%
  ungroup() %>% 
  mutate(ndvi_t2 = ifelse(year == YEAR2,ndvi,0),
         nbri_t2 = ifelse(year == YEAR2,nbri,0),
         ndvi_t1 = ifelse(year == YEAR1,ndvi,0),
         nbri_t1 = ifelse(year == YEAR1,nbri,0),
         d_ndvi_ann = ifelse(year == YEAR2,d_ndvi,0),
         d_nbri_ann = ifelse(year == YEAR2,d_nbri,0)) %>% 
  group_by(PLT_CN,YEAR1,YEAR2,REMPER) %>% 
  summarise(ndvi_t2 = sum(ndvi_t2),
            nbri_t2 = sum(nbri_t2),
            ndvi_t1 = sum(ndvi_t1),
            nbri_t1 = sum(nbri_t1),
            
            d_ndvi_tot = sum(d_ndvi, na.rm=T),
            d_nbri_tot = sum(d_nbri, na.rm=T),
            d_ndvi_abs = sum(abs(d_ndvi), na.rm=T),
            d_nbri_abs = sum(abs(d_nbri), na.rm=T),
            
            di_ndvi_tot = sum(di_ndvi, na.rm=T),
            di_nbri_tot = sum(di_nbri, na.rm=T)) %>% 
  ungroup()


klam.extract2 <- klam.extract %>% 
  mutate(YEAR2 = MEASYEAR,
         YEAR1 = round(MEASYEAR-REMPER)) %>%
  select(PLT_CN,YEAR1,YEAR2,MEASYEAR,REMPER,
         contains("ndvi")) %>% 
  pivot_longer(cols = c(contains("ndvi")),
               names_to = "year",
               values_to = "ndvi") %>% 
  mutate(year = substr(year, start=2, stop = 5)) %>% 
  left_join(klam.extract %>% 
              sf::st_drop_geometry() %>% 
              mutate(YEAR2 = MEASYEAR,
                     YEAR1 = round(MEASYEAR-REMPER)) %>%
              select(PLT_CN,YEAR1,YEAR2,MEASYEAR,REMPER,
                     contains("nbri")) %>% 
              pivot_longer(cols = c(contains("nbri")),
                           names_to = "year",
                           values_to = "nbri") %>% 
              mutate(year = substr(year, start=2, stop = 5)),
            by=c("PLT_CN","REMPER","YEAR1","YEAR2","MEASYEAR","year")) %>% 
  filter(!is.na(REMPER)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  filter(year %in% YEAR1:YEAR2) %>% 
  group_by(PLT_CN) %>%
  mutate(d_ndvi = ndvi - lag(ndvi),
         d_nbri = nbri - lag(nbri),
         di_ndvi = ndvi - first(ndvi),
         di_nbri = nbri - first(nbri)) %>%
  ungroup() %>% 
  mutate(ndvi_t2 = ifelse(year == MEASYEAR,ndvi,0),
         nbri_t2 = ifelse(year == MEASYEAR,nbri,0),
         ndvi_t1 = ifelse(year == YEAR1,ndvi,0),
         nbri_t1 = ifelse(year == YEAR1,nbri,0),) %>% 
  group_by(PLT_CN,YEAR1,YEAR2,REMPER,MEASYEAR) %>% 
  summarise(ndvi_t2 = sum(ndvi_t2, na.rm=T),
            nbri_t2 = sum(nbri_t2, na.rm=T),
            ndvi_t1 = sum(ndvi_t1, na.rm=T),
            nbri_t1 = sum(nbri_t1, na.rm=T),
            
            d_ndvi_tot = sum(d_ndvi, na.rm=T),
            d_nbri_tot = sum(d_nbri, na.rm=T),
            d_ndvi_abs = sum(abs(d_ndvi), na.rm=T),
            d_nbri_abs = sum(abs(d_nbri), na.rm=T),
            
            di_ndvi_tot = sum(di_ndvi),
            di_nbri_tot = sum(di_nbri)) %>% 
  ungroup()


## 5) linking with change estimates
## 
## We probably want a couple different estimates:
## - mortality (individual and basal area)
## - insect mortality
## - tpa and basal area change

thresh <- 12.7

### tpa mortality
# malh.est.tph <- growMort_dlp.metric(db = fia,
#                                     polys = malh.fp %>% 
#                                       sf::st_as_sf(),
#                                     byPlot = T,
#                                     returnSpatial = T,
#                                     sizeThresh = thresh,
#                                     evals = c(41903, 411903),
#                                     totals = T)
# 
# klam.est.tph <- growMort_dlp.metric(db = fia,
#                                     polys = klam.fp,
#                                     byPlot = T, 
#                                     returnSpatial = F,
#                                     sizeThresh = thresh,
#                                     evals = c(41903, 411903),
#                                     totals = T)

### bah mortality
malh.est.bah <- growMort_dlp.metric(db = fia,
                                    polys = malh.fp,
                                    byPlot = T,
                                    returnSpatial = F,
                                    sizeThresh = thresh,
                                    stateVar = "BAA",
                                    evals = c(41903, 411903),
                                    totals = T)

klam.est.bah <- growMort_dlp.metric(db = fia,
                                    polys = klam.fp,
                                    byPlot = T, 
                                    returnSpatial = F,
                                    sizeThresh = thresh,
                                    stateVar = "BAA",
                                    evals = c(41903, 411903),
                                    totals = T)

### insect specific estimates
# 
# malh.est.agent <- growMort_dlp.metric(db = fia,
#                                     polys = malh.fp,
#                                     byPlot = T,
#                                     grpBy = agent_key,
#                                     returnSpatial = F,
#                                     sizeThresh = thresh,
#                                     stateVar = "BAA",
#                                     evals = c(41903, 411903),
#                                     totals = T) %>% 
#   filter(agent_key == "insect") %>% 
#   group_by(PLT_CN) %>% 
#   filter(YEAR == max(YEAR)) %>% 
#   ungroup()
# 
# klam.est.agent <- growMort_dlp.metric(db = fia,
#                                     polys = klam.fp,
#                                     byPlot = T, 
#                                     grpBy = agent_key,
#                                     returnSpatial = F,
#                                     sizeThresh = thresh,
#                                     stateVar = "BAA",
#                                     evals = c(41903, 411903),
#                                     totals = T)
# 
# ### trying a new one to get at change in dead tree basal area
# 
# malh.est.dead <- growMort_dlp.metric(db = fia,
#                                     polys = malh.fp,
#                                     byPlot = T,
#                                     grpBy = STATUSCD,
#                                     returnSpatial = F,
#                                     sizeThresh = thresh,
#                                     stateVar = "BAA",
#                                     evals = c(41903, 411903),
#                                     totals = T) %>% 
#   filter(STATUSCD == 2)
# 
# klam.est.dead <- growMort_dlp.metric(db = fia,
#                                     polys = klam.fp,
#                                     byPlot = T, 
#                                     grpBy = STATUSCD,
#                                     returnSpatial = F,
#                                     sizeThresh = thresh,
#                                     stateVar = "BAA",
#                                     evals = c(41903, 411903),
#                                     totals = T)

##

# filtering to exclude plots impacted by fire, etc

malh.plts <- malh.est.bah %>% pull(PLT_CN)
# 
# malh.filt1 <- fia$COND %>% 
#   filter(PLT_CN %in% malh.plts,
#          !DSTRBCD1 %in% 30:32 |
#          DSTRBCD2 %in% 30:32 |
#          DSTRBCD3 %in% 30:32,
#          !TRTCD1 %in% 10:50) %>% 
#   pull(PLT_CN) %>% unique()
  
malh.filt2 <- fia$COND %>% 
  filter(PLT_CN %in% malh.plts,
         DSTRBCD1 %in% 10:12 |
           DSTRBCD2 %in% 10:12 |
           DSTRBCD3 %in% 10:12) %>% 
  pull(PLT_CN) %>% unique()
# 
# malh.filt3 <- fia$TREE %>%
#   filter(PLT_CN %in% malh.plts,
#          agent_key == "insect") %>%
#   pull(PLT_CN) %>% unique()
# 
klam.plts <- klam.est.bah %>% pull(PLT_CN)
# 
# klam.filt1 <- fia$COND %>% 
#   filter(PLT_CN %in% klam.plts,
#          !DSTRBCD1 %in% 30:32 |
#          DSTRBCD2 %in% 30:32 |
#          DSTRBCD3 %in% 30:32,
#          !TRTCD1 %in% 10:50) %>% 
#   pull(PLT_CN) %>% unique()
#   
klam.filt2 <- fia$COND %>%
  filter(PLT_CN %in% klam.plts,
         DSTRBCD1 %in% 10:12 |
           DSTRBCD2 %in% 10:12 |
           DSTRBCD3 %in% 10:12) %>%
  pull(PLT_CN) %>% unique()
# 
# klam.filt3 <- fia$TREE %>%
#   filter(PLT_CN %in% klam.plts,
#          agent_key == "insect") %>%
#   pull(PLT_CN) %>% unique()

## building model ----

#### model is a simple mixed model that combines both case studies with a random intercept. We only use plots that have insects recorded as a disturbance code at at least one condition level (DSTRBCD1-DSTRBCD3)
m0 <- malh.est.bah %>% 
  left_join(malh.extract2, by = "PLT_CN") %>%
  mutate(case="malh") %>% 
  bind_rows(klam.est.bah %>%
              left_join(klam.extract2, by = "PLT_CN") %>%
              mutate(case = "klam")) %>%
  filter(PLT_CN %in% c(malh.filt2,klam.filt2)) %>% #heres the filter
  lme4::lmer(CHNG_BAH ~ d_nbri_tot + (1|case), data = .)


malh.est.bah %>% 
  left_join(malh.extract2, by = "PLT_CN") %>%
  mutate(case="malh") %>% 
  bind_rows(klam.est.bah %>%
              left_join(klam.extract2, by = "PLT_CN") %>%
              mutate(case = "klam")) %>%
  filter(PLT_CN %in% c(malh.filt2,klam.filt2)) %>% 
  bind_cols(predict(m0,se = T)) %>% 
  ggplot(.,
         aes(x = d_nbri_tot,
             y = (CHNG_BAH),
             col = case)) +
  geom_point(cex = 3, alpha = 0.6) +
    geom_ribbon(aes(ymin = fit-(se.fit*1.96),
                  ymax = fit+(se.fit*1.96),
                  group = case),
              fill="gray",
              col=NA,
              alpha = 0.5) +
  geom_line(aes(y = fit),
            lwd=1.5) +
  labs(x = "NBR change (net)",
       y = "Basal area change (m2/ha)")


### ------------------- try to do some simple prediction and spatialization

### malheur

malh.nbri.calc <- malh.nbri %>% 
  rasterToPoints() %>% 
  as.data.frame() %>% 
  mutate(X1995_nbri_d10y = X1995_nbri - X1985_nbri,
         X1996_nbri_d10y = X1996_nbri - X1986_nbri,
         X1997_nbri_d10y = X1997_nbri - X1987_nbri,
         X1998_nbri_d10y = X1998_nbri - X1988_nbri,
         X1999_nbri_d10y = X1999_nbri - X1989_nbri,
         X2000_nbri_d10y = X2000_nbri - X1990_nbri,
         X2001_nbri_d10y = X2001_nbri - X1991_nbri,
         X2002_nbri_d10y = X2002_nbri - X1992_nbri,
         X2003_nbri_d10y = X2003_nbri - X1993_nbri,
         X2004_nbri_d10y = X2004_nbri - X1994_nbri,
         X2005_nbri_d10y = X2005_nbri - X1995_nbri,
         X2006_nbri_d10y = X2006_nbri - X1996_nbri,
         X2007_nbri_d10y = X2007_nbri - X1997_nbri,
         X2008_nbri_d10y = X2008_nbri - X1998_nbri,
         X2009_nbri_d10y = X2009_nbri - X1999_nbri,
         X2010_nbri_d10y = X2010_nbri - X2000_nbri,
         X2011_nbri_d10y = X2011_nbri - X2001_nbri,
         X2012_nbri_d10y = X2012_nbri - X2002_nbri,
         X2013_nbri_d10y = X2013_nbri - X2003_nbri,
         X2014_nbri_d10y = X2014_nbri - X2004_nbri,
         X2015_nbri_d10y = X2015_nbri - X2005_nbri,
         X2016_nbri_d10y = X2016_nbri - X2006_nbri,
         X2017_nbri_d10y = X2017_nbri - X2007_nbri,
         X2018_nbri_d10y = X2018_nbri - X2008_nbri,
         X2019_nbri_d10y = X2019_nbri - X2009_nbri,
         X2020_nbri_d10y = X2020_nbri - X2010_nbri,
         X2021_nbri_d10y = X2021_nbri - X2011_nbri,
         X2022_nbri_d10y = X2022_nbri - X2012_nbri,
         X2023_nbri_d10y = X2023_nbri - X2013_nbri)

years <- 2011:2019
malh.pred <- malh.nbri[[which(names(malh.nbri) %in% paste0("X",years,"_nbri"))]]
names(malh.pred) <- paste0(years,"_dbah")
for(i in 1:length(names(malh.pred))){
  
  year <- years[i]
  
  malh.pred[[i]][!is.na(malh.pred[[i]][])] <- 
    predict(m0,
            newdata = malh.nbri.calc %>% 
              select(d_nbri_tot = paste0("X",year,"_nbri_d10y")) %>% 
              mutate(case="malh"))
  
  malh.pred[[i]][is.na(malh.cause[[paste0("cause_",year)]][])] <- NA
  
}

#NOTE: this is the SE of the marginal prediction; i.e., does not include variance of the random intercept

malh.err <- malh.nbri[[which(names(malh.nbri) %in% paste0("X",years,"_nbri"))]]
names(malh.err) <- paste0(years,"_se")
for(i in 1:length(names(malh.err))){
  
  year <- years[i]
  
  newdat <- malh.nbri.calc %>% 
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

### klamath

klam.nbri.calc <- klam.nbri %>% 
  rasterToPoints() %>% 
  as.data.frame() %>% 
  mutate(X1995_nbri_d10y = X1995_nbri - X1985_nbri,
         X1996_nbri_d10y = X1996_nbri - X1986_nbri,
         X1997_nbri_d10y = X1997_nbri - X1987_nbri,
         X1998_nbri_d10y = X1998_nbri - X1988_nbri,
         X1999_nbri_d10y = X1999_nbri - X1989_nbri,
         X2000_nbri_d10y = X2000_nbri - X1990_nbri,
         X2001_nbri_d10y = X2001_nbri - X1991_nbri,
         X2002_nbri_d10y = X2002_nbri - X1992_nbri,
         X2003_nbri_d10y = X2003_nbri - X1993_nbri,
         X2004_nbri_d10y = X2004_nbri - X1994_nbri,
         X2005_nbri_d10y = X2005_nbri - X1995_nbri,
         X2006_nbri_d10y = X2006_nbri - X1996_nbri,
         X2007_nbri_d10y = X2007_nbri - X1997_nbri,
         X2008_nbri_d10y = X2008_nbri - X1998_nbri,
         X2009_nbri_d10y = X2009_nbri - X1999_nbri,
         X2010_nbri_d10y = X2010_nbri - X2000_nbri,
         X2011_nbri_d10y = X2011_nbri - X2001_nbri,
         X2012_nbri_d10y = X2012_nbri - X2002_nbri,
         X2013_nbri_d10y = X2013_nbri - X2003_nbri,
         X2014_nbri_d10y = X2014_nbri - X2004_nbri,
         X2015_nbri_d10y = X2015_nbri - X2005_nbri,
         X2016_nbri_d10y = X2016_nbri - X2006_nbri,
         X2017_nbri_d10y = X2017_nbri - X2007_nbri,
         X2018_nbri_d10y = X2018_nbri - X2008_nbri,
         X2019_nbri_d10y = X2019_nbri - X2009_nbri,
         X2020_nbri_d10y = X2020_nbri - X2010_nbri,
         X2021_nbri_d10y = X2021_nbri - X2011_nbri,
         X2022_nbri_d10y = X2022_nbri - X2012_nbri,
         X2023_nbri_d10y = X2023_nbri - X2013_nbri)

years <- 2000:2017
klam.pred <- klam.nbri[[which(names(klam.nbri) %in% paste0("X",years,"_nbri"))]]
names(klam.pred) <- paste0(years,"_dbah")
for(i in 1:length(names(klam.pred))){
  
  year <- years[i]
  
  klam.pred[[i]][!is.na(klam.pred[[i]][])] <- 
    predict(m0,
            newdata = klam.nbri.calc %>% 
              select(d_nbri_tot = paste0("X",year,"_nbri_d10y")) %>% 
              mutate(case="klam"))
  
  klam.pred[[i]][is.na(klam.cause[[paste0("cause_",year)]][])] <- NA
  
}

klam.err <- klam.nbri[[which(names(klam.nbri) %in% paste0("X",years,"_nbri"))]]
names(klam.err) <- paste0(years,"_se")
for(i in 1:length(names(klam.err))){
  
  year <- years[i]
  
  klam.err[[i]][!is.na(klam.err[[i]][])] <- 
    predict(m0,
            newdata = klam.nbri.calc %>% 
              select(d_nbri_tot = paste0("X",year,"_nbri_d10y")) %>% 
              mutate(case="klam"),
            se.fit = T,
            re.form = NA) %>% 
    as.data.frame() %>% 
    pull(se.fit)
  
  klam.err[[i]][is.na(klam.cause[[paste0("cause_",year)]][])] <- NA
  
}































