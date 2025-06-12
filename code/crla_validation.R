### pulling FIA data for Crater Lake validation ----

fia <- readFIA(dir = "/Users/DanielPerret/Box/01. daniel.perret Workspace/PROJECTS/plots-planes-pixels/data/OR_FIA_041825/",
               common=T, states=c("OR"))

exact.coords <- read.csv("D:/coords_format.csv",header=T) %>% 
  select(PLT_CN = ID1,
         LAT_EXACT = lat,
         LON_EXACT = long)

#creating some fields in various tables

fia$PLOT <- fia$PLOT %>% 
  mutate(pltID = paste(UNITCD,STATECD,COUNTYCD,PLOT,sep="_"),
         PLT_CN = CN) %>% 
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

#### spatdat----

# WGS84
old.proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Albers Equal Area; centered in western US
base.proj <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"


# shps
crla.fp <- readOGR(dsn = "/Users/DanielPerret/Box/01. daniel.perret Workspace/PROJECTS/plots-planes-pixels/data/ADS_data/shapefiles",
                   layer = "crla_mpb_fp",
                   verbose = F) %>% 
  spTransform(.,
              CRSobj = CRS(base.proj))

sp.fia <- fia$PLOT %>% 
  filter(!is.na(LON_EXACT)) %>% 
  SpatialPointsDataFrame(coords = .[,c("LON_EXACT","LAT_EXACT")],
                         data = .,
                         proj4string = CRS(old.proj)) %>% 
  spTransform(.,
              CRSobj = CRS(base.proj))

crla.fia <- sp.fia[crla.fp,]

#### crla bah est

crla.est.bah <- rFIA::growMort(db = fia,
                               polys = crla.fp,
                               byPlot = T,
                               returnSpatial = F,
                               treeDomain = DIA>5,
                               stateVar = "BAA",
                               totals = T)

# 
# crla.est.bah <- growMort_dlp.metric(db = fia,
#                                     polys = crla.fp,
#                                     byPlot = T,
#                                     returnSpatial = F,
#                                     sizeThresh = thresh,
#                                     stateVar = "BAA",
#                                     evals = c(41903, 411903),
#                                     totals = T)
crla.est.bah %>% 
  group_by(pltID) %>% 
  filter(YEAR == max(YEAR)) %>% 
  view()

#### disturbed plots

crla.filt <- fia$COND %>% 
  filter(PLT_CN %in% crla.est.bah$PLT_CN,
         DSTRBCD1 %in% 10:12 |
           DSTRBCD2 %in% 10:12 |
           DSTRBCD3 %in% 10:12) %>% 
  pull(PLT_CN) %>% unique()

#### extracting NBR timeseries and calculating difference

crla.nbri <- stack("/Users/DanielPerret/Box/PlotsPlanesPixels/data/MPB_casestudy/crla/crlaNBR19852023.tif")
names(crla.nbri) <- paste0(names(crla.nbri),"_nbri")

crla.extract <- raster::extract(crla.nbri, crla.fia, df=T, sp=T, buffer=45,fun=mean)


crla.extract2 <- crla.extract %>% 
  sf::st_as_sf() %>%
  mutate(YEAR2 = MEASYEAR,
         YEAR1 = as.integer(round(MEASYEAR-REMPER))) %>%
  select(PLT_CN,YEAR1,YEAR2,REMPER,
         contains("nbri")) %>% 
  pivot_longer(cols = c(contains("nbri")),
               names_to = "year",
               values_to = "nbri") %>% 
  mutate(year = as.integer(substr(year, start=2, stop = 5))) %>% 
   filter(!is.na(REMPER)) %>% sf::st_drop_geometry() %>% 
  ungroup() %>% 
  group_by(PLT_CN) %>%   
  filter(year %in% YEAR1:YEAR2) %>% 
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


#### making crla predictions

crla.pred <- crla.est.bah %>% 
  #filter(PLT_CN %in% crla.filt) %>% 
  left_join(.,
            crla.extract2 %>% 
              #filter(PLT_CN%in%crla.filt) %>% 
              select(PLT_CN) %>% 
              mutate(pred = predict(m0,
                    newdata = crla.extract2 %>% 
                     # filter(PLT_CN %in% crla.filt) %>% 
                      select(PLT_CN, d_nbri_tot) %>% 
                      mutate(case="crla"),
                    re.form = NA
                    #allow.new.levels=T
                    )))


crla.pred %>% 
  ggplot(.,
         aes(x = CHNG_BAA,
             y = pred)) +
  geom_point(pch = 19,
             alpha = 0.6,
             size = 4) +
  geom_abline(slope=1, intercept=0) +
  geom_smooth() +
  labs(x = "Observed BAA change", y = "Predicted BAA change")

summary(lm(crla.pred$pred~crla.pred$CHNG_BAA))

cor(x=crla.pred$pred, y=crla.pred$CHNG_BAA, use="pairwise.complete.obs")























