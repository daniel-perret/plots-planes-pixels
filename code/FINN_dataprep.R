## Preparing Oregon FIA data for ingestion into FINN model
## 08 August 2025
## D. Perret

## (1) Download FIA data for the state of Oregon

### using rFIA

library(rFIA)

dir.path = "/your/data/directory/here/" # <- change this to match your file structure

rFIA::getFIA(states = "OR",
             dir = dir.path,
             common = TRUE)

### using the FIA DataMart

#### This is the way that I tend to access data. https://apps.fs.usda.gov/fia/datamart/datamart.html
#### Navigate to the state you want to download, and there's an option to download all state tables as .csv files in a zip archive.

## (2) Load FIA data into data object

fia <- readFIA(dir = dir.path,
               common = t,
               states = "OR")

## (3) Putting together tree data

tree.dat <- fia$TREE %>% 
  filter(INVYR > 2000, # only take trees from annual inventories
         DIA > 5) %>%  # only count trees on subplots 
  left_join(fia$PLOT %>% 
              select(CN, MEASYEAR),
            by = c("PLT_CN" = "CN")) %>% 
  mutate(treeID = paste(COUNTYCD, PLOT, SUBP, TREE, sep="_"), # unique IDs that are temporally consistent
         siteID = paste(COUNTYCD, PLOT,sep="_"),
         patchID = paste(COUNTYCD, PLOT,SUBP,sep="_"),
         dbh = DIA*2.54, # converting dbh from in to cm
         status = case_when(STATUSCD == 1 & is.na(PREV_TRE_CN) ~ "new", # trees that are alive and not recorded prior
                            STATUSCD == 1 & !is.na(PREV_TRE_CN) ~ "alive",
                            STATUSCD == 2 ~ "dead"),
         mort_cause = case_when(STATUSCD==2 & AGENTCD %in% c(00,70) ~ "unknown",
                                STATUSCD==2 & AGENTCD == 10 ~ "insect",
                                STATUSCD==2 & AGENTCD == 20 ~ "disease",
                                STATUSCD==2 & AGENTCD == 30 ~ "fire",
                                STATUSCD==2 & AGENTCD == 40 ~ "animal",
                                STATUSCD==2 & AGENTCD == 50 ~ "weather",
                                STATUSCD==2 & AGENTCD == 60 ~ "competition",
                                STATUSCD==2 & AGENTCD == 80 ~ "land use",
                                STATUSCD==2 & is.na(AGENTCD) & 
                                  (PREV_STATUS_CD==1 | 
                                     is.na(PREV_STATUS_CD)) ~ "unknown")) %>%
  select(siteID, patchID, treeID, year = MEASYEAR,
         species = SPCD, dbh, status, mort_cause) #note: species codes can be converted to full names using the species reference table found here: https://usfs-public.app.box.com/v/FIA-TreeSpeciesList/folder/290601914132

## (4) site data

site.dat <- fia$PLOT %>% 
  mutate(siteID = paste(COUNTYCD, PLOT, sep="_")) %>% 
  filter(INVYR > 2000,
         PLOT_STATUS_CD == 1, # plots that were actually measured
         siteID %in% tree.dat$siteID) %>% # just making sure data are consistent between tables
  select(siteID,
         year = MEASYEAR,
         x = LON,
         y = LAT)
