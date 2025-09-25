mpb.dat <- malh.est.bah %>% 
  left_join(malh.extract2 %>% 
              select(-REMPER), by = "PLT_CN") %>% 
  mutate(case = "mpb",
         case2 = "malh") %>% 
  bind_rows(klam.est.bah %>% 
              left_join(klam.extract2 %>% 
                          select(-REMPER), by = "PLT_CN") %>% 
              mutate(case = "mpb",
                     case2 = "klam"))

psme.dat <- psme.est.bah %>% 
  left_join(psme.extract2 %>% 
              select(-REMPER), by = "PLT_CN")



mpb.dat %>% 
  filter(PLT_CN %in% c(malh.filt2,klam.filt2)) %>% 
  ggplot() + 
  # geom_point(aes(x = nbri_t2,
  #                y = CURR_BAA),
  #            pch = 19,
  #            alpha = 0.3,
  #            size = 1) +
  # geom_point(aes(x = nbri_t1,
  #                y = PREV_BAA),
  #            pch = 19,
  #            alpha = 0.3,
  #            size = 1) +
  geom_segment(aes(x = nbri_t1/1000,
                   xend = nbri_t2/1000,
                   y = PREV_BAA,
                   yend = CURR_BAA,
                   col = "MPB"),
               alpha = 0.7,
               lwd=1,
               arrow = arrow(angle=25, length = unit(0.1, "inches"), type = "closed")) +
  
  # geom_segment(data = mpb.dat %>% 
  #                filter(PLT_CN %in% c(malh.filt2,klam.filt2)),
  #              aes(x = nbri_t1,
  #                  xend = nbri_t2,
  #                  y = PREV_BAA,
  #                  yend = CURR_BAA,
  #                  col = case2),
  #              alpha = 0.7,
  #              arrow = arrow(angle = 25, length = unit(0.14, "inches"),type = "closed"),
  #              lwd = 1.3)+
  
  geom_segment(data = psme.dat %>% 
                 filter(PSME.prop>0.5),
               aes(x = nbri_t1/1000,
                   xend = nbri_t2/1000,
                   y = all.BAA.prev,
                   yend = all.BAA.curr,
                   col = "Medford PSME"),
               alpha = 0.7,
               lwd=1,
               arrow = arrow(angle=25, length = unit(0.1, "inches"), type = "closed")) +
  # geom_point(data = mpb.dat %>% 
  #              filter(PLT_CN %in% c(malh.filt2,klam.filt2)),
  #            aes(x = nbri_t2,
  #                y = CURR_BAA),
  #            pch = 19,
  #            alpha = 0.6,
  #            size = 3,
  #            col = "firebrick3") +
  # geom_point(data = mpb.dat %>% 
  #              filter(PLT_CN %in% c(malh.filt2,klam.filt2)),
  #            aes(x = nbri_t1,
  #                y = PREV_BAA),
  #            pch = 19,
  #            alpha = 0.6,
  #            size = 3,
  #            col = "dodgerblue3") +
  labs(x = "Normalized Burn Ratio (NBR)",
       y = "Stand Basal Area (sq. ft/ac)") +
  scale_color_manual(values = c("Medford PSME" = "red",
                                "MPB" = "black"),
                     aesthetics = "col",
                     name = "Case study")
  


