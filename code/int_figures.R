mpb.dat %>% 
  bind_cols(predict(m0,se = T)) %>% 
  ggplot(.,
         aes(x = d_nbr_tot/1000,
             y = (CHNG_BAA_TOT_ha),
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
       y = "Basal area change (m2/ha)")+
  theme(legend.position="none") +
  lims(y = c(-55,25),
       x = c(-0.6,0.5))



ggeffects::ggpredict(model = m1, 
                     terms = c("d_nbr_tot [-750:500, by = 10]",
                               "PSME.prop [0.25, 0.5, 0.75, 1]")) %>% 
  rename(d_nbr_tot = x,
         PSME.prop = group) %>% 
  ggplot(.,
         aes(x = d_nbr_tot/1000,
             y = predicted,
             group = PSME.prop)) +
  geom_point(inherit.aes=F,
             data = psme.dat %>% 
               mutate(prop.group = case_when(PSME.prop < 0.25 ~ 0.25,
                                             PSME.prop < 0.5 & PSME.prop >= 0.25 ~ 0.5,
                                             PSME.prop < 0.75 & PSME.prop >= 0.5 ~ 0.75,
                                             PSME.prop <= 1 & PSME.prop >= 0.75 ~ 1)) %>% 
               na.omit(),
             aes(x = d_nbr_tot/1000,
                 y = CHNG_BAA_TOT_ha,
                 col = as.character(prop.group)),
             pch = 19, size = 3, alpha = 0.4) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = PSME.prop),
              alpha = 0.1) +
  geom_line(aes(col = PSME.prop),
            alpha = 0.8,
            lwd=2) +
  scale_color_manual(values = c("0.25" = "darkgoldenrod4",
                                "0.5" = "darkgoldenrod2",
                                "0.75" = "darkolivegreen3",
                                "1" = "forestgreen"),
                     aesthetics = c("fill","col"),
                     name = "PSME proportion") + 
  labs(x = "dNBR", y = "Net basal area change") +
  theme(legend.position = "none") +
  lims(y = c(-55,25))
,
       x = c(-0.6,0.5))



ggeffects::ggpredict(model = m2, 
                     terms = c("d_nbr_tot [-200:200, by = 10]",
                               "abies.prop.prev [0.25, 0.5, 0.75, 1]")) %>% 
  rename(d_nbr_tot = x,
         abies.prop.prev = group) %>% 
  ggplot(.,
         aes(x = d_nbr_tot/1000,
             y = predicted,
             group = abies.prop.prev)) +
  geom_point(inherit.aes=F,
             data = ocho.dat %>% 
               mutate(prop.group = case_when(abies.prop.prev < 0.25 ~ 0.25,
                                             abies.prop.prev < 0.5 & abies.prop.prev >= 0.25 ~ 0.5,
                                             abies.prop.prev < 0.75 & abies.prop.prev >= 0.5 ~ 0.75,
                                             abies.prop.prev <= 1 & abies.prop.prev >= 0.75 ~ 1)) %>% 
               na.omit(),
             aes(x = d_nbr_tot/1000,
                 y = CHNG_BAA_TOT_ha,
                 col = as.character(prop.group)),
             pch = 19, size = 3, alpha = 0.4) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = abies.prop.prev),
              alpha = 0.1) +
  geom_line(aes(col = abies.prop.prev),
            alpha = 0.8,
            lwd=2) +
  scale_color_manual(values = c("0.25" = "darkgoldenrod4",
                                "0.5" = "darkgoldenrod2",
                                "0.75" = "darkolivegreen3",
                                "1" = "forestgreen"),
                     aesthetics = c("fill","col"),
                     name = "Abies proportion") + 
  labs(x = "dNBR", y = "Net basal area change (HOST)") +
  theme(legend.position="none") +
  lims(y = c(-55,25),
       x = c(-0.6,0.5))
