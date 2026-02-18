df <- data.frame(values = values(ocho.pred.lcms %>%  
                                   terra::subset(., paste0(2021:2023)))) %>%
                                   #app(.,sum,na.rm=T))) %>% 
  pivot_longer(cols=where(is.numeric)) %>% 
  na.omit()

breaks <- quantile(df$value,
           probs = c(0,0.25,0.5,0.75,1))

df$quartile = cut(df$value,
                  breaks = breaks,
                  labels = c("high","cons","mod","low"))

df %>% 
  ggplot(.,
         aes(x = value,
             fill = quartile)) +
  geom_histogram() +
  lims(x = c(-15,5))+
  scale_color_manual(values = c("low" = "tan",
                                "mod" = "gold2",
                                "cons" = "orange",
                                "high" = "firebrick2"),
                     aesthetics = c("fill"),
                     name = "Severity class") +
  geom_vline(xintercept = breaks[2:4]) +
  theme(legend.position = "none") +
  labs(x = "dBAH",
       y = "count")
               
df %>% 
  ggplot(.,
         aes(x = value)) +
  geom_histogram() +
  lims(x = c(-120,15))+
  theme(legend.position = "none") +
  labs(x = "dBAH",
       y = "count")
               