prop.impact <- 0.0026
n.impact <- 2.8
prop.sample <- 0.1
n.plots <- 1087

dat <- data.frame(id = 1:n.plots,
                  impact = 0)
dat$impact[sample(1:n.plots, size = round(n.plots*prop.impact))] <- 1
#dat$impact[sample(1:n.plots, size = n.impact)] <- 1

out.det <- c()
out.imp <- c()

for(i in 1:10000){
  
  det <- 0
  imp <- 0
  iter.det <- 0
  iter.imp <- 0
  sum <- 0
  rows <- 1:n.plots
  
  while(det==0 | imp==0){
    
    if(det==0) {iter.det <- iter.det+1}
    if(imp==0) {iter.imp <- iter.imp+1}
    
    if(length(rows)>round(n.plots*prop.sample)){
          samp <- sample(rows, size = round(n.plots*prop.sample))
    } else {
      samp <- rows
    }
    
    if(sum(dat$impact[samp])>0) {
      
      det <- 1
      print("success")
      sum <- sum+sum(dat$impact[samp])
      
      rows <- rows[-which(rows%in%samp)]
      
      if(sum >= round(prop.impact*n.plots)){
        imp <- 1
      }
      
    } else {

      rows <- rows[-which(rows%in%samp)]
      
      print("failure")
      
    }
  }
  
  out.det <- c(out.det,iter.det)
  out.imp <- c(out.imp, iter.imp)
  
}

hist(out.det)
#hist(out.imp)

mean(out.det)
#mean(out.imp)


data.frame(yr = out.det) %>% 
  ggplot() +
  geom_histogram(aes(x = yr),
                 bins = 10,
                 fill = "gray65") +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Years until detection",
       y = "Count") +
  geom_vline(xintercept = mean(out.det),
             col = "red", lwd = 1.7)
  













