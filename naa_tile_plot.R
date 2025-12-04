################################################################################################
# plot ftn for NAA Proc Error
################################################################################################
library(tidyr); library(ggplot2)
plot_naa_re <- function(mods=NULL, rec_age=2, save.name=NULL, plot.dir=NULL)  {
  names(mods) <- save.name
  years = mods[[1]]$years_full[-1]
  n_years = length(years)
  n_ages = mods[[1]]$env$data$n_ages
  ages <- rec_age:(rec_age+n_ages-1)
  
  
  df.NAA <- data.frame(matrix(NA, nrow=0, ncol=n_ages+2))
  colnames(df.NAA) <- c(paste0("Age_",1:n_ages),"Year", "Model")
  
  for(i in 1:length(mods)){
    tmp = data.frame(mods[[i]]$rep$NAA_devs[1,1,2:(n_years+1),])
    tmp$Year <- years
    colnames(tmp) <- c(paste0("Age_",ages),"Year")
    tmp$Model = names(mods)[i]
    df.NAA <- rbind(df.NAA, tmp)
  }
  
  write.csv(df.NAA, file=file.path(plot.dir, paste0(save.name,"_NAA_RE.csv")))
  
  df.plot <- df.NAA %>% tidyr::pivot_longer(-c(Year,Model),
                                            names_to = "Age",
                                            names_prefix = "Age_",
                                            names_transform = list(Age = as.integer),
                                            values_to = "NAA_re")
  
  png(filename = file.path(plot.dir, paste0(save.name,"_NAA_RE.png")), width = 8, height = 8.5, res = 200, units='in')
  print(ggplot(df.plot, ggplot2::aes(x=Year, y=Age)) +
          geom_tile(aes(fill=NAA_re)) +
          scale_x_continuous(expand=c(0,0)) +
          scale_y_continuous(expand=c(0,0), breaks=ages) +
          coord_cartesian(ylim = c(rec_age-0.5, rec_age+n_ages-0.5)) +
          theme_bw() +
          facet_grid(rows=vars(Model), drop=F) +
          scale_fill_gradient2(name = "", low = scales::muted("blue"), mid = "white", high = scales::muted("red")) 
  )
  dev.off()
  
  ## same plot but without age rec since they are so strong:
  png(filename = file.path(plot.dir, paste0(save.name, "_noRecr_NAA_RE.png")), width = 8, height = 8.5, res = 200, units='in')
  print(ggplot(df.plot[df.plot$Age>rec_age,], ggplot2::aes(x=Year, y=Age)) +
          geom_tile(aes(fill=NAA_re)) +
          scale_x_continuous(expand=c(0,0)) +
          scale_y_continuous(expand=c(0,0), breaks=ages) +
          theme_bw() +
          facet_grid(rows=vars(Model), drop=F) +
          scale_fill_gradient2(name = "", low = scales::muted("blue"), mid = "white", high = scales::muted("red")) 
  )
  dev.off()
}
mod <- readRDS('base_north.rds')

plot_naa_re(mods=list(mod),rec_age=2, save.name = "Base_North", plot.dir=paste0(getwd(),"/plots/"))



