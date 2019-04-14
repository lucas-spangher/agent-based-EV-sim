setwd("C:/Users/Gordon/Documents/EVsim")
require(ggplot2) 
require(reshape)
require(dplyr)
require(RColorBrewer)

sim_comp_all <- list(readRDS('sim_comp_1.RDS'),
                     readRDS('sim_comp_2.RDS'),
                     readRDS('sim_comp_3.RDS'))

sim_comp_all[[2]]$run <- sim_comp_all[[2]]$run + 100
sim_comp_all[[3]]$run <- sim_comp_all[[3]]$run + 200
sim_comp_all <- do.call('rbind',sim_comp_all)

sim_phev_decarb <- readRDS('sim_phev_decarb.RDS')
sim_comp_phev <- data.frame(do.call('rbind',sim_phev_decarb),scen='phev_decarb',run=rep(1:length(sim_phev_decarb),each=nrow(sim_phev_decarb[[1]])))
sim_comp_phev$year <- sim_comp_phev$scaleYear + 2010
sim_comp_all$scen <- as.character(sim_comp_all$scen)
sim_comp_all <- rbind(sim_comp_all,sim_comp_phev)

form <- as.formula(paste0('cbind(',paste(names(sim_comp_all)[2:(ncol(sim_comp_all)-3)],collapse=','),')~scen+year'))
sim_comp_all[is.na(sim_comp_all)] <- 0
sim_means <- aggregate(form,sim_comp_all,mean,na.rm=T)
sim_max <- aggregate(form,sim_comp_all,max,na.rm=T)
sim_min <- aggregate(form,sim_comp_all,min,na.rm=T)
sim_co2 <- data.frame(year=sim_means$year,scen=sim_means$scen,
                      co2_mean=sim_means$co2emissions,co2_max=sim_max$co2emissions,co2_min=sim_min$co2emissions)

saveRDS(sim_comp_all,'sim_comp_300.RDS')
saveRDS(sim_co2,'sim_co2_300.RDS')

# EV scenario
sim_co2$elec <- 'Current electricity trends'
sim_co2$elec[grepl('decarb',sim_co2$scen)] <- 'Rapid decarbonization'
ggplot(subset(sim_co2,grepl('f0.*FALSE|phev',scen))) +
  geom_line(aes(x=year,y=co2_mean/1e9,color=gsub('.*0_|_FA.*|_decarb','',scen),group=scen),lwd=1) +
  facet_wrap(~elec) +
  # geom_point(data=subset(sim_co2,grepl('f0.*FALSE',scen) & year%%10==0),
  # aes(x=year,y=co2_mean/1e9,color=gsub('.*0_|_FA.*','',scen)),size=2) +
  geom_ribbon(aes(x=year,ymax=co2_max/1e9,ymin=co2_min/1e9,fill=gsub('.*0_|_FA.*|_decarb','',scen),group=scen),alpha=0.2) +
  scale_fill_manual(name='EV scenario',
                    labels=c('Base case','No EVs','100% penetration - fast','100% penetration - slow','PHEV','Base penetration - slow'),
                    values=brewer.pal(6,'Dark2')[c(5,2,3:4,1,6)]) +
  scale_color_manual(name='EV scenario',
                     labels=c('Base case','No EVs','100% penetration - fast','100% penetration - slow','PHEV','Base penetration - slow'),
                     values=brewer.pal(6,'Dark2')[c(5,2,3:4,1,6)]) +
  ylim(c(0,2)) +
  theme_bw() +
  labs(x='Year',y='CO2 emissions (billion tons equivalent)')
ggsave('evscen_comp 0319.jpg')


# electricity scenarios
ggplot(carbon) +
  geom_line(aes(x=year,y=bau,color='Current electricity trends'),lwd=1) +
  geom_line(aes(x=year,y=decarb,color='Rapid decarbonization'),lwd=1) +
  labs(x='Year',y='Carbon intensity of electricity (gCO2/kWh)') +
  scale_color_discrete(name='Scenario') +
  theme_bw() +
  theme(legend.position = c(0.75,0.85))
ggsave('elec_scen.jpeg')
