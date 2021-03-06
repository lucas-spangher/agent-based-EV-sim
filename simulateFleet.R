setwd("C:/Users/Gordon/Documents/EVsim")
# install.packages('Kmisc');install.packages('bayesm');install.packages('arm');install.packages('fGarch')
require(ggplot2) 
require(Kmisc)
require(bayesm)
require(arm)
require(fGarch)
require(reshape)
require(dplyr)
load("sim_gb.rdata")

# predDFs <- predDFs[-(1:14),]
# predPropsS <- predPropsS[-(1:14),]
# 
# toLong <- function(df,cols=16:21) {
#   data.frame(year=rep(START_YEAR:END_YEAR,length(cols)),
#              co2=as.numeric(as.matrix(df[,cols])),
#              type=factor(rep(names(df)[cols],each=nrow(df))))
# }



sim_all <- NULL
co2_long_all <- NULL
co2_list <- NULL
sim_list <- NULL
it <- 200
for(r in 1:100 + it) {
  print(r)
  sim_all <- NULL
  
  
  for(ev in c('ice','ev')[1]) {
    for(elec in c('bau','decarb')) {
      for(fleet in c('f0','f50','f100')) {
        for(hirep in c(F,T)) {
          if(ev=='ev') {
            for(scen in c('base','high_fast','slow','high_slow')) {
              set.seed(r)
              nm <- paste(ev,elec,fleet,scen,hirep,sep='_');print(nm)
              sim_all[[nm]] <- simulateFleet(ev_ms=scen,fleet_scen = fleet,elec_scen = elec,scrappingRate = 1+1*hirep);co2_long_all[[nm]] <- toLong(sim_all[[nm]])
            }
          } else {
            set.seed(r)
            nm <- paste(ev,elec,fleet,hirep,sep='_');print(nm)
            sim_all[[nm]] <- simulateFleet(replacedByEV = NULL,enterSwitch=F,fleet_scen = fleet,elec_scen = elec,scrappingRate = 1+1*hirep);co2_long_all[[nm]] <- toLong(sim_all[[nm]])
          }
        }
      }
    }
  }
  
  set.seed(r);sim_all[['phev']] <- simulateFleet(fleet_scen='f0',phev=0.35);co2_long_all[['phev']] <- toLong(sim_all[['phev']]) # 35% PHEVs
  set.seed(r);sim_all[['base_fleet50_ev']] <- simulateFleet(fleet_scen='f50',fleet_ev=2010);co2_long_all[['base_fleet50_ev']] <- toLong(sim_all[['base_fleet50_ev']]) #
  set.seed(r);sim_all[['base_fleet100_ev']] <- simulateFleet(fleet_scen='f100',fleet_ev=2010);co2_long_all[['base_fleet100_ev']] <- toLong(sim_all[['base_fleet100_ev']]) #
  set.seed(r);sim_all[['car']] <- simulateFleet(replacedByEV = vTypes[1:2]);co2_long_all[['car']] <- toLong(sim_all[['car']]) # Only Car, Car SUV
  set.seed(r);sim_all[['lt']] <- simulateFleet(replacedByEV = vTypes[-(1:2)]);co2_long_all[['lt']] <- toLong(sim_all[['lt']]) # No Cars or Car SUV
  
  sim_list[[r - it]] <- sim_all
  saveRDS(sim_list,paste0('sim_list 0319_',it,'b.RDS'))
}
sim_ev <- readRDS(paste0('sim_list 0319_b.RDS'))
for(i in 1:length(sim_ev)) {
  sim_ev[[i]][grepl('ice',names(sim_ev[[i]]))] <- sim_list[[i]][grepl('ice',names(sim_ev[[i]]))]
}
sim_list <- sim_ev
# sim_list <- readRDS('sim_list.RDS')
sim_comp1 <- lapply(sim_list,function(l)data.frame(do.call('rbind',l),scen=rep(names(l),each=nrow(l[[1]]))))
sim_comp <- data.frame(do.call('rbind',sim_comp1),run=rep(1:length(sim_comp1),each=nrow(sim_comp1[[1]])))
sim_comp$year <- sim_comp$scaleYear + 2010
form <- as.formula(paste0('cbind(',paste(names(sim_comp)[2:(ncol(sim_comp)-3)],collapse=','),')~scen+year'))
sim_comp[is.na(sim_comp)] <- 0
sim_means <- aggregate(form,sim_comp,mean,na.rm=T)
sim_max <- aggregate(form,sim_comp,max,na.rm=T)
sim_min <- aggregate(form,sim_comp,min,na.rm=T)
sim_co2 <- data.frame(year=sim_means$year,scen=sim_means$scen,
                      co2_mean=sim_means$co2emissions,co2_max=sim_max$co2emissions,co2_min=sim_min$co2emissions)

saveRDS(sim_co2,'sim_co2_3.RDS')
saveRDS(sim_comp,'sim_comp_3.RDS')

# Comparison plots----------
library(RColorBrewer)

# EV scenario
sim_co2$elec <- 'Current electricity trends'
sim_co2$elec[grepl('decarb',sim_co2$scen)] <- 'Rapid decarbonization'
ggplot(subset(sim_co2,grepl('f0.*FALSE|phev',scen))) +
  geom_line(aes(x=year,y=co2_mean/1e9,color=gsub('.*0_|_FA.*','',scen),group=scen),lwd=1) +
  facet_wrap(~elec) +
  # geom_point(data=subset(sim_co2,grepl('f0.*FALSE',scen) & year%%10==0),
             # aes(x=year,y=co2_mean/1e9,color=gsub('.*0_|_FA.*','',scen)),size=2) +
  geom_ribbon(aes(x=year,ymax=co2_max/1e9,ymin=co2_min/1e9,fill=gsub('.*0_|_FA.*','',scen),group=scen),alpha=0.2) +
  scale_fill_manual(name='EV scenario',
                    #labels=c('Base case','No EVs','100% penetration - fast','100% penetration - slow','Base penetration - slow','PHEV'),
                    values=brewer.pal(6,'Dark2')[c(5,2,3:4,1,6)]) +
  scale_color_manual(name='EV scenario',
                     labels=c('Base case','No EVs','100% penetration - fast','100% penetration - slow','Base penetration - slow','PHEV'),
                     values=brewer.pal(6,'Dark2')[c(5,2,3:4,1,6)]) +
  ylim(c(0,2)) +
  theme_bw() +
  labs(x='Year',y='CO2 emissions (billion tons equivalent)')
ggsave('evscen_comp 0319.jpg')


# Fleet scenarios
ggplot(subset(sim_co2,grepl('bau.*base_FALSE|bau.*0_FALSE|fleet.*ev',scen))) +
  geom_line(aes(x=year,y=co2_mean/1e9,color=gsub('.*_f|_F.*|_bas.*|.*t|_ev','',scen),lty=grepl('ice',scen),group=scen),lwd=1) +
  geom_point(data=subset(sim_co2,grepl('bau.*base_FALSE|bau.*0_FALSE|fleet.*ev',scen) & year%%10==0),
             aes(x=year,y=co2_mean/1e9,color=gsub('.*_f|_F.*|_bas.*|.*t|_ev','',scen),shape=grepl('_ev',scen)),size=2) +
  geom_ribbon(aes(x=year,ymax=co2_max/1e9,ymin=co2_min/1e9,fill=gsub('.*_f|_F.*|_bas.*|.*t|_ev','',scen),group=scen),alpha=0.2) +
  scale_fill_manual(name='Fleet % of VMT',values=brewer.pal(3,'Set1')) +
  scale_shape_manual(name='Fleet EV mandate',values=c(19,8),labels=c('No mandate','Mandate')) +
  scale_linetype_discrete(name='EV scenario',labels=c('Base forecast','No EVs')) +
  scale_color_manual(name='Fleet % of VMT',values=brewer.pal(3,'Set1')) +
  ylim(c(0,2)) +
  theme_bw() +
  labs(x='Year',y='CO2 emissions (billion tons equivalent)')
ggsave('fleet_comp.jpg')

# Scrappage scenarios
ggplot(subset(sim_co2,grepl('f0_base.*|bau_f0_[A-Z]',scen))) +
  geom_line(aes(x=year,y=co2_mean/1e9,color=gsub('_[A-Z].*','',scen),lty=gsub('.*_[A-Z]','',scen),group=scen),lwd=1) +
  geom_point(data=subset(sim_co2,grepl('f0_base.*|bau_f0_[A-Z]',scen) & year%%10==0),
             aes(x=year,y=co2_mean/1e9,color=gsub('_[A-Z].*','',scen),shape=gsub('.*_[A-Z]','',scen)),size=2) +
  geom_ribbon(aes(x=year,ymax=co2_max/1e9,ymin=co2_min/1e9,fill=gsub('_[A-Z].*','',scen),group=scen),alpha=0.2) +
  scale_fill_manual(name='EV scenario',labels=c('Base forecast','Base penetration,\ndecarbonized electricity','No EVs'),
                    values=brewer.pal(3,'Set1')[c(2,3,1)]) +
  scale_shape_manual(name='Scrappage rate',values=c(19,8),labels=c('1x normal','2x normal')) +
  scale_linetype_discrete(name='Scrappage rate',labels=c('1x normal','2x normal')) +
  scale_color_manual(name='EV scenario',labels=c('Base forecast','Base penetration,\ndecarbonized electricity','No EVs'),
                     values=brewer.pal(3,'Set1')[c(2,3,1)]) +
  ylim(c(0,2)) +
  theme_bw() +
  labs(x='Year',y='CO2 emissions (billion tons equivalent)')
ggsave('scrap_comp.jpg')

# Vehicle type scenarios
ggplot(subset(sim_co2,grepl('bau_f0_base_F|bau_f0_F|^car|lt',scen))) +
  geom_line(aes(x=year,y=co2_mean/1e9,color=scen,group=scen),lwd=1) +
  geom_point(data=subset(sim_co2,grepl('bau_f0_base_F|bau_f0_F|^car|lt',scen) & year%%10==0),
             aes(x=year,y=co2_mean/1e9,color=scen),size=2) +
  geom_ribbon(aes(x=year,ymax=co2_max/1e9,ymin=co2_min/1e9,fill=scen,group=scen),alpha=0.2) +
  scale_fill_manual(name='EV scenario',labels=c('Replace cars first','Base case','No EVs','Replace light trucks first'),
                    values=brewer.pal(4,'Set1')[c(2,3,1,4)]) +
  #scale_shape_manual(name='Scrappage rate',values=c(19,8),labels=c('1x normal','2x normal')) +
  #scale_linetype_discrete(name='Scrappage rate',labels=c('1x normal','2x normal')) +
  scale_color_manual(name='EV scenario',labels=c('Replace cars first','Base case','No EVs','Replace light trucks first'),
                     values=brewer.pal(4,'Set1')[c(2,3,1,4)]) +
  ylim(c(0,2)) +
  theme_bw() +
  labs(x='Year',y='CO2 emissions (billion tons equivalent)')
ggsave('vtype_comp.jpg')


# Source breakdown plots------------
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

library(reshape2)
em_vars <- c('gasoline','electricity','ice_prod','bev_prod','ice_eol','bev_eol')
sim_means_long <- melt(sim_means, id.vars=c("scen", "year"))
emissions_long <- subset(sim_means_long,variable %in% em_vars)
emissions_long$variable <- factor(emissions_long$variable,levels=levels(emissions_long$variable)[c(22,20,18,21,19,17)])
ggplot(subset(emissions_long,scen=='ev_bau_f0_base_FALSE')) +
  geom_area(aes(x=year,y=value/1e9,fill=variable),position='stack') +
  ylim(c(0,2)) +
  theme_bw() +
  scale_fill_manual(name='Emissions source',values=brewer.pal(6,'Set1')[c(1,2,3,6,5,4)],
                    labels=c('BEV scrapping','BEV production','Electricity','ICE scrapping','ICE production','Gasoline')) +
  labs(x='Year',y='CO2 emissions (gigatons equivalent)')
ggsave('emissions breakdown - base.jpg')

ggplot(subset(emissions_long,scen=='base_fleet100_ev')) +
  geom_area(aes(x=year,y=value/1e9,fill=variable),position='stack') +
  ylim(c(0,2)) +
  theme_bw() +
  scale_fill_manual(name='Emissions source',values=brewer.pal(6,'Set1')[c(1,2,3,6,5,4)],
                    labels=c('BEV scrapping','BEV production','Electricity','ICE scrapping','ICE production','Gasoline')) +
  labs(x='Year',y='CO2 emissions (gigatons equivalent)')
ggsave('emissions breakdown - fleet_ev.jpg')


# Cumulative emissions---------
sim_means <- sim_means[order(sim_means$scen,sim_means$year),]
sim_means$cum_co2 <- cumsum(sim_means$co2emissions)
for(i in which(!duplicated(sim_means$scen))[-1]) {
  sim_means$cum_co2[i:nrow(sim_means)] <- sim_means$cum_co2[i:nrow(sim_means)] - sim_means$cum_co2[i-1]
}

sim_cum <- cbind(subset(sim_means,year==2050)[c('scen','cum_co2','co2emissions')],
                 subset(sim_means,year==2100)[c('cum_co2','co2emissions')])
names(sim_cum) <- c('scenario','cum_co2_2050','co2_2050','cum_co2_2100','co2_2100')
sim_cum <- sim_cum[order(sim_cum[,2]),]

sim_cum$elec_decarb <- grepl('decarb',sim_cum$scenario)
sim_cum$high_scrap <- grepl('TRUE',sim_cum$scenario)
sim_cum$fleet_scen <- 50*grepl('50',sim_cum$scenario) + 100*grepl('100',sim_cum$scenario)
sim_cum$diffusion <- gsub('.*0_|_[A-Z].*','',sim_cum$scenario)
sim_cum$diffusion <- gsub('lt|car|ev','base',sim_cum$diffusion)
sim_cum$diffusion <- gsub('FALSE|TRUE','ice_only',sim_cum$diffusion)
sim_cum$fleet_mandate <- grepl('_ev$',sim_cum$scenario)
sim_cum
write.csv(sim_cum,'result summary.csv',row.names=F)

sim_sel <- subset(sim_cum,scenario %in% c('base_fleet100_ev','ev_decarb_f0_high_fast_FALSE','ev_decarb_f0_base_FALSE','ev_bau_f100_base_FALSE',
                               'ev_bau_f0_base_FALSE','ev_bau_f0_base_TRUE','ev_bau_f0_high_fast_FALSE'))
sim_sel[,2:5] <- sapply(rbind(sim_sel[,2:5],sim_cum[nrow(sim_cum),2:5]),function(v) (1 - v/v[length(v)])[-length(v)])
sim_sel <- sim_sel[order(sim_sel[,2],decreasing=T),]
sim_sel$scenario <- factor(sim_sel$scenario,levels=sim_sel$scenario)
levels(sim_sel$scenario) <- c('Base case','2x scrapping','Faster EV\ndiffusion',
                              '100% fleet\n(base EV diffusion)','Decarbonized grid\n(base EV diffusion)',
                              'Decarbonized grid\nand faster EV diffusion',
                              '100% fleet and\nfleet EV mandate') %>% rev
ggplot(sim_sel) +
  geom_bar(aes(x=scenario,y=100*cum_co2_2100,fill='2100'),stat='identity',position = 'dodge') +
  geom_bar(aes(x=scenario,y=100*cum_co2_2050,fill='2050'),stat='identity',position = 'dodge') +
  scale_fill_manual(name='Year',values=brewer.pal(3,'Dark2')[1:2]) + 
  labs(x='Scenario',y='Cumulative emissions reductions (%)') +
  theme_classic() +
  coord_flip() +
  theme(legend.position = c(0.8,0.8))
ggsave('cum_reductions.jpg')

# EV scenarios
head(ev_curves)
ev_curves_long <- melt(ev_curves,id.vars = 'year')
ggplot(ev_curves_long) +
  geom_line(aes(x=year+5,y=value,color=variable,group=variable),lwd=1) +
  geom_point(data=bnef,aes(x=year,y=ms,shape='BNEF projection'),size=2) +
  
  labs(x='Year',y='EV market share (% of new vehicles)') +
  scale_color_discrete(name='Scenario',labels=c('Base case','Slow diffusion','Slow diffusion, high saturation','Fast diffusion, high saturation')) +
  theme_classic() +
  scale_shape_discrete(name='') +
  theme(legend.position = c(0.8,0.25))
ggsave('ev curves.jpg')
