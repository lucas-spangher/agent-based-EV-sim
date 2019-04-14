setwd("C:/Users/Gordon/Documents/EVsim")
install.packages('Kmisc');install.packages('bayesm');install.packages('arm');install.packages('fGarch')
require(ggplot2) 
require(Kmisc)
require(bayesm)
require(arm)
require(fGarch)
require(reshape)
require(dplyr)
load("sim_gb.rdata") #GB: file doesn't exist

######### Poisson process model ########## 

####### Scaffolding new data 
START_YEAR=2010
END_YEAR=2100

newYears = c(START_YEAR:END_YEAR) 

newDF=data.frame(years=newYears)

### 1. PREDICTING FUEL EFFICIENCY -------------------------------------------------- 
# read in data 

fuelDF = read.csv("FuelDB.csv")
fuelDF=fuelDF[-1,]

names(fuelDF)=gsub("[.]", "", names(fuelDF))
names(fuelDF)[names(fuelDF)%in%c("X","X1","X2","X3")]=c("LabE","CityE","HwyE","CombE")
names(fuelDF) = gsub("X","", names(fuelDF))

fuelDF = lapply(fuelDF, function(var){
    if (grepl("%", var[6])){
        var = gsub("%","",var)
        var = as.numeric(as.character(var))
        var = var*.01
    }
    if (sum(!is.na(as.numeric(substring(var,1,1))))){
             var= as.numeric(as.character(var))
    }
    return(var)
    }) %>% data.frame

allDF = fuelDF

fuelDF=fuelDF[fuelDF$ModelYear>START_YEAR-1,] # leaving everything before 1996 out 
fuelDF=fuelDF[fuelDF$VehicleType!="All",] # leaving aggregate counts out 
fuelDF$scaleYear = fuelDF$ModelYear-min(fuelDF$ModelYear,na.rm=T)
fuelDF=fuelDF[!is.na(fuelDF$VehicleType),]
fuelDF=fuelDF[!is.na(fuelDF$ModelYear),]
fuelDF=droplevels(fuelDF)
fuelDF$after2005 = fuelDF$ModelYear>2005
    
### predicting efficiency fits 

predYear= c(0:(END_YEAR-START_YEAR))
types = unique(fuelDF$VehicleType)
predDF = expand.grid(predYear,types)
predDF= droplevels(data.frame(scaleYear = predDF[,1], VehicleType = predDF[,2]))
#pred= predict(reslm, newdata = predDF) #sin(2*pi/per*fuelDF$scaleYear), cos(2*pi/per*fuelDF$scaleYear)))
#predDF$pred=pred

# bayes hierarchical regression 

vehicleType = as.data.frame(model.matrix(~0+fuelDF$VehicleType, contrasts.arg=unique(fuelDF$VehicleType)))

regdata = apply(vehicleType, 2, FUN=function(Type){
    Type=Type*c(1:nrow(fuelDF))
    y = fuelDF$CombE[Type]
    X = cbind(1,fuelDF$scaleYear[Type], fuelDF$after2005[Type], fuelDF$scaleYear[Type]*fuelDF$after2005[Type])
    return(list(y=y,X=X))
    })

Data=list(regdata= regdata)
Mcmc= list(R=2000, keep=1)

hierMod = rhierLinearModel(Data=Data,Mcmc=Mcmc)
means = as.data.frame(apply(hierMod$betadraw,c(1,2),mean))
row.names(means)=unique(fuelDF$VehicleType)

#predicting 
predDF1 = predDF
predDF1$after2005=predDF1$scaleYear>2005-min(fuelDF$ModelYear,na.rm=T)
predDF1$pred = means$V1[predDF1$VehicleType]+
                    means$V3[predDF1$VehicleType]*predDF1$after2005+ 
                    (means$V2[predDF1$VehicleType]+means$V4[predDF1$VehicleType]*predDF1$after2005)*predDF1$scaleYear

predDFs=predDF1
predDFs$after2005=NULL
predDFs = reshape(predDFs, timevar = "VehicleType", 
                        idvar= "scaleYear", direction = "wide")
names(predDFs)=gsub("pred.","",names(predDFs))
predDFs$scaleYear= NULL

predDFs_long <- data.frame(yr=rep(1:nrow(predDFs),ncol(predDFs)),
                                        val=as.numeric(as.matrix(predDFs)),
                                        type=rep(names(predDFs),each=nrow(predDFs)))
ggplot(predDFs_long) +
  geom_line(aes(x=yr,y=val,color=type,group=type))
# GB: I don't understand why we're using baye's regression, but the result seems fine.
# GB: Not sure if it makes sense to base fuel efficiency on CAFE standards, i.e. if standards determine consumption, more EVs would simply mean lower efficiency in the rest of the fleet. Maybe just clarify we're assuming ICEV efficiency will improve to track planned CAFE before Trump, which seemed like a reasonable estimate of technical capability (add citation).
######## Conclusion: use this predicted variable. 

########### PREDICT VEHICLE SALES TO DO VEHICLE PROPORTIONS---------------
require(reshape)
require(nlme)

proportions = read.csv("PeriodSalesProportions.csv")
row.names(proportions)=proportions[,1]
proportions=proportions[,-1]
yearsToExclude=c("X","X1980","X1985","X1990","X1991","X1992","X1993","X1994","X1995")
proportions[,names(proportions)%in% yearsToExclude]=NULL
names(proportions)=gsub("X","",names(proportions))
proportions=na.omit(proportions)
proportions=as.matrix(proportions)/100
t.prop = as.data.frame(t(proportions))
t.prop=log(t.prop/(1-t.prop))

t.prop$time=row.names(t.prop)

prop = melt(t.prop, id="time")
prop$time=as.numeric(prop$time)
prop$scaleYear=prop$time-min(prop$time)
names(prop)[names(prop)=="variable"]="VehicleType"

propMod = lmList(value~scaleYear|VehicleType, data= prop)

predProps= data.frame(scaleYear=predDF1$scaleYear, VehicleType = predDF1$VehicleType)
predProps$props = predict(propMod, predProps)
predProps$props= exp(predProps$props)/(1+exp(predProps$props))

# putting them in proportions
predPropsS = reshape(predProps, timevar = "VehicleType", 
                        idvar= "scaleYear", direction = "wide")
totals= rowSums(predPropsS[,!names(predPropsS)%in%c("scaleYear")])
tempYear=predPropsS$scaleYear
predPropsS = predPropsS/totals
predPropsS$scaleYear = tempYear
predPropsS$scaleYear=NULL
names(predPropsS)=gsub("props.","",names(predPropsS))

predPropFinal = stack(predPropsS, select=names(predPropsS)[!names(predPropsS)%in%c("scaleYear")])
names(predPropFinal)=c("prop","VehicleType")
predPropFinal$VehicleType=gsub("props.","",predPropFinal$VehicleType)

ggplot(data.frame(predPropFinal,yr=rep(tempYear,5))) +
  geom_line(aes(x=yr,y=prop,color=VehicleType,group=VehicleType))
# VMT by age--------------------

VMTcsv = read.csv("vmt_byage.csv")
VMTcsv$Age = VMTcsv$X
VMT_by_age = as.data.frame(apply(VMTcsv, 2, function(var){
    var = gsub(",","",var)
    var = as.numeric(as.character(var))
    return(var)
    }))
row.names(VMT_by_age) = VMT_by_age$Age
names(VMT_by_age) = gsub("[.]"," ", names(VMT_by_age))
# VMT_by_age$EV= VMT_by_age$Car
VMT_by_age$"X 1"=NULL
VMT_by_age$X=NULL
VMT_by_age = melt(VMT_by_age, id="Age")
names(VMT_by_age)[names(VMT_by_age)=="variable"] = "VehicleType"

VMT_rates <- c(1,VMT_by_age[-1,3]/VMT_by_age[-nrow(VMT_by_age),3])
VMT_rates[!duplicated(VMT_by_age$VehicleType)] <- 1
summary(VMT_rates)
VMT_by_age$rate <- VMT_rates
VMT_by_age <- subset(VMT_by_age,VehicleType!='Other truck')
ggplot(subset(VMT_by_age,!grepl('Other',VehicleType))) +
  geom_line(aes(x=Age,y=value,color=VehicleType,group=VehicleType))



# ANL efficiency estimates------------------
logistic <- function(x,x_0,k,L) {
  L/(1+exp(-k*(x-x_0)))
}
scrap_probs <- read.csv('scrap probs.csv')
scrap_probs$scrap_fleet <- logistic(0:31,2,2,1)

eff <- read.csv('efficiency.csv')
prod_emissions <- read.csv('production emissions.csv')
eol_emissions <- read.csv('eol emissions.csv')

head(eff)
eff0 <- eff
eff <- subset(eff0,grepl('avg',scenario)|year<2020)
names(eff) <- gsub('efficiency','avg',names(eff))
eff <- cbind(eff,subset(eff0,grepl('high',scenario)|year<2020)[,grepl('efficiency',names(eff0))])
names(eff) <- gsub('efficiency','high',names(eff))
eff <- cbind(eff,subset(eff0,grepl('low',scenario)|year<2020)[,grepl('efficiency',names(eff0))])
names(eff) <- gsub('efficiency','low',names(eff))

eff <- cbind(eff,abs(eff[,grepl('low',names(eff))] - eff[,grepl('high',names(eff))])/4)
names(eff)[ncol(eff)-2:0] <- gsub('low','sd',names(eff)[ncol(eff)-2:0])
eff$scenario <- NULL
summary(eff)

carbon <- read.csv('carbon_intensity.csv')
carbon$year <- 2016:2050
carbon <- carbon[,-(1:2)]
names(carbon)[1] <- 'bau'
carbon2 <- data.frame(year=2051:2100)
carbon2$bau <- predict(lm(bau~year,carbon),newdata=carbon2)
carbon <- rbind(carbon,carbon2)
carbon <- rbind(data.frame(bau=471,year=2010:2015),carbon)
carbon$decarb <- logistic(1:nrow(carbon),16,-.2,471)

plot(carbon$year,carbon$carbon_intensity,type='l')

head(eff)

eff_full <- data.frame(vehType=rep(unique(eff$vehType)[-1],each=36),
                       year=rep(2010:2045,length(unique(eff$vehType)[-1])))
i=21
for(i in 1:nrow(eff_full)) {
  di=5
  if(eff_full$year[i]>2029) di=15
  sel <- subset(eff,vehType==eff_full$vehType[i] & abs(year - eff_full$year[i])<di & (di<10 | year>2029))
  
  
  for(var in names(sel)[grepl('avg|high|low',names(sel))]) {
    
    eff_full[[var]][i] <- weighted.mean(sel[[var]],di - abs(sel$year - eff_full$year[i]))
    
  }
  
}

v=eff_full$avg_bev;type='Car'
eff_pred <- lapply(eff_full[grepl('avg|high|low',names(eff_full))],function(v) {
  dt <- NULL
  for(type in unique(eff_full$vehType)) {
    test <- data.frame(eff=v[eff_full$vehType==type],y=eff_full$year[eff_full$vehType==type])
    pred <- predict(lm(eff~y,test),newdata=data.frame(y=2046:2100))*0:54/55 + subset(test,y==2045)$eff*55:1/55
    dt <- c(dt,pred)
  }
  dt
})

eff_full <- rbind(eff_full,data.frame(vehType=rep(unique(eff_full$vehType),each=55),
                                      year=rep(2046:2100,length(unique(eff_full$vehType))),eff_pred))

eff_full <- eff_full[order(eff_full$vehType,eff_full$year),]
# 
# ggplot(eff_full) +
#   geom_line(aes(x=year,y=avg_bev,color=vehType,group=vehType))
# 
# ggplot(eff_full) +
#   geom_line(aes(x=year,y=avg_ice,color=vehType,group=vehType))


eff_full$carbon_bau <- carbon$bau
eff_full$carbon_decarb <- carbon$decarb

eff_full0 <- eff_full
# eff_full <- eff_full0
eff_full$avg_bev_bau <- eff_full0$avg_bev*eff_full0$carbon_bau/1000
eff_full$high_bev_bau <- eff_full0$high_bev*eff_full0$carbon_bau/1000
eff_full$low_bev_bau <- eff_full0$low_bev*eff_full0$carbon_bau/1000

eff_full$avg_bev_decarb <- eff_full0$avg_bev*eff_full0$carbon_decarb/1000
eff_full$high_bev_decarb <- eff_full0$high_bev*eff_full0$carbon_decarb/1000
eff_full$low_bev_decarb <- eff_full0$low_bev*eff_full0$carbon_decarb/1000

eff_full$avg_ice <- 1/eff_full0$avg_ice*12000
eff_full$high_ice <- 1/eff_full0$high_ice*12000
eff_full$low_ice <- 1/eff_full0$low_ice*12000

eff_full$avg_turbo <- 1/eff_full0$avg_turbo*12000
eff_full$high_turbo <- 1/eff_full0$high_turbo*12000
eff_full$low_turbo <- 1/eff_full0$low_turbo*12000

eff_full[eff_full$year>2045,grepl('ice|turbo',names(eff_full))] <- eff_full[eff_full$year==2045,grepl('ice|turbo',names(eff_full))][rep(1:5,each=55),]
eff_full$cafe_ice <- 1/subset(predDFs_long,yr>14)$val*12000
eff_full[eff_full$vehType=='Van',grepl('ice|turbo',names(eff_full))] <- 
  subset(eff_full,vehType=='Pickup')[,grepl('ice|turbo',names(eff_full))] *
  subset(eff_full,vehType=='Van')$cafe_ice / subset(eff_full,vehType=='Pickup')$cafe_ice

ggplot(eff_full) +
  geom_line(aes(x=year,y=avg_bev_bau,color=vehType,group=vehType,lty='BEV - high carbon')) +
  geom_line(aes(x=year,y=avg_bev_decarb,color=vehType,group=vehType,lty='BEV - low carbon')) +
  geom_line(aes(x=year,y=avg_ice,color=vehType,group=vehType,lty='ICEV - no CAFE')) +
  geom_line(aes(x=year,y=cafe_ice,color=vehType,group=vehType,lty='ICEV - CAFE')) +
  geom_point(data=subset(eff_full,year%%10==0),aes(x=year,y=avg_bev_bau,color=vehType,shape='BEV - high carbon')) +
  geom_point(data=subset(eff_full,year%%10==0),aes(x=year,y=avg_bev_decarb,color=vehType,shape='BEV - low carbon')) +
  geom_point(data=subset(eff_full,year%%10==0),aes(x=year,y=avg_ice,color=vehType,shape='ICEV - no CAFE')) +
  geom_point(data=subset(eff_full,year%%10==0),aes(x=year,y=cafe_ice,color=vehType,shape='ICEV - CAFE')) +
  scale_shape_manual(name='Scenario',values=c(16,16,8,8)) +
  scale_linetype_manual(name='Scenario',values=c(2,1,1,2))

eff_full <- cbind(eff_full,abs(eff_full[,grepl('low',names(eff_full))] - eff_full[,grepl('high',names(eff_full))])/4)
names(eff_full)[ncol(eff_full)-4:0] <- gsub('low','sd',names(eff_full)[ncol(eff_full)-4:0])

head(eff_full)

# vehTypes <- factor(unique(eff_full$vehType))
# EV market share scenarios-------------


bnef <- data.frame(year=3:8*5+2000,
                   ms=c(.01,.03,.05,.24,.43,.54))

logi_fit <- function(df,mx=0) {
  x=df[,1]
  y=df[,2]
  
  if(mx==0) {
    optim(par=c(mean(y),.5,1),function(par) {
      logi <- logistic(x - min(x),par[1],par[2],par[3])
      sum((logi-y)^2)
    })
  } else {
    optim(par=c(mean(y),.5),function(par) {
      logi <- logistic(x - min(x),par[1],par[2],mx)
      sum((logi-y)^2)
    })
  }
  
}

logi <- logi_fit(bnef)
plot(logistic(1:91,43,.12,1),type='l',lwd=2)
points(bnef$year-2015,bnef$ms,col='blue',pch=20,cex=2)
lines(logistic(1:91,20,.2,1),col='green3',type='l',lwd=2)
lines(logistic(1:91,16.2,.32,.57),col='red3',type='l',lwd=2)
lines(logistic(1:91,43,.12,.57),col='red',type='l',lwd=2)

ev_curves <- data.frame(year=2010:2100,
                        base=logistic(1:91,16.2,.32,.57),
                        slow=logistic(1:91,43,.12,.57),
                        high_slow=logistic(1:91,43,.12,1),
                        high_fast=logistic(1:91,20,.2,1))

fleet_curves <- data.frame(year=2010:2100,
                        f0=0,
                        f10=logistic(1:91,20,.2,.1),
                        f50=logistic(1:91,20,.2,.5),
                        f100=logistic(1:91,20,.2,1))

# Final sim inputs-------



# buying rate: assume MIT paper -- 14.56 million in 1996, .5% growth rate every year after that
infoDF$bought = (17630/12.1)*1.005^(infoDF$year-1996)# 12.07 = 176 mill / 14.56 mill 


### VMT forcing 
vmt_raw <- read.csv('us vmt.csv')
vmt_raw <- sapply(vmt_raw,function(v)as.numeric(gsub(',','',as.character(v)))) %>% data.frame

vmt_data = data.frame(year=vmt_raw$Year,
                       VMT=rowSums(vmt_raw[,c(6:7,9:10)],na.rm=T)*1e2)

vmt_data <- subset(vmt_data,!is.na(year))
vmt_data$change <- c(1,(vmt_data$VMT[-1]/vmt_data$VMT[-nrow(vmt_data)])^(1/diff(vmt_data$year)))
VMT_predict = data.frame(year=START_YEAR:END_YEAR)
VMT_predict$VMT = predict(lm(VMT~year,vmt_data), VMT_predict)
# 
# veh_raw <- read.csv('vehicle ownership.csv')
# veh_raw <- sapply(veh_raw,function(v)as.numeric(gsub(',','',as.character(v)))) %>% data.frame
# veh_data <- data.frame(year=veh_raw$X,
#                        nveh=rowSums(veh_raw[,c(6:7,9:10)],na.rm=T)/1e4)
# veh_data <- subset(veh_data,!is.na(year))
vTypes = c("Car","Car SUV", "Van","Truck SUV","Pickup")

eff_full$bev_del <- c(1,eff_full$avg_bev[-1]/eff_full$avg_bev[-nrow(eff_full)])
eff_full$bev_del[!duplicated(eff_full$vehType)] <- 1
summary(eff_full$bev_del)


predDFs_del <- sapply(predDFs[-(1:14),],function(v) c(1,v[-1]/v[-length(v)]))

### linearly extrapolate EV efficiency 
ev_del <- NULL
for(v in unique(eff_full$vehType)) {
  ev_del[[v]] <- subset(eff_full,vehType==v)[['bev_del']]
}
ev_del <- data.frame(ev_del)
names(ev_del) <- gsub('\\.',' ',names(ev_del))
ev_del <- ev_del[vTypes]
summary(eff_full$sd_bev/eff_full$avg_bev)
summary(eff_full$sd_ice/eff_full$avg_ice)

ev_eff <- NULL
for(v in unique(eff_full$vehType)) {
  ev_eff[[v]] <- subset(eff_full,vehType==v)[['avg_bev']]
}
ev_eff <- data.frame(ev_eff)
names(ev_eff) <- gsub('\\.',' ',names(ev_eff))
ev_eff <- ev_eff[vTypes]
head(predPropsS)
predProp_del <- sapply(predPropsS[-(1:14),],function(v) c(1,v[-1]/v[-length(v)]))

# Simulation function-------------
predFlat <- sapply(predPropsS,function(v)rep(v[1],length(v))) %>% data.frame
# testing: fleetSave=T;START_YEAR=2010;scrappingRate=1;buyingRate=1;predictedProportions= predPropsS;start=round(veh_data$nveh[veh_data$year==START_YEAR]);elec_scen='decarb';ev_ms='base';ev_effscen='avg';fleet_scen='f50';fleet_ev=F;efficiencyEV=100;replacedByEV=vTypes;initialVMT=16000;EVS_START=2015;percentRetrofit=0;T_ADOPT=10;fusion_Switch=FALSE;fusion_CI=0;fusion_Scenario="BAU";enterSwitch=FALSE;vehicleEntrances=NA
# replacedByEV = NULL;fleet_scen = fleet;elec_scen = elec;scrappingRate = 1+1*hirep
fleet_scen='f100';set.seed(1);scrappingRate=1;fleetSave=F;replacedByEV=NULL
ice_effscen='low'
simulateFleet <-  function(START_YEAR=2010, 
                           END_YEAR=2100, 
                           scrappingRate=1, 
                           buyingRate=1, 
                           predictedProportions=predPropsS, 
                           # predDFs=predDFs, 
                           start=round(veh_data$nveh[veh_data$year==START_YEAR]),
                           ev_ms='base', 
                           ev_effscen='avg',
                           elec_scen='bau',
                           ice_effscen=NULL,
                           efficiencyEV=100, 
                           replacedByEV = vTypes, 
                           initialVMT=16000, 
                           EVS_START = 2010, 
                           # VMT_by_age=VMT_by_age,
                           percentRetrofit=0,
                           phev=0,
                           fleetSave=F,
                           T_ADOPT = 10,
                           fleet_scen='f0',
                           fleet_ev=2200,
                           enterSwitch =T,
                           vehicleEntrances = data.frame(type=vTypes,year=c(2010,2016,2020,2018,2020))) {
  # Initiate simulation--------
  if(fleetSave) fleet <- NULL
  print("Starting data prep... ")
  scrap_probs <- read.csv('scrap probs.csv')
  scrap_probs$scrap_fleet <- logistic(0:31,2,2,1)
  
  scrap_probs[,-1] <- sapply(scrap_probs[,-1],function(v) pmin(1,v*scrappingRate))
  outputDF = data.frame(scaleYear=0)
  co2DF = data.frame(gasoline=0,
                     electricity=0,
                     ice_prod=0,
                     bev_prod=0,
                     ice_eol=0,
                     bev_eol=0)
  
  simDF = data.frame(seqNum = c(1:start), age = round(pmax(1, rsnorm(start, mean = 8.4, sd =6, xi=374))))
  types = rep(vTypes, round(predictedProportions[1,]*start))
  types[length(types):nrow(simDF)]="Car"
  simDF$VehicleType = types
  simDF$efficiency = rnorm(start, as.numeric(predDFs[1,as.character(simDF$VehicleType)]), 1)
  simDF$VMT = VMT_by_age[match(paste(simDF$age, simDF$VehicleType), paste(VMT_by_age$Age, VMT_by_age$VehicleType)), "value"] * rgamma(nrow(simDF),shape=3,scale=1/3)
  simDF$EV <- F
  simDF$fleet <- F
  
  # dealing with percent EV yearly changing 
  tempPerc = ev_curves[[ev_ms]]
  
  ev_eff_0 <- ev_eff[1,]
  ice_eff_0 <- predDFs[1,]
  props_0 <- predPropsS[1,]
  change_0 <- 1.01
  
  infoDF <- subset(infoDF,year>=START_YEAR & year<=END_YEAR)
  
  print("starting simulation")
  # Run simulation---------
  # testing: i=1
  for(i in c(1:(END_YEAR-START_YEAR+1))){
    # print(i);simDF <- fleet0[[i]]
    scaleYear = infoDF$year[i]-START_YEAR 
    lastCar= simDF$seqNum[nrow(simDF)]
    numCars = nrow(simDF)
    percentEVYearly1 = tempPerc[i]

    # if((START_YEAR+i-1) %% 10==0) print(paste(infoDF$year[i], ": ", numScrap, " retired, ", numBuy, 
    #                            " bought, ", mean(simDF$age), " average age, ", sum(is.na(simDF$VMT)), " VMT NAs, ",
    #                            sum(simDF$VehicleType=="EV"), " EVs, ", nrow(simDF), " fleet size, ", max(simDF$age), " max age",
    #                            "num replaced by EV: ", length(replacedByEV)))
    
    # scrap some 
    toScrap = which(rbinom(numCars,1,scrap_probs$scrap_car[pmin(31,simDF$age)+1]*(!simDF$fleet & grepl('Car',simDF$VehicleType)) +
      scrap_probs$scrap_lt[pmin(31,simDF$age)+1]*(!simDF$fleet & !grepl('Car',simDF$VehicleType)) +
      scrap_probs$scrap_fleet[pmin(31,simDF$age)+1]*simDF$fleet)==1)
    
    scrap_types = factor(simDF$VehicleType[toScrap],levels=vTypes)
    scrap_typesEV = factor(simDF$VehicleType[toScrap][simDF$EV[toScrap]],levels=vTypes)
    vmt_0 <- sum(simDF$VMT)
    simDF=simDF[-toScrap,]

    #update others
    simDF$age=simDF$age+1
    simDF$VMT[!simDF$fleet] = simDF$VMT[!simDF$fleet] * VMT_by_age[match(paste(simDF$age[!simDF$fleet], simDF$VehicleType[!simDF$fleet]), paste(VMT_by_age$Age, VMT_by_age$VehicleType)), "rate"] + rnorm(sum(!simDF$fleet),0,100) # initialVMT*exp(-simDF$age * growthChars[simDF$VehicleType,"changeVMT"])+rnorm(nrow(simDF), 0,1)
    
    # buy more 
    props_0 = props_0 * rnorm(length(props_0),predProp_del[i,],0.005)
    props_0 = props_0/sum(props_0)
    ice_eff_0 = ice_eff_0 * rnorm(length(ice_eff_0),predDFs_del[i,],0.005)
    ev_eff_0 = ev_eff_0 * rnorm(length(ev_eff_0),as.numeric(ev_del[i,]),0.005)
    props$scaleYear=NULL
    change_0 <- rnorm(1,1.01,.001)
    vmt_add <- vmt_0*change_0 - sum(simDF$VMT)
    numBuy <- vmt_add/weighted.mean(subset(VMT_by_age,Age==1)$value,props_0)
    
    if(numBuy >0) {
      typesToBuy = round(numBuy*props_0)
      
      carsBought = data.frame(seqNum=c(lastCar:(lastCar+sum(typesToBuy)-1)), age=1, VehicleType=rep(vTypes, typesToBuy))
      carsBought$efficiency = as.numeric(ice_eff_0[as.character(carsBought$VehicleType)])*rnorm(sum(typesToBuy),1,.02)
      carsBought$VMT = VMT_by_age[match(paste("1", carsBought$VehicleType), paste(VMT_by_age$Age, VMT_by_age$VehicleType)), "value"]  * rgamma(nrow(carsBought),shape=3,scale=1/3)  # rnorm(sum(typesToBuy),initialVMT,100)
      carsBought$EV <- F
      carsBought$fleet <- F
      
      # add fleet vehicles-----
      fleet_vmt <- fleet_curves[[fleet_scen]][i]*(sum(simDF$VMT)+sum(carsBought$VMT))
      
      fleetToAdd <- round((fleet_vmt - sum(simDF$VMT[simDF$fleet]))/1e5)
      
      if(fleetToAdd > 0) {
        fleetNew <- carsBought[sample(nrow(carsBought),fleetToAdd,replace = T),]
        fleetNew$VMT <- 1e5*rnorm(nrow(fleetNew),1,0.1)
        fleetNew$fleet <- T
        nPrivate <- max(0,(sum(carsBought$VMT) - sum(fleetNew$VMT))/mean(carsBought$VMT))
        carsPrivate <- carsBought[sample(nrow(carsBought),nPrivate,replace = T),]
        carsBought_new <- rbind(carsPrivate,fleetNew)
        carsBought <- carsBought_new
        carsBought$seqNum <- lastCar + 1:nrow(carsBought)
      }
      
      carsBought$VMT <- carsBought$VMT*vmt_add/sum(carsBought$VMT)
      simDF=rbind(simDF,carsBought)
      
      
      buy_types <- factor(simDF$VehicleType[simDF$seqNum %in% carsBought$seqNum],levels=vTypes)
      buy_typesEV <- NULL
      
      # print('hi2')
      
      if (i>(EVS_START-START_YEAR)){
        
        # EV component -- replacing some  
        if(i>fleet_ev-START_YEAR) {
          simDF$efficiency[simDF$fleet & !simDF$EV] = as.numeric(ev_eff_0[as.character(simDF$VehicleType[simDF$fleet & !simDF$EV])])*rnorm(sum(simDF$fleet & !simDF$EV),1,.02)
          
          simDF$EV[simDF$fleet] <- T
        }
        
        if (enterSwitch){
          replacedByEV = vehicleEntrances$type[vehicleEntrances$year<infoDF$year[i]]
        } 
        
        poolToReplace = carsBought$seqNum[carsBought$VehicleType%in%replacedByEV]
        if(i>fleet_ev-START_YEAR) poolToReplace = carsBought$seqNum[carsBought$VehicleType%in%replacedByEV & !carsBought$fleet]
        gross_ev <- floor(percentEVYearly1*nrow(subset(carsBought,!fleet | fleet_ev>2100)))
        toAddEV = min(gross_ev,length(poolToReplace))  # chooses how many to replace by EV
        toReplaceEV = sample(poolToReplace, toAddEV) # chooses which to replace 
        
        if(toAddEV<gross_ev & toAddEV>0) {
          toReplaceEV = c(toReplaceEV,sample(carsBought$seqNum[-poolToReplace], 
                                             round(percentEVYearly1*nrow(carsBought) - toAddEV)))
          toAddEV <- length(toReplaceEV)
        }
        
        replaceSeqs = match(toReplaceEV, simDF$seqNum)
        simDF$efficiency[replaceSeqs] = as.numeric(ev_eff_0[as.character(simDF$VehicleType[replaceSeqs])])*rnorm(toAddEV,1,.02)
        
        simDF$EV[replaceSeqs]=T
        # simDF$fleet[replaceSeqs] = F
        buy_typesEV <- factor(simDF$VehicleType[replaceSeqs],levels=vTypes)
        buy_types <- factor(simDF$VehicleType[simDF$seqNum %in% carsBought$seqNum & !simDF$EV],levels=vTypes)
        
        # EV Retrofitting -- retrofitting some 
        poolToRetrofit = simDF$seqNum[simDF$VehicleType%in%replacedByEV]
        toAddEV = floor(percentRetrofit*length(poolToRetrofit))  # chooses how many to retrofit by EV
        toRetrofitEV = sample(poolToRetrofit, toAddEV) # chooses which to replace 
        retrofitSeqs = match(toRetrofitEV, simDF$seqNum)
        simDF$efficiency[retrofitSeqs]=efficiencyEV+scaleYear 
        simDF$EV[retrofitSeqs]=T
        
        
        
      }  
    }
    
    outputDF[i,] <- NA
    outputDF$scaleYear[i]=scaleYear
    outputDF$numCars[i] = nrow(simDF)
    outputDF$totalVMT[i] = sum(simDF$VMT, na.rm=T)
    
    # calculating more output with EVs
    outputDF$propEVS[i] = sum(simDF$EV)/nrow(simDF)
    outputDF$propFleet[i] = sum(simDF$fleet)/nrow(simDF)
    outputDF$propCars[i] = sum(simDF$VehicleType=="Car")/nrow(simDF)
    outputDF$propCarSUV[i] = sum(simDF$VehicleType=="Car SUV")/nrow(simDF)
    outputDF$propTruck[i] = sum(simDF$VehicleType=="Truck SUV")/nrow(simDF)
    outputDF$propVan[i] = sum(simDF$VehicleType=="Van")/nrow(simDF)
    outputDF$propPickup[i] = sum(simDF$VehicleType=="Pickup")/nrow(simDF)
    
    ev_vmt <- sum(simDF[simDF$EV, "VMT"])*(1 - phev/2)
    gas_vmt <- outputDF$totalVMT[i] - ev_vmt
    
    outputDF$EV_VMT[i] = ev_vmt
    outputDF$fleetVMT[i] = sum(simDF[simDF$fleet, "VMT"])
    
    outputDF$gasolineConsumption[i] = sum(simDF$VMT[!simDF$EV]/simDF$efficiency[!simDF$EV], na.rm=T)*10000*gas_vmt/sum(simDF$VMT[!simDF$EV])
    outputDF$gasolineEfficiency[i] = sum(simDF$VMT[!simDF$EV])/outputDF$gasolineConsumption[i]*10000
    outputDF$fleetEfficiency[i] = weighted.mean(simDF$efficiency[!simDF$EV & simDF$fleet],simDF$VMT[!simDF$EV & simDF$fleet])
    outputDF$electricityConsumption[i] = sum(simDF$VMT[simDF$EV]*simDF$efficiency[simDF$EV], na.rm=T)*10*ev_vmt/(sum(simDF[simDF$EV, "VMT"]) + .0001)
    
    outputDF$co2emissions[i] = outputDF$gasolineConsumption[i]*.012 + outputDF$electricityConsumption[i]*carbon[[elec_scen]][i]/1e6 +
      1e4*sum(c( prod_emissions$green_ice[as.numeric(buy_types)] ,
      prod_emissions$green_bev[as.numeric(buy_typesEV)] ,
      eol_emissions$green_ice[as.numeric(scrap_types)] ,
      eol_emissions$green_bev[as.numeric(scrap_typesEV)] ,
      
      carbon[[elec_scen]][i] * prod_emissions$energy_intensity_ice[as.numeric(buy_types)] ,
      carbon[[elec_scen]][i] * prod_emissions$energy_intensity_bev[as.numeric(buy_typesEV)] ,
      carbon[[elec_scen]][i] * eol_emissions$energy_intensity_ice[as.numeric(scrap_types)] ,
      carbon[[elec_scen]][i] * eol_emissions$energy_intensity_bev[as.numeric(scrap_typesEV)] ))
    
    co2DF[i,] = c(outputDF$gasolineConsumption[i]*.012,
                  outputDF$electricityConsumption[i]*carbon[[elec_scen]][i]/1e6,
                  1e4*sum(c( prod_emissions$green_ice[as.numeric(buy_types)] ,
                         carbon[[elec_scen]][i] * prod_emissions$energy_intensity_ice[as.numeric(buy_types)])),
                  1e4*sum(c( prod_emissions$green_bev[as.numeric(buy_typesEV)] ,
                         carbon[[elec_scen]][i] * prod_emissions$energy_intensity_bev[as.numeric(buy_typesEV)])) ,
                  1e4*sum(c( eol_emissions$green_ice[as.numeric(buy_types)] ,
                         carbon[[elec_scen]][i] * eol_emissions$energy_intensity_ice[as.numeric(buy_types)])),
                  1e4*sum(c( eol_emissions$green_bev[as.numeric(buy_typesEV)] ,
                         carbon[[elec_scen]][i] * eol_emissions$energy_intensity_bev[as.numeric(buy_typesEV)])) )
    
    
    if(fleetSave) fleet <- c(fleet,list(simDF))
  }
  outputDF <- cbind(outputDF,co2DF)
  print("ending simulation")
  return(outputDF)
}


save.image("sim_gb.rdata")

# Test sim analysis---------
output_hirep <- outputDF
output_base <- outputDF
output_fleet <- outputDF

plot(output_base$totalVMT)
lines(output_fleet$totalVMT)
lines(output_hirep$totalVMT,col='red')

for(i in 1:length(fleet)) {
  fleet[[i]]$year <- i
  fleet0[[i]]$year <- i
}
fleet_df <- do.call('rbind',fleet)
fleet0_df <- do.call('rbind',fleet0)
fleet_df$gasoline <- fleet_df$VMT/fleet_df$efficiency
fleet0_df$gasoline <- fleet0_df$VMT/fleet0_df$efficiency

eff_agg <- aggregate(cbind(VMT,efficiency,gasoline)~age+VehicleType+fleet+EV+year,fleet_df,sum)
eff0_agg <- aggregate(cbind(VMT,efficiency,gasoline)~age+VehicleType+fleet+EV+year,fleet0_df,sum)
eff_agg$efficiency <- eff_agg$VMT/eff_agg$gasoline
eff0_agg$efficiency <- eff0_agg$VMT/eff0_agg$gasoline

ggplot(aggregate(gasoline~age+year,subset(eff_agg,!EV),sum)) +
  geom_line(aes(x=year,y=gasoline,color=age,group=age)) +
  ylim(c(0,2e6))

ggplot(aggregate(gasoline~age+year,subset(eff0_agg,!EV & !fleet),sum)) +
  geom_line(aes(x=year,y=gasoline,color=age,group=age)) +
  ylim(c(0,2e6))

ggplot(aggregate(VMT~age+year,subset(eff_agg,!EV),sum)) +
  geom_line(aes(x=year,y=VMT,color=age,group=age))+
  ylim(c(0,8e7))

ggplot(aggregate(VMT~age+year,subset(eff0_agg,!EV),sum)) +
  geom_line(aes(x=year,y=VMT,color=age,group=age))+
  ylim(c(0,8e7))

test <- aggregate(cbind(VMT,gasoline)~age+year,subset(eff_agg,!EV),sum)
test0 <- aggregate(cbind(VMT,gasoline)~age+year,subset(eff0_agg,!EV),sum)
test$efficiency <- test$VMT/test$gasoline
test0$efficiency <- test0$VMT/test0$gasoline

ggplot(test) +
  geom_line(aes(x=year,y=efficiency,color=age,group=age))+
  scale_color_gradientn(colors=rainbow(10)) +
  ylim(c(0,70))
ggplot(test0) +
  geom_line(aes(x=year,y=efficiency,color=age,group=age))+
  scale_color_gradientn(colors=rainbow(10)) +
  ylim(c(0,70))
