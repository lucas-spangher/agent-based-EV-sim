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
                           fleetSave=F,
                           T_ADOPT = 10,
                           fleet_scen='f0',
                           fleet_ev=2200,
                           enterSwitch =FALSE,
                           vehicleEntrances = NA) {
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
    
    outputDF$gasolineConsumption[i] = sum(simDF$VMT[!simDF$EV]/simDF$efficiency[!simDF$EV], na.rm=T)*10000
    outputDF$gasolineEfficiency[i] = sum(simDF$VMT[!simDF$EV])/outputDF$gasolineConsumption[i]*10000
    outputDF$fleetEfficiency[i] = weighted.mean(simDF$efficiency[!simDF$EV & simDF$fleet],simDF$VMT[!simDF$EV & simDF$fleet])
    outputDF$electricityConsumption[i] = sum(simDF$VMT[simDF$EV]*simDF$efficiency[simDF$EV], na.rm=T)*10
    
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
    outputDF$EV_VMT[i] = sum(simDF[simDF$EV, "VMT"])
    outputDF$fleetVMT[i] = sum(simDF[simDF$fleet, "VMT"])
    
    
    if(fleetSave) fleet <- c(fleet,list(simDF))
  }
  outputDF <- cbind(outputDF,co2DF)
  print("ending simulation")
  return(outputDF)
}