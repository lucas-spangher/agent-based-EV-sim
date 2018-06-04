
simulateFleet = function(START_YEAR, 
    END_YEAR, 
    scrappingRate=1, 
    buyingRate=1, 
    predictedProportions, 
    predDFs, 
    start,
	percentEVYearly=.1, 
    efficiencyEV=100, 
    replacedByEV = c("Car", "Car SUV"), 
    initialVMT=16000, 
    EVS_START = 2020, 
    VMT_by_age,
    percentRetrofit=0, 
    T_ADOPT = 10,
    fusion_Switch=FALSE,
    fusion_CI = 0,
    fusion_Scenario = "BAU",
    enterSwitch =FALSE,
    vehicleEntrances = NA){
    
    print("Starting data prep... ")

    infoDF= data.frame(year = c(START_YEAR:END_YEAR))

    outputDF = data.frame(scaleYear=0, 
                        numCars=10, 
                        propEVS=0,
                        gasolineConsumptionWithEVs=0,
                        averageGasolineConsumptionWithEVs=0,
                        efficiencyWithEVs=0)

    infoDF$scrappage = 5.77*(infoDF$year %in% c(1995:1999))+
                  5.7*(infoDF$year %in% c(2000:2004))+
                  6.09 *(infoDF$year %in% c(2005:2009))+
                  6.34*(infoDF$year %in% c(2010:2014))+
                  6.56*(infoDF$year %in% c(2015:END_YEAR))  ## This is based on MOBILE6
    infoDF$scrappage = (infoDF$scrappage)*.01*scrappingRate

    # buying rate: assume MIT paper -- 14.56 million in 1996, .5% growth rate every year after that
    infoDF$bought = (start/12.1)*1.005^(infoDF$year-1996)*buyingRate # 12.07 = 176 mill / 14.56 mill 

    vTypes = c("Car","Car SUV", "Van","Truck SUV","Pickup")
        
    growthChars=data.frame(growthParam = c(.28,.28,.22,.22,.22,.28), 
        medianAge = c(16.9,16.9, 15.5,15.5,15.5,16.9),
        changeVMT = c(.04,.04,.05,.05,.05,.04))
    row.names(growthChars) = c(vTypes,"EV")

    simDF = data.frame(seqNum = c(1:start), age = round(pmax(1, rsnorm(start, mean = 8.4, sd =6, xi=374))))
    types = rep(vTypes, round(predictedProportions[1,]*start))
    types[length(types):nrow(simDF)]="Car"
    simDF$VehicleType = types # may have to modify that 
    simDF$efficiency = rnorm(start, as.numeric(predDFs[1,as.character(simDF$VehicleType)]), 1)
    simDF$VMT = VMT_by_age[match(paste(simDF$age, simDF$VehicleType), paste(VMT_by_age$Age, VMT_by_age$VehicleType)), "value"] + rnorm(nrow(simDF),0,100)   #initialVMT*exp(-simDF$age * growthChars[simDF$VehicleType,"changeVMT"])
    levels(simDF$VehicleType)=c(levels(simDF$VehicleType),"EV")

    # dealing with percent EV yearly changing 
    if (length(percentEVYearly==1)){
        tempPerc = s_curve(percentEVYearly, START_YEAR, END_YEAR, T_ADOPT)
    } else {
        print("error in s curve input")
        return
    }

    ### linearly extrapolate EV efficiency 

    GREET_DF = data.frame(years = c(2016, 2026, 2040), mpge= c(106,110,145.2))
    frontRunner_EV_efficiency =data.frame(years=c(START_YEAR:END_YEAR))
    EV_eff_mod = lm(mpge~years, data =GREET_DF)
    frontRunner_EV_efficiency$mpge = predict(EV_eff_mod, frontRunner_EV_efficiency)

    ### VMT forcing 

    VMT_short_train = c(2104416, 2024757, 2015714, 2025745, 2046282, 2062828, 2074423, 2072071, 2147840)
    VMT_long_train = c(586618,605456,617534,622712,604175,601232,603307,638484,631852)

    VMT_total = VMT_short_train + VMT_long_train

    VMT_df_data = data.frame(years= c(2007:2015),VMT= VMT_total*1e2)

    VMT_mod = lm(VMT~years, data = VMT_df_data)

    VMT_predict = data.frame(years = c(START_YEAR:END_YEAR))
    VMT_predict$VMT = predict(VMT_mod, VMT_predict)

    print("starting simulation")
    for(i in c(1:nrow(infoDF))){
        scaleYear = infoDF$year[i]-START_YEAR 
        lastCar= simDF$seqNum[nrow(simDF)]
        numCars = nrow(simDF)
        percentEVYearly1 = tempPerc[i]

        # if (infoDF$year[i]>vanEnter){
        #     replacedByEV =  c("Car", "Car SUV", "Van") 
        # }
        # if(infoDF$year[i]>TruckSUVEnter){
        #     replacedByEV = c("Car", "Car SUV", "Van", "Truck SUV") 
        # }
        # if (infoDF$year[i]>PickupEnter){
        #     replacedByEV = c("Car", "Car SUV", "Van", "Truck SUV", "Pickup") 
        # }

        # draw scrappage and buying numbers
        numScrap = rpois(1, infoDF$scrappage[i]*numCars)
        numBuy = rpois(1, infoDF$bought[i])
        
        if(i %% 10==0) print(paste(infoDF$year[i], ": ", numScrap, " retired, ", numBuy, 
            " bought, ", mean(simDF$age), " average age, ", sum(is.na(simDF$VMT)), " VMT NAs, ",
            sum(simDF$VehicleType=="EV"), " EVs, ", nrow(simDF), " fleet size, ", max(simDF$age), " max age",
            "num replaced by EV: ", length(replacedByEV)))

        # scrap some 
        toScrap = sample(c(1:numCars), numScrap, prob =simDF$age/max(simDF$age)) # (1-1/(1+exp(-growthChars[simDF$VehicleType,"growthParam"]*(simDF$age-growthChars[simDF$VehicleType,"medianAge"])))))  #
        simDF=simDF[-toScrap,]
        
        #update others
        simDF$age=simDF$age+1
        simDF$VMT= VMT_by_age[match(paste(simDF$age, simDF$VehicleType), paste(VMT_by_age$Age, VMT_by_age$VehicleType)), "value"] + rnorm(nrow(simDF),0,100) # initialVMT*exp(-simDF$age * growthChars[simDF$VehicleType,"changeVMT"])+rnorm(nrow(simDF), 0,1)
    
        # buy more 
        props = predictedProportions[i,]
        props$scaleYear=NULL
        typesToBuy = round(numBuy*props)
        efficiency_t = cbind(predDFs[i,], "EV"=frontRunner_EV_efficiency[i, "mpge"])
        
        carsBought = data.frame(seqNum=c(lastCar:(lastCar+sum(typesToBuy)-1)), age=1, VehicleType=rep(vTypes, typesToBuy))
        carsBought$efficiency = rnorm(sum(typesToBuy), as.numeric(efficiency_t[as.character(carsBought$VehicleType)]), 1)
        carsBought$VMT = VMT_by_age[match(paste("1", carsBought$VehicleType), paste(VMT_by_age$Age, VMT_by_age$VehicleType)), "value"] + rnorm(nrow(carsBought),0,100) # rnorm(sum(typesToBuy),initialVMT,100)
        simDF=rbind(simDF,carsBought)
    
        # calculating fleet without EVs  
        outputDF[i,]=NA
        outputDF$scaleYear[i]=scaleYear
        outputDF$numCars[i] = nrow(simDF)
        outputDF$totalVMT[i] = sum(simDF$VMT, na.rm=T)

        if (i>(EVS_START-START_YEAR)){

            # EV component -- replacing some  

            if (enterSwitch){
                replacedByEV = vehicleEntrances$type[vehicleEntrances$year<infoDF$year[i]]
            } 

            poolToReplace = carsBought$seqNum[carsBought$VehicleType%in%replacedByEV]
            toAddEV = floor(percentEVYearly1*length(poolToReplace))  # chooses how many to replace by EV
            toReplaceEV = sample(poolToReplace, toAddEV) # chooses which to replace 
            replaceSeqs = match(toReplaceEV, simDF$seqNum)
            simDF$efficiency[replaceSeqs]=frontRunner_EV_efficiency[i, "mpge"]
            simDF$VehicleType[replaceSeqs]="EV"
            simDF$VMT[replaceSeqs] = VMT_by_age[VMT_by_age$Age==1 & VMT_by_age$VehicleType=="Car", "value"]+
                rnorm(length(replaceSeqs), 0, 100)

            # EV Retrofitting -- retrofitting some 
           poolToRetrofit = simDF$seqNum[simDF$VehicleType%in%replacedByEV]
           toAddEV = floor(percentRetrofit*length(poolToRetrofit))  # chooses how many to retrofit by EV
           toRetrofitEV = sample(poolToRetrofit, toAddEV) # chooses which to replace 
           retrofitSeqs = match(toRetrofitEV, simDF$seqNum)
           simDF$efficiency[retrofitSeqs]=efficiencyEV+scaleYear 
           simDF$VehicleType[retrofitSeqs]="EV"
        
        }

        # force VMT of fleet to match national prediction 

        VMT_forcing_ratio = VMT_predict$VMT[i]/outputDF$totalVMT[i]
        simDF$VMT = simDF$VMT*VMT_forcing_ratio

        # calculating more output with EVs
        outputDF$propEVS[i] = sum(simDF$VehicleType=="EV")/nrow(simDF)
        outputDF$propCars[i] = sum(simDF$VehicleType=="Car")/nrow(simDF)
        outputDF$propCarSUV[i] = sum(simDF$VehicleType=="Car SUV")/nrow(simDF)
        outputDF$propTruck[i] = sum(simDF$VehicleType=="Truck SUV")/nrow(simDF)
        outputDF$efficiencyWithEVs[i]= mean(simDF$efficiency)
        outputDF$gasolineConsumptionWithEVs[i] = sum(simDF$VMT/simDF$efficiency, na.rm=T)*10000
        outputDF$averageGasolineConsumptionWithEVs[i] = mean(simDF$VMT/simDF$efficiency, na.rm=T)
        outputDF$scaledEfficiencyWithEVs[i]= coef(effMod)[1]*mean(simDF$efficiency, na.rm=T)
        outputDF$scaledGasolineConsumptionWithEVs[i] = sum(simDF$VMT/(coef(effMod)[1]*simDF$efficiency), na.rm=T)*10000    
        outputDF$EV_VMT[i] = sum(simDF[simDF$VehicleType=="EV", "VMT"])
        outputDF$EV_Efficiency[i] = mean(simDF[simDF$VehicleType=="EV", "efficiency"])
    
        if (fusion_Switch){
            CI_scenario = fusion_CI[,fusion_Scenario]
            before = rep(CI_scenario[1], 2014 - START_YEAR)
            CI_scenario = c(before, CI_scenario)

            notEVs = simDF[simDF$VehicleType!="EV",]
            EVs = simDF[simDF$VehicleType=="EV",]
            carbonEmitted_notEVs = sum(notEVs$VMT/(coef(effMod)[1]*notEVs$efficiency), na.rm=T)*10000*20*4.53e-13
            carbonEmitted_EVs = sum(EVs$VMT / EVs$efficiency / .031 ) * CI_scenario[i] * 10000 * .001 * 1e-9
            outputDF$carbon_emitted_EVs[i]= carbonEmitted_EVs
            outputDF$carbon_emitted_notEVs[i]= carbonEmitted_notEVs
            outputDF$carbonScenario[i] = CI_scenario[i]
        }


        # moving year by year graphs
       
 #       ggplot(simDF) + geom_histogram(aes(x=age, fill=VehicleType), alpha=.75)+
 #          xlim(0,110)+ylim(0,10000)+ ggtitle(paste("Efficiency distribution in year",infoDF$year[i]))
         


 #       ggsave(filename = paste("outputGraphs/age/", infoDF$year[i],".png",sep=""))
        }

        print("ending simulation")
        return(outputDF)
}

#system("cd outputGraphs/efficiency/")
#system("ffmpeg -f image2 -i outputGraphs/efficiency/%d.png outputGraphs/efficiency/video.mpg")


