###### graphing.R ######### 
require(ggplot2)

ggplot(fuelDF)+
        geom_point(aes(ModelYear, CombE, shape=after2005, color= VehicleType), size = 5, alpha = .5)

## insights --> either do the regression and analysis on "all" or leave "all" out. 

## gasoline versus VMT 

ggplot()+
    geom_point(aes(x=totalVMT$Million.Miles, y= ))


###### plotting variables of hierarchical model

dims = dim(hierMod$betadraw)

for(i in c(1:dims[1])){
    for (j in c(1:dims[2])){
        qplot(hierMod$betadraw[i,j,]) + ggtitle(paste(unique(fuelDF$VehicleType)[i], j))
        ggsave(filename = paste("hierRegressionCoefs/",i,j,".png",sep=""))
        }
    }

ggplot()+
    geom_point(data= fuelDF, aes(x=scaleYear,y=CombE),color= "black", size = 5, alpha= .5)+ 
    geom_point(data=predDF, aes(x=scaleYear,y=pred), color = "red", size = 2)

# hierarchical regression fit 
ggplot()+
    geom_point(data= fuelDF, aes(x=scaleYear,y=CombE),color= "black", size = 5, alpha= .5)+ 
    geom_point(data=predDF1, aes(x=scaleYear,y=pred), color = "red", size = 2)

### proportions

ggplot(prop)+geom_point(aes(x=time,y=value, color =variable))


ggplot(predProps)+geom_point(aes(x=scaleYear, y= props,color= VehicleType))


predProps$pred=predPropFinal$prop
predDFs$scaleYear= outputDF$scaleYear
effDF$scaleYear = c(0:19)

newEfficiencyLaws= diag(as.matrix(predPropsS)%*%t(as.matrix(predDFs)))
newEL = data.frame(scaleYear=c(0:54), newEL=newEfficiencyLaws)

ggplot(predProps)+geom_point(aes(x=scaleYear, y= pred,color= VehicleType))

# outputs

ggplot(outputDF)+geom_point(aes(x=scaleYear, y=efficiency))+
    geom_point(data=simDF, aes(x=jitter(scaleYear), y=efficiency, color = VehicleType), alpha=.1)

ggplot(outputDF)+ geom_point(aes(x=scaleYear, y= efficiency))+
    geom_point(data= newEL, aes(x=scaleYear, y=newEL), color = "red")


ggplot(outputDF)+geom_line(aes(x=scaleYear, y=gasolineConsumption), alpha=.5, size =1)+
                 geom_line(aes(x=scaleYear, y= gasolineConsumptionWithEVs), color= "red", size =1, alpha=.5)



### comparison of the ages of draws and the ages of cars at the end of the simulation 
ggplot()+
    geom_histogram(aes(x=BAU[[2]]$age, fill ="end"), alpha= .5)+
    geom_histogram(aes(x=pmax(rsnorm(nrow(BAU[[2]]), 8.4,6,374),1), fill = "draws"), alpha = .4)


#  putting them all on one graph 

compiledDF = data.frame(scaleYear = outputDF.05$scaleYear+1996, 
                        gas0 = outputDF.05$scaledGasolineConsumption,
                        gas.05 = outputDF.05$scaledGasolineConsumptionWithEVs)
   
compiledDF$gas.1= outputDF.10$scaledGasolineConsumptionWithEVs  
compiledDF$gas.25 = outputDF.25$scaledGasolineConsumptionWithEVs 
compiledDF$gas.5 = outputDF.50$scaledGasolineConsumptionWithEVs
compiledDF$gas.75 = outputDF.75$scaledGasolineConsumptionWithEVs
compiledDF$gas1 = outputDF.100$scaledGasolineConsumptionWithEVs
      
ggplot(compiledDF)+geom_line(aes(x=scaleYear,y=gas0*19.6, color = "No EVs"), alpha=.5, size =1)+ 
                 geom_line(aes(x=scaleYear, y= gas.05*19.6, color = "05% sales/year"), size =1, alpha=.5)+
                 geom_line(aes(x=scaleYear, y= gas.1*19.6, color = "10% sales/year"), size =1, alpha=.5)+
                 geom_line(aes(x=scaleYear, y= gas.25*19.6, color = "25% sales/year"), size =1, alpha=.5)+
                 geom_line(aes(x=scaleYear, y= gas.5*19.6, color= "50% sales/year"), size =1, alpha=.5)+
                 geom_line(aes(x=scaleYear, y= gas.75*19.6, color = "75% sales/year"), size =1, alpha=.5)+
                 geom_line(aes(x=scaleYear, y= gas1*19.6, color = "100% sales/year"),  size =1, alpha=.5)+ 
                 labs(title="The effect of differing amounts of EV sales on yearly fleet emissions 1996 to 2050")+
                 xlab("Year")+ylab("Fleet-wide Yearly Carbon Dioxide Emissions")+
                 coord_cartesian(ylim=c(0,38e+10))+
                 scale_colour_manual("",
                         breaks = c("No EVs","05% sales/year","10% sales/year","25% sales/year",
                            "50% sales/year","75% sales/year","100% sales/year"),
                         values = c("No EVs"="black","05% sales/year"="red", "10% sales/year"="orange", 
                            "25% sales/year"="yellow", "50% sales/year"="green",
                            "75% sales/year"="blue","100% sales/year"="purple"))+
                theme(text=element_text(size=14))

# Gallons a day 

ggplot()+
    geom_line(data= EVBAU, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "BAU"), size = 2, alpha = .7)+
    geom_line(data= outputDF.05, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "05%"), size = 2, alpha = .7)+
    geom_line(data= outputDF.10, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "10%"), size = 2, alpha = .7)+
    geom_line(data= EVContr, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "25%"), size = 2, alpha = .7)+
    geom_line(data= outputDF.50, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "50%"), size = 2, alpha = .7)+
    geom_line(data= outputDF.75, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "75%"), size = 2, alpha = .7)+
    geom_line(data= outputDF.100, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "100%"), size = 2, alpha = .7)+
    ylim(0,1.5)+ylab("CO2 Emissions, scaled to ``On-the-road'' efficiency, in GTons")+xlab("Year")+
    theme(text = element_text(size = 24))+ 
    ggtitle("EVs replacing Car Sales, starting in 2020 with 10-year ramp-up")

# plots for presentation 

tempData = list(EVBAU, outputDF.10, outputDF.50, outputDF.100)

debtBreakpoints= sapply(tempData, function(df){
    cum_gas = cumsum(df$scaledGasolineConsumptionWithEVs)
    cum_GTons = cum_gas*8.8*10^(-12) # 8.8 kg of CO2 in gallon of gas and 10^-12 GTons in a kg
    breakPoint = which(cum_GTons>56)[1] ## carbon budget
    return(c(df$scaleYear[breakPoint], df$scaledGasolineConsumptionWithEVs[breakPoint]))
    })
debtBreakpoints=data.frame(year = debtBreakpoints[1,], scaledGas =debtBreakpoints[2,])

data= lapply(tempData, function(df){
    cum_carbon = cumsum(df$scaledGasolineConsumptionWithEVs)*8.8*10^-12
    breakPoint = which(cum_carbon>56)[1]  ### <- carbon budget 
    year = df$scaleYear[breakPoint]
    return(df[df$scaleYear<=year,])
    })

ggplot()+
    geom_line(data= data[[1]], aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "BAU"), size = 2, alpha = .7)+
    geom_line(data= data[[2]], aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "10%"), size = 2, alpha = .7)+
    geom_line(data= data[[3]], aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "50%"), size = 2, alpha = .7)+
    geom_line(data= data[[4]], aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "100%"), size = 2, alpha = .7)+
    geom_point(data = debtBreakpoints[c(1:4),], aes(x=year+1996, y = scaledGas*20*4.53e-13), size = 8)+
    scale_colour_manual("", breaks = c("BAU", "10%", "50%", "100%"),values = c("BAU"="grey1", "10%"="grey2", "50%" = "grey3", "100%"= "red"))+
    ylim(0,1.6)+ylab("CO2 Emissions, scaled to ``On-the-road'' efficiency, in GTons")+xlab("Year")+xlim(1996,2100)+
    theme(text = element_text(size = 24))+ggtitle("EVs replacing Car Sales, starting in 2020 with 10-year ramp-up")

ggplot()+
    geom_line(data= EVBAU, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "BAU"), size = 2, alpha = .7)+
    scale_colour_manual("", breaks = c("BAU", "10%", "50%", "100%"),values = c("BAU"="grey1", "10%"="grey2", "50%" = "grey3", "100%"= "red"))+
    ylim(0,1.6)+ylab("CO2 Emissions, scaled to ``On-the-road'' efficiency, in GTons")+xlab("Year")+xlim(1996,2100)+
    theme(text = element_text(size = 24))+ggtitle("EVs replacing Car Sales, starting in 2020 with 10-year ramp-up")

ggplot()+
    geom_line(data= EVBAU, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "BAU"), size = 2, alpha = .7)+
    geom_line(data= outputDF.10, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "10%"), size = 2, alpha = .7)+
    scale_colour_manual("", breaks = c("BAU", "10%", "50%", "100%"),values = c("BAU"="grey1", "10%"="red"))+
    ylim(0,1.6)+ylab("CO2 Emissions, scaled to ``On-the-road'' efficiency, in GTons")+xlab("Year")+xlim(1996,2100)+
    theme(text = element_text(size = 24))+ggtitle("EVs replacing Car Sales, starting in 2020 with 10-year ramp-up")

ggplot()+
    geom_line(data= EVBAU, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "BAU"), size = 2, alpha = .7)+
    geom_line(data= outputDF.10, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "10%"), size = 2, alpha = .7)+
    geom_line(data= outputDF.50, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "50%"), size = 2, alpha = .7)+
    scale_colour_manual("", breaks = c("BAU", "10%", "50%", "100%"),values = c("BAU"="grey1", "10%"="grey2", "50%" = "red", "100%"= "red"))+
    ylim(0,1.6)+ylab("CO2 Emissions, scaled to ``On-the-road'' efficiency, in GTons")+xlab("Year")+xlim(1996,2100)+
    theme(text = element_text(size = 24))+ggtitle("EVs replacing Car Sales, starting in 2020 with 10-year ramp-up")

ggplot()+
    geom_line(data= EVBAU, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "BAU"), size = 2, alpha = .7)+
    geom_line(data= outputDF.10, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "10%"), size = 2, alpha = .7)+
    geom_line(data= outputDF.50, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "50%"), size = 2, alpha = .7)+
    geom_line(data= outputDF.100, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20*4.53e-13, color = "100%"), size = 2, alpha = .7)+
    scale_colour_manual("", breaks = c("BAU", "10%", "50%", "100%"),values = c("BAU"="grey1", "10%"="grey2", "50%" = "grey3", "100%"= "red"))+
    ylim(0,1.6)+ylab("CO2 Emissions, scaled to ``On-the-road'' efficiency, in GTons")+xlab("Year")+xlim(1996,2100)+
    theme(text = element_text(size = 24))+ggtitle("EVs replacing Car Sales, starting in 2020 with 10-year ramp-up")

######## cashForClunkers

ggplot()+
    geom_line(data=CashForClunkers[[1]], aes(x=scaleYear+1996, y =scaledGasolineConsumptionWithEVs*20*4.53e-13, color=".5 * BAU"), size=2, alpha= .7)+
    geom_line(data=CashForClunkers[[2]], aes(x=scaleYear+1996, y =scaledGasolineConsumptionWithEVs*20*4.53e-13, color="1 * BAU"), size=2, alpha= .7)+
    geom_line(data=CashForClunkers[[3]], aes(x=scaleYear+1996, y =scaledGasolineConsumptionWithEVs*20*4.53e-13, color="1.25 * BAU"), size=2, alpha= .7)+
    geom_line(data=CashForClunkers[[4]], aes(x=scaleYear+1996, y =scaledGasolineConsumptionWithEVs*20*4.53e-13, color="1.5 * BAU"), size=2, alpha= .7)+
    geom_line(data=CashForClunkers[[5]], aes(x=scaleYear+1996, y =scaledGasolineConsumptionWithEVs*20*4.53e-13, color="2 * BAU"), size=2, alpha= .7)+
    ylab("CO2 Emissions (GTons)")+xlab("Year")+theme(text=element_text(size=24))+ylim(0,2)+
    ggtitle("Cash For Clunkers: Worth it to increase buying rates?")



#### different car types 

ggplot()+
    geom_line(data= BAU, aes(x= scaleYear, y= scaledGasolineConsumptionWithEVs, color = "No EVs"), size = 2, alpha = .7)+
    geom_line(data= carSim, aes(x= scaleYear, y= scaledGasolineConsumptionWithEVs, color = "Cars only"), size = 2, alpha = .7)+
    geom_line(data= carCarSUV, aes(x= scaleYear, y= scaledGasolineConsumptionWithEVs, color = "Cars, Car SUVs"), size = 2, alpha = .7)+
    geom_line(data= carCarSUVVan, aes(x= scaleYear, y= scaledGasolineConsumptionWithEVs, color = "Cars, Car SUVs, Vans"), size = 2, alpha = .7)+
    geom_line(data= carCarSUVVanTruck, aes(x= scaleYear, y= scaledGasolineConsumptionWithEVs, color = "Cars, Car SUVs, Vans, Trucks"), size = 2, alpha = .7)+
    geom_line(data= carCarSUVVanTruckPickup, aes(x= scaleYear, y= scaledGasolineConsumptionWithEVs, color = "All replaced"), size = 2, alpha = .7)+
    ggtitle("The effect of different LD Vehicle Types Replaced, 25% new sales")+
    theme(text=element_text(size = 20))+
    ylim(0,1.5e+11)

ggplot()+
    geom_line(data= BAU, aes(x= scaleYear, y= scaledGasolineConsumptionWithEVs, color = "No EVs"), size = 2, alpha = .7)+
    geom_line(data= carSimRetro.05, aes(x= scaleYear, y= scaledGasolineConsumptionWithEVs, color = "Cars only"), size = 2, alpha = .7)+
    geom_line(data= carCarSUVRetro.05, aes(x= scaleYear, y= scaledGasolineConsumptionWithEVs, color = "Cars, Car SUVs"), size = 2, alpha = .7)+
    geom_line(data= carCarSUVVanRetro.05, aes(x= scaleYear, y= scaledGasolineConsumptionWithEVs, color = "Cars, Car SUVs, Vans"), size = 2, alpha = .7)+
    geom_line(data= carCarSUVVanTruckRetro.05, aes(x= scaleYear, y= scaledGasolineConsumptionWithEVs, color = "Cars, Car SUVs, Vans, Trucks"), size = 2, alpha = .7)+
    geom_line(data= carCarSUVVanTruckPickupRetro.05, aes(x= scaleYear, y= scaledGasolineConsumptionWithEVs, color = "All replaced"), size = 2, alpha = .7)+
    ggtitle("Proportions of EVS in fleet when different types retrofitted, 5% of total fleet")+
    theme(text=element_text(size = 20))+
    ylim(0,1.5e+11)


# retrofitting 

ggplot()+
    geom_line(data= BAU, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20, color = "BAU"), size = 2, alpha = .7)+
    geom_line(data= retroDF.05, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20, color = "05%"), size = 2, alpha = .7)+
    geom_line(data= retroDF.10, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20, color = "10%"), size = 2, alpha = .7)+
    geom_line(data= retroDF.25, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20, color = "25%"), size = 2, alpha = .7)+
    geom_line(data= retroDF.50, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20, color = "50%"), size = 2, alpha = .7)+
    geom_line(data= retroDF.75, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20, color = "75%"), size = 2, alpha = .7)+
    geom_line(data= retroDF.100, aes(x= scaleYear+1996, y= scaledGasolineConsumptionWithEVs*20, color = "100%"), size = 2, alpha = .7)+
    ylim(0,3e+12)+ylab("CO2 Emissions, scaled to ``On-the-road'' efficiency")+xlab("Year")+ggtitle("Retrofitting of Cars, Car SUVs, and Trucks after 2018; percents shown are retrofits per year")+
    theme(text = element_text(size = 14))


## version with black dots for breakpoints 
tempData = list(EVBAU, outputDF.05, outputDF.1, outputDF.25, outputDF.5, outputDF.75, outputDF1)

debtBreakpoints= sapply(tempData, function(df){
    cum_gas = cumsum(df$scaledGasolineConsumptionWithEVs)
    cum_GTons = cum_gas*8.8*10^(-12) # 8.8 kg of CO2 in gallon of gas and 10^-12 GTons in a kg
    breakPoint = which(cum_GTons>4)[1] ## carbon budget
    return(c(df$scaleYear[breakPoint], df$scaledGasolineConsumptionWithEVs[breakPoint]))
    })
debtBreakpoints=data.frame(year = debtBreakpoints[1,], scaledGas =debtBreakpoints[2,])

data= lapply(tempData, function(df){
    cum_carbon = cumsum(df$scaledGasolineConsumptionWithEVs)*8.8*10^-12
    breakPoint = which(cum_carbon>4)[1]  ### <- carbon budget 
    year = df$scaleYear[breakPoint]
    return(df[df$scaleYear<=year,])
    })

ggplot()+
    geom_line(data= data[[1]], aes(x=scaleYear, y=scaledGasolineConsumptionWithEVs, color = "00%"), size= 2, alpha=.6)+
    geom_line(data= data[[2]], aes(x=scaleYear, y=scaledGasolineConsumptionWithEVs, color = "10%"), size= 2, alpha=.6)+
    geom_line(data= data[[3]], aes(x=scaleYear, y=scaledGasolineConsumptionWithEVs, color = "25%"), size= 2, alpha=.6)+
    geom_line(data= data[[4]], aes(x=scaleYear, y=scaledGasolineConsumptionWithEVs, color = "50%"), size= 2, alpha=.6)+
    geom_line(data= data[[5]], aes(x=scaleYear, y=scaledGasolineConsumptionWithEVs, color = "75%"), size= 2, alpha=.6)+
    geom_line(data= data[[6]], aes(x=scaleYear, y=scaledGasolineConsumptionWithEVs, color = "99%"), size= 2, alpha=.6)+
    geom_point(data= debtBreakpoints, aes(x= year, y= scaledGas), size = 7.5, alpha = .7)+
    ggtitle("Policy is needed in addition to technical breakthroughs: even over a large difference in EV adoption in the US Light Duty 
        Vehicle Fleet (% of yearly EV aales) the fleet emits enough to reach a cumulative carbon debt of 4 GtCO2 before 2050)")+
    xlab("Scaled Year, with 1996 represented as 0")+
    theme(text= element_text(size =22), plot.title=element_text(hjust=.5))+
    ylab("Total gasoline Consumption (gallons)")+
    xlim(0,50)+ylim(0,2e+10)


## version with black dots for breakpoints 
tempData = list(EVBAU,outputDF.10, outputDF.50, outputDF.100)

debtBreakpoints= sapply(tempData, function(df){
    cum_gas = cumsum(df$scaledGasolineConsumptionWithEVs)
    cum_GTons = cum_gas*8.8*10^(-12) # 8.8 kg of CO2 in gallon of gas and 10^-12 GTons in a gallon of gas 
    breakPoint = which(cum_GTons>13)[1] ## carbon budget
    return(c(df$scaleYear[breakPoint], df$scaledGasolineConsumptionWithEVs[breakPoint]))
    })
debtBreakpoints=data.frame(year = debtBreakpoints[1,], scaledGas =debtBreakpoints[2,])

data= lapply(tempData, function(df){
    cum_carbon = cumsum(df$scaledGasolineConsumptionWithEVs)*8.8*10^-12
    breakPoint = which(cum_carbon>13)[1]  ### <- carbon budget 
    year = df$scaleYear[breakPoint]
    return(df[df$scaleYear<=year,])
    })

ggplot()+
    geom_line(data= data[[1]], aes(x=scaleYear, y=scaledGasolineConsumptionWithEVs*8.8*10^-12, color = "00%"), size= 2, alpha=.6)+
    geom_line(data= data[[2]], aes(x=scaleYear, y=scaledGasolineConsumptionWithEVs*8.8*10^-12, color = "10%"), size= 2, alpha=.6)+
    geom_line(data= data[[3]], aes(x=scaleYear, y=scaledGasolineConsumptionWithEVs*8.8*10^-12, color = "50%"), size= 2, alpha=.6)+
    geom_line(data= data[[4]], aes(x=scaleYear, y=scaledGasolineConsumptionWithEVs*8.8*10^-12, color = "99%"), size= 2, alpha=.6)+
    geom_point(data= debtBreakpoints, aes(x= year, y= scaledGas*8.8*10^-12), size = 7.5, alpha = .7)+
    ggtitle("Policy is needed in addition to technical breakthroughs: even over a large difference in EV adoption in the US Light Duty 
        Vehicle Fleet (% of yearly EV aales) the fleet emits enough to reach a cumulative carbon debt of 4 GtCO2 before 2050)")+
    xlab("Scaled Year, with 1996 represented as 0")+
    theme(text= element_text(size =22), plot.title=element_text(hjust=.5))+
    ylab("Total gasoline Consumption (gallons)")+
    xlim(0,50)#+ylim(0,2e+11)

# cash for clunkers

ccDF = outputDF.05
ccDF$gasolineConsumption.5 = outputDF.5Buying$scaledGasolineConsumption
ccDF$gasolineConsumption1.25 = outputDF1.25Buying$scaledGasolineConsumption
ccDF$gasolineConsumption1.5 = outputDF1.5Buying$scaledGasolineConsumption
ccDF$gasolineConsumption1.75 = outputDF1.75Buying$scaledGasolineConsumption
ccDF$gasolineConsumption2 = outputDF2Buying$scaledGasolineConsumption
ccDF$Year= ccDF$scaleYear+1996

ggplot(ccDF) + geom_line(aes(x= Year, y = scaledGasolineConsumption*19.6, color = "BAU"), size = 2)+
                geom_line(aes(x=Year, y= gasolineConsumption.5*19.6, color = ".5*BAU"), size = 2)+
                geom_line(aes(x=Year, y= gasolineConsumption1.25*19.6, color = "1.25*BAU"), size = 2)+
                geom_line(aes(x=Year, y= gasolineConsumption1.5*19.6, color = "1.5*BAU"), size = 2)+
                geom_line(aes(x=Year, y= gasolineConsumption1.75*19.6, color = "1.75*BAU"), size = 2)+
                geom_line(aes(x=Year, y= gasolineConsumption2*19.6, color = "2*BAU"), size = 2)+
                ggtitle("The effect of changing BAU buying and scrapping rates on yearly fleet-wide emissions from 1996 to 2050")+
                ylab("Fleet-wide Yearly Carbon Dioxide Emissions")+ylim(0,48e+10)+
                theme(text=element_text(size=16))#+
                scale_colour_manual("", breaks = c("BAU", ".5*BAU","1.25*BAU","1.5*BAU","1.75*BAU","2*BAU"))+


####### gasoline consumption and VMT comparisons 

ggplot()+
    geom_line(data= gasoline, aes(x=year, y= thousandGallonsDay*1000*365, color = "actuals"), size = 2)+
    geom_line(data= EVBAU, aes(x=scaleYear + 1996, y=scaledGasolineConsumptionWithEVs/10, color = "predicted scaled"), size = 2)+
    ylim(0,3e+10)+ ggtitle("Predicted Gasoline use Versus Actual")+
    theme(text=element_text(size=18))

####### gasoline consumption and VMT comparisons 

ggplot() +
    geom_line(data= totalVMT, aes(
        x=Year., y= Million.Miles, color = "actuals"), size = 2) +
    geom_line(data= EVBAU, aes(
            x=scaleYear + 1996, y=totalVMT/100, color = "predicted scaled"), #10^2*10^4 = 10^6 
        size = 2) +
    ggtitle("Predicted VMT use Versus Actual") +
    ylim(0,3.5e6) +
    theme(text=element_text(size=18))

######## exploring the shapes of curves a bit 

ggplot()+
    geom_line(data= EVBAU, aes(x=scaleYear+1996, y= propEVS, color= "BAU"), alpha = .7, size = 2)+    
    geom_line(data= outputDF.10, aes(x=scaleYear+1996, y= propEVS, color= "10%"), alpha = .7, size = 2)+
    geom_line(data= outputDF.50, aes(x=scaleYear+1996, y= propEVS, color= "50%"), alpha = .7, size = 2)+
    geom_line(data= outputDF.100, aes(x=scaleYear+1996, y= propEVS, color= "99.99%"), alpha = .7, size = 2)+
    ylab("Proportion of EVS in each simulation")+ xlab("year")+
    theme(text= element_text(size =16))+
    ggtitle("Market saturation is quickly reached in all scenarios")

### adding in S-curves....!

t = c(1:(END_YEAR-START_YEAR))
percentEVYearly.12= .12/(1+exp(18 - .58*t))
percentEVYearly.85= .85/(1+exp(18 - .58*t))
percentEVYearly.99= .99/(1+exp(18 - .58*t))


qplot(t+1996, percentEVYearly)+ggtitle("S-curve adoption for Woods McKenzie Electric Car model")+ylab("Total Percent Sales per Year")+
    xlab("Year")+theme(text=element_text(size=20))


### wood mcKenzie stuff 

ggplot()+
    geom_line(data= BAU, aes(x=scaleYear+1996, y= propEVS, color= "BAU"), alpha = .7, size = 2)+    
    geom_line(data= wmDF.12, aes(x=scaleYear+1996, y= propEVS, color= "Wood McKenzie expectation (12% sales)"), alpha = .7, size = 2)+
    geom_line(data= wmDF.85, aes(x=scaleYear+1996, y= propEVS, color= "Wood McKenzie aggressive (85% sales)"), alpha = .7, size = 2)+
    geom_line(data= wmDF.99, aes(x=scaleYear+1996, y= propEVS, color= "Wood McKenzie Extreme (99% sales)"), alpha = .7, size = 2)+
    ylab("Proportion of EVS in each simulation")+ xlab("year")+
    theme(text= element_text(size =16))+ylim(0,1)
    ggtitle("Market saturation occurs quickly and at low percentages")

ggplot()+
    geom_line(data= BAU, aes(x=scaleYear+1996, y= scaledGasolineConsumptionWithEVs, color= "BAU"), alpha = .7, size = 2)+
    geom_line(data= wmDF.12, aes(x=scaleYear+1996, y= scaledGasolineConsumptionWithEVs, color= "Wood McKenzie expectation (12% sales)"), alpha = .7, size = 2)+
    geom_line(data= wmDF.85, aes(x=scaleYear+1996, y= scaledGasolineConsumptionWithEVs, color= "Wood McKenzie aggressive (85% sales)"), alpha = .7, size = 2)+
    geom_line(data= wmDF.99, aes(x=scaleYear+1996, y= scaledGasolineConsumptionWithEVs, color= "Wood McKenzie Extreme (99% sales)"), alpha = .7, size = 2)+
    ylab("Gasoline Consumption")+ xlab("year")+
    theme(text= element_text(size =16))+ ylim(0,1.5e+11)+
    ggtitle("Wood-McKenzie scenarios for growth in EVs show little reductions in ARPA-E model")



### after plotcon 2016 visuals 

require(vegalite)

dat <- jsonlite::fromJSON('[
    {"a": "A","b": 28}, {"a": "B","b": 55}, {"a": "C","b": 43},
    {"a": "D","b": 91}, {"a": "E","b": 81}, {"a": "F","b": 53},
    {"a": "G","b": 19}, {"a": "H","b": 87}, {"a": "I","b": 52}
  ]')

vegalite() %>%
  add_data(dat) %>%
  encode_x("a", "ordinal") %>%
  encode_y("b", "quantitative") %>%
  mark_bar() -> vl

 vl


# story 

ggplot()+
    geom_line(data= EVBAU, aes()



#### VMT forcing 

ggplot()+ geom_point(data= VMT_predict, aes(years, VMT, color = "predicted"))+
    geom_point(data = VMT_df_data, aes(years, VMT, color = "data"))


ggplot()+geom_point(data=outputDF.75.5a, aes(x=scaleYear+1994, y= propEVS))
ggplot()+geom_point(data=outputDF.100.5a, aes(x=scaleYear, y= propEVS))


s_curve_test_.1 = data.frame(
    year = c(1994:2050), 
    percent= s_curve(.1, START_YEAR = 1994, END_YEAR = 2050, T_ADOPT=20)
    )

s_curve_test_.75 = data.frame(
    year = c(1994:2020),
    percent = s_curve(.75, START_YEAR = 1994, END_YEAR = 2020, T_ADOPT=5)
    )

s_curve_test_1 = data.frame(
    year = c(1994:2020),
    percent = s_curve(1, START_YEAR = 1994, END_YEAR = 2020, T_ADOPT=5)
    )

ggplot()+   
    geom_point(data = s_curve_test_.1, 
        aes(x= year, y= percent))







Hello Ann! 
















