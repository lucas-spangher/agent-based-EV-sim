setwd("/Users/lucas.spangher/Documents/ARPA-E Files/VMT_ChrisAtkinson")

load("sim.rdata")
require(ggplot2) 
require(Kmisc)
require(bayesm)
require(arm)
require(fGarch)

source("utils.r")
source("simulateFleet.R")
################ SIMULATION ######################
#
# MIT study: 176380000 vehicles on the road  
#  --> do in units of ten thousands. Each car you sample represents 10000 cars
# RITA statistics: in 1995 average car age was 8.4

# predDFs has efficiency predictions by car type 
# predPropsS

# initializing it

start = 1763
changeVMT=-.005
initialVMT=16000
vTypes = unique(fuelDF$VehicleType)

## the electric car component 

percentEVYearly =0.1
efficiencyEV = 100
replacedByEV= c("Car", "Car SUV")

# running it for each year 
source("utils.r")
source("simulateFleet.R")

vehicleEntrancesFast = data.frame(
	type = c("Car","Car SUV", "Truck SUV","Van","Pickup"), # truck SUV
	year = c(2016, 2017, 2018, 2018, 2019)) 

vehicleEntrancesMedium = data.frame(
	type = c("Car","Car SUV", "Truck SUV","Van","Pickup"),
	year = c(2016, 2020, 2022, 2024, 2028))

vehicleEntrancesSlow = data.frame(
	type = c("Car","Car SUV", "Truck SUV","Van","Pickup"),
	year = c(2016, 2025, 2030, 2035, 2040))

vehicleEntrancesVSlow = data.frame(
	type = c("Car","Car SUV", "Truck SUV","Van","Pickup"),
	year = c(2016, 2035, 2024, 2075, 2080))

vehicleEntrancesNoVan = data.frame(
	type = c("Car","Car SUV", "Truck SUV","Van","Pickup"),
	year = c(2016, 2020, 2024, 2100, 2028))

vehicleEntrancesNoVanOrPickup = data.frame(
	type = c("Car","Car SUV", "Truck SUV","Van","Pickup"),
	year = c(2016, 2020, 2024, 2100, 2100))

vehicleEntrancesNoTruck = data.frame(
	type = c("Car","Car SUV", "Truck SUV","Van","Pickup"),
	year = c(2016, 2020, 2100, 2100, 2028))

vehicleEntrancesNoCarSUV = data.frame(
	type = c("Car","Car SUV", "Truck SUV","Van","Pickup"),
	year = c(2016, 2100, 2024, 2024, 2028))

vehicleEntrancesNoCar = data.frame(
	type = c("Car","Car SUV", "Truck SUV","Van","Pickup"),
	year = c(2100, 2020, 2024, 2024, 2028))

vehicleEntrancesSlow$type[which(vehicleEntrancesSlow$year<2031)]

EVBAU = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=0.00001, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, enterSwitch = TRUE, vehicleEntrances = vehicleEntrancesSlow)
outputDF.10 = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.1, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, enterSwitch = TRUE, vehicleEntrances = vehicleEntrancesSlow)
outputDF.50 = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.5, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, enterSwitch = TRUE, vehicleEntrances = vehicleEntrancesSlow)
outputDF.75 = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.75, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, enterSwitch = TRUE, vehicleEntrances = vehicleEntrancesSlow)
outputDF.100 = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.99, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, enterSwitch = TRUE, vehicleEntrances = vehicleEntrancesSlow)

outputDF.50.fast = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.5, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, enterSwitch = TRUE, vehicleEntrances = vehicleEntrancesFast)
outputDF.50.medium = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.5, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, enterSwitch = TRUE, vehicleEntrances = vehicleEntrancesMedium)
outputDF.50.slow = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.5, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, enterSwitch = TRUE, vehicleEntrances = vehicleEntrancesSlow)
outputDF.50.vslow = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.5, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, enterSwitch = TRUE, vehicleEntrances = vehicleEntrancesVSlow)
outputDF.50.noVan = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.5, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, enterSwitch = TRUE, vehicleEntrances = vehicleEntrancesNoVan)
outputDF.50.noVanOrPickup = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.5, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, enterSwitch = TRUE, vehicleEntrances = vehicleEntrancesNoVanOrPickup)
outputDF.50.noTruck = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.5, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, enterSwitch = TRUE, vehicleEntrances = vehicleEntrancesNoTruck)
outputDF.50.noCar = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.5, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, enterSwitch = TRUE, vehicleEntrances = vehicleEntrancesNoCar)
outputDF.50.noCarSUV = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.5, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, enterSwitch = TRUE, vehicleEntrances = vehicleEntrancesNoCarSUV)


outputDF.05 = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.05, START_YEAR, END_YEAR), start= 17630, VMTcsv = VMT_by_age)
outputDF.10 = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.1, START_YEAR, END_YEAR), start= 17630, VMTcsv = VMT_by_age)
outputDF.25 = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.25, START_YEAR, END_YEAR), start= 17630, VMTcsv = VMT_by_age)
outputDF.50 = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.5, START_YEAR, END_YEAR), start= 17630, VMTcsv = VMT_by_age)
outputDF.72 = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.72, START_YEAR, END_YEAR), start= 17630, VMTcsv = VMT_by_age)
outputDF.75 = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.75, START_YEAR, END_YEAR), start= 17630, VMTcsv = VMT_by_age)
outputDF.78 = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.78, START_YEAR, END_YEAR), start= 17630, VMTcsv = VMT_by_age)
outputDF.100 = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.99, START_YEAR, END_YEAR), start= 17630, VMTcsv = VMT_by_age)

BAU = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs,  percentEVYearly=0, start= 17630, VMTcsv = VMT_by_age)
carSim = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.25, start= 17630, VMTcsv = VMT_by_age, replacedByEV = c("Car"))
carCarSUV = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.25, start= 17630, VMTcsv = VMT_by_age,replacedByEV = c("Car","Car SUV"))
carCarSUVVan = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.25, start= 17630, VMTcsv = VMT_by_age,replacedByEV = c("Car","Car SUV","Van"))
carCarSUVVanTruck = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.25, start= 17630, VMTcsv = VMT_by_age,replacedByEV = c("Car","Car SUV","Van","Truck SUV"))
carCarSUVVanTruckPickup = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=.25, start= 17630, VMTcsv = VMT_by_age,replacedByEV = c("Car","Car SUV","Van","Truck SUV","Pickup"))

BAU = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs,  percentEVYearly=0, start= 17630, VMTcsv = VMT_by_age, percentRetrofit =0)
carSimRetro.05 = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=0, start= 17630, VMTcsv = VMT_by_age, replacedByEV = c("Car"), percentRetrofit =.05)
carCarSUVRetro.05 = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=0, start= 17630, VMTcsv = VMT_by_age,replacedByEV = c("Car","Car SUV"), percentRetrofit =.05)
carCarSUVVanRetro.05 = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=0, start= 17630, VMTcsv = VMT_by_age,replacedByEV = c("Car","Car SUV","Van"), percentRetrofit =.05)
carCarSUVVanTruckRetro.05 = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=0, start= 17630, VMTcsv = VMT_by_age,replacedByEV = c("Car","Car SUV","Van","Truck SUV"), percentRetrofit =.05)
carCarSUVVanTruckPickupRetro.05 = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=0, start= 17630, VMTcsv = VMT_by_age,replacedByEV = c("Car","Car SUV","Van","Truck SUV","Pickup"), percentRetrofit =.05)




CashForClunkers=list()
START_YEAR=1996
END_YEAR=2100
CashForClunkers[[1]] = simulateFleet(START_YEAR=START_YEAR, END_YEAR=END_YEAR, buyingRate= .5, scrappingRate=.5, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.1, START_YEAR, END_YEAR), start= 17630, VMT_by_age=VMT_by_age)
CashForClunkers[[2]] = simulateFleet(START_YEAR=START_YEAR, END_YEAR=END_YEAR, buyingRate= 1, scrappingRate= 1, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.1, START_YEAR, END_YEAR), start= 17630, VMT_by_age=VMT_by_age)
CashForClunkers[[3]] = simulateFleet(START_YEAR=START_YEAR, END_YEAR=END_YEAR, buyingRate= 1.25, scrappingRate= 1.25, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.1, START_YEAR, END_YEAR), start= 17630, VMT_by_age=VMT_by_age)
CashForClunkers[[4]] = simulateFleet(START_YEAR=START_YEAR, END_YEAR=END_YEAR, buyingRate= 1.5, scrappingRate= 1.5, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.1, START_YEAR, END_YEAR), start= 17630, VMT_by_age=VMT_by_age)
CashForClunkers[[5]] = simulateFleet(START_YEAR=START_YEAR, END_YEAR=END_YEAR, buyingRate= 2, scrappingRate=2, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.1, START_YEAR, END_YEAR), start= 17630, VMT_by_age=VMT_by_age)


CashForClunkers2=list()
START_YEAR=1996
CashForClunkers2[[1]] = simulateFleet(START_YEAR=START_YEAR, END_YEAR=END_YEAR, buyingRate= .5, scrappingRate=.5, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.1, START_YEAR, END_YEAR),
 start= 17630, VMT_by_age=VMT_by_age, 
 enterSwitch=TRUE, vehicleEntrances = vehicleEntrancesMedium)
CashForClunkers2[[2]] = simulateFleet(START_YEAR=START_YEAR, END_YEAR=END_YEAR, buyingRate= 1, scrappingRate= 1, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.1, START_YEAR, END_YEAR),
 start= 17630, VMT_by_age=VMT_by_age, 
 enterSwitch=TRUE, vehicleEntrances = vehicleEntrancesMedium)
CashForClunkers2[[3]] = simulateFleet(START_YEAR=START_YEAR, END_YEAR=END_YEAR, buyingRate= 1.25, scrappingRate= 1.25, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.1, START_YEAR, END_YEAR),
 start= 17630, VMT_by_age=VMT_by_age, 
 enterSwitch=TRUE, vehicleEntrances = vehicleEntrancesMedium)
CashForClunkers2[[4]] = simulateFleet(START_YEAR=START_YEAR, END_YEAR=END_YEAR, buyingRate= 1.5, scrappingRate= 1.5, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.1, START_YEAR, END_YEAR),
 start= 17630, VMT_by_age=VMT_by_age, 
 enterSwitch=TRUE, vehicleEntrances = vehicleEntrancesMedium)
CashForClunkers2[[5]] = simulateFleet(START_YEAR=START_YEAR, END_YEAR=END_YEAR, buyingRate= 2, scrappingRate=2, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=s_curve(.1, START_YEAR, END_YEAR),
 start= 17630, VMT_by_age=VMT_by_age, 
 enterSwitch=TRUE, vehicleEntrances = vehicleEntrancesMedium)



##### wood mckenzie analysis 
t = c(1:(END_YEAR-START_YEAR+1))
percentEVYearly.0= .001/(1+exp(18 - .58*t))
percentEVYearly.12= .12/(1+exp(18 - .58*t))
percentEVYearly.33= .33/(1+exp(18 - .58*t))
percentEVYearly.85= .85/(1+exp(18 - .58*t))
percentEVYearly.99= .99/(1+exp(18 - .58*t))

BAU = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=0, start= 17630, VMTcsv = VMT_by_age)
wmDF.12 = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=percentEVYearly.12, start= 17630, VMTcsv = VMT_by_age)
wmDF.85 = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=percentEVYearly.85, start= 17630, VMTcsv = VMT_by_age)
wmDF.99 = simulateFleet(START_YEAR=1996, END_YEAR=2050, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=percentEVYearly.99, start= 17630, VMTcsv = VMT_by_age)


### middle case 
midCase =simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions=predPropsS, predDFs=predDFs, percentEVYearly=percentEVYearly.33, start= 17630, VMTcsv = VMT_by_age, EVS_START = 2020)
EVBAU =simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=percentEVYearly.0, start= 17630, VMTcsv = VMT_by_age, EVS_START = 2010)



### probs

EVBAU = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, percentEVYearly=0.00001, 
	start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=10)

vehicleEntrancesFast = data.frame(
	type = c("Car","Car SUV", "Truck SUV","Van","Pickup"), # truck SUV
	year = c(2016, 2017, 2018, 2018, 2019)) 

vehicleEntrancesMedium = data.frame(
	type = c("Car","Car SUV", "Truck SUV","Van","Pickup"),
	year = c(2016, 2020, 2022, 2024, 2028))

vehicleEntrancesSlow = data.frame(
	type = c("Car","Car SUV", "Truck SUV","Van","Pickup"),
	year = c(2016, 2022, 2028, 2031, 2035))

# Low 

outputDF.10.5a = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.10,start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=5,
	enterSwitch = TRUE, vehicleEntrances = vehicleEntrancesFast)
outputDF.75.5a = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.75, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=5,
	enterSwitch=TRUE, vehicleEntrances=vehicleEntrancesFast)
outputDF.100.5a = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.99, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=5,
	enterSwitch=TRUE, vehicleEntrances=vehicleEntrancesFast)
outputDF.10.10a = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.10,	start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=10,
	vehicleEntrances=vehicleEntrancesMedium)
outputDF.75.10a = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.75, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=10,
	vehicleEntrances=vehicleEntrancesMedium)
outputDF.100.10a = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.99, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=10,
	vehicleEntrances=vehicleEntrancesMedium)
outputDF.10.20a = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.10,	start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=20,
	vehicleEntrances=vehicleEntrancesSlow)
outputDF.75.20a = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.75, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=20,
	vehicleEntrances=vehicleEntrancesSlow)
outputDF.100.20a = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.99, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=20,
	vehicleEntrances=vehicleEntrancesSlow)

write.csv(EVBAU, "EVBAU.csv")
write.csv(outputDF.10.5a, "outputDF.10.5a.csv")
write.csv(outputDF.75.5a, "outputDF.50.5a.csv")
write.csv(outputDF.100.5a ,"outputDF.100.5a.csv")
write.csv(outputDF.10.10a,"outputDF.10.10a.csv")
write.csv(outputDF.75.10a , "outputDF.50.10a.csv")
write.csv(outputDF.100.10a,"outputDF.100.10a.csv")
write.csv(outputDF.10.20a,"outputDF.10.20a.csv")
write.csv(outputDF.75.20a,"outputDF.50.20a.csv")
write.csv(outputDF.100.20a,"outputDF.100.20a.csv")



# calculating electricity demand: 

# gallons of gas equivalent * 33.4 kWh

gas_to_kWh_conversion_vec = rep(.031, length= END_YEAR-1996+1)

electricityDemandDF = data.frame(BAU = EVBAU$EV_VMT / (EVBAU$EV_Efficiency * gas_to_kWh_conversion_vec),
	slow.low = outputDF.10.20a$EV_VMT/(outputDF.10.20a$EV_Efficiency * gas_to_kWh_conversion_vec),
	slow.medium = outputDF.75.20a$EV_VMT/(outputDF.75.20a$EV_Efficiency * gas_to_kWh_conversion_vec),
	slow.high = outputDF.100.20a$EV_VMT/(outputDF.100.20a$EV_Efficiency * gas_to_kWh_conversion_vec),
	medium.low = outputDF.10.10a$EV_VMT/(outputDF.10.10a$EV_Efficiency * gas_to_kWh_conversion_vec),
	medium.medium = outputDF.75.10a$EV_VMT/(outputDF.75.10a$EV_Efficiency * gas_to_kWh_conversion_vec),
	medium.high = outputDF.100.10a$EV_VMT/(outputDF.100.10a$EV_Efficiency * gas_to_kWh_conversion_vec),
	fast.low = outputDF.10.5a$EV_VMT/(outputDF.10.5a$EV_Efficiency * gas_to_kWh_conversion_vec),
	fast.medium = outputDF.75.5a$EV_VMT/(outputDF.75.5a$EV_Efficiency * gas_to_kWh_conversion_vec),
	fast.high = outputDF.100.5a$EV_VMT/(outputDF.100.5a$EV_Efficiency * gas_to_kWh_conversion_vec)
	) # check hat this scales 

electricityDemandDF$year = 1996:END_YEAR

electricityDemandDF[is.na(electricityDemandDF)]=0
electricityDemandDF_TWh = electricityDemandDF*1e-9 * 1e4 # kwh to twh conversion, and then scale up 
electricityDemandDF_TWh$year  = 1996:END_YEAR

write.csv(electricityDemandDF_TWh, "electricityDemandDF_TWh.csv")

#### Using the Power plant carbon intensity output 

# carbon intensity: kg CO2e / kWh  

carbonIntensityScenarios = read.csv("/Users/lucas.spangher/Documents/ARPA-E Files/PowerPlantModel/carbonIntensityDF.csv")


outputDF.BAU = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=0,	start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016)
outputDF.10.5a_wNuclear = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.10,	start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=5,
	fusion_Switch = TRUE, fusion_CI = carbonIntensityScenarios, fusion_Scenario = "low.fast",
	vehicleEntrances=vehicleEntrancesFast)
outputDF.75.5a_wNuclear = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.75, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=5,
	fusion_Switch = TRUE, fusion_CI = carbonIntensityScenarios, fusion_Scenario = "medium.fast",
	vehicleEntrances=vehicleEntrancesFast)
outputDF.100.5a_wNuclear = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.99, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=5,
	fusion_Switch = TRUE, fusion_CI = carbonIntensityScenarios, fusion_Scenario = "high.fast",
	vehicleEntrances=vehicleEntrancesFast)
outputDF.10.10a_wNuclear = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.10,	start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=10,
	fusion_Switch = TRUE, fusion_CI = carbonIntensityScenarios, fusion_Scenario = "low.medium",
	vehicleEntrances=vehicleEntrancesMedium)
outputDF.75.10a_wNuclear = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.75, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=10,
	fusion_Switch = TRUE, fusion_CI = carbonIntensityScenarios, fusion_Scenario = "medium.medium",vehicleEntrances=vehicleEntrancesMedium)
outputDF.100.10a_wNuclear = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.99, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=10,
	fusion_Switch = TRUE, fusion_CI = carbonIntensityScenarios, fusion_Scenario = "high.medium",
	vehicleEntrances=vehicleEntrancesMedium)
outputDF.10.20a_wNuclear = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.10,	start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=20,
	fusion_Switch = TRUE, fusion_CI = carbonIntensityScenarios, fusion_Scenario = "low.slow",
	vehicleEntrances=vehicleEntrancesSlow)
outputDF.75.20a_wNuclear = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.75, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=20,
	fusion_Switch = TRUE, fusion_CI = carbonIntensityScenarios, fusion_Scenario = "medium.slow",
	vehicleEntrances=vehicleEntrancesSlow)
outputDF.100.20a_wNuclear = simulateFleet(START_YEAR=1996, END_YEAR=2100, predictedProportions= predPropsS, predDFs=predDFs, 
	percentEVYearly=.99, start= 17630, VMT_by_age = VMT_by_age, EVS_START = 2016, T_ADOPT=20,
	fusion_Switch = TRUE, fusion_CI = carbonIntensityScenarios, fusion_Scenario = "high.slow",
	vehicleEntrances=vehicleEntrancesSlow)

write.csv(outputDF.BAU, "EVBAU.csv")
write.csv(outputDF.10.5a_wNuclear, "outputDF.10.5a_wNuclear.csv")
write.csv(outputDF.75.5a_wNuclear, "outputDF.50.5a_wNuclear.csv")
write.csv(outputDF.100.5a_wNuclear,"outputDF.100.5a_wNuclear.csv")
write.csv(outputDF.10.10a_wNuclear,"outputDF.10.10a_wNuclear.csv")
write.csv(outputDF.75.10a_wNuclear, "outputDF.50.10a_wNuclear.csv")
write.csv(outputDF.100.10a_wNuclear,"outputDF.100.10a_wNuclear.csv")
write.csv(outputDF.10.20a_wNuclear,"outputDF.10.20a_wNuclear.csv")
write.csv(outputDF.75.20a_wNuclear,"outputDF.50.20a_wNuclear.csv")
write.csv(outputDF.100.20a_wNuclear,"outputDF.100.20a_wNuclear.csv")

### staggered entrances:















