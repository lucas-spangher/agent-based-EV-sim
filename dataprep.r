######### Poisson process model ########## 
setwd("/Users/lucas.spangher/Documents/ARPA-E Files/VMT_ChrisAtkinson")

require(ggplot2) 
require(Kmisc)
require(bayesm)
require(arm)
require(fGarch)
require(reshape)

load("sim.rdata")

####### Scaffolding new data 
START_YEAR=1996
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

fuelDF = dapply(fuelDF, function(var){
    if (grepl("%", var[6])){
        var = gsub("%","",var)
        var = as.numeric(as.character(var))
        var = var*.01
    }
    if (sum(!is.na(as.numeric(substring(var,1,1))))){
             var= as.numeric(as.character(var))
    }
    return(var)
    })

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

######## Conclusion: use this predicted variable. 

########### PREDICT VEHICLE SALES TO DO VEHICLE PROPORTIONS
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

##########  gasoline sales comparison

gasoline= read.csv("U.S._Total_Gasoline_Retail_Sales_by_Refiners.csv", skip=4)
names(gasoline)=c("year","thousandGallonsDay")
gasoline$thousandGallonsDay=as.numeric(as.character(gasoline$thousandGallonsDay))
gasoline= gasoline[gasoline$year>START_YEAR-1,]
gasoline=gasoline[order(gasoline$year),]
gasoline$totalgasoline = gasoline$thousandGallonsDay*1000*365

totalVMT = na.omit(read.csv("10315_vmt_traveled.csv"))
totalVMT=totalVMT[totalVMT$X...Year.>START_YEAR-1,]
totalVMT$Million.Miles = gsub(",","", totalVMT$Million.Miles)
totalVMT$Million.Miles=as.numeric(as.character(totalVMT$Million.Miles))

obsEff = (totalVMT$Million.Miles*100000)/(gasoline$thousandGallonsDay*1000*365)
obsEff[17:20]=24

effDF = data.frame(obsEff= obsEff, labelEff = allDF$CombE[allDF$CarorTruck=="All"&allDF$ModelYear>(START_YEAR-1)])
effMod = lm(obsEff~0+labelEff, data= effDF)

# VMT csv 

VMTcsv = read.csv("VMT.csv")
VMTcsv$Age = VMTcsv$X
VMT_by_age = as.data.frame(apply(VMTcsv, 2, function(var){
    var = gsub(",","",var)
    var = as.numeric(as.character(var))
    return(var)
    }))
row.names(VMT_by_age) = VMT_by_age$Age
names(VMT_by_age) = gsub("[.]"," ", names(VMT_by_age))
VMT_by_age$EV= VMT_by_age$Car
VMT_by_age$"X 1"=NULL
VMT_by_age$X=NULL
VMT_by_age = melt(VMT_by_age, id="Age")
names(VMT_by_age)[names(VMT_by_age)=="variable"] = "VehicleType"

save.image("sim.rdata")




















