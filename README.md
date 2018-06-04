# agent-based-EV-sim
Accompanying data and code for a paper submitted to Transport Series D. 


dataprep.R contains all the commands for reading associated files into R and converting them for use later on in the scripts. This may be skipped by reading into data "sim.rdata" -- this line is included in the beginning of "simulation.r"

simulation.R is the main file for running scenarios. Each model run is a run of the function, simulateFleet(). Various input parameters are changed in each run of the function. This is the main file you should familiarize yourself with if you would like to make more runs of the model. 

simulateFleet.R is the file in which the simulateFleet() function defined. You should look into this file if you'd like to see how the mechanics of the model run. 

graphing.R contains many different graphs composed using ggplot(). All the graphs in the paper are here, plus others. 

appendix.R contains some ancillary functions. 
