##OMNI
#Loads all R scrips and relevant libraries

library(tidyverse)
library(pbapply)
library(dplyr) #for n_distinct
library(zoo) #for rollmean
library(matrixStats) #for rowVars
#library(Directional) #For directional statistics
#library(CircStats)
library(circular)
library(bpnreg)


source("Timeseries.R")
source("Mass Timeseries.R")

#Import and processing
source("Import.R")

#Superfunctions
source("Subfunctions/massfunction.R")       #Produces a grid of graphs from different data files at the same time slice
source("Subfunctions/timeslice.R")          #Produces a grid of graphs from the same data file at different time slices

#Subfunctions
source("Subfunctions/active.R")             #Determines which trajectories are active during a given timeframe
#source("Subfunctions/circVars.R")          #Used to extract circular statistics; currently unused
source("Subfunctions/extract.R")            #Extracts a trajs/locs array from a masstrajs/masslocs list
source("Subfunctions/legendno.R")           #Adds color-coded legend to the graph
source("Subfunctions/minmax.R")             #Finds/converts mintime and maxtime arguments in various functions
source("Subfunctions/outbound.R")           #Filters data to only inbound or outbound ants
source("Subfunctions/rm0.R")                #Removes trajectories with a median speed of 0 from trajs-style data (used in Import.R)


#Timeseries functions
source("antleng.R")                         #Graphs the length/duration of trajectories over time
source("antnetx.R")                         #Graphs the x-component of velocity over time
source("antno.R")                           #Graphs number of ants in a given frame

source("antmedians.R")
source("anthead.R")



#Vector functions



#Distribution functions
source("antheaddist.R")

source("antmeansdist.R")
source("antlengdist.R")



#Other
source("antlengvar.R")
source("antnumspeed.R")
source("antnumvar.R")
source("antnumleng.R")

trajs <- Import.single(b2023_06_28L)
locs <- Import.single(b2023_06_28L, output = 1)
masstrajs <- Import.mass()
#masslocs <- Import.mass(output.mass = 1)
