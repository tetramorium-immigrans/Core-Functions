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
library(Matrix)
#library(psych)
#library(lmomco) #harmonic mean
library(AICcmodavg)

source("Timeseries.R")
source("Mass Timeseries.R")

#Import and processing
source("Import.R")

#Superfunctions
source("Subfunctions/massfunction.R")       #Produces a grid of graphs from different data files at the same time point
source("Subfunctions/timeslice.R")          #Produces a grid of graphs from the same data file at different time points

#Subfunctions
source("Subfunctions/active.R")             #Determines which trajectories are active during a given timeframe
source("Subfunctions/antplot.R")            #General plotting function used for time series graphs (calls legendno as well)
#source("Subfunctions/circVars.R")          #Used to extract circular statistics (no longer used)
source("Subfunctions/direction.R")          #Filters data to only inbound or outbound ants
#source("Subfunctions/extract.R")            #Extracts a trajs/locs array from a masstrajs/masslocs list (no longer used)
source("Subfunctions/interna.R")            #Replaces 0s in data with NAs then uses approx to interpolate values
source("Subfunctions/legendno.R")           #Adds color-coded legend to the graph
source("Subfunctions/minmax.R")             #Finds/converts mintime and maxtime arguments in various functions
source("Subfunctions/rm0.R")                #Removes trajectories with a median speed of 0 from trajs-style data (used in Import.R)


#Timeseries functions
source("antleng.R")                         #Graphs the length/duration of trajectories over time
source("antnetx.R")                         #Graphs the x-component of velocity over time
source("antno.R")                           #Graphs number of ants over time
source("antphase.R")                        #Graphs rate of change of number of ants
source("antspeed.R")                        #Graphs the speed of ants over time
source("anttort.R")                         #Graphs tortuosity of trajectories over time

source("antmedians.R")
source("anthead.R")
source("antheadvar.R")

source("cmf.R")                             #Cumulative myrmibution function (cmf) that graphs the balance of how many ants have gone out and in over time
source("propx.R")                           #Proportion of ants going in or out at a given time


#Vector functions
source("antlocs.R")                         #Graphs the position and heading vector of all ants at a particular time


#Distribution functions
source("antheaddist.R")

source("antmeansdist.R")
source("antlengdist.R")


#Analysis functions
source("phases.R")


#Other
source("antst.R")                           #Space-time plot for ant movements
source("antfund.R")                         #Fundamental diagrams

source("antlengvar.R")
source("antnumspeed.R")
source("antnumvar.R")
source("antnumleng.R")

#trajs <- Import.single(b2023_06_28L)
#locs <- Import.single(b2023_06_28L, output = 1)
#masstrajs <- Import.mass()
#masslocs <- Import.mass(output.mass = 1)
