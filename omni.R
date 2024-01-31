##OMNI
#Loads all R scrips and relevant libraries

library(tidyverse)
library(pbapply)
library(dplyr) #for n_distinct
library(zoo) #for rollmean
library(matrixStats) #for rowVars


source("Import.R")






source("Timeseries.R")
source("Mass Timeseries.R")

#Import and processing

#Timeseries functions
source("antmedians.R")
source("anthead.R")

#Vector functions

#Distribution functions
source("antmeansdist.R")
source("antlengdist.R")

#Other
source("antnumspeed.R")