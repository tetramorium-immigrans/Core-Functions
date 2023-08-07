#####Setting up data arrays######

#add a way to decide on the path of the data other than the default
library(pbapply)

#dat = data to import
#output = whether locations (1) or velocities (2) are returned
#sconv = conversion factor from pixels to cm (0.4/25 cm/pixel)
#vconv = conversion factor from pixels/frame to cm/sec (5 frames/sec * 0.4/25 cm/pixel)
#default.path = runs from a hard-coded location for convenience, but can be told to look for a file elsewhere if necessary

Import.single <- function(path, output = 2, sconv = 0.016, vconv = 0.08, default.path = TRUE){
  
  #If/else for default.path; FALSE IS CURRENTLY NOT WORKING
  if(default.path == TRUE){
    trajdat <- read.csv(paste0("D:\\Processed\\",deparse(substitute(path)), ".csv"))
  }else{
    trajdat <- read.csv(deparse(substitute(path)))
  }
  
  if(output == 2){
    trajs <- array(data=NA, dim=c(max(trajdat$Time), max(trajdat$X..Trajectory), 4)) #[,,1-4] = 1) x component, 2) y component, 3) direction in radians, and 4) magnitude of velocity vector
    
    for (i in 1:nrow(trajdat)) {
      o <- trajdat$Time[i]                                  #Get time point; [i,,] in the main array
      n <- trajdat$X..Trajectory[i]                         #Get trajectory being analyzed; [,i,] in the main array
      
      if(trajdat$heading[i] == -1){                         #Heading -1 indicates no movement in AnTracks so enter 0 for all the Cartesian and polar vector values
        trajs[o,n,] <- 0
      } else{                                               #Otherwise convert AnTracks' degree-based polar coordinates to x,y in [,,1],[,,2], respectively, for graphing; [,,3] and [,,4] are retained as magnitude and direction for some niche uses
        trajs[o,n,1] <- trajdat$speed[i] * vconv * cos(-trajdat$heading[i] * pi / 180)  #Negative trajdat$heading because AnTracks operates in the 4th quadrant rather than 1st and need to reverse directions
        trajs[o,n,2] <- trajdat$speed[i] * vconv * sin(-trajdat$heading[i] * pi / 180)  
        trajs[o,n,3] <- -trajdat$heading[i]
        trajs[o,n,4] <- trajdat$speed[i]*vconv
      }
    }
    return(trajs)
    
  }else if(output == 1){
    locs <- array(data=NA, dim=c(max(trajdat$Time), max(trajdat$X..Trajectory), 2))  #[,,1-2] = x,y coordinates of location vector
    maxy <- max(trajdat$y)                                  #To save finding max(y) over and over
    
    for(i in 1:nrow(trajdat)){
      o <- trajdat$Time[i]                                  #Get time point; [i,,] in the main array
      n <- trajdat$X..Trajectory[i]                         #Get trajectory being analyzed; [,i,] in the main array
      
      locs[o,n,1] <- trajdat$x[i]*sconv
      locs[o,n,2] <- (maxy - trajdat$y[i])*sconv  #(max - value) because AnTracks' coordinates start in the upper left; need to reverse the y for plotting purposes
    }
    return(locs)
    
  }else{
    print("Invalid output value, choose 1 or 2")
  }
}


#This function imports multiple files and has the option of processing them
Import.massold <- function(process = TRUE, output.mass = 2, sconv.mass = 0.016, vconv.mass = 0.08){
  owd <- getwd()
  setwd("D:\\Processed\\")
  temp = list.files(pattern="*.csv")
  trajlist = pblapply(temp, read.csv)
  setwd(owd)
  
  if(process == TRUE){
    #trajlist <- pblapply(trajlist, data.frame)
    trajlist <- pblapply(trajlist, Process.single, output = output.mass, sconv = sconv.mass, vconv = vconv.mass)
  }
  
  #output <- list(temp, myfiles)
  return(trajlist)
}

#Function to get names of files
Import.mass <- function(process = FALSE, output.mass = 2, sconv.mass = 0.016, vconv.mass = 0.08){
  owd <- getwd()                          #Save old working directory because for some reason the following functions only work in the current working directory
  setwd("D:\\Processed\\")
  naymes = list.files(pattern="*.csv")    #Get names of .csv files in wd
  trajlist = pblapply(naymes, read.csv)   #read.csv all the files in the folder
  setwd(owd)
  
  naymes <- gsub('.csv','',naymes)        #get rid of .csv at the end of the file names to make graph labeling prettier
  outputtrajs <- list(naymes, trajlist)   #combine names and trajs into a single output list for the function
  names(outputtrajs) <- c("Names", "Data")#
  
  if(process == TRUE){
    #trajlist <- pblapply(trajlist, data.frame)
    
    #outputtrajs[2] <- pblapply(outputtrajs[2], Process.single, output = output.mass, sconv = sconv.mass, vconv = vconv.mass)
    
    for(i in 1:length(outputtrajs$Data)){
      outputtrajs$Data[i] <- Process.single(outputtrajs$Data[i], output = output.mass, sconv = sconv.mass, vconv = vconv.mass)
      print(i)
    }
  }
  
  return(outputtrajs)
}


Process.single <- function(trajdat, output = 2, sconv = 0.016, vconv = 0.08){
  trajdat <- data.frame(trajdat)
  
  if(output == 2){
    trajs <- array(data=NA, dim=c(max(trajdat$Time), max(trajdat$X..Trajectory), 4)) #[,,1-4] = 1) x component, 2) y component, 3) direction in radians, and 4) magnitude of velocity vector
    
    for (i in 1:nrow(trajdat)) {
      o <- trajdat$Time[i]                                  #Get time point; [i,,] in the main array
      n <- trajdat$X..Trajectory[i]                         #Get trajectory being analyzed; [,i,] in the main array
      
      if(trajdat$heading[i] == -1){                         #Heading -1 indicates no movement in AnTracks so enter 0 for all the Cartesian and polar vector values
        trajs[o,n,] <- 0
      } else{                                               #Otherwise convert AnTracks' degree-based polar coordinates to x,y in [,,1],[,,2], respectively, for graphing; [,,3] and [,,4] are retained as magnitude and direction for some niche uses
        trajs[o,n,1] <- trajdat$speed[i] * vconv * cos(-trajdat$heading[i] * pi / 180)  #Negative trajdat$heading because AnTracks operates in the 4th quadrant rather than 1st and need to reverse directions
        trajs[o,n,2] <- trajdat$speed[i] * vconv * sin(-trajdat$heading[i] * pi / 180)  
        trajs[o,n,3] <- -trajdat$heading[i]
        trajs[o,n,4] <- trajdat$speed[i]*vconv
      }
    }
    return(trajs)
    
  }else if(output == 1){
    locs <- array(data=NA, dim=c(max(trajdat$Time), max(trajdat$X..Trajectory), 2))  #[,,1-2] = x,y coordinates of location vector
    maxy <- max(trajdat$y)                                  #To save finding max(y) over and over
    
    for(i in 1:nrow(trajdat)){
      o <- trajdat$Time[i]                                  #Get time point; [i,,] in the main array
      n <- trajdat$X..Trajectory[i]                         #Get trajectory being analyzed; [,i,] in the main array
      
      locs[o,n,1] <- trajdat$x[i]*sconv
      locs[o,n,2] <- (maxy - trajdat$y[i])*sconv  #(max - value) because AnTracks' coordinates start in the upper left; need to reverse the y for plotting purposes
    }
    return(locs)
    
  }else{
    print("Invalid output value, choose 1 or 2")
  }
}


#antmax <- max(rowSums(!is.na(trajs[,,1])))             #Greatest number of trajectories in region during any frame
#mintime <- min(trajdat$Time)                           #Starting frame
#maxtime <- max(trajdat$Time)                           #Ending frame
#maxtraj <- max(trajdat$X..Trajectory)                  #Number of total trajectories




#locs[trajdat$Time, trajdat$X..Trajectory, 1] <- trajdat$x*sconv
#locs[trajdat$Time, trajdat$X..Trajectory, 1] <- (max(trajdat$y) - trajdat$y)*sconv