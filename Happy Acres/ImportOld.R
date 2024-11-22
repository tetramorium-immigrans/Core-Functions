#####Setting up data arrays######

##Import.single + Process.single
#Imports and processes a file into the appropriate trajs or locs form
#Trajs form is a 3-dimensional array: (time in frames) x (trajectory #) x (x component of velocity vector/y component of velocity vector/heading in polar coordinates/magnitude of velocity vector)
#Locs form is a 3-dimensional array: (time in frames) x (trajectory #) x (x coordinate/y coordinate/path length/distance_to_start)

#path = data to import, defaulting to a hardcoded directory
#output = whether a locs (1) or trajs (2) data frame is returned
#rm0 = keep (0) or remove (1) trajectories with a median speed of 0 (typically dead ants or other inconsequential objects)
#sconv = conversion factor from pixels to cm (27/480 cm/pixel)
#vconv = conversion factor from pixels/frame to cm/sec (5 * 27/480)
#default.path = runs from a hard-coded location for convenience, but can be told to look for a file elsewhere if necessary (NOT WORKING)

##Import.single

Import.single <- function(path, 
                          process = TRUE, output = 2, rm0 = 1,
                          sconv = 0.05625, vconv = 0.28125, 
                          default.path = TRUE){
  
  #If/else for default.path; FALSE IS CURRENTLY NOT WORKING
  if(default.path == TRUE){
    trajraw <- read.csv(paste0("D:\\Processed\\",deparse(substitute(path)), ".csv"))
  }else{
    trajraw <- read.csv(deparse(substitute(path)))
  }
  
  if(process == TRUE){
    Process.single(trajdat = trajraw, output.p = output, rm0.p = rm0, sconv.p = sconv, vconv.p = vconv)
  }else{
    return(trajraw)
  }
  
}

##Process.single

Process.single <- function(trajdat, output.p = 2, rm0.p = 1,
                           sconv.p = 0.05625, vconv.p = 0.28125){
  trajdat <- data.frame(trajdat)
  
  if(output.p == 2){
    trajs <- array(data=NA, dim=c(max(trajdat$Time), max(trajdat$X..Trajectory), 4))
    #trajs <- matrix(data=NA, dim=c(max(trajdat$Time), max(trajdat$X..Trajectory), 4))
    
    for (i in 1:nrow(trajdat)) {
      o <- trajdat$Time[i]                                  #Get time point; [i,,] in the main array
      n <- trajdat$X..Trajectory[i]                         #Get trajectory being analyzed; [,i,] in the main array
      
      if(trajdat$heading[i] == -1){                         #Heading -1 indicates no movement in AnTracks so enter 0 for all the Cartesian and polar vector values
        trajs[o,n,] <- 0
      } else{                                               #Otherwise convert AnTracks' degree-based polar coordinates to x,y in [,,1],[,,2], respectively, for graphing; [,,3] and [,,4] are retained as magnitude and direction
        trajs[o,n,3] <- -trajdat$heading[i] *  pi / 180                #Negative trajdat$heading because AnTracks operates in the 4th quadrant rather than 1st and need to reverse directions
        trajs[o,n,4] <- trajdat$speed[i]*vconv.p
        trajs[o,n,1] <- trajs[o,n,4] * cos(trajs[o,n,3])  
        trajs[o,n,2] <- trajs[o,n,4] * sin(trajs[o,n,3])  
      }
    }
    
    #rm0.p
    if(rm0.p == 1){
      rmlist <- tapply(trajdat$speed, factor(trajdat$X..Trajectory), median)
      trajs <- trajs[,!rmlist ==0,]
    }
    
    return(trajs)
    
  }else if(output.p == 1){
    locs <- array(data=NA, dim=c(max(trajdat$Time), max(trajdat$X..Trajectory), 4))  #[,,1-2] = x,y coordinates of location vector
    maxy <- max(trajdat$y)                                  #To save finding max(y) over and over
    
    for(i in 1:nrow(trajdat)){
      o <- trajdat$Time[i]                                  #Get time point; [i,,] in the main array
      n <- trajdat$X..Trajectory[i]                         #Get trajectory being analyzed; [,i,] in the main array
      
      locs[o,n,1] <- trajdat$x[i]*sconv.p
      locs[o,n,2] <- (maxy - trajdat$y[i])*sconv.p  #(max - value) because AnTracks' coordinates start in the upper left; need to reverse the y for plotting purposes
      
      if(length(trajdat$pathlength[i]) == 0){
        locs[o,n,3] <- NA
      }else{
        locs[o,n,3] <- trajdat$pathlength[i]*sconv.p
      }
      
      if(length(trajdat$distance_to_start[i]) == 0){
        locs[o,n,4] <- NA
      }else{
        locs[o,n,4] <- trajdat$distance_to_start[i]*sconv.p
      }
      
    }
    
    #rm0.p
    if(rm0.p == 1){
      rmlist <- tapply(trajdat$speed, factor(trajdat$X..Trajectory), median)
      locs <- locs[,!rmlist ==0,]
    }
    
    return(locs)
    
  }else{
    print("Invalid output.p value, choose 1 or 2")
  }
}


##Import.mass
#Function to import and optionally process multiple .csv files at a time
##DEFAULT PATH NOT IMPLEMENTED

#add path here, remove .mass
Import.mass <- function(folder, cate = "*",
                        process = TRUE, output.mass = 2, rm0.mass = 1,
                        sconv.mass = 0.05625, vconv.mass = 0.28125,
                        default.path = TRUE){
  
  #Getting the files from a given directory
  owd <- getwd()                           #Save old working directory because for some reason the following functions only work in the current working directory
  #setwd("D:\\Processed\\Mass\\")
  setwd(paste0("D:\\Processed\\",folder))
  naymes <- list.files(pattern=paste0(cate,".+csv"))    #Get names of .csv files in wd
  print("Importing files:")
  print(naymes)
  trajlist <- pblapply(naymes, read.csv)   #read.csv all the files in the folder
  setwd(owd)
  
  #Processing
  if(process == TRUE){
    print("Processing .csv files")
    trajlist <- pbsapply(trajlist, Process.single, 
                         output.p = output.mass, rm0.p = rm0.mass, 
                         sconv.p = sconv.mass, vconv.p = vconv.mass)
  }else{print("Skipping processing")}
  
  #Prettying things up
  naymes <- gsub('.csv','',naymes)        #get rid of .csv at the end of the file names to make graph labeling prettier
  masstrajs <- list(naymes, trajlist)     #combine names and trajs into a single output list for the function
  names(masstrajs) <- c("Names", "Data")  #Name columns
  
  return(masstrajs)
}


