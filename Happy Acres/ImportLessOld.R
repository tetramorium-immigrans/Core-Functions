#####Setting up data arrays######

##Import.single + Process.single + Import.mass
#Imports and processes files into a list of sparse matrices 

#path = data to import, defaulting to a hardcoded directory (no quotes, forward slash / between folders)

#process = whether Import.single/.mass should merely import the .csv file or convert it to sparse matrices
#interna = whether to interpolate missing data points
#rm0 = whether to remove trajectories with a median speed of 0 (typically dead ants or other inconsequential objects)
#*Warning: running interna = TRUE and rm0 = FALSE at the same time results in undesirable smoothing out of data

#extended = whether to also process pathlength, distance_to_start, and mean squared distance (included in the .csv file I don't often use them)

#sconv = conversion factor from pixels to cm (27/480 cm/pixel)
#vconv = conversion factor from pixels/frame to cm/sec (5 * 27/480)
#default.path = runs from a hard-coded location for convenience, but can be told to look for a file elsewhere if necessary (NOT WORKING)


#jholding pen
#trajtest <- trajs[,,1]
#trajtest[trajtest == 0] <- -1
#trajtest[is.na(trajtest)] <- 0

#apply(trajs[,,1], 1, function(x){mean(x[x>0])})

#megatraj[[1]][[1]][1,5]


##Import.single

Import.single <- function(path, 
                          process = TRUE, interna = TRUE, rm0 = TRUE,
                          extended = FALSE,
                          sconv = 0.05625, vconv = 0.28125, 
                          default.path = TRUE){
  
  #If/else for default.path; FALSE IS CURRENTLY NOT WORKING
  #(no quotes, forward slash / between folders)
  if(default.path == TRUE){
    trajraw <- read.csv(paste0("D:\\Processed\\",deparse(substitute(path)), ".csv"))
  }else{
    trajraw <- read.csv(deparse(substitute(path)))
  }
  
  if(process == TRUE){
    Process.single(trajdat = trajraw, interna.p = interna, rm0.p = rm0, extended.p = extended, sconv.p = sconv, vconv.p = vconv)
  }else{
    return(trajraw)
  }
  
}

##Process.single

Process.single <- function(trajdat, interna.p = TRUE, rm0.p = TRUE,
                           extended.p = FALSE,
                           sconv.p = 0.05625, vconv.p = 0.28125){
  trajdat <- data.frame(trajdat)  #code won't work if it's not a data frame
  
  #rm0.p - identify trajectories with a median speed of 0 (for use in removal at the end)
  if(rm0.p == TRUE){
    rmlist <- tapply(trajdat$speed, factor(trajdat$X..Trajectory), median)
  }
  
  #For the following variables, check if it exists in tradjat then make a sparse matrix:
  #x
  #y
  #speed
  #heading
  #xvel (checks for speed/heading, as this is calculated from those by the code)
  #yvel (checks for speed/heading, as this is calculated from those by the code)
  #pathlength
  #distance_to_start, shortened to distance in the output
  #msd
  

  #Finding re-used dimensions for the sparse matrices
  sparsedims <- c(max(trajdat$X..Trajectory), max(trajdat$Time))              
  
  #Import will error of x does not exist
  #Initializes the return list, including the vectors of min/max ranges for purposes of interpolation (interna subfunction)
  if("x" %in% colnames(trajdat)){
    x <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
    x[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$x*sconv.p
    
    trajreturn <- list(datmin = apply(x, 1, function(z){min(which(z > 0))}), datmax = apply(x, 1, function(z){max(which(z > 0))}))  
    
    if(interna.p == TRUE){
      trajreturn <- c(trajreturn, x = interna(x, trajreturn$datmin, trajreturn$datmax))
    }else{
      trajreturn <- c(trajreturn, x = x)
    }
  }
  
  if("y" %in% colnames(trajdat)){
    y <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
    y[cbind(trajdat$X..Trajectory, trajdat$Time)] <- (max(trajdat$y) - trajdat$y)*sconv.p  #AnTracks operates in the 4th quadrant so need to flip it to first by reversing based off of max(y)
    
    if(interna.p == TRUE){
      trajreturn <- c(trajreturn, y = interna(y, trajreturn$datmin, trajreturn$datmax))
    }else{
      trajreturn <- c(trajreturn, y = y)
    }
  }
  
  if("speed" %in% colnames(trajdat)){
    speed <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
    #trajdat$speed[trajdat$speed == 0] <- 1e-10                                              #replacing 0 speed entries with 1e-10 to differentiate them from the 0s of the sparse matrix (important for later calculations of mean, etc which ignore 0s)
    speed[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$speed*vconv.p
    
    if(interna.p == TRUE){
      trajreturn <- c(trajreturn, speed = interna(speed, trajreturn$datmin, trajreturn$datmax))
    }else{
      trajreturn <- c(trajreturn, speed = speed)
    }
  }
  
  if("heading" %in% colnames(trajdat)){
    heading <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
    heading[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$heading
    heading[heading == -1] <- 0
    
    if(interna.p == TRUE){
      heading <- interna(heading, trajreturn$datmin, trajreturn$datmax, inter = FALSE)
      trajreturn <- c(trajreturn, heading = -heading * pi/180)
    }else{
      trajreturn <- c(trajreturn, heading = heading)
    }
    
    #heading[cbind(trajdat$X..Trajectory, trajdat$Time)] <- -trajdat$heading *  pi / 180    #AnTracks operates in the 4th quadrant so need to flip it to first, then converting to radians for R's sake
    #trajreturn <- c(trajreturn, heading = -heading * pi / 180)
    #trajreturn <- c(trajreturn, heading = heading)
  }
  
  #xvel
  if("speed" %in% colnames(trajdat) & "heading" %in% colnames(trajdat)){
    xvel <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
    
    xvel <- trajreturn$speed * cos(trajreturn$heading)
    #xvel[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$speed * vconv.p * cos(-trajdat$heading*pi/180)
    trajreturn <- c(trajreturn, xvel = xvel)
  }
  
  #yvel
  if("speed" %in% colnames(trajdat) & "heading" %in% colnames(trajdat)){
    yvel <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
    
    yvel <- trajreturn$speed * sin(trajreturn$heading)
    #yvel[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$speed * vconv.p * sin(-trajdat$heading*pi/180)
    trajreturn <- c(trajreturn, yvel = yvel)
  }
  
  if("pathlength" %in% colnames(trajdat) & extended.p == TRUE){
    pathlength <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
    trajdat$pathlength[trajdat$pathlength == 0] <- 1e-10                                              #replacing 0 speed entries with 1e-10 to differentiate them from the 0s of the sparse matrix (important for later calculations of mean, etc which ignore 0s)
    pathlength[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$pathlength*sconv.p
    
    if(interna.p == TRUE){
      trajreturn <- c(trajreturn, pathlength = interna(pathlength, trajreturn$datmin, trajreturn$datmax))
    }else{
      trajreturn <- c(trajreturn, pathlength = pathlength)
    }
  }
  
  if("distance_to_start" %in% colnames(trajdat) & extended.p == TRUE){
    distance <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
    trajdat$distance_to_start[trajdat$distance_to_start == 0] <- 1e-10                                              #replacing 0 speed entries with 1e-10 to differentiate them from the 0s of the sparse matrix (important for later calculations of mean, etc which ignore 0s)
    distance[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$distance_to_start*sconv.p
    
    if(interna.p == TRUE){
      trajreturn <- c(trajreturn, distance = interna(distance, trajreturn$datmin, trajreturn$datmax))
    }else{
      trajreturn <- c(trajreturn, distance = distance)
    }
  }
  
  if("msd" %in% colnames(trajdat) & extended.p == TRUE){
    msd <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
    trajdat$msd[trajdat$msd == 0] <- 1e-10                                              #replacing 0 speed entries with 1e-10 to differentiate them from the 0s of the sparse matrix (important for later calculations of mean, etc which ignore 0s)
    msd[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$msd*sconv.p    ##DON'T FORGET TO CHECK THIS CONVERSION FACTOR
    
    if(interna.p == TRUE){
      trajreturn <- c(trajreturn, msd = interna(msd, trajreturn$datmin, trajreturn$datmax))
    }else{
      trajreturn <- c(trajreturn, msd = msd)
    }
  }
  
  #rm0.p - removing objects that never move (median speed of 0)
  if(rm0.p == TRUE){
    trajreturn$datmin <- trajreturn$datmin[!rmlist == 0]
    trajreturn$datmax <- trajreturn$datmax[!rmlist == 0]
    
    trajreturn$x <- trajreturn$x[which(!rmlist == 0),]
    trajreturn$y <- trajreturn$y[which(!rmlist == 0),]
    
    trajreturn$speed <- trajreturn$speed[which(!rmlist == 0),]
    trajreturn$heading <- trajreturn$heading[which(!rmlist == 0),]
    
    trajreturn$xvel <- trajreturn$xvel[which(!rmlist == 0),]
    trajreturn$yvel <- trajreturn$yvel[which(!rmlist == 0),]
    
    if(extended.p == TRUE){
      trajreturn$pathlength <- trajreturn$pathlength[which(!rmlist == 0),]
      trajreturn$distance <- trajreturn$distance[which(!rmlist == 0),]
      trajreturn$msd <- trajreturn$msd[which(!rmlist == 0),]
    }
    
    # lapply(trajreturn, function(rm){
    #   rm <- rm[!rmlist == 0,]
    # })
  }
  
  return(trajreturn)
}


##Import.mass
#Function to import and process multiple .csv files at a time
##DEFAULT PATH NOT IMPLEMENTED

#add path here, remove .mass
Import.mass <- function(folder, cate = "*",
                        process = TRUE, interna.mass = TRUE, rm0.mass = TRUE,
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
                         interna.p = interna.mass, rm0.p = rm0.mass, 
                         sconv.p = sconv.mass, vconv.p = vconv.mass)
  }else{print("Skipping processing")}
  
  #Prettying things up
  naymes <- gsub('.csv','',naymes)        #get rid of .csv at the end of the file names to make graph labeling prettier
  masstrajs <- list(naymes, trajlist)     #combine names and trajs into a single output list for the function
  names(masstrajs) <- c("Names", "Data")  #Name columns
  
  return(masstrajs)
}


