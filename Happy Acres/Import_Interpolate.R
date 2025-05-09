#####Setting up data arrays######

##Import.single + Process.single + Import.mass
#Imports and processes files into a list of sparse matrices 

#path = data to import, defaulting to a hardcoded directory (no quotes, forward slash / between folders)

#process = whether Import.single/.mass should merely import the .csv file or convert it to sparse matrices
#rm0 = whether to remove trajectories with a median speed of 0 (typically dead ants or other inconsequential objects)
#*Warning: running rm0 = FALSE leads to errors if the data has any 0 rows

#extended = whether to also process pathlength, distance_to_start, and mean squared distance (included in the .csv file I don't often use them)

#conv = conversion factor from pixels to cm (27/480 cm/pixel, used for x/y/pathlength/distance/msd conversion)
#frate = conversion factor from pixels to seconds (5s, multiplied along with conv for speed/xvel/yvel)
#default.path = runs from a hard-coded location for convenience, but can be told to look for a file elsewhere if necessary (NOT WORKING)
#mass = flag for whether the function is being used by Import.mass (used mainly to avoid text spam when importing many files)


#jholding pen

#apply(trajs[,,1], 1, function(x){mean(x[x>0])})

#megatraj[[1]][[1]][1,5]


##Import.single

Import.single <- function(path, 
                          process = TRUE, rm0 = TRUE, 
                          extended = FALSE,
                          conv = 0.05625, frate = 5, 
                          default.path = TRUE, mass = FALSE){
  
  #If/else for default.path; FALSE IS CURRENTLY NOT WORKING
  #(no quotes, forward slash / between folders)
  if(default.path == TRUE){
    trajraw <- read.csv(paste0("D:\\Processed\\",deparse(substitute(path)), ".csv"))
  }else{
    trajraw <- read.csv(deparse(substitute(path)))
  }
  
  if(process == TRUE){
    Process.single(trajdat = trajraw, rm0.p = rm0, extended.p = extended, conv.p = conv, frate.p = frate, mass.p = mass)
  }else{
    return(trajraw)
  }
  
}

##Process.single

Process.single <- function(trajdatinput, rm0.p = TRUE,
                           extended.p = FALSE,
                           conv.p = 0.05625, frate.p = 5,
                           mass.p = FALSE){
  trajdat <- data.frame(trajdatinput)  #code won't work if it's not a data frame
  
  #Shows progress bar timers if being individually processed but omits them during mass processing for clarity's sake
  if(mass.p == TRUE){
    pboptions(type = "none")
  }else{
    pboptions(type = "timer")
  }

  ##Setting up sparse matrices for non-extended variables
  
  sparsedims <- c(max(trajdat$X..Trajectory), max(trajdat$Time))
  
  x <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
  y <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
  speed <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
  heading <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
  xvel <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
  yvel <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
  
  ##Populating matrices with data and finding the ranges in which data exists
  
  x[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$x * conv.p
  y[cbind(trajdat$X..Trajectory, trajdat$Time)] <- (max(trajdat$y) - trajdat$y) * conv.p   #AnTracks operates in the 4th quadrant so need to flip it to first by reversing based off of max(y)
  speed[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$speed * conv.p * frate.p
  
  trajdat$heading[trajdat$heading == 0] <- 1e-7                                 #Replacing true 0s with near-0 placeholders so not to be confused with missing data in the sparse matrix
  heading[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$heading        #This is in 4th quadrant rather than 1st and in degrees rather than radians; will be converted below
  
  #rm0.p - removing the rows of objects that never move (median speed of 0)
  if(rm0.p == TRUE){
    rmlist <- tapply(trajdat$speed, factor(trajdat$X..Trajectory), median)
    x <- x[which(!rmlist == 0),]
    y <- y[which(!rmlist == 0),]
    speed <- speed[which(!rmlist == 0),]
    heading <- heading[which(!rmlist == 0),]
    }
  
  # datmin = apply(x, 1, function(z){min(which(z > 0))})
  # datmax = apply(x, 1, function(z){max(which(z > 0))})
  # 
  # trajreturn <- list(datrange = cbind(datmin, datmax))
  
  ##Interpolate simpler values
  
  if(mass.p == FALSE){print('Interpolating x values')}
  trajreturn <- list(x = interna(x))
  if(mass.p == FALSE){print('Interpolating y values')}
  trajreturn <- c(trajreturn, y = interna(y))
  
  speed[which(heading == -1)] <- 1e-7                                           #Giving true 0s a value so that interna can interpolate the remainder properly
  if(mass.p == FALSE){print('Interpolating speed values')}
  trajreturn <- c(trajreturn, speed = interna(speed))
  
  ##Prepping heading
  
  trajreturn <- c(trajreturn, heading = heading)                                #This is here purely because I like the order of values to go x/y/speed/heading/xvel/yvel
  
  heading[which(heading == -1)] <- NA                                           #Replace -1s in heading with NAs
  if(mass.p == FALSE){print('Interpolating heading values')}                    #Actually a lie, I'm not interpolating here but it makes the user readout more friendly
  
  intout <- pbapply(heading, 1, function(z){                                    #Replace missing frame 0s in heading with NAs
    tmin <- min(which(z != 0))
    tmax <- max(which(z != 0))
    z[tmin:tmax] <- replace(z[tmin:tmax], which(z[tmin:tmax] == 0), NA)
    z
  })
  heading <- Matrix(t(intout), sparse = TRUE)
  
  heading <- -heading * pi / 180                                                #Moving heading to 1st quadrant and radians
  
  ##Interpolating more complex values
  
  xvel <- trajreturn$speed * cos(heading)                                       #Counting on NAs in heading to return NAs in the xvel and yvel; multiplying by speed takes care of cos(0) = 1 in the sparse matrix
  yvel <- trajreturn$speed * sin(heading)
  if(mass.p == FALSE){print('Interpolating x-velocity values')}
  trajreturn <- c(trajreturn, xvel = interna(xvel))                             #Fills in NAs in xvel
  if(mass.p == FALSE){print('Interpolating y-velocity values')}
  trajreturn <- c(trajreturn, yvel = interna(yvel))                             #Fills in NAs in yvel
  
  #heading[which(is.na(heading))] <- -acos(xtemp[which(is.na(heading))])         #Use xvel to back-calculate an interpolated heading (since can't linearly estimate polar coordinates)
  heading[which(is.na(heading))] <- -acos(xvel[which(is.na(heading))]/trajreturn$speed[which(is.na(heading))])
  trajreturn$heading <- heading                                                 #Overwrite the earlier placeholder with the final data
  
  ##Adding in extended values if flagged

  if("pathlength" %in% colnames(trajdat) & extended.p == TRUE){
    pathlength <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
    trajdat$pathlength[trajdat$pathlength == 0] <- 1e-7                                              #replacing 0 speed entries with 1e-7 to differentiate them from the 0s of the sparse matrix 
    pathlength[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$pathlength*conv.p
    
    if(mass.p == FALSE){print('Interpolating pathlength values')}
    pathlength <- pathlength[which(!rmlist == 0),]
    trajreturn <- c(trajreturn, pathlength = interna(pathlength))
  }
  
  if("distance_to_start" %in% colnames(trajdat) & extended.p == TRUE){
    distance <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
    trajdat$distance_to_start[trajdat$distance_to_start == 0] <- 1e-7                                              #replacing 0 speed entries with 1e-7 to differentiate them from the 0s of the sparse matrix 
    distance[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$distance_to_start*conv.p
    
    if(mass.p == FALSE){print('Interpolating distance to start values')}
    distance <- distance[which(!rmlist == 0),]
    trajreturn <- c(trajreturn, dts = interna(distance))
  }
  
  # if("msd" %in% colnames(trajdat) & extended.p == TRUE){
  #   msd <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
  #   trajdat$msd[trajdat$msd == 0] <- 1e-7                                              #replacing 0 speed entries with 1e-7 to differentiate them from the 0s of the sparse matrix 
  #   msd[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$msd*conv.p    ##DON'T FORGET TO CHECK THIS CONVERSION FACTOR
  #   
  #   if(interna.p == TRUE){
  #     msd <- msd[!rmlist == 0,]
  #     trajreturn <- c(trajreturn, msd = interna(msd))
  #   }else{
  #     trajreturn <- c(trajreturn, msd = msd)
  #   }
  # }
  
  pboptions(type = "timer")
  return(trajreturn)
  
}


##Import.mass
#Function to import and process multiple .csv files at a time

#folder = which subfolder under the hard-coded D:/Processed to look in
#category = criteria for import based on file names (see README in Processed folder for details).  Must be entered within quotes.
#Ex: Import.mass(300, "a") will get all files in the 300 folder which are a-quality

#Rest same as above


##DEFAULT PATH NOT IMPLEMENTED

Import.mass <- function(folder, category = "*", noncategory,
                        process = TRUE, rm0.mass = TRUE,
                        extended.mass = FALSE,
                        conv.mass = 0.05625, frate.mass = 5,
                        default.path = TRUE, mass.mass = TRUE, ...){
  
  #Getting the files from a given directory
  owd <- getwd()                                                                #Save old working directory
  setwd(paste0("D:\\Processed\\",folder))
  naymes <- list.files(pattern=paste0(category,".+csv"))                        #Get names of .csv files in the working directory that match the category criteria
  
  if(missing(noncategory) == FALSE){
    naymes <- naymes[-grep("f", naymes)]                                        #remove files that match noncategory
  }
  
  print("Importing files:")
  print(naymes)
  trajlist <- pblapply(naymes, read.csv)                                        #read.csv all the files in the folder that match the category criteria
  setwd(owd)                                                                    #Return to old working directory
  
  #Processing
  if(process == TRUE){
    print("Processing .csv files:")
    trajout <- vector(mode = "list", length = length(trajlist))
    for(i in 1:length(trajlist)){
      print(paste0("+[", i, "/", length(trajlist), "]: ", naymes[i]))
      trajout[[i]] <- Process.single(trajlist[i],
                              rm0.p = rm0.mass,
                              extended.p = extended.mass,
                              conv.p = conv.mass, frate.p = frate.mass,
                              mass.p = mass.mass, ...)
    }
    
    # trajout <- pbsapply(trajlist, Process.single,                            
    #                      rm0.p = rm0.mass,
    #                      extended.p = extended.mass,
    #                      conv.p = conv.mass, frate.p = frate.mass,
    #                      mass.p = mass.mass, ...)
    
    #trajout <- f
    #Take trajlist, divide it into length(naymes) bits, distribute these into sub-lists
    
  }else{print("Skipping processing")}
  
  #Prettying things up
  naymes <- gsub('.csv','',naymes)                                              #get rid of .csv at the end of the file names to make graph labeling prettier
  names(trajout) <- naymes
  
  # masstrajs <- list(naymes, trajout)                                            #combine names and trajs into a single output list for the function
  # names(masstrajs) <- c("Names", "Data")                                        #Name columns
  
  return(trajout)
}

