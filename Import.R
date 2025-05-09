#####Setting up data arrays######

##Import.single + Process.single + Import.mass
#Imports and processes files into a list of sparse matrices 

#path = data to import, defaulting to a hardcoded directory (no quotes, forward slash / between folders)

#process = whether Import.single/.mass should merely import the .csv file or convert it to sparse matrices
#rm0 = whether to remove trajectories with a median speed of 0 (typically dead ants or other inconsequential objects)
#*Warning: running rm0 = FALSE leads to errors if the data has any 0 rows

#extended = whether to also process pathlength, distance_to_start, and mean squared distance (included in the .csv file I don't often use them)

#conv = conversion factor from pixels to cm (1/14 cm/pixel, used for x/y/pathlength/distance conversion)
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
                          conv = 1/14, frate = 5, 
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
                           conv.p = 1/14, frate.p = 5,
                           mass.p = FALSE){
  trajdat <- data.frame(trajdatinput)  #code won't work if it's not a data frame

  #Steps:
  #1) Set up sparse matrices
  #2) Populate matrices
  #3) ID frames with no data and tag with NAs
  #4) Change all -1 headings to NAs and move to first quadrant
  #5) Calculate xvel and yvel
  #6) Remove stationary objects
  #7) Implement extended variables
  #8) Return data
  
  ##1) Setting up sparse matrices for non-extended variables
  
  sparsedims <- c(max(trajdat$X..Trajectory), max(trajdat$Time))
  
  x <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
  y <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
  speed <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
  heading <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
  xvel <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
  yvel <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
  
  ##2) Populating matrices
  
  x[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$x * conv.p
  y[cbind(trajdat$X..Trajectory, trajdat$Time)] <- (max(trajdat$y) - trajdat$y) * conv.p   #AnTracks operates in the 4th quadrant so need to flip it to first by reversing based off of max(y)
  speed[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$speed * conv.p * frate.p
  heading[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$heading
  
  ##3) IDing frames with no data (no x-coordinate) and replacing them with NAs
  
  for(i in 1:dim(x)[1]){
    tmin <- min(which(x[i,] != 0))
    tmax <- max(which(x[i,] != 0))

    x[i,tmin:tmax] <- replace(x[i,tmin:tmax], which(x[i,tmin:tmax] == 0), NA)

  }
  
  # x <- pbsapply(1:dim(x)[1], function(z){
  #   tmin <- min(which(x[z,] != 0))
  #   tmax <- max(which(x[z,] != 0))
  # 
  #   x[which(x[z,tmin:tmax] == 0)] <- NA
  #   x[z,]
  # 
  # })

  y[is.na(x)] <- NA
  speed[is.na(x)] <- NA
  heading[is.na(x)] <- NA
  
  ##4) Replace -1s in heading with NAs and moving to first quadrant
  
  heading[heading == -1] <- NA  
  heading <- -heading * pi / 180
  
  ##5) Calculate xvel and yvel
  
  xvel <- speed * cos(heading)                                       
  yvel <- speed * sin(heading)
  
  xvel[speed == 0] <- 0   #0 speed causes -1 (NA) in heading, which causes NA in xvel/yvel; replacing these with 0s because they aren't truly missing data
  yvel[speed == 0] <- 0
  
  ##6) Removing stationary objects (median speed of 0)
  if(rm0.p == TRUE){
    rmlist <- tapply(trajdat$speed, factor(trajdat$X..Trajectory), median, na.rm = TRUE)
    x <- x[which(!rmlist == 0),]
    y <- y[which(!rmlist == 0),]
    speed <- speed[which(!rmlist == 0),]
    heading <- heading[which(!rmlist == 0),]
    xvel <- xvel[which(!rmlist == 0),]
    yvel <- yvel[which(!rmlist == 0),]
    }
  
  ###7) Adding in extended values if flagged

  if("pathlength" %in% colnames(trajdat) & extended.p == TRUE){
    pathlength <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
    pathlength[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$pathlength*conv.p
    
    if(rm0.p == TRUE){
      pathlength <- pathlength[which(!rmlist == 0),]
    }
    
    pathlength[is.na(x)] <- NA
    
  }
  
  if("distance_to_start" %in% colnames(trajdat) & extended.p == TRUE){
    distance <- Matrix(data = 0, nrow = sparsedims[1], ncol = sparsedims[2], sparse = TRUE)
    distance[cbind(trajdat$X..Trajectory, trajdat$Time)] <- trajdat$distance_to_start*conv.p
    
    if(rm0.p == TRUE){
      distance <- distance[which(!rmlist == 0),]
    }
    
    distance[is.na(x)] <- NA
    
  }
  
  ##8) Return data
  
  if(extended.p == TRUE){
    trajreturn <- list(x = x, y = y, speed = speed, heading = heading, xvel = xvel, yvel = yvel, pathlength = pathlength, distance = distance) 
  }else{
    trajreturn <- list(x = x, y = y, speed = speed, heading = heading, xvel = xvel, yvel = yvel) 
  }
  
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

