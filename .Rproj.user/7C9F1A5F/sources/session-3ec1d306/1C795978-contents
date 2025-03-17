#antvel

#Calculates x- or y-velocity with time.  Also able to output values for number x speed

#dat is data, a list of multiple sparse matrices
#graph is whether to output a graph
#datreturn is whether to return the values of the calculations

#--xvel is which axis to analyze:
#TRUE = x-velocity
#FALSE = y-velocity
#--direction is how the analysis is restricted:
#(0) = all trajectories (default)
#(1) = inbound only
#(2) = outbound only
#--bytime is how to analyze the data:
#TRUE = calculates the instantaneous speed of the ants in each frame
#FALSE = calculates the overall speed of each trajectory and then uses that value to calculate the frame speed (default)
#--fn is which statistic to return:
#vari = variance 
#amean = arithmetic mean (default)
#med = median
#add = sum

#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s); anywhere you see "frate*60" is converting frames to minutes or back

#lspace is the "line spacing" for graphical purposes (default: 10m)
#winspace (window space) and winsize (window size) are how far apart to place the average points and the window within which the average is taken (default: 30s)

#binno (bin number) is the number of colors used to code for relative numbers on the trail (default: 6)
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise (default: 2, upper right)
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)
#titles is the option to manually enter a plot title; utilized by Mass.Function to pass file names for graph identification

antvel <- function(dat = trajs, graph = TRUE, datreturn = FALSE,
                     xvel = TRUE, direction = 0, bytime = FALSE, fn = "amean",
                     mintime, maxtime, frate = 5, 
                     lspace = 10, winspace = 30, winsize = 30,
                     binno = 6, legloc = 2, keepscale = FALSE,
                     titles, ...){
  
  antvel.f(dat.antvel = dat, graph.antvel = graph, datreturn.antvel = datreturn,
             xvel.antvel = xvel, direction.antvel = direction, bytime.antvel = bytime, fn.antvel = fn, 
             mintime.antvel = mintime, maxtime.antvel = maxtime, frate.antvel = frate, 
             lspace.antvel = lspace, winspace.antvel = winspace, winsize.antvel = winsize,
             binno.antvel = binno, legloc.antvel = legloc, keepscale.antvel = keepscale, 
             titles.antvel = titles, ...)
}

antvel.f <- function(dat.antvel, graph.antvel, datreturn.antvel,
                       xvel.antvel, direction.antvel, bytime.antvel, fn.antvel, 
                       mintime.antvel, maxtime.antvel, frate.antvel,
                       lspace.antvel, winspace.antvel, winsize.antvel,
                       binno.antvel, legloc.antvel, keepscale.antvel,
                       titles.antvel, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.antvel <- max(colSums((dat.antvel$x) > 0))
  
  #mintime/maxtime: finding and converting
  minmaxf.antvel <- Minmax(dat.antvel, mintime = mintime.antvel, maxtime = maxtime.antvel, lspace = lspace.antvel, frate = frate.antvel)
  mintimef.antvel <- minmaxf.antvel[1]
  maxtimef.antvel <- minmaxf.antvel[2]
  lspacef.antvel <- minmaxf.antvel[3]
  
  #Converting other input arguments to frames
  winspacef.antvel <- winspace.antvel*frate.antvel
  winsizef.antvel <- round(winsize.antvel * frate.antvel / 2)                #Halved since the value will be used to extend to either side of a point
  
  #Getting graph colors (important: BEFORE filtering)
  graphcol <- floor( (colSums((dat.antvel$x[,mintimef.antvel:maxtimef.antvel]) != 0)/antmax.antvel*binno.antvel)+1 ) #figuring out graph point coloration
  
  #filter based on x/y and direction
  
  if(xvel.antvel == TRUE){
    filterdat <- dat.antvel$xvel
  }else{
    filterdat <- dat.antvel$yvel
  }
  
  if(direction.antvel == 1){                                                      
    innie <- which((apply(dat.antvel$x, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) > 0) == TRUE) #Find which trajectories are inbound
    filterdat <- filterdat[innie,]
  }else if(direction.antvel == 2){                                                      
    outie <- which((apply(dat.antvel$x, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) < 0) == TRUE) #Find which trajectories are direction
    filterdat <- filterdat[outie,]
  }
  
  #Main calculations
  
  activetrajs <- apply(filterdat, 2, function(z){which(z != 0)})                #Identifying active trajectories in each frame
  filterdat[filterdat == 1e-7] <- 0                                             #Converting the "true 0s" back to 0s
  
  if(bytime.antvel == FALSE){
    
    if(fn.antvel == "vari"){
      trajdat <- apply(filterdat, 1, function(z){
        var(z[min(which(z != 0)):max(which(z != 0))])
      })
      
      keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
        mean(trajdat[activetrajs[[z]]])
      })
      
      plotdat <- keepdat[mintimef.antvel:maxtimef.antvel]
      ylab.antvel <- paste0("Variance of ", if(xvel.antvel == TRUE){"x-velocity"}else{"y-velocity"}," (cm/s)^2")
      
    }else if(fn.antvel == "amean"){
      trajdat <- apply(filterdat, 1, function(z){
        mean(z[min(which(z != 0)):max(which(z != 0))])
      })
      
      keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
        mean(trajdat[activetrajs[[z]]])
      })
      
      plotdat <- keepdat[mintimef.antvel:maxtimef.antvel]
      ylab.antvel <- paste0("Arithmetic mean of ", if(xvel.antvel == TRUE){"x-velocity"}else{"y-velocity"}," (cm/s)")
      
    }else if(fn.antvel == "med"){
      trajdat <- apply(filterdat, 1, function(z){
        median(z[min(which(z != 0)):max(which(z != 0))])
      })
      
      keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
        median(trajdat[activetrajs[[z]]])
      })
      
      plotdat <- keepdat[mintimef.antvel:maxtimef.antvel]
      ylab.antvel <- paste0("Median of ", if(xvel.antvel == TRUE){"x-velocity"}else{"y-velocity"}," (cm/s)")
      
    }else if(fn.antvel == "add"){
      trajdat <- apply(filterdat, 1, function(z){
        sum(z[min(which(z != 0)):max(which(z != 0))])
      })
      
      keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
        sum(trajdat[activetrajs[[z]]])
      })
      
      plotdat <- keepdat[mintimef.antvel:maxtimef.antvel]
      ylab.antvel <- paste0("Sum of ", if(xvel.antvel == TRUE){"x-velocity"}else{"y-velocity"}," (cm/s)")
    }
    
  }else if(bytime.antvel == TRUE){
    
    if(fn.antvel == "vari"){
      keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
        var(filterdat[activetrajs[[z]],z])
      })
      
      plotdat <- keepdat[mintimef.antvel:maxtimef.antvel]
      ylab.antvel <- paste0("Variance of ", if(xvel.antvel == TRUE){"x-velocity"}else{"y-velocity"}," (cm/s)^2")
      
    }else if(fn.antvel == "amean"){
      keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
        mean(filterdat[activetrajs[[z]],z])
      })
      
      plotdat <- keepdat[mintimef.antvel:maxtimef.antvel]
      ylab.antvel <- paste0("Arithmetic mean of ", if(xvel.antvel == TRUE){"x-velocity"}else{"y-velocity"}," (cm/s)")
      
    }else if(fn.antvel == "med"){
      keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
        median(filterdat[activetrajs[[z]],z])
      })
      
      plotdat <- keepdat[mintimef.antvel:maxtimef.antvel]
      ylab.antvel <- paste0("Median of ", if(xvel.antvel == TRUE){"x-velocity"}else{"y-velocity"}," (cm/s)")
      
    }else if(fn.antvel == "add"){
      keepdat <- colSums(filterdat)
      
      # keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
      #   sum(trajdat[activetrajs[[z]]])
      # })
      # 
      plotdat <- keepdat[mintimef.antvel:maxtimef.antvel]
      ylab.antvel <- paste0("Sum of ", if(xvel.antvel == TRUE){"x-velocity"}else{"y-velocity"}," (cm/s)")
    }
    
  }
  
  #Output graph and data
  
  if(graph.antvel == FALSE & datreturn.antvel == TRUE){
    return(trajdat)
    
    # antvelreturn <- list(plotdat, unlist(lapply(activetrajs, length)))
    # names(antvelreturn) <- c("plotdat", "activetrajs")
    # return(antvelreturn)
    
    #return(unlist(lapply(activetrajs, length)))
    
  }
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.antvel)){
    plotitle <- paste0(if(fn.antvel == "vari"){'Harmonic mean of'
      }else if(fn.antvel == "amean"){'Arithmetic mean of'
      }else if(fn.antvel == "med"){'Median of'
      }else if(fn.antvel == "add"){'Sum of'},
      
      if(xvel.antvel == TRUE){' x-velocity of'}else{' y-velocity of'},
      
      if(direction.antvel==1){
           ' inbound'
         }else if(direction.antvel==2){
           ' outbound'
         }else{
           ' inbound and outbound'
         },
         
         if(bytime.antvel == TRUE){
           ' ants ('
         }else if(bytime.antvel == FALSE){
           ' active trajectories ('
         }, round(mintimef.antvel / (frate.antvel*60)), 'm to ', round(maxtimef.antvel/(frate.antvel*60)),'m)'
    )
    
  }else{plotitle <- titles.antvel}
  
  antplot(mintimef.antplot = mintimef.antvel, maxtimef.antplot = maxtimef.antvel, plotdat.antplot = plotdat, keepdat.antplot = keepdat,
          #ylim.antplot = if(keepscale.antvel == TRUE){c(min(keepdat, na.rm = TRUE),max(keepdat, na.rm = TRUE))}else{NULL},
          main.antplot = plotitle, ylab.antplot = ylab.antvel,
          antmax.antplot = antmax.antvel, col.antplot = graphcol,
          frate.antplot = frate.antvel, lspacef.antplot = lspacef.antvel, winspacef.antplot = winspacef.antvel, winsizef.antplot = winsizef.antvel,
          binno.antplot = binno.antvel, legloc.antplot = legloc.antvel, keepscale.antplot = keepscale.antvel)
  
  if(datreturn.antvel == TRUE){
    return(plotdat)
  }
  
}
