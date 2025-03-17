#Anttort
#Calculates trajectory tortuosity

#dat is data, a list of multiple sparse matrices
#graph is whether to output a graph
#datreturn is whether to return the values of the calculations

#--direction is how the analysis is restricted:
#(0) = all trajectories (default)
#(1) = inbound only
#(2) = outbound only
#--bytime is how to analyze the data:
#TRUE = calculates the tortuosity up to that point for each trajectory active in a frame
#FALSE = calculates the total tortuosity of each trajectory and then uses that value to calculate the frame speed (default)
#--fn is which statistic to return:
#vari = variance 
#amean = arithmetic mean (default)
#med = median

#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s); anywhere you see "frate*60" is converting frames to minutes or back

#lspace is the "line spacing" for graphical purposes (default: 10m)
#winspace (window space) and winsize (window size) are how far apart to place the average points and the window within which the average is taken (default: 30s)

#binno (bin number) is the number of colors used to code for relative numbers on the trail (default: 6)
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise (default: 2, upper right)
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)
#titles is the option to manually enter a plot title; utilized by Mass.Function to pass file names for graph identification

anttort <- function(dat = trajs, graph = TRUE, datreturn = FALSE,
                   direction = 0, bytime = FALSE, fn = "amean",
                   mintime, maxtime, frate = 5, 
                   lspace = 10, winspace = 30, winsize = 30,
                   binno = 6, legloc = 2, keepscale = FALSE,
                   titles, ...){
  
  anttort.f(dat.anttort = dat, graph.anttort = graph, datreturn.anttort = datreturn,
           direction.anttort = direction, bytime.anttort = bytime, fn.anttort = fn, 
           mintime.anttort = mintime, maxtime.anttort = maxtime, frate.anttort = frate, 
           lspace.anttort = lspace, winspace.anttort = winspace, winsize.anttort = winsize,
           binno.anttort = binno, legloc.anttort = legloc, keepscale.anttort = keepscale, 
           titles.anttort = titles, ...)
}

anttort.f <- function(dat.anttort, graph.anttort, datreturn.anttort,
                     direction.anttort, bytime.anttort, fn.anttort, 
                     mintime.anttort, maxtime.anttort, frate.anttort,
                     lspace.anttort, winspace.anttort, winsize.anttort,
                     binno.anttort, legloc.anttort, keepscale.anttort,
                     titles.anttort, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.anttort <- max(colSums((dat.anttort$x) > 0))
  
  #mintime/maxtime: finding and converting
  minmaxf.anttort <- Minmax(dat.anttort, mintime = mintime.anttort, maxtime = maxtime.anttort, lspace = lspace.anttort, frate = frate.anttort)
  mintimef.anttort <- minmaxf.anttort[1]
  maxtimef.anttort <- minmaxf.anttort[2]
  lspacef.anttort <- minmaxf.anttort[3]
  
  #Converting other input arguments to frames
  winspacef.anttort <- winspace.anttort*frate.anttort
  winsizef.anttort <- round(winsize.anttort * frate.anttort / 2)                #Halved since the value will be used to extend to either side of a point
  
  #Getting graph colors (important: BEFORE filtering)
  graphcol <- floor( (colSums((dat.anttort$x[,mintimef.anttort:maxtimef.anttort]) != 0)/antmax.anttort*binno.anttort)+1 ) #figuring out graph point coloration
  
  #filter based on x/y and direction
  
  filterpath <- dat.anttort$pathlength
  filterdist <- dat.anttort$distance_to_start
  
  if(direction.anttort == 1){                                                      
    innie <- which((apply(dat.anttort$x, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) > 0) == TRUE) #Find which trajectories are inbound
    filterpath <- filterpath[innie,]
    filterdist <- filterdist[innie,]
  }else if(direction.anttort == 2){                                                      
    outie <- which((apply(dat.anttort$x, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) < 0) == TRUE) #Find which trajectories are outbound
    filterpath <- filterpath[outie,]
    filterdist <- filterdist[outie,]
  }
  
  #Main calculations
  
  if(direction.anttort == 1){                                                   #Identifying active trajectories in each frame
    activetrajs <- apply(dat.anttort$x[innie,], 2, function(z){which(z != 0)})
  }else if(direction.anttort == 2){
    activetrajs <- apply(dat.anttort$x[outie,], 2, function(z){which(z != 0)})
  }else{
    activetrajs <- apply(dat.anttort$x, 2, function(z){which(z != 0)})
  }
  
  #activetrajs <- apply(filterdat, 2, function(z){which(z != 0)})                #Identifying active trajectories in each frame
  #filterdat[filterdat == 1e-7] <- 0                                             #Converting the "true 0s" back to 0s
  
  if(bytime.anttort == FALSE){
    
    filterdat <- sapply(1:dim(filterpath)[1], function(z){
      tortmax <- max(which(filterpath[z,] != 0))
      filterpath[z, tortmax] / filterdist[z, tortmax]
    })
    
    
    if(fn.anttort == "vari"){
        keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
        var(filterdat[activetrajs[[z]]])
      })
      
      plotdat <- keepdat[mintimef.anttort:maxtimef.anttort]
      ylab.anttort <- 'Variance of tortuosity'
      
    }else if(fn.anttort == "amean"){
        keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
        mean(filterdat[activetrajs[[z]]])
      })
      
      plotdat <- keepdat[mintimef.anttort:maxtimef.anttort]
      ylab.anttort <- 'Mean of tortuosity'
      
    }else if(fn.anttort == "med"){
        keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
        median(filterdat[activetrajs[[z]]])
      })
      
      plotdat <- keepdat[mintimef.anttort:maxtimef.anttort]
      ylab.anttort <- 'Median of tortuosity'
      
    }
    
  }else if(bytime.anttort == TRUE){
    
    print("bytime = TRUE not implemented")
    break
    
    if(fn.anttort == "vari"){
      keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
        var(filterdat[activetrajs[[z]],z])
      })
      
      plotdat <- keepdat[mintimef.anttort:maxtimef.anttort]
      ylab.anttort <- paste0("Variance of ", if(xvel.anttort == TRUE){"x-velocity"}else{"y-velocity"}," (cm/s)^2")
      
    }else if(fn.anttort == "amean"){
      keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
        mean(filterdat[activetrajs[[z]],z])
      })
      
      plotdat <- keepdat[mintimef.anttort:maxtimef.anttort]
      ylab.anttort <- paste0("Arithmetic mean of ", if(xvel.anttort == TRUE){"x-velocity"}else{"y-velocity"}," (cm/s)")
      
    }else if(fn.anttort == "med"){
      keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
        median(filterdat[activetrajs[[z]],z])
      })
      
      plotdat <- keepdat[mintimef.anttort:maxtimef.anttort]
      ylab.anttort <- paste0("Median of ", if(xvel.anttort == TRUE){"x-velocity"}else{"y-velocity"}," (cm/s)")
      
    }
  }
  
  #Output graph and data
  
  if(graph.anttort == FALSE & datreturn.anttort == TRUE){
    return(trajdat)
    
    # anttortreturn <- list(plotdat, unlist(lapply(activetrajs, length)))
    # names(anttortreturn) <- c("plotdat", "activetrajs")
    # return(anttortreturn)
    
    #return(unlist(lapply(activetrajs, length)))
    
  }
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.anttort)){
    plotitle <- paste0(if(fn.anttort == "vari"){'Variance of '
    }else if(fn.anttort == "amean"){'Mean '
    }else if(fn.anttort == "med"){'Median '},
    
    ' tortuosity of',
    
    if(direction.anttort==1){
      ' inbound'
    }else if(direction.anttort==2){
      ' outbound'
    }else{
      ' inbound and outbound'
    },
    
    if(bytime.anttort == TRUE){
      ' ants ('
    }else if(bytime.anttort == FALSE){
      ' trajectories ('
    }, round(mintimef.anttort / (frate.anttort*60)), 'm to ', round(maxtimef.anttort/(frate.anttort*60)),'m)'
    )
    
  }else{plotitle <- titles.anttort}
  
  antplot(mintimef.antplot = mintimef.anttort, maxtimef.antplot = maxtimef.anttort, plotdat.antplot = plotdat, keepdat.antplot = keepdat,
          #ylim.antplot = if(keepscale.anttort == TRUE){c(min(keepdat, na.rm = TRUE),max(keepdat, na.rm = TRUE))}else{NULL},
          main.antplot = plotitle, ylab.antplot = ylab.anttort,
          antmax.antplot = antmax.anttort, col.antplot = graphcol,
          frate.antplot = frate.anttort, lspacef.antplot = lspacef.anttort, winspacef.antplot = winspacef.anttort, winsizef.antplot = winsizef.anttort,
          binno.antplot = binno.anttort, legloc.antplot = legloc.anttort, keepscale.antplot = keepscale.anttort)
  
  if(datreturn.anttort == TRUE){
    return(plotdat)
  }
  
}
