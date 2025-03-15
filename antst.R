#antst (ant space-time)
#Produces a space-time plot of the ants in the region

#dat is data, a list of multiple sparse matrices
#graph is whether to output a graph
#datreturn is whether to return the values of the calculations

#--direction is how the analysis is restricted:
#(0) = all trajectories (default)
#(1) = inbound only
#(2) = outbound only

#--xmin, xmax, ymin, and ymax all restrict the region of analysis
#[Figure out and note typical y-values later]

#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s); anywhere you see "frate*60" is converting frames to minutes or back

#titles is the option to manually enter a plot title; utilized by Mass.Function to pass file names for graph identification

antst <- function(dat = trajs, graph = TRUE, datreturn = FALSE,
                    direction = 0, 
                    xmin, xmax,
                    ymin, ymax,
                    mintime, maxtime, frate = 5, 
                    titles, ...){
  
  antst.f(dat.antst = dat, graph.antst = graph, datreturn.antst = datreturn,
            direction.antst = direction, 
            xmin.antst = xmin, xmax.antst = xmax,
            ymin.antst = ymin, ymax.anst = ymax,
            mintime.antst = mintime, maxtime.antst = maxtime, frate.antst = frate, 
            titles.antst = titles, ...)
}

antst.f <- function(dat.antst, graph.antst, datreturn.antst,
                      direction.antst,
                      xmin.antst, xmax.antst,
                      ymin.antst, ymax.antst,
                      mintime.antst, maxtime.antst, frate.antst,
                      titles.antst, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  #antmax.antst <- max(colSums((dat.antst$x) > 0))
  
  #Finding and converting boundary conditions
  minmaxf.antst <- Minmax(dat.antst, mintime = mintime.antst, maxtime = maxtime.antst, frate = frate.antst)
  mintimef.antst <- minmaxf.antst[1]
  maxtimef.antst <- minmaxf.antst[2]
  
  if(missing(xmin.antst)){xmin.antst <- min(dat.antst$x)}
  if(missing(xmax.antst)){xmax.antst <- max(dat.antst$x)}
  if(missing(ymin.antst)){ymin.antst <- min(dat.antst$y)}
  if(missing(ymax.antst)){ymax.antst <- max(dat.antst$y)}
  
  #Getting graph colors (important: BEFORE filtering)
  # binno.antst <- 6
  # graphcol <- floor( (colSums((dat.antst$x[,mintimef.antst:maxtimef.antst]) != 0)/antmax.antst*binno.antst)+1 ) #figuring out graph point coloration
  
  #Filtering data based on parameters
  
  if(direction.antst == 0){
    filterdat <- dat.antst$x
    yf <- dat.antst$y
  }else if(direction.antst == 1){                                                      
    innie <- which((apply(dat.antst$x, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) > 0) == TRUE) #Find which trajectories are inbound
    filterdat <- dat.antst$x[innie,]
    yf <- dat.antst$y[innie,]
  }else if(direction.antst == 2){                                                      
    outie <- which((apply(dat.antst$x, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) < 0) == TRUE) #Find which trajectories are outbound
    filterdat <- dat.antst$x[outie,]
    yf <- dat.antst$y[outie,]
  }
  
  filterdat <- filterdat[,mintimef.antst:maxtimef.antst]                        #Cutting x data down to specified time frame
  yf <- yf[,mintimef.antst:maxtimef.antst]                                      #Cutting y data down to specified time frame
  
  #filterdat <- pbapply(filterdat, 1, smooth.spline)$y
  
  filterdat[which(filterdat > xmax.antst | filterdat < xmin.antst)] <- NA       #Removing any x values outside the specified domain
  filterdat[which(yf > ymax.antst | yf < ymin.antst)] <- NA                     #Removing any x values which have a y value outside the specified range
  filterdat[filterdat == 0] <- NA                                               #Setting 0's to NAs for purposes of matplot later
  
  #Main calculations
  
  # activetrajs <- apply(filterdat, 2, function(z){which(z != 0)})                #Identifying active trajectories in each frame
  # 
  # indices <- unlist(sapply(mintimef.antst:maxtimef.antst, FUN = function(z){
  #   rep.int(z, length(activetrajs[[z - mintimef.antst + 1]]))                   #Indexing done this way so the frame labels match the overall graphs
  # }))
  # 
  # xpos <- unlist(pbsapply(1:length(activetrajs), FUN = function(z){
  #   filterdat[activetrajs[[z]], z]
  # }))
  
  #Output graph and data
  
  if(graph.antst == FALSE & datreturn.antst == TRUE){
    return(filterdat[is.na(filterdat)] <- 0)
  }
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.antst)){
    plotitle <- paste0('Space-time plot of',
         if(direction.antst==1){
           ' inbound ants ('
         }else if(direction.antst==2){
           ' outbound ants ('
         }else{
           ' inbound and outbound ants ('
         },
         round(mintimef.antst / (frate.antst*60)), 'm to ', round(maxtimef.antst/(frate.antst*60)),'m)'
    )
    
  }else{plotitle <- titles.antst}
  
  matplot(t(filterdat), mintimef.antst:maxtimef.antst, type = "l",
          main = plotitle, xlab = 'Position', ylab = 'Time (frames)')
  
  # plot(xpos, indices,
  #      main = plotitle, xlab = 'Position', ylab = 'Time (frames)')
  
  if(datreturn.antst == TRUE){
    return(filterdat[is.na(filterdat)] <- 0)
  }
  
}
