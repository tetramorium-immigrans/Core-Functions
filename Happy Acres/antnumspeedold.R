##Antnumspeed
#OLD VERSION BEFORE SUBFUNCTION UPDATE; KEPT AROUND JUST IN CASE I EVER NEED TO REVERT

#dat is data; by default it is indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#outbound is how the analysis is restricted; 0 = all trajectories, 1 = outbound only, 2 = inbound only
#mintime and maxtime restrict the time (mintime is 1 by default for indexing purposes)
#binno (bin number) is the number of colors used to code for relative numbers on the trail
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)

Antnumspeed <- function(dat = trajs, outbound = 0,
                       mintime = 0, maxtime,
                       binno = 6, legloc = 2, keepscale = FALSE,
                       titles = 'Relationship of # of ants on trail to mean'){
  Antnumspeed.f(dat, outbound, mintime, maxtime, binno, legloc, keepscale, titles)
}

Antnumspeed.f <- function(trajs.antnumspeed, outbound.antnumspeed = 1,
                         mintime.antnumspeed, maxtime.antnumspeed,
                         binno.antnumspeed, legloc.antnumspeed, keepscale.antnumspeed,
                         titles.antnumspeed){
  
  #Greatest number of trajectories in region during any frame (for purposes of coloring)
  antmax.antnumspeed <- max(rowSums(!is.na(trajs.antnumspeed[,,4])))             
  
  #mintime: converting input argument from minutes to frames, then setting min frame to 1 if it is 0 to avoid indexing problems below
  mintimef.antnumspeed <- mintime.antnumspeed*300
  if(mintimef.antnumspeed == 0){
    mintimef.antnumspeed <- 1
  }
  
  #maxtime: ending frame found if not given as an argument, else converting input from minutes to frames
  if(missing(maxtime.antnumspeed)){
    maxtimef.antnumspeed <- dim(trajs.antnumspeed)[1]
  }else{
    maxtimef.antnumspeed <- maxtime.antnumspeed*300                                #Converting argument from minutes to frames
  }
  
  #Assembles the main title of the plot based on input arguments
  plotitle <- paste0(titles.antnumspeed,
    if(outbound.antnumspeed==1){
      ' outbound'
    }else if(outbound.antnumspeed==2){
      ' inbound'
    }else{
      ' inbound and outbound'
    },
      ' trajectory speed (', mintime.antnumspeed, 'm to ', round(maxtimef.antnumspeed/300),'m)'
   # if(rm0.antnumspeed==1){
   #    ', zeros removed'
   #  }else{
   #    ', zeros not removed'
   #  }
  )
  
  #outbound: getting x,y values then filters data based on whether the selected trajectories are outbound (1) or inbound (2)
  eks <- rowSums(!is.na(trajs.antnumspeed[mintimef.antnumspeed:maxtimef.antnumspeed,,4]))
  graphcol <- floor( (rowSums(!is.na(trajs.antnumspeed[mintimef.antnumspeed:maxtimef.antnumspeed,,4]))/antmax.antnumspeed*binno.antnumspeed)+1 )
  #wye <- rowMeans(trajs.antnumspeed[mintimef.antnumspeed:maxtimef.antnumspeed,,4], na.rm = TRUE)
  
  if(outbound.antnumspeed == 1){
    #eks <- eks[colSums(trajs.antnumspeed[,,1], na.rm = TRUE) < 0]
    trajs.antnumspeed <- trajs.antnumspeed[,colSums(trajs.antnumspeed[,,1], na.rm = TRUE) < 0,]
    #eks <- rowSums(!is.na(trajs.antnumspeed[mintimef.antnumspeed:maxtimef.antnumspeed,,4]))
    #wye <- rowMeans(trajs.antnumspeed[mintimef.antnumspeed:maxtimef.antnumspeed,,4], na.rm = TRUE)
    
    #wye <- wye[colSums(trajs.antnumspeed[,,1], na.rm = TRUE) < 0]
  }else if(outbound.antnumspeed == 2){
    #eks <- eks[colSums(trajs.antnumspeed[,,1], na.rm = TRUE) > 0]
    trajs.antnumspeed <- trajs.antnumspeed[,colSums(trajs.antnumspeed[,,1], na.rm = TRUE) > 0,]
    
    #wye <- wye[colSums(trajs.antnumspeed[,,1], na.rm = TRUE) > 0]
  }
  
  #eks <- rowSums(!is.na(trajs.antnumspeed[mintimef.antnumspeed:maxtimef.antnumspeed,,4]))
  wye <- rowMeans(trajs.antnumspeed[mintimef.antnumspeed:maxtimef.antnumspeed,,4], na.rm = TRUE)
  
  #Plotting function with KEEPSCALE option
  if(keepscale.antnumspeed == FALSE){
    plot(x = eks, y = wye,
         main = plotitle, xlab = '# of ants on trail', ylab = ' mean speed of ants on trail (cm/s)',
         col = graphcol)
         #col = floor( (rowSums(!is.na(trajs.antnumspeed[mintimef.antnumspeed:maxtimef.antnumspeed,,4]))/antmax.antnumspeed*binno.antnumspeed)+1 ))
  }else{
    wyekeep <- rowMeans(trajs.antnumspeed[,,4], na.rm = TRUE)
    plot(x = eks, y = wye,
         main = plotitle, xlab = '# of ants on trail', ylab = ' mean speed of ants on trail (cm/s)',
         xlim = c(0,antmax.antnumspeed), ylim = c(min(wyekeep, na.rm = TRUE), max(wyekeep, na.rm = TRUE)),
         col = graphcol)
         #col = floor( (rowSums(!is.na(trajs.antnumspeed[mintimef.antnumspeed:maxtimef.antnumspeed,,4]))/antmax.antnumspeed*binno.antnumspeed)+1 ))
  }
  
  
  
  #dims.antnumspeed <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) #Values for time lines below; dims.antnumspeed[1:4] are size of the plot, dims.antnumspeed[5] is calculating where to put the text just above the bottom of the frame, dims.antnumspeed[6] is where the dotted line ends just above the text
  
  #Add mean line of entire time series (not just selected time)
  abline(mean(trajs.antnumspeed[,,4], na.rm = TRUE), 0, lty = 5)
  
  #Add trend line (of only selected time)
  abline(lm(wye~eks), lty = 3)
  
  #Add legend
  
  Legendno(type = 1, binno = binno.antnumspeed, legloc = legloc.antnumspeed, antmax = antmax.antnumspeed)
  
  # if(legloc.antnumspeed == 1){
  #   legend("topleft", inset = 0.03, title = "Ants in region",
  #          legend=c(paste("0 to", floor(antmax.antnumspeed/binno.antnumspeed)),paste(floor(antmax.antnumspeed/binno.antnumspeed*((2:binno.antnumspeed)-1))+1, "to", floor(antmax.antnumspeed/binno.antnumspeed*(2:binno.antnumspeed)))),
  #          fill = 1:binno.antnumspeed)
  # }else if(legloc.antnumspeed == 2){
  #   legend("topright", inset = 0.03, title = "Ants in region",
  #          legend=c(paste("0 to", floor(antmax.antnumspeed/binno.antnumspeed)),paste(floor(antmax.antnumspeed/binno.antnumspeed*((2:binno.antnumspeed)-1))+1, "to", floor(antmax.antnumspeed/binno.antnumspeed*(2:binno.antnumspeed)))),
  #          fill = 1:binno.antnumspeed)
  # }else if(legloc.antnumspeed == 3){
  #   legend("bottomright", inset = 0.03, title = "Ants in region",
  #          legend=c(paste("0 to", floor(antmax.antnumspeed/binno.antnumspeed)),paste(floor(antmax.antnumspeed/binno.antnumspeed*((2:binno.antnumspeed)-1))+1, "to", floor(antmax.antnumspeed/binno.antnumspeed*(2:binno.antnumspeed)))),
  #          fill = 1:binno.antnumspeed)
  # }else if(legloc.antnumspeed == 4){
  #   legend("bottomleft", inset = 0.03, title = "Ants in region",
  #          legend=c(paste("0 to", floor(antmax.antnumspeed/binno.antnumspeed)),paste(floor(antmax.antnumspeed/binno.antnumspeed*((2:binno.antnumspeed)-1))+1, "to", floor(antmax.antnumspeed/binno.antnumspeed*(2:binno.antnumspeed)))),
  #          fill = 1:binno.antnumspeed)
  # }else{
  #   #print("No legend added; choose 1-4 for topleft, topright, bottomright, or bottomleft.")
  # }
}



Antnumspeed.mass <- function(dat = trajs, outbound = 0, rm0 = 1,
                             mintime = 0, maxtime, winsize = 10,
                             xdim, ydim,
                             binno = 6, legloc = 0, keepscale = TRUE){
  
  #Ending frame found if not given as an argument
  if(missing(maxtime)){
    maxtime <- floor(dim(dat)[1]/300)
  }
  
  #Getting how many graphs to be produced then converting it to a vector for the loop
  graphno <- floor((maxtime - mintime) / winsize)
  graphnoi <- seq(from = mintime, by = winsize, length.out = graphno)
  
  #Getting xdim and ydim if not supplied and setting up graph grid (need to refine so it does better than a square always)
  if(missing(xdim)){
    xdim <- ceiling(sqrt(graphno))
  }
  
  if(missing(ydim)){
    ydim <- ceiling(sqrt(graphno))
  }
  
  nf <- par()
  par(mfrow = c(ydim,xdim))
  
  #Loop that populates the graph grid
  for(i in graphnoi){
    #if(i > (xdim * ydim)){break}                                          #stops loop if it goes beyond the designated grid size
    
    Antnumspeed(dat = dat, outbound = outbound, rm0 = rm0, 
            mintime = i, maxtime = i + winsize,
            binno = binno, legloc = legloc, keepscale = keepscale)
    
  }
  
  par(mfrow = c(1,1)) #return to default 1x1 graph
  
}


