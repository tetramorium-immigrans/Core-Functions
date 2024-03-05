##ANDHEAD
#Change in heading variance with time.  Graphs a mean line as well.

#dat is data, indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#outbound is how the analysis is restricted; 0 = all trajectories, 1 = outbound only, 2 = inbound only
#bytime is whether to take the variance between all trajectory headings in a frame (1) or the mean variance of all trajectories active during that frame (0)
#-CURRENTLY VERY LAGGY TO USE 0, WAIT FOR CODE UPDATE

#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s); anywhere you see "frate*60" is converting frames to minutes or back

#winspace (window space) and winsize (window size) are how far apart to place the average points and the window within which the average is taken (default: 20s)

#binno (bin number) is the number of colors used to code for relative numbers on the trail
#lspace is the "line spacing" for graphical purposes (default: 10m)
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)
#titles is an argument passed by Mass.Function used to title the graph with the file name for identification

Anthead <- function(dat = trajs, outbound = 0, bytime = 1,
                    mintime, maxtime, frate = 5,
                    winspace = 20, winsize = 20,
                    binno = 6, lspace = 10, legloc = 2, keepscale = FALSE, titles, ...){
  Anthead.f(dat, outbound, bytime, mintime, maxtime, frate, winspace, winsize, binno, lspace, legloc, keepscale, titles, ...)
}

Anthead.f <- function(trajs.anthead, outbound.anthead, bytime.anthead,
                      mintime.anthead, maxtime.anthead, frate.anthead,
                      winspace.anthead, winsize.anthead, 
                      binno.anthead, lspace.anthead, legloc.anthead, keepscale.anthead, titles.anthead, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.anthead <- max(rowSums(!is.na(trajs.anthead[,,1])))
  
  #mintime/maxtime: finding and converting
  minmaxf.anthead <- Minmax(trajs.anthead, mintime = mintime.anthead, maxtime = maxtime.anthead, lspace = lspace.anthead, frate = frate.anthead)
  mintimef.anthead <- minmaxf.anthead[1]
  maxtimef.anthead <- minmaxf.anthead[2]
  lspacef.anthead <- minmaxf.anthead[3]
  
  #Converting other input arguments to frames
  winspacef.anthead <- winspace.anthead*frate.anthead
  winsizef.anthead <- round(winsize.anthead * frate.anthead / 2)     #Halved since the value will be used to extend to either side of a point
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.anthead)){
    plotitle <- paste0('Variance of',
     if(outbound.anthead==1){
       ' outbound'
     }else if(outbound.anthead==2){
       ' inbound'
     }else{
       ' inbound and outbound'
     },
     ' ant heading (', round(mintimef.anthead / (frate.anthead*60)), 'm to ', round(maxtimef.anthead/(frate.anthead*60)),'m)'
    )
    
  }else{plotitle <- titles.anthead}
  
  
  #Main plotting function
  graphcol <- floor( (rowSums(!is.na(trajs.anthead[mintimef.anthead:maxtimef.anthead,,1]))/antmax.anthead*binno.anthead)+1 ) #determining overall graph color (must come before filtering)
  trajs.anthead <- Outbound(trajs.anthead, outbound.anthead)  #Filtering to inbound/outbound if entered
  
  if(keepscale.anthead == TRUE){                                          #If keeping the scale, find out the appropriate scale for the full data set then plot
    if(bytime.anthead == 1){
      rvars <- rowVars(trajs.anthead[,,3], na.rm = TRUE)
      ylab.anthead <- "Variance of ant headings (degrees)"
    }else{
      rvars <- sapply(1:dim(trajs.anthead)[1], FUN = function(x){mean(colVars(trajs.anthead[,,3], na.rm = TRUE)[!is.na(trajs.anthead[x,,3])])})
      ylab.anthead <- "Mean variance of trajectory headings (degrees)"
    }
    plot(x = mintimef.anthead:maxtimef.anthead, y = rvars[mintimef.anthead:maxtimef.anthead], 
         ylim = c(min(rvars, na.rm = TRUE), max(rvars, na.rm = TRUE)),
         main = plotitle, xlab = "Frame Number", ylab = ylab.anthead, 
         col = graphcol)
  }else{
    plot(x = mintimef.anthead:maxtimef.anthead, y = rowVars(trajs.anthead[mintimef.anthead:maxtimef.anthead,,3], na.rm = TRUE)[1:length(mintimef.anthead:maxtimef.anthead)], 
         main = plotitle, xlab = "Frame Number", ylab = "Variance of ant headings (degrees)", 
         col = graphcol)
  }
  
  #Values for time lines below:
  #dims[1:4] are size of the plot
  #dims[5] is calculating where to put the text just above the bottom of the frame
  #dims[6] is where the dotted line ends just above the text (currently unused)
  dims.anthead <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) 
  
  #Adding visuals to graph
  abline(h = 0, lty = 3)    #x-axis
  abline(h = mean(rowVars(trajs.anthead[mintimef.anthead:maxtimef.anthead,,3], na.rm = TRUE), na.rm = TRUE), lty = 3)   #mean line of variance in mintime:maxtime
  
  linseq <- seq(from = 0, to = maxtimef.anthead, by = lspacef.anthead)          #Find where to put minute lines and draw them with labels at the bottom
  abline(v = linseq, lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))
  text(linseq, dims.anthead[5], labels = paste0(linseq / (frate.anthead*60), "m"), col = rgb(0,0,0, alpha = 0.45))
  
  Legendno(type = 1, binno = binno.anthead, legloc = legloc.anthead, antmax = antmax.anthead) #Legend
  
  dotseq <- seq(from = mintimef.anthead, to = maxtimef.anthead, by = winspacef.anthead)
  dotseq <- dotseq[(dotseq - winsizef.anthead) > 0 & (dotseq + winsizef.anthead) < maxtimef.anthead]
  varseq <- sapply(dotseq, FUN = function(x){mean(rvars[(x - winsizef.anthead):(x + winsizef.anthead)], na.rm = TRUE)})
  
  points(x = dotseq, y = varseq, pch = 18, col = "gray26")
  lines(x = dotseq, y = varseq, col = "gray26")

  #Code for placing average points and lines; if/else if/else for when the window would go below 0, above maxtimef, or otherwise is in the middle respectively
  #I want to replace this eventually because it's old and janky, but my attempts at new code above have been unsuccessful.
  # for(i in seq(from = mintimef.anthead, to = maxtimef.anthead, by = winspacef.anthead)){
  #   if((i - winsizef.anthead) < 1){
  #     points(x = i, y = mean(rowVars(trajs.anthead[(1):(i+winsizef.anthead),,3], na.rm = TRUE)), pch = 18, col = "gray26")
  #     if(i != 0){ #if statement to avoid erroring on first segment with a 0 index
  #       segments(i - winspacef.anthead, mean(rowVars(trajs.anthead[(1):(i-winspacef.anthead+winsizef.anthead),,3], na.rm = TRUE)),
  #                i, mean(rowVars(trajs.anthead[(1):(i+winsizef.anthead),,3], na.rm = TRUE)), col = "gray26")
  #     }
  #   }else if((i + winsizef.anthead) > maxtimef.anthead){
  #     points(x = i, y = mean(rowVars(trajs.anthead[(i-winsizef.anthead):(maxtimef.anthead),,3], na.rm = TRUE)), pch = 18, col = "gray26")
  #     segments(i - winspacef.anthead, mean(rowVars(trajs.anthead[(i-winspacef.anthead-winsizef.anthead):(maxtimef.anthead),,3], na.rm = TRUE)),
  #              i, mean(rowVars(trajs.anthead[(i-winsizef.anthead):(maxtimef.anthead),,3], na.rm = TRUE)), col = "gray26")
  #   }else{
  #     points(x = i, y = mean(rowVars(trajs.anthead[(i-winsizef.anthead):(i+winsizef.anthead),,3], na.rm = TRUE)), pch = 18, col = "gray26")
  #     if((i-winspacef.anthead-winsizef.anthead) > 0){ #if statement to avoid erroring on segments outside the index
  #       segments(i - winspacef.anthead, mean(rowVars(trajs.anthead[(i-winspacef.anthead-winsizef.anthead):(i-winspacef.anthead+winsizef.anthead),,3], na.rm = TRUE)),
  #                i, mean(rowVars(trajs.anthead[(i-winsizef.anthead):(i+winsizef.anthead),,3], na.rm = TRUE)), col = "gray26")
  #     }
  #   }
  # }

  
}