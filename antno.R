##Antno
#Graphs the number of ants in the region over time

#dat is data; by default it is indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#outbound is how the analysis is restricted; 0 = all trajectories, 1 = outbound only, 2 = inbound only
#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#conversion is the factor for turning mintime/maxtime into frames (300 = 5 frames/s * 60 s/m by default in the Minmax subfunction)
#binno (bin number) is the number of colors used to code for relative numbers on the trail
#lspace (line space) is a graphical argument for how far apart the vertical time lines are spaced on the graph (in minutes)
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)


#binno.antno is the "bin number" for how many categories for coloring purposes
#lspace.antno is the "line spacing" for graphical purposes (3000 frames = 10 minutes)

Antno <- function(dat = trajs, outbound = 0, prop = FALSE,
                  mintime = 0, maxtime, conversion,
                  binno = 6, lspace = 10, legloc = 2){
  Antno.f(dat, outbound, prop, mintime, maxtime, conversion, binno, lspace, legloc)
}

Antno.f <- function(trajs.antno, outbound.antno, prop.antno,
                    mintime.antno, maxtime.antno, conversion.antno,
                    binno.antno, lspace.antno, legloc.antno){
  
  #Greatest number of trajectories in region during any frame (for purposes of coloring)
  antmax.antno <- max(rowSums(!is.na(trajs.antno[,,4])))             
  
  #mintime/maxtime/lspace: finding and converting
  minmaxf.antno <- Minmax(trajs.antno, mintime = mintime.antno, conversion = conversion.antno)[1]
  mintimef.antno <- minmaxf.antno[1]
  maxtimef.antno <- minmaxf.antno[2]
  lspacef.antno <- minmaxf.antno[3]
  
  #Assembles the main title of the plot based on input arguments
  plotitle <- paste0(titles.antno,
   if(outbound.antno==1){
     ' outbound'
   }else if(outbound.antno==2){
     ' inbound'
   }else{
     ' inbound and outbound'
   },
   ' trajectory speed (', mintime.antno, 'm to ', round(maxtimef.antno/300),'m)'
  )
  
  #Plotting function which plots either the total number of ants (FALSE) or the proportion of ants relative to antmax (TRUE)
  if(prop.antno == FALSE){
    plot(rowSums(!is.na(trajs.antno[,,1])), main = titles.antno, xlab = "Frame number", ylab = "Number of individual ants on trail", col = floor( (rowSums(!is.na(trajs.antno[,,1]))/antmax.antno*binno.antno)+1 ))
    dims.antno <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) #Values for time lines below; dims.antno[1:4] are size of the plot, dims.antno[5] is calculating where to put the text just above the bottom of the frame, dims.antno[6] is where the dotted line ends just above the text
    
    for(i in seq(from = 0, to = maxtime.antno, by = lspace.antno)){     #Adds and labels dotted lines every 10 minutes
      lines(c(i,i), c(dims.antno[4],dims.antno[6]), type = "l", lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))
      text(i, dims.antno[5], labels = paste0(round(i / 300, 2), "m"), col = rgb(0,0,0, alpha = 0.45))
    }
    
  }else{
    plot(rowSums(!is.na(trajs.antno[,,1]))/antmax.antno, main = titles.antno, xlab = "Frame number", ylab = "Proportion of ants on trail", col = floor( (rowSums(!is.na(trajs.antno[,,1]))/antmax.antno*binno.antno)+1 ))
    dims.antno <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) #Values for time lines below; dims.antno[1:4] are size of the plot, dims.antno[5] is calculating where to put the text just above the bottom of the frame, dims.antno[6] is where the dotted line ends just above the text
    
    for(i in seq(from = 0, to = maxtime.antno, by = lspace.antno)){     #Adds and labels dotted lines every 10 minutes
      lines(c(i,i), c(dims.antno[4],dims.antno[6]), type = "l", lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))
      text(i, dims.antno[5], labels = paste0(round(i / 300, 2), "m"), col = rgb(0,0,0, alpha = 0.45))
    }
  }
  
  #Adding visuals to the graph
  for(i in seq(from = 0, to = maxtimef.antno, by = lspacef.antno)){     #Adds and labels dotted lines every (lspace) minutes
    lines(c(i,i), c(dims.antno[4],dims.antno[6]), type = "l", lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))
    text(i, dims.antno[5], labels = paste0(round(i / 300, 2), "m"), col = rgb(0,0,0, alpha = 0.45))
  }
  Legendno(type = 1, binno = binno.antno, legloc = legloc.antno, antmax = antmax.antno) #Legend
  
}


Antno.mass <- function(dat = masstrajs, xdim, ydim, prop = FALSE,
                       binno = 6, lspace = 10, legloc = 1){
  
  if(missing(xdim)){
    xdim <- ceiling(sqrt(length(dat$Data)))
  }
  
  if(missing(ydim)){
    ydim <- ceiling(sqrt(length(dat$Data)))
  }
  
  nf <- par()
  par(mfrow = c(ydim,xdim))
  
  for(i in 1:length(dat$Data)){
    if(i > (xdim * ydim)){break}                                          #stops loop if it goes beyond the designated grid size
    trajtemp <- Process.single(dat$Data[i])
    Antno(trajtemp, prop, binno, lspace, legloc, titles = dat$Names[i])
    
    #Antno(data.frame(dat[i]), prop, binno, lspace, legloc)
  }
  
  par(mfrow = c(1,1))
  
}