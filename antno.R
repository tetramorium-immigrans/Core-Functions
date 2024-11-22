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
                  mintime, maxtime, frate = 5,
                  binno = 6, lspace = 10, legloc = 2, keepscale = FALSE,
                  titles, ...){
  Antno.f(dat, outbound, prop, mintime, maxtime, frate, binno, lspace, legloc, keepscale, titles, ...)
}

Antno.f <- function(trajs.antno, outbound.antno, prop.antno,
                    mintime.antno, maxtime.antno, frate.antno,
                    binno.antno, lspace.antno, legloc.antno, keepscale.antno,
                    titles.antno, ...){
  
  #Greatest number of trajectories in region during any frame (for purposes of coloring)
  antmax.antno <- max(rowSums(!is.na(trajs.antno[,,4])))             
  
  #mintime/maxtime: finding and converting
  minmaxf.antno <- Minmax(trajs.antno, mintime = mintime.antno, maxtime = maxtime.antno, lspace = lspace.antno, frate = frate.antno)
  mintimef.antno <- minmaxf.antno[1]
  maxtimef.antno <- minmaxf.antno[2]
  lspacef.antno <- minmaxf.antno[3]
  
  #Assembles the main title of the plot based on input arguments
  if(missing(titles.antno)){
    plotitle <- paste0('Total number of',
     if(outbound.antno==1){
       ' outbound'
     }else if(outbound.antno==2){
       ' inbound'
     },
     ' ants in region (', round(mintimef.antno / (frate.antno*60)), 'm to ', round(maxtimef.antno/(frate.antno*60)),'m)'
    )
  }else{plotitle <- titles.antno}
  
  
  graphcol <- floor( (rowSums(!is.na(trajs.antno[mintimef.antno:maxtimef.antno,,1]))/antmax.antno*binno.antno)+1 ) #determining overall graph color (must come before filtering)
  if(outbound.antno != 0){trajs.antno <- Outbound(trajs.antno, outbound.antno)}
  
  if(prop.antno == FALSE){
    plotno <- rowSums(!is.na(trajs.antno[,,1]))
  }else{
    plotno <- rowSums(!is.na(trajs.antno[,,1]))/antmax.no
    }
  
  plot(x = mintimef.antno:maxtimef.antno, y = plotno[mintimef.antno:maxtimef.antno],
       ylim = if(keepscale.antno == TRUE){c(0,antmax.antno)}else{NULL},
       main = plotitle, xlab = "Frame Number", ylab = if(prop.antno == FALSE){"Number of ants in region"}else{"Proportion of ants relative to max in region"},
       col = graphcol)
  
  #Values for time lines below:
  #dims[1:4] are size of the plot
  #dims[5] is calculating where to put the text just above the bottom of the frame
  #dims[6] is where the dotted line ends just above the text (currently unused)
  dims.antno <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) 
  
  #Adding visuals to graph
  linseq <- seq(from = 0, to = maxtimef.antno, by = lspacef.antno)          #Find where to put minute lines
  abline(v = linseq, lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))        #Draw minute lines
  text(linseq, dims.antno[5], labels = paste0(linseq / (frate.antno*60), "m"), #Add labels to minute lines
       col = rgb(0,0,0, alpha = 0.45))
  
  Legendno(type = 1, binno = binno.antno, legloc = legloc.antno,            #Add legend
           antmax = antmax.antno) 
  
}

