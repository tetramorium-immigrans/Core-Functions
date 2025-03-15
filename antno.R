##Antno
#Graphs the number of ants in the region over time

#dat is data
#graph is whether to return a graph or just the calculations

#outbound is how the analysis is restricted
#(0) - "false" or inbound only
#(1) - "true" or outbound only
#(2) - "both"
#prop is whether to calculate the values as a fraction of the maximum rather than counts

#mintime and maxtime restrict time being examined (entered in minutes)
#frate = "frame rate", or frames/s

#binno (bin number) is the number of colors used to code for relative numbers on the trail
#lspace (line space) is a graphical argument for how far apart the vertical time lines are spaced on the graph (in minutes)
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)

#titles is used by mass.function to title the graphs, otherwise the title self-assembles

antno <- function(dat = trajs, graph = TRUE, datreturn = FALSE,
                  outbound = 2, prop = FALSE,
                  mintime, maxtime, frate = 5,
                  binno = 6, lspace = 10, legloc = 2, keepscale = FALSE,
                  titles, ...){
  antno.f(dat, graph, datreturn, outbound, prop, mintime, maxtime, frate, binno, lspace, legloc, keepscale, titles, ...)
}

antno.f <- function(dat.antno, graph.antno, datreturn.antno,
                    outbound.antno, prop.antno,
                    mintime.antno, maxtime.antno, frate.antno,
                    binno.antno, lspace.antno, legloc.antno, keepscale.antno,
                    titles.antno, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.antno <- max(colSums((dat.antno$x) > 0))
  
  #mintime/maxtime: finding and converting
  minmaxf.antno <- Minmax(dat.antno, mintime = mintime.antno, maxtime = maxtime.antno, lspace = lspace.antno, frate = frate.antno)
  mintimef.antno <- minmaxf.antno[1]
  maxtimef.antno <- minmaxf.antno[2]
  lspacef.antno <- minmaxf.antno[3]
  
  graphcol <- floor( (colSums((dat.antno$x[,mintimef.antno:maxtimef.antno]) != 0)/antmax.antno*binno.antno)+1 ) #figuring out graph point coloration
  
  if(outbound.antno == 0){                                                      #keepsums calculated for potential use in keepscale based on overall data
    innie <- which((apply(dat.propx$x, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) > 0) == TRUE) #Find which trajectories are inbound
    keepno <- colSums(dat.antno$x[innie,] > 0)
    plotno <- keepno[mintimef.antno:maxtimef.antno]
    ylab.antno <- "inbound ants in region"
    
  }else if(outbound.antno == 1){
    outie <- which((apply(dat.propx$x, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) < 0) == TRUE) #Find which trajectories are outbound
    keepno <- colSums(dat.antno$x[outie,] > 0)
    plotno <- keepno[mintimef.antno:maxtimef.antno]
    ylab.antno <- "outbound ants in region"
    
  }else{
    keepno <- colSums(dat.antno$x > 0)
    plotno <- keepno[mintimef.antno:maxtimef.antno]
    ylab.antno <- "ants in region"
  }
  
  if(prop.antno == TRUE){
    plotno <- plotno / antmax.antno
  }
  
  ##Return data if not graphing
  if(graph.antno == FALSE){
    return(keepno)
    #return(data.frame(x = mintimef.antno:maxtimef.antno, y = keepno))
  }
  
  ##Plotting function
  
  #Assembles the main title of the plot based on input arguments
  if(missing(titles.antno)){
    plotitle <- paste0('Number of',
       if(outbound.antno==1){
         ' outbound'
       }else if(outbound.antno==0){
         ' inbound'
       },
       ' ants in region (', round(mintimef.antno / (frate.antno*60)), 'm to ', round(maxtimef.antno/(frate.antno*60)),'m)'
    )
  }else{plotitle <- titles.antno}
  
  plot(x = mintimef.antno:maxtimef.antno, y = plotno[mintimef.antno:maxtimef.antno],
       ylim = if(keepscale.antno == TRUE){c(0,antmax.antno)}else{NULL},
       main = plotitle, xlab = "Frame Number", 
       ylab = if(prop.antno == FALSE){
            paste("Number of", ylab.antno)
         }else{
           paste("Proportion of", ylab.antno)},
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

