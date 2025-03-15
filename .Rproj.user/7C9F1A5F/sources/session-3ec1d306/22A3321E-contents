#antplot
#Subfunction for plotting, labeling, etc. my graphs

antplot <- function(mintimef.antplot, maxtimef.antplot, plotdat.antplot, keepdat.antplot,
                    ylim.antplot,
                    main.antplot, ylab.antplot,
                    antmax.antplot, col.antplot,
                    frate.antplot = 5, lspacef.antplot = 10, winspacef.antplot = 30, winsizef.antplot = 30, 
                    binno.antplot = 6, legloc.antplot = 2, keepscale.antplot = FALSE, ...){
  
  plot(x = mintimef.antplot:maxtimef.antplot, y = plotdat.antplot, 
       ylim = ylim.antplot,
       main = main.antplot, xlab = "Frame Number", ylab = ylab.antplot, 
       col = col.antplot)
  
  #Values for time lines below:
  #dims[1:4] are size of the plot
  #dims[5] is calculating where to put the text just above the bottom of the frame
  #dims[6] is where the dotted line ends just above the text (currently unused)
  dims.antplot <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) 
  
  ##Adding visuals to graph
  
  #x-axis and grand mean line
  abline(h = 0, lty = 3)                                                        
  abline(h = mean(keepdat.antplot, na.rm = TRUE), lty = 3)                      

  #mean line for selected range (if keepscale is TRUE)
  if(keepscale.antplot == TRUE){
    abline(h = mean(keepdat.antplot, na.rm = TRUE), lty = 3, col = "orange")           
  }
  
  #Direction arrows for Bait and Nest
  arrows(x0 = rep(dims.antplot[1]+(par("usr")[2]-par("usr")[1])/60, 2), y0 = c((par("usr")[4]-par("usr")[3])/60, (par("usr")[3]-par("usr")[4])/60), y1 = c((par("usr")[4]-par("usr")[3])/12, (par("usr")[3]-par("usr")[4])/12), length = 0.2)
  text(x = rep(dims.antplot[1]+(par("usr")[2]-par("usr")[1])/60, 2), y = c((par("usr")[4]-par("usr")[3])/9, (par("usr")[3]-par("usr")[4])/9), labels = c("Nest", "Bait"), srt = 90)
  
  #Minute lines
  linseq <- seq(from = 0, to = maxtimef.antplot, by = lspacef.antplot)          
  abline(v = linseq, lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))        
  text(linseq, dims.antplot[5], labels = paste0(linseq / (frate.antplot*60), "m"), 
       col = rgb(0,0,0, alpha = 0.45))
  
  #Trend lines
  dotseq <- seq(from = mintimef.antplot, to = maxtimef.antplot, by = winspacef.antplot) #Find where to put the points
  dotseq <- dotseq[(dotseq - winsizef.antplot) > 0 & (dotseq + winsizef.antplot) < maxtimef.antplot] #Remove points that would cause the function to out-of-bounds
  
  varseq <- sapply(dotseq, FUN = function(x){
    mean(keepdat.antplot[(x - winsizef.antplot):(x + winsizef.antplot)])
  })
  
  #varseq <- sapply(dotseq, FUN = function(x){mean(keepdat.antplot[(x - winsizef.antplot):(x + winsizef.antplot)]*abs(keepdat.antplot[(x - winsizef.antplot):(x + winsizef.antplot)]), na.rm = TRUE)}) #Get mean of mean x-vecs in range
  
  points(x = dotseq, y = varseq, pch = 18, col = "gray26")                      #Add average points
  lines(x = dotseq, y = varseq, col = "gray26")                                 #Add connection lines between average points
  
  Legendno(type = 1, binno = binno.antplot, legloc = legloc.antplot,            #Add legend
           antmax = antmax.antplot) 
  
}

