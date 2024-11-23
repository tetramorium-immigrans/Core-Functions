#Cumulative myrmibution function (cmf)
#Totals up how many trajectories have gone in or out by a given frame to produce an overall total flow

#find min time when trajectory is active
#assess whether it is out or in
#Go from min time to min time adding or subtracting a direction and then graph over time

cmf <- function(dat = trajs,
                mintime, maxtime, frate = 5,
                binno = 6, lspace = 10, legloc = 2, keepscale = FALSE, titles, ...){
  
  cmf.f(dat.cmf = dat, mintime.cmf = mintime, maxtime.cmf = maxtime, frate.cmf = frate, binno.cmf = binno, 
        lspace.cmf = lspace, legloc.cmf = legloc, keepscale.cmf = keepscale, titles.cmf = titles)
}

cmf.f <- function(dat.cmf = trajs,
                  mintime.cmf, maxtime.cmf, frate.cmf,
                  binno.cmf, lspace.cmf, legloc.cmf, keepscale.cmf, titles.cmf, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.cmf <- max(colSums((dat.cmf$x) > 0))
  
  #mintime/maxtime: finding and converting
  minmaxf.cmf <- Minmax(dat.cmf, mintime = mintime.cmf, maxtime = maxtime.cmf, lspace = lspace.cmf, frate = frate.cmf)
  mintimef.cmf <- minmaxf.cmf[1]
  maxtimef.cmf <- minmaxf.cmf[2]
  lspacef.cmf <- minmaxf.cmf[3]
  
  ecks <- dat.cmf$x[,mintimef.cmf:maxtimef.cmf]
  
  datmin <- apply(ecks, 1, function(z){min(which(z != 0))})
  #datmax <- apply(ecks, 1, function(z){max(which(z != 0))})
  
  innie <- apply(ecks, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) > 0
  
  # datmin <- c()
  # datmax <- c()
  # 
  # innie <- pbapply(ecks, 1, function(z){
  #   mintemp <- min(which(z != 0))
  #   maxtemp <- max(which(z != 0))
  #   
  #   datmin <- c(datmin, mintemp)
  #   datmax <- c(datmax, maxtemp)
  #   
  #   z[maxtemp] - z[mintemp]
  #   })
  
  #go through each innie and add or subtract 1 from all subsequent values
  
  maxval <- dim(ecks)[2]
  cmfvalues <- rep(0, length.out = maxval)
  
  for(i in 1:length(datmin)){
    if(innie[i] == TRUE){
      cmfvalues[datmin[i]:maxval] <- cmfvalues[datmin[i]:maxval] + 1
    }else{
      cmfvalues[datmin[i]:maxval] <- cmfvalues[datmin[i]:maxval] - 1
    }
  }
  
  graphcol <- floor( (colSums((dat.cmf$x[,mintimef.cmf:maxtimef.cmf]) != 0)/antmax.cmf*binno.cmf)+1 ) #figuring out graph point coloration
  
  counts <- sum(innie)                                                          #For the title
  
  plot(x = mintimef.cmf:maxtimef.cmf, y = cmfvalues, 
       #ylim = if(keepscale.cmf == TRUE){c(min(keepsums, na.rm = TRUE),max(keepsums, na.rm = TRUE))}else{NULL},
       main = paste0("Cumulative direction of ant trajecories with time (in/out ratio of ", counts, "/", length(innie) - counts, ")"),
       xlab = "Frame Number", ylab = "Balance of ants in and out", 
       col = graphcol)
  
  
  #Adding graphical aspects to plot
  dims.cmf <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60))
  
  arrows(x0 = rep(dims.cmf[1]+(par("usr")[2]-par("usr")[1])/60, 2), y0 = c((par("usr")[4]-par("usr")[3])/60, (par("usr")[3]-par("usr")[4])/60), y1 = c((par("usr")[4]-par("usr")[3])/12, (par("usr")[3]-par("usr")[4])/12), length = 0.2)
  text(x = rep(dims.cmf[1]+(par("usr")[2]-par("usr")[1])/60, 2), y = c((par("usr")[4]-par("usr")[3])/9, (par("usr")[3]-par("usr")[4])/9), labels = c("Nest", "Bait"), srt = 90)
  
  abline(h = 0, lty = 3)                                                        #x-axis
  linseq <- seq(from = 0, to = maxtimef.cmf, by = lspacef.cmf)                  #Find where to put minute lines
  abline(v = linseq, lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))        #Draw minute lines
  text(linseq, dims.cmf[5], labels = paste0(linseq / (frate.cmf*60), "m"),      #Add labels to minute lines
       col = rgb(0,0,0, alpha = 0.45))
  
  Legendno(type = 1, binno = binno.cmf, legloc = legloc.cmf,                    #Add legend
           antmax = antmax.cmf) 
  
}