#Cumulative formicibution function (cff)
#Totals up how many trajectories have gone in or out by a given frame to produce an overall total flow

#find min time when trajectory is active
#assess whether it is out or in
#Go from min time to min time adding or subtracting a direction and then graph over time

cff <- function(dat = trajs,
                mintime, maxtime, frate = 5,
                binno = 6, lspace = 10, legloc = 2, keepscale = FALSE, titles, ...){
  
  cff.f(dat.cff = dat, mintime.cff = mintime, maxtime.cff = maxtime, frate.cff = frate, binno.cff = binno, 
        lspace.cff = lspace, legloc.cff = legloc, keepscale.cff = keepscale, titles.cff = titles)
}

cff.f <- function(dat.cff = trajs,
                  mintime.cff, maxtime.cff, frate.cff,
                  binno.cff, lspace.cff, legloc.cff, keepscale.cff, titles.cff, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.cff <- max(colSums((dat.cff$x) > 0))
  
  #mintime/maxtime: finding and converting
  minmaxf.cff <- Minmax(dat.cff, mintime = mintime.cff, maxtime = maxtime.cff, lspace = lspace.cff, frate = frate.cff)
  mintimef.cff <- minmaxf.cff[1]
  maxtimef.cff <- minmaxf.cff[2]
  lspacef.cff <- minmaxf.cff[3]
  
  ecks <- dat.cff$x[,mintimef.cff:maxtimef.cff]
  
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
  cffvalues <- rep(0, length.out = maxval)
  
  for(i in 1:length(datmin)){
    if(innie[i] == TRUE){
      cffvalues[datmin[i]:maxval] <- cffvalues[datmin[i]:maxval] + 1
    }else{
      cffvalues[datmin[i]:maxval] <- cffvalues[datmin[i]:maxval] - 1
    }
  }
  
  graphcol <- floor( (colSums((dat.cff$x[,mintimef.cff:maxtimef.cff]) != 0)/antmax.cff*binno.cff)+1 ) #figuring out graph point coloration
  
  counts <- sum(innie)                                                          #For the title
  
  plot(x = mintimef.cff:maxtimef.cff, y = cffvalues, 
       #ylim = if(keepscale.cff == TRUE){c(min(keepsums, na.rm = TRUE),max(keepsums, na.rm = TRUE))}else{NULL},
       main = paste0("Cumulative direction of ant trajecories with time (in/out ratio of ", counts, "/", length(innie) - counts, ")"),
       xlab = "Frame Number", ylab = "Balance of ants in and out", 
       col = graphcol)
  
  
  #Adding graphical aspects to plot
  dims.cff <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60))
  
  arrows(x0 = rep(dims.cff[1]+(par("usr")[2]-par("usr")[1])/60, 2), y0 = c((par("usr")[4]-par("usr")[3])/60, (par("usr")[3]-par("usr")[4])/60), y1 = c((par("usr")[4]-par("usr")[3])/12, (par("usr")[3]-par("usr")[4])/12), length = 0.2)
  text(x = rep(dims.cff[1]+(par("usr")[2]-par("usr")[1])/60, 2), y = c((par("usr")[4]-par("usr")[3])/9, (par("usr")[3]-par("usr")[4])/9), labels = c("Nest", "Bait"), srt = 90)
  
  abline(h = 0, lty = 3)                                                        #x-axis
  linseq <- seq(from = 0, to = maxtimef.cff, by = lspacef.cff)                  #Find where to put minute lines
  abline(v = linseq, lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))        #Draw minute lines
  text(linseq, dims.cff[5], labels = paste0(linseq / (frate.cff*60), "m"),      #Add labels to minute lines
       col = rgb(0,0,0, alpha = 0.45))
  
  Legendno(type = 1, binno = binno.cff, legloc = legloc.cff,                    #Add legend
           antmax = antmax.cff) 
  
}