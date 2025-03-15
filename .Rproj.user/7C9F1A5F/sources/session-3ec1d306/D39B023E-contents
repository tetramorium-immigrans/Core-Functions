#antfund
#Creates fundamental diagrams (density-speed, density-flow, and speed-flow)
#(currently only density-speed working)

#dat is data, a list of multiple sparse matrices
#graph is whether to output a graph
#datreturn is whether to return the values of the calculations

#--direction is how the analysis is restricted:
#(0) = all trajectories (default)
#(1) = inbound only
#(2) = outbound only
#--bytime is how to analyze the data:
#TRUE = calculates the instantaneous speed of the ants in each frame
#FALSE = calculates the overall speed of each trajectory and then uses that value to calculate the frame speed (default)
#--fn is which measure of central tendency to use:
#dsp = density-speed
#dfl = density-flow
#sfl = speed-flow

#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s); anywhere you see "frate*60" is converting frames to minutes or back

#titles is the option to manually enter a plot title; utilized by Mass.Function to pass file names for graph identification

antfund <- function(dat = trajs, graph = TRUE, datreturn = FALSE,
                     direction = 0, bytime = TRUE, fn = "dsp", rand = FALSE,
                     xmin, xmax,
                     ymin, ymax,
                     mintime, maxtime, frate = 5, 
                     titles, ...){
  
  antfund.f(dat.antfund = dat, graph.antfund = graph, datreturn.antfund = datreturn,
             direction.antfund = direction, bytime.antfund = bytime, fn.antfund = fn, rand.antfund = rand,
             xmin.antfund = xmin, xmax.antfund = xmax,
             ymin.antfund = ymin, ymax.antfund = ymax,
             mintime.antfund = mintime, maxtime.antfund = maxtime, frate.antfund = frate, 
             titles.antfund = titles, ...)
}

antfund.f <- function(dat.antfund, graph.antfund, datreturn.antfund,
                       direction.antfund, bytime.antfund, fn.antfund, rand.antfund,
                       xmin.antfund, xmax.antfund,
                       ymin.antfund, ymax.antfund,
                       mintime.antfund, maxtime.antfund, frate.antfund,
                       titles.antfund, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  #antmax.antfund <- max(colSums((dat.antfund$x) > 0))
  
  #Minx and maxes: finding and converting
  minmaxf.antfund <- Minmax(dat.antfund, mintime = mintime.antfund, maxtime = maxtime.antfund, frate = frate.antfund)
  mintimef.antfund <- minmaxf.antfund[1]
  maxtimef.antfund <- minmaxf.antfund[2]
  lspacef.antfund <- minmaxf.antfund[3]
  
  if(missing(xmin.antfund)){xmin.antfund <- min(dat.antfund$x)}
  if(missing(xmax.antfund)){xmax.antfund <- max(dat.antfund$x)}
  if(missing(ymin.antfund)){ymin.antfund <- min(dat.antfund$y)}
  if(missing(ymax.antfund)){ymax.antfund <- max(dat.antfund$y)}
  
  #Getting graph colors (important: BEFORE filtering)
  #graphcol <- floor( (colSums((dat.antfund$x[,mintimef.antfund:maxtimef.antfund]) != 0)/antmax.antfund*binno.antfund)+1 ) #figuring out graph point coloration
  
  #filter based on direction and specified area
  
  if(direction.antfund == 0){
    filterdat <- dat.antfund$speed
    filterx <- dat.antfund$x
    filtery <- dat.antfund$y
  }else if(direction.antfund == 1){                                                      
    innie <- which((apply(dat.antfund$x, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) > 0) == TRUE) #Find which trajectories are inbound
    filterdat <- dat.antfund$speed[innie,]
    filterx <- dat.antfund$x[innie,]
    filtery <- dat.antfund$y[innie,]
  }else if(direction.antfund == 2){                                                      
    outie <- which((apply(dat.antfund$x, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) < 0) == TRUE) #Find which trajectories are outbound
    filterdat <- dat.antfund$speed[outie,]
    filterx <- dat.antfund$x[outie,]
    filtery <- dat.antfund$y[outie,]
  }
  
  filterdat[which(filterx > xmax.antfund | filterx < xmin.antfund)] <- NA
  filterdat[which(filtery > ymax.antfund | filtery < ymin.antfund)] <- NA
  filterdat[filterdat == 0] <- NA
  
  #Main calculations
  
  #activetrajs <- apply(filterdat, 2, function(z){which(z != 0)})                #Identifying active trajectories in each frame
  #keepcount <- unlist(lapply(activetrajs, length)) / ((xmax.antfund - xmin.antfund) * (ymax.antfund - ymin.antfund))

  keepcount <- colSums(!is.na(filterdat)) / ((xmax.antfund - xmin.antfund) * (ymax.antfund - ymin.antfund))
  
  if(bytime.antfund == FALSE & rand.antfund == FALSE){
    
    if(fn.antfund == "dsp"){
      trajdat <- rowMeans(filterdat, na.rm = TRUE)
      
      activetrajs <- apply(filterdat, 2, function(z){which(!is.na(z))})                #Identifying active trajectories in each frame
      
      # trajdat <- apply(filterdat, 1, function(z){
      #   mean(z[min(which(z != 0)):max(which(z != 0))])
      # })
       
      keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
        mean(trajdat[activetrajs[[z]]])
      })
      
      plotcount <- keepcount[mintimef.antfund:maxtimef.antfund]
      plotdat <- keepdat[mintimef.antfund:maxtimef.antfund]
      ylab.antfund <- "Mean speed (cm/s)"
      
    }else if(fn.antfund == "dfl"){
      #PH
      
    }else if(fn.antfund == "sfl"){
      #PH
    }
    
  }else if(bytime.antfund == TRUE & rand.antfund == FALSE){
    
    if(fn.antfund == "dsp"){
      keepdat <- colMeans(filterdat, na.rm = TRUE)
      
      # keepdat <- pbsapply(1:dim(filterdat)[2], function(z){
      #   mean(filterdat[activetrajs[[z]], z])
      # })
      
      plotcount <- keepcount[mintimef.antfund:maxtimef.antfund]
      plotdat <- keepdat[mintimef.antfund:maxtimef.antfund]
      ylab.antfund <- "Mean speed (cm/s)"
      
    }else if(fn.antfund == "dfl"){
      #PH
      
    }else if(fn.antfund == "sfl"){
      #PH
    }
    
  }
  
  if(rand.antfund == TRUE){
    minz <- apply(filterdat, 1, function(z){
      min(which(!is.na(z)))
    })
    
    maxz <- apply(filterdat, 1, function(z){
      max(which(!is.na(z)))
    })
    
    minz[minz < mintimef.antfund] <- mintimef.antfund
    maxz[maxz > maxtimef.antfund] <- maxtimef.antfund
    
    tim <- pbsapply(1:length(minz), function(z){
      if(maxz[z] < minz[z]){return(NA)}
      return(sample(minz[z]:maxz[z], 1))
    })
    
    plotcount <- keepcount[tim]
    plotdat <- filterdat[cbind(1:dim(filterdat)[1], tim)]
    ylab.antfund <- "Sample speed (cm/s)"
    
    # / ((xmax.antfund - xmin.antfund) * (ymax.antfund - ymin.antfund))
  }
  
  #Output graph and data
  
  if(datreturn.antfund == TRUE){
    antfundreturn <- data.frame(plotdat = plotdat, plotcount = plotcount)
    #names(antfundreturn) <- c("plotdat", "plotcount")
    return(antfundreturn)
  }
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.antfund)){
    plotitle <- paste0(if(fn.antfund == "dsp"){'Density x speed of'}else if(fn.antfund == "dfl"){'Arithmetic mean speed of'}else{'median speed of'},
         if(direction.antfund==1){
           ' inbound'
         }else if(direction.antfund==2){
           ' outbound'
         }else{
           ' inbound and outbound'
         },
         
         if(bytime.antfund == TRUE){
           ' ants ('
         }else if(bytime.antfund == FALSE){
           ' active trajectories ('
         }, round(mintimef.antfund / (frate.antfund*60)), 'm to ', round(maxtimef.antfund/(frate.antfund*60)),'m)'
    )
    
  }else{plotitle <- titles.antfund}
  
  #hotfix to make the graphs prettier
  # plotcount <- plotcount[plotdat < 1.5]
  # plotdat <- plotdat[plotdat < 1.5]
  
  plot(plotcount, plotdat,
       #ylim = c(0, 1.5),
       main = plotitle, xlab = 'Density (ants/cm^2)', ylab = ylab.antfund)
  
  x0 <- seq(min(plotcount), max(plotcount), length = 100)
  lmdat <- data.frame(plotdat = plotdat, plotcount = plotcount)
  
  lm1 <- lm(plotdat ~ plotcount, lmdat)
  #lm2 <- lm(plotdat ~ plotcount + I(plotcount^2), lmdat)
  lm3 <- lm(plotdat ~ plotcount + I(plotcount^2) + I(plotcount^3), lmdat)
  #lm4 <- lm(plotdat ~ plotcount + I(plotcount^2) + I(plotcount^3) + I(plotcount^4), lmdat)
  #lm5 <- lm(plotdat ~ plotcount + I(plotcount^2) + I(plotcount^3) + I(plotcount^4) + I(plotcount^5), lmdat)
  lm6 <- lm(plotdat ~ plotcount + I(plotcount^2) + I(plotcount^3) + I(plotcount^4) + I(plotcount^5) + I(plotcount^6), lmdat)
  #lm7 <- lm(plotdat ~ plotcount + I(plotcount^2) + I(plotcount^3) + I(plotcount^4) + I(plotcount^5) + I(plotcount^6) + I(plotcount^7), lmdat)
  #lm8 <- lm(plotdat ~ plotcount + I(plotcount^2) + I(plotcount^3) + I(plotcount^4) + I(plotcount^5) + I(plotcount^6) + I(plotcount^7) + I(plotcount^8), lmdat)
  lm9 <- lm(plotdat ~ plotcount + I(plotcount^2) + I(plotcount^3) + I(plotcount^4) + I(plotcount^5) + I(plotcount^6) + I(plotcount^7) + I(plotcount^8) + I(plotcount^9), lmdat)
  
  
  y1 <- predict.lm(lm1, newdata = data.frame(plotcount = x0))
  #y2 <- predict.lm(lm2, newdata = data.frame(plotcount = x0))
  y3 <- predict.lm(lm3, newdata = data.frame(plotcount = x0))
  #y4 <- predict.lm(lm4, newdata = data.frame(plotcount = x0))
  #y5 <- predict.lm(lm5, newdata = data.frame(plotcount = x0))
  y6 <- predict.lm(lm6, newdata = data.frame(plotcount = x0))
  #y7 <- predict.lm(lm7, newdata = data.frame(plotcount = x0))
  #y8 <- predict.lm(lm8, newdata = data.frame(plotcount = x0))
  y9 <- predict.lm(lm9, newdata = data.frame(plotcount = x0))
  
  lines(x0, y1, col = 2)
  #lines(x0, y2, col = 3)
  lines(x0, y3, col = 3)
  #lines(x0, y4, col = 5)
  #lines(x0, y5, col = 6)
  lines(x0, y6, col = 4)
  #lines(x0, y7, col = 8)
  #lines(x0, y8, col = 9)
  lines(x0, y9, col = 5)
  
  legend(x = "topright", inset = 0.03, title = "Models", 
         legend = c("x", "x^3", "x^6", "x^9"),
         fill = c(2, 3, 4, 5))
  
  preds <- predict.lm(lm1, newdata = data.frame(plotcount = x0), interval = 'confidence')
  
  lines(x0, preds[ ,3], lty = 'dashed', col = 'blue')
  lines(x0, preds[ ,2], lty = 'dashed', col = 'blue')
  polygon(c(rev(x0), x0), c(rev(preds[ ,3]), preds[ ,2]), col = rgb(1,1,1,0.75), border = NA)
  
  print(summary(lm1))
  print(aictab(cand.set = list(lm1, lm3, lm6, lm9), modnames = c("x^1","x^3","x^6","x^9")))
  
  #print(aic(data.frame(plotdat = plotdat, plotcount = plotcount)))

  
}
