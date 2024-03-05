##Antlengvar
#A comparison of the length of trajectories with their circular variance

#dat is data, indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#outbound is how the analysis is restricted; 0 = all trajectories, 1 = outbound only, 2 = inbound only
#active is whether to take the variance between all trajectory headings in a frame (TRUE) or the mean variance of all trajectories active during that frame (FALSE)

#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s)

#binno (bin number) is the number of colors used to code for relative numbers on the trail
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)

#titles is the option to manually enter a plot title; utilized by Mass.Function to pass file names for graph identification

Antlengvar <- function(dat = trajs, datlocs = locs, outbound = 0,
                      chron = FALSE, lrestrict, urestrict,
                      mintime, maxtime, frate = 5,
                      binno = 6, legloc = 2, keepscale = FALSE, 
                      titles, ...){
  Antlengvar.f(dat, datlocs, outbound, chron, lrestrict, urestrict, mintime, maxtime, frate, binno, legloc, keepscale, titles, ...)
}

Antlengvar.f <- function(trajs.antlengvar, locs.antlengvar, outbound.antlengvar,
                        chron.antlengvar, lrestrict.antlengvar, urestrict.antlengvar,
                        mintime.antlengvar, maxtime.antlengvar, frate.antlengvar,
                        binno.antlengvar, legloc.antlengvar, keepscale.antlengvar,
                        titles.antlengvar, ...){
  
  #Greatest number of trajectories in region during any frame (for purposes of coloring)
  #antmax.antlengvar <- max(rowSums(!is.na(trajs.antlengvar[,,4])))             
  
  #mintime/maxtime: finding and converting
  minmaxf.antlengvar <- Minmax(trajs.antlengvar, mintime = mintime.antlengvar, maxtime = maxtime.antlengvar, frate = frate.antlengvar)
  mintimef.antlengvar <- minmaxf.antlengvar[1]
  maxtimef.antlengvar <- minmaxf.antlengvar[2]
  
  #Data found from whole set
  print('Calculating angular variances')
  wyesums <- (pbapply(trajs.antlengvar[,,3], 2, angular.variance, na.rm = TRUE))/2  #Find all the variances (for purposes of keepscale)
  
  if(chron.antlengvar == TRUE){
    eksums <- colSums(!is.na(trajs.antlengvar[,,4])) / frate.antlengvar         #Find all durations (for purposes of keepscale)
    xlab.antlengvar <- "Duration of trajectory (seconds)"
    
  }else{
    eksums <- apply(locs.antlengvar[,,3], 2, max, na.rm = TRUE)                 #Find all lengths (for purposes of keepscale)
    xlab.antlengvar <- "Length of trajectory (cm)"
  }
  
  #Filtering the data down
  
  
  eksplot <- eksums[Active(trajs.antlengvar, mintimef.antlengvar:maxtimef.antlengvar)] #Filter eks to given time
  wyeplot <- wyesums[Active(trajs.antlengvar, mintimef.antlengvar:maxtimef.antlengvar)] #Filter wye to given time
  
  if(outbound.antlengvar != 0){                                                 #Filter for outbound
    trajs.antlengvar <- Outbound(trajs.antlengvar, outbound.antlengvar)
    locs.antlengvar <- Outbound(trajs.antlengvar, outbound.antlengvar, locs = locs.antlengvar, returnlocs = TRUE)
  }
  
  if(!missing(lrestrict.antlengvar)){                                          #Filtering for lower and upper restricted ranges
    xyremove <- eksplot > lrestrict.antlengvar
    eksplot <- eksplot[xyremove]
    wyeplot <- wyeplot[xyremove]
  }

  if(!missing(urestrict.antlengvar)){
    xyremove <- eksplot < urestrict.antlengvar
    eksplot <- eksplot[xyremove]
    wyeplot <- wyeplot[xyremove]
  }
  

  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.antlengvar)){
    plotitle <- paste0(if(chron.antlengvar==TRUE){'Duration'}else{'Length'},' vs. circular variance of heading in',
       if(outbound.antlengvar==1){
         ' outbound'
       }else if(outbound.antlengvar==2){
         ' inbound'
       }else{
         ' inbound and outbound'
       },
       ' trajectories (', round(mintimef.antlengvar / (frate.antlengvar*60)), 'm to ', round(maxtimef.antlengvar/(frate.antlengvar*60)),'m)'
    )
    
  }else{plotitle <- titles.antlengvar}
  
  
  #Plotting function with KEEPSCALE option
  plot(x = eksplot, y = wyeplot,
       xlim = if(keepscale.antlengvar == TRUE){c(min(eksums, na.rm = TRUE),max(eksums, na.rm = TRUE))}else{NULL},
       ylim = if(keepscale.antlengvar == TRUE){c(min(wyesums, na.rm = TRUE),max(wyesums, na.rm = TRUE))}else{NULL},
       main = plotitle, xlab = xlab.antlengvar, ylab = 'Angular variance of trajectory headings')
  
  
  #Adding visuals to graph
  abline(h = mean(wyesums, na.rm = TRUE), lty = 5)  #Mean of circular variance of ALL data (not just of filtered data)
  abline(lm(wyeplot~eksplot), lty = 3)                                    #Linear trend line (of only filtered data)
  #Legendno(type = 1, binno = binno.antlengvar, legloc = legloc.antlengvar, antmax = antmax.antlengvar) #Legend
}

