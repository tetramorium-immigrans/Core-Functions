##Antnumvar
#A comparison of the circular variation in heading with with the number of ants in the frame

#dat is data, indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#outbound is how the analysis is restricted; 0 = all trajectories, 1 = outbound only, 2 = inbound only
#active is whether to take the variance between all trajectory headings in a frame (TRUE) or the mean variance of all trajectories active during that frame (FALSE)

#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s)

#binno (bin number) is the number of colors used to code for relative numbers on the trail
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)

#titles is the option to manually enter a plot title; utilized by Mass.Function to pass file names for graph identification

Antnumvar <- function(dat = trajs, outbound = 0, active = TRUE,
                        mintime, maxtime, frate = 5,
                        binno = 6, legloc = 2, keepscale = FALSE, 
                        titles, ...){
  Antnumvar.f(dat, outbound, active, mintime, maxtime, frate, binno, legloc, keepscale, titles, ...)
}

Antnumvar.f <- function(trajs.antnumvar, outbound.antnumvar, active.antnumvar,
                          mintime.antnumvar, maxtime.antnumvar, frate.antnumvar,
                          binno.antnumvar, legloc.antnumvar, keepscale.antnumvar,
                          titles.antnumvar, ...){
  
  #Greatest number of trajectories in region during any frame (for purposes of coloring)
  antmax.antnumvar <- max(rowSums(!is.na(trajs.antnumvar[,,4])))             
  
  #mintime/maxtime: finding and converting
  minmaxf.antnumvar <- Minmax(trajs.antnumvar, mintime = mintime.antnumvar, maxtime = maxtime.antnumvar, frate = frate.antnumvar)
  mintimef.antnumvar <- minmaxf.antnumvar[1]
  maxtimef.antnumvar <- minmaxf.antnumvar[2]
  
  #X-axis and graphcol found first from total data set
  eks <- rowSums(!is.na(trajs.antnumvar[mintimef.antnumvar:maxtimef.antnumvar,,4]))
  graphcol <- floor( (rowSums(!is.na(trajs.antnumvar[mintimef.antnumvar:maxtimef.antnumvar,,4]))/antmax.antnumvar*binno.antnumvar)+1 )
  
  #Y-axis (means) found after set is filtered for inbound/outbound
  if(outbound.antnumvar != 0){
    trajs.antnumvar <- Outbound(trajs.antnumvar, outbound.antnumvar)
  }
  
  if(active.antnumvar == TRUE){
    cvars <- (apply(trajs.antnumvar[,,3], 2, angular.variance, na.rm = TRUE))/2       #CIRCULAR VERSION (divide by 2 because a.v returns value as *2)
    
    keepvars <- pbsapply(1:dim(trajs.antnumvar)[1], FUN = function(x){            #Find the mean of the trajectory variances for total time (keepscale again)
      activetrajs <- Active(trajs.antnumvar, x)
      if(length(activetrajs) > 0){                                              #If there are no trajectories active at a time return 0, otherwise mean
        mean(cvars[activetrajs], na.rm = TRUE)
      }else{return(NA)}
    })
    
    wye <- keepvars[mintimef.antnumvar:maxtimef.antnumvar]                     #Subset those to be graphed
    
    ylab.antnumvar <- "Mean circular variance of trajectory headings"
    
  }else if(active.antnumvar == FALSE){
    wye <- (apply(trajs.antnumvar[mintimef.antnumvar:maxtimef.antnumvar,,3], 1, angular.variance, na.rm = TRUE))/2
    
    ylab.antnumvar <- "Circular variance of ant headings"
  }
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.antnumvar)){
    plotitle <- paste0('# of ants vs. circular variance of heading in',
       if(outbound.antnumvar==1){
         ' outbound'
       }else if(outbound.antnumvar==2){
         ' inbound'
       }else{
         ' inbound and outbound'
       },
       ' ants (', round(mintimef.antnumvar / (frate.antnumvar*60)), 'm to ', round(maxtimef.antnumvar/(frate.antnumvar*60)),'m)'
    )
    
  }else{plotitle <- titles.antnumvar}
  
  
  #Plotting function with KEEPSCALE option
  plot(x = eks, y = wye,
       xlim = if(keepscale.antnumvar == TRUE){c(0,antmax.antnumvar)}else{NULL},
       ylim = if(keepscale.antnumvar == TRUE){c(min(keepvars, na.rm = TRUE),max(keepvars, na.rm = TRUE))}else{NULL},
       main = plotitle, xlab = '# of ants in region', ylab = ylab.antnumvar,
       col = graphcol)
  
  
  #Adding visuals to graph
  abline(mean(trajs.antnumvar[,,4], na.rm = TRUE), 0, lty = 5)  #Mean of circular variance of ALL data (not just mintime-maxtime)
  abline(lm(wye~eks), lty = 3)                                    #Linear trend line (of only selected time)
  Legendno(type = 1, binno = binno.antnumvar, legloc = legloc.antnumvar, antmax = antmax.antnumvar) #Legend
}

