##Antnumleng
#A comparison of the mean trajectory speed with the number of ants in the frame

#dat is data, indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#outbound is how the analysis is restricted; 0 = all trajectories, 1 = outbound only, 2 = inbound only (CURRENTLY NOT WORKING)

#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s)

#binno (bin number) is the number of colors used to code for relative numbers on the trail
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)

#titles is the option to manually enter a plot title; utilized by Mass.Function to pass file names for graph identification

Antnumleng <- function(dat = locs, outbound = 0,
                        mintime, maxtime, frate = 5,
                        binno = 6, legloc = 2, keepscale = FALSE, 
                        titles, ...){
  Antnumleng.f(dat, outbound, mintime, maxtime, frate, binno, legloc, keepscale, titles, ...)
}

Antnumleng.f <- function(locs.antnumleng, outbound.antnumleng,
                          mintime.antnumleng, maxtime.antnumleng, frate.antnumleng,
                          binno.antnumleng, legloc.antnumleng, keepscale.antnumleng,
                          titles.antnumleng, ...){
  
  #Greatest number of trajectories in region during any frame (for purposes of coloring)
  antmax.antnumleng <- max(rowSums(!is.na(locs.antnumleng[,,4])))             
  
  #mintime/maxtime: finding and converting
  minmaxf.antnumleng <- Minmax(locs.antnumleng, mintime = mintime.antnumleng, maxtime = maxtime.antnumleng, frate = frate.antnumleng)
  mintimef.antnumleng <- minmaxf.antnumleng[1]
  maxtimef.antnumleng <- minmaxf.antnumleng[2]
  
  #X-axis and graphcol found first from total data set
  ekstotal <- rowSums(!is.na(locs.antnumleng[,,1]))
  eks <- ekstotal[mintimef.antnumleng:maxtimef.antnumleng]
  
  graphcol <- floor( (rowSums(!is.na(locs.antnumleng[mintimef.antnumleng:maxtimef.antnumleng,,1]))/antmax.antnumleng*binno.antnumleng)+1 )
 
  # #Y-axis (means) found after set is filtered for inbound/outbound
  # if(outbound.antnumleng != 0){
  #   locs.antnumleng <- Outbound(locs.antnumleng, outbound.antnumleng)
  # }
  
  wyetotal <- colMaxs(locs.antnumleng[,,3], na.rm = TRUE)
  wyemeans <- pbsapply(1:dim(locs.antnumleng)[1], FUN = function(x){
    activetrajs <- Active(locs.antnumleng, x)
    if(length(activetrajs) > 0){
      mean(wyetotal[activetrajs], na.rm = TRUE)
    }else{return(NA)}
  })
  
  wye <- wyemeans[mintimef.antnumleng: maxtimef.antnumleng]
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.antnumleng)){
    plotitle <- paste0('# of ants vs. mean',
       if(outbound.antnumleng==1){
         ' outbound'
       }else if(outbound.antnumleng==2){
         ' inbound'
       }else{
         ' inbound and outbound'
       },
       ' trajectory length (', round(mintimef.antnumleng / (frate.antnumleng*60)), 'm to ', round(maxtimef.antnumleng/(frate.antnumleng*60)),'m)'
    )
    
  }else{plotitle <- titles.antnumleng}
  
  plot(x = eks, y = wye,
       xlim = if(keepscale.antnumleng == TRUE){c(min(eks, na.rm = TRUE),max(eks, na.rm = TRUE))}else{NULL},
       ylim = if(keepscale.antnumleng == TRUE){c(min(wye, na.rm = TRUE),max(wye, na.rm = TRUE))}else{NULL},
       main = plotitle, xlab = '# of ants in region', ylab = 'mean trajectory length (cm)',
       col = graphcol
       )
  
  #Adding visuals to graph
  abline(mean(locs.antnumleng[,,3], na.rm = TRUE), 0, lty = 5)  #Mean of mean speeds of ALL data (not just mintime-maxtime)
  abline(lm(wye~eks), lty = 3)                                    #Linear trend line (of only selected time)
  Legendno(type = 1, binno = binno.antnumleng, legloc = legloc.antnumleng, antmax = antmax.antnumleng) #Legend
}

