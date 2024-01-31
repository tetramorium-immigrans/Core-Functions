##Antmediansdist
#Graphs time series of median speed of all ants in a given frame

#dat is data; by default it is indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#rm0 is whether to remove trajectories with a median speed of 0 (typically dead ants or other inconsequential objects); 1 removes, 0 keeps
#mintime and maxtime restrict the time (mintime is 1 by default for indexing purposes)
#binno (bin number) is the number of colors used to code for relative numbers on the trail
#lspace (line space) is how far apart the minute lines are on the graph
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise

Antmedians <- function(dat = trajs, rm0 = 1,
                     mintime = 1, maxtime,
                     binno = 6, lspace = 10, legloc = 1,
                     titles = "Median speeds over time"){
  Antmedians.f(dat, rm0, mintime, maxtime, binno, lspace, legloc, titles)
}

Antmedians.f <- function(trajs.antmedians, rm0.antmedians,
                       mintime.antmedians, maxtime.antmedians,
                       binno.antmedians, lspace.antmedians, legloc.antmedians,
                       titles.antmedians){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.antmedians <- max(rowSums(!is.na(trajs.antmedians[,,1])))             
  
  #Removing trajectories with a median of 0
  if(rm0.antmedians == 1){
    trajsm <- colMedians(trajs.antmedians[,,4], na.rm = TRUE)
    trajs.antmedians <- trajs.antmedians[,!trajsm == 0,]
  }
  
  #Converting input arguments from minutes to frames
  mintimef.antmedians <- mintime.antmedians*300
  lspacef.antmedians <- lspace.antmedians*300
  
  #Ending frame found if not given as an argument
  if(missing(maxtime.antmedians)){
    maxtimef.antmedians <- dim(trajs.antmedians)[1]
  }else{
    maxtimef.antmedians <- maxtime.antmedians*300                                #Converting argument from minutes to frames
  }
  
  #old simple plotting function
  #plot(rowMedians(trajs.antmedians[,,4], na.rm=TRUE), main = titles.antmedians, xlab = "Frame number", ylab = "Mean speed (cm/sec)", col = floor( (rowSums(!is.na(trajs.antmedians[,,1]))/antmax.antmedians*binno.antmedians)+1 ))
  
  #new plotting function with mintime and maxtime
  rmeans <- rowMedians(trajs.antmedians[,,4], na.rm=TRUE)
  plot(x = mintimef.antmedians:maxtimef.antmedians, y = rmeans[mintimef.antmedians:maxtimef.antmedians], 
       main = titles.antmedians, xlab = "Frame number", ylab = "Mean speed (cm/sec)", 
       col = floor( (rowSums(!is.na(trajs.antmedians[,,1]))/antmax.antmedians*binno.antmedians)+1 ))
  dims.antmedians <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) #Values for time lines below; dims.antmedians[1:4] are size of the plot, dims.antmedians[5] is calculating where to put the text just above the bottom of the frame, dims.antmedians[6] is where the dotted line ends just above the text
  
  #Add minute lines
  for(i in seq(from = 0, to = maxtimef.antmedians, by = lspacef.antmedians)){      #Adds and labels dotted lines every 10 minutes
    lines(c(i,i), c(dims.antmedians[4],dims.antmedians[6]), type = "l", lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))
    text(i, dims.antmedians[5], labels = paste0(i / 300, "m"), col = rgb(0,0,0, alpha = 0.45))
  }
  
  #Add median line of entire time series (not just selected time)
  lines(c(-1000,maxtimef.antmedians), rep(median(trajs.antmedians[,,4], na.rm = TRUE),2), type = "l", lty = 5)  
  
  #Add legend
  if(legloc.antmedians == 1){
    legend("topleft", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antmedians/binno.antmedians)),paste(floor(antmax.antmedians/binno.antmedians*((2:binno.antmedians)-1))+1, "to", floor(antmax.antmedians/binno.antmedians*(2:binno.antmedians)))),
           fill = 1:binno.antmedians)
  }else if(legloc.antmedians == 2){
    legend("topright", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antmedians/binno.antmedians)),paste(floor(antmax.antmedians/binno.antmedians*((2:binno.antmedians)-1))+1, "to", floor(antmax.antmedians/binno.antmedians*(2:binno.antmedians)))),
           fill = 1:binno.antmedians)
  }else if(legloc.antmedians == 3){
    legend("bottomright", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antmedians/binno.antmedians)),paste(floor(antmax.antmedians/binno.antmedians*((2:binno.antmedians)-1))+1, "to", floor(antmax.antmedians/binno.antmedians*(2:binno.antmedians)))),
           fill = 1:binno.antmedians)
  }else if(legloc.antmedians == 4){
    legend("bottomleft", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antmedians/binno.antmedians)),paste(floor(antmax.antmedians/binno.antmedians*((2:binno.antmedians)-1))+1, "to", floor(antmax.antmedians/binno.antmedians*(2:binno.antmedians)))),
           fill = 1:binno.antmedians)
  }else{
    print("No legend added; choose 1-4 for topleft, topright, bottomright, or bottomleft.")
  }
}
