#List of functions
#Antno: "Ant number", returns a graph of time vs. ant number
#Antsums: "Sum of speeds", returns a graph of time vs. sum of speeds
#Antmeans: "Mean of speeds", returns a graph of time vs. mean of speeds
#Antnetx: "Net x", returns a graph of time vs. summed x-component of velocities
#Antnety: "Net y", returns a graph of time vs. summed y-component of velocities

#Packages
library(matrixStats) #for rowVars
library(zoo) #for rollmean
source("Import.R")


#SUM OF SPEEDS
#trajs.antsums is the data
#binno.antsums is the "bin number" for how many categories for coloring purposes
#lspace.antsums is the "line spacing" for graphical purposes (3000 frames = 10 minutes)

Antsums <- function(dat = trajs, 
                    binno = 6, lspace = 3000, legloc = 1){
  Antsums.f(dat, binno, lspace, legloc)
}

Antsums.f <- function(trajs.antsums, 
                      binno.antsums, lspace.antsums, legloc.antsums){
  antmax.antsums <- max(rowSums(!is.na(trajs.antsums[,,1])))             #Greatest number of trajectories in region during any frame
  maxtime.antsums <- dim(trajs.antsums)[1]                               #Ending frame
  
  plot(rowSums(trajs.antsums[,,4], na.rm=TRUE), main = "Sum of speeds over time", xlab = "Frame number", ylab = "Sum of speeds (cm/sec)", col = floor( (rowSums(!is.na(trajs.antsums[,,1]))/antmax.antsums*binno.antsums)+1 )) #Floor+1 used rather than ceiling so that 0 values are colored black
  dims.antsums <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60))
  
  for(i in seq(from = 0, to = maxtime.antsums, by = lspace.antsums)){     #Adds and labels dotted lines every 10 minutes
    lines(c(i,i), c(dims.antsums[4],dims.antsums[6]), type = "l", lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))
    text(i, dims.antsums[5], labels = paste0(i / 300, "m"), col = rgb(0,0,0, alpha = 0.45))
  }
  
  #Add legend
  if(legloc.antsums == 1){
    legend("topleft", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antsums/binno.antsums)),paste(floor(antmax.antsums/binno.antsums*((2:binno.antsums)-1))+1, "to", floor(antmax.antsums/binno.antsums*(2:binno.antsums)))),
           fill = 1:binno.antsums)
  }else if(legloc.antsums == 2){
    legend("topright", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antsums/binno.antsums)),paste(floor(antmax.antsums/binno.antsums*((2:binno.antsums)-1))+1, "to", floor(antmax.antsums/binno.antsums*(2:binno.antsums)))),
           fill = 1:binno.antsums)
  }else if(legloc.antsums == 3){
    legend("bottomright", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antsums/binno.antsums)),paste(floor(antmax.antsums/binno.antsums*((2:binno.antsums)-1))+1, "to", floor(antmax.antsums/binno.antsums*(2:binno.antsums)))),
           fill = 1:binno.antsums)
  }else if(legloc.antsums == 4){
    legend("bottomleft", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antsums/binno.antsums)),paste(floor(antmax.antsums/binno.antsums*((2:binno.antsums)-1))+1, "to", floor(antmax.antsums/binno.antsums*(2:binno.antsums)))),
           fill = 1:binno.antsums)
  }else{
    print("No legend added; choose 1-4 for topleft, topright, bottomright, or bottomleft.")
  }
}



##MEAN SPEED
#trajs.antmeans is the data
#binno.antmeans is the "bin number" for how many categories for coloring purposes
#lspace.antmeans is the "line spacing" for graphical purposes (3000 frames = 10 minutes)

Antmeans <- function(dat = trajs, rm0 = 1,
                     mintime = 1, maxtime,
                     binno = 6, lspace = 10, legloc = 1,
                     titles = "mean speeds over time"){
  Antmeans.f(dat, rm0, mintime, maxtime, binno, lspace, legloc, titles)
}

Antmeans.f <- function(trajs.antmeans, rm0.antmeans,
                       mintime.antmeans, maxtime.antmeans,
                       binno.antmeans, lspace.antmeans, legloc.antmeans,
                       titles.antmeans){
  
  #Ending frame found if not given as an argument
  if(missing(maxtime.antmeans)){
    maxtimef.antmeans <- dim(trajs.antmeans)[1]
  }else{
    maxtimef.antmeans <- maxtime.antmeans*300                                #Converting argument from minutes to frames
  }
  
  antmax.antmeans <- max(rowSums(!is.na(trajs.antmeans[,,1])))             #Greatest number of trajectories in region during any frame
  
  #rm0: Removing trajectories with a median speed of 0
  if(rm0.antmeans == 1){
    trajsm <- colMedians(trajs.antmeans[,,4], na.rm = TRUE)
    trajs.antmeans <- trajs.antmeans[,!trajsm == 0,]
  }
  
  #Converting input arguments to frames
  mintimef.antmeans <- mintime.antmeans*300
  lspacef.antmeans <- lspace.antmeans*300
  
  #old simple plotting function
  #plot(rowMeans(trajs.antmeans[,,4], na.rm=TRUE), main = titles.antmeans, xlab = "Frame number", ylab = "Mean speed (cm/sec)", col = floor( (rowSums(!is.na(trajs.antmeans[,,1]))/antmax.antmeans*binno.antmeans)+1 ))
  
  #new plotting function with mintime and maxtime
  rmeans <- rowMeans(trajs.antmeans[,,4], na.rm=TRUE)
  plot(x = mintimef.antmeans:maxtimef.antmeans, y = rmeans[mintimef.antmeans:maxtimef.antmeans], 
       main = titles.antmeans, xlab = "Frame number", ylab = "Mean speed (cm/sec)", 
       col = floor( (rowSums(!is.na(trajs.antmeans[,,1]))/antmax.antmeans*binno.antmeans)+1 ))
  dims.antmeans <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) #Values for time lines below; dims.antmeans[1:4] are size of the plot, dims.antmeans[5] is calculating where to put the text just above the bottom of the frame, dims.antmeans[6] is where the dotted line ends just above the text
  
  for(i in seq(from = 0, to = maxtimef.antmeans, by = lspacef.antmeans)){      #Adds and labels dotted lines every 10 minutes
    lines(c(i,i), c(dims.antmeans[4],dims.antmeans[6]), type = "l", lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))
    text(i, dims.antmeans[5], labels = paste0(i / 300, "m"), col = rgb(0,0,0, alpha = 0.45))
  }
  
  lines(c(-1000,maxtimef.antmeans), rep(mean(trajs.antmeans[,,4], na.rm = TRUE),2), type = "l", lty = 5)    #Dotted line representing mean of whole time series
  
  #Add legend
  if(legloc.antmeans == 1){
    legend("topleft", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antmeans/binno.antmeans)),paste(floor(antmax.antmeans/binno.antmeans*((2:binno.antmeans)-1))+1, "to", floor(antmax.antmeans/binno.antmeans*(2:binno.antmeans)))),
           fill = 1:binno.antmeans)
  }else if(legloc.antmeans == 2){
    legend("topright", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antmeans/binno.antmeans)),paste(floor(antmax.antmeans/binno.antmeans*((2:binno.antmeans)-1))+1, "to", floor(antmax.antmeans/binno.antmeans*(2:binno.antmeans)))),
           fill = 1:binno.antmeans)
  }else if(legloc.antmeans == 3){
    legend("bottomright", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antmeans/binno.antmeans)),paste(floor(antmax.antmeans/binno.antmeans*((2:binno.antmeans)-1))+1, "to", floor(antmax.antmeans/binno.antmeans*(2:binno.antmeans)))),
           fill = 1:binno.antmeans)
  }else if(legloc.antmeans == 4){
    legend("bottomleft", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antmeans/binno.antmeans)),paste(floor(antmax.antmeans/binno.antmeans*((2:binno.antmeans)-1))+1, "to", floor(antmax.antmeans/binno.antmeans*(2:binno.antmeans)))),
           fill = 1:binno.antmeans)
  }else{
    print("No legend added; choose 1-4 for topleft, topright, bottomright, or bottomleft.")
  }
}





#PROPORTION OF INBOUND/OUTBOUND/NEITHER

Antpropx <- function(dat = trajs, winspace = 20, winsize = 20,
                     mintime = 0, maxtime,
                     binno = 6, lspace = 10, legloc = 1, outbound = 1){
  Antpropx.f(dat,winspace,winsize,mintime, maxtime,binno,lspace,legloc,outbound)
}

Antpropx.f <- function(trajs.antpropx, winspace.antpropx, winsize.antpropx,
                       mintime.antpropx, maxtime.antpropx,
                       binno.antpropx, lspace.antpropx, legloc.antpropx, outbound.antpropx){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.antpropx <- max(rowSums(!is.na(trajs.antpropx[,,1])))
  
  #Ending frame found if not given as an argument
  if(missing(maxtime.antpropx)){
    maxtimef.antpropx <- dim(trajs.antpropx)[1]
  }else{
    maxtimef.antpropx <- maxtime.antpropx*300                                #Converting argument from minutes to frames
  }
  
  #Converting input arguments to frames
  winspacef.antpropx <- winspace.antpropx*5
  winsizef.antpropx <- round(winsize.antpropx * 5 / 2)                       #Halved since the value will be used to extend to either side of a point
  mintimef.antpropx <- mintime.antpropx*300
  lspacef.antpropx <- lspace.antpropx*300
  
  #Get proportion of outbound ants; 1 = outbound, 0 = inbound, 2 = both outbound and inbound, -1 = no motion
  if(outbound.antpropx == 1){
    rprops <- rowSums(trajs.antpropx[,,1] < 0, na.rm = TRUE) / rowSums(!is.na(trajs.antpropx[,,1]), na.rm = TRUE)
  }else if(outbound.antpropx == 0){
    rprops <- rowSums(trajs.antpropx[,,1] > 0, na.rm = TRUE) / rowSums(!is.na(trajs.antpropx[,,1]), na.rm = TRUE)
  }else if(outbound.antpropx == 2){
    rprops <- (rowSums(trajs.antpropx[,,1] < 0, na.rm = TRUE) / rowSums(!is.na(trajs.antpropx[,,1]), na.rm = TRUE)) + (rowSums(trajs.antpropx[,,1] > 0, na.rm = TRUE) / rowSums(!is.na(trajs.antpropx[,,1]), na.rm = TRUE))
  }else if(outbound.antpropx == -1){
    rprops <- rowSums(trajs.antpropx[,,1] == 0, na.rm = TRUE) / rowSums(!is.na(trajs.antpropx[,,1]), na.rm = TRUE)
  }else{
    stop("Invalid outbound value; choose -1 to 2 for no x-axis motion, inbound, outbound, or inbound + outbound.")
  }
  
  #kludge fix for rprops[0] not being a proper index
  if(mintimef.antpropx == 0){
    mintimef.antpropx <- mintimef.antpropx + 1
  }
  
  #Main plotting function
  plot(x = mintimef.antpropx:maxtimef.antpropx, y = rprops[mintimef.antpropx:maxtimef.antpropx],
       xlab = "Frame Number", ylab = "",
       col = floor( (rowSums(!is.na(trajs.antpropx[mintimef.antpropx:maxtimef.antpropx,,1]))/antmax.antpropx*binno.antpropx)+1 ))
  dims.antpropx <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) #Values for time lines below; dims[1:4] are size of the plot, dims[5] is calculating where to put the text just above the bottom of the frame, dims[6] is where the dotted line ends just above the text
  
  #Adding title to plot based on outbound value
  if(outbound.antpropx == 1){
    title(main = paste0("Proportion of outbound ants (mean window size ", winsize.antpropx, "s every ", winspace.antpropx, "s)"), ylab = "Proportion of outbound ants")
  }else if(outbound.antpropx == 0){
    title(main = paste0("Proportion of inbound ants (mean window size ", winsize.antpropx, "s every ", winspace.antpropx, "s)"), ylab = "Proportion of inbound ants")
  }else if(outbound.antpropx == 2){
    title(main = paste0("Proportion of inbound+outbound ants (mean window size ", winsize.antpropx, "s every ", winspace.antpropx, "s)"), ylab = "Proportion of inbound+outbound ants")
  }else if(outbound.antpropx == -1){
    title(main = paste0("Proportion of ants with no x-motion (mean window size ", winsize.antpropx, "s every ", winspace.antpropx, "s)"), ylab = "Proportion of ants with no x-motion")
  }
  
  #propseq <- seq(mintimef.antpropx, maxtimef.antpropx, winspacef.antpropx)
  #propxmeans <- matrix(data = NA, nrow = length(propseq), ncol = 2)
  #propxmeans[,1] <- propseq
  
  
  #Code for placing average points and lines; if/else if/else for when the window would go below 0, above maxtimef, or otherwise is in the middle respectively
  for(i in seq(from = mintimef.antpropx, to = maxtimef.antpropx, by = winspacef.antpropx)){       
    if((i - winsizef.antpropx) < 1){
      points(x = i, y = mean((rprops[(1):(i+winsizef.antpropx)])), pch = 18, col = "gray26")
      if((i-winspacef.antpropx+winsizef.antpropx) > 0){ #if statement to avoid erroring on first segment with a sub-0 index
        segments(i - winspacef.antpropx, mean((rprops[(1):(i-winspacef.antpropx+winsizef.antpropx)])), 
                 i, mean((rprops[(1):(i+winsizef.antpropx)])), col = "gray26")
      }
    }else if((i + winsizef.antpropx) > maxtimef.antpropx){
      points(x = i, y = mean((rprops[(i-winsizef.antpropx):(maxtimef.antpropx)])), pch = 18, col = "gray26")
      segments(i - winspacef.antpropx, mean((rprops[(i-winspacef.antpropx-winsizef.antpropx):(maxtimef.antpropx)])), 
               i, mean((rprops[(i-winsizef.antpropx):(maxtimef.antpropx)])), col = "gray26")
    }else{
      points(x = i, y = mean((rprops[(i-winsizef.antpropx):(i+winsizef.antpropx)])), pch = 18, col = "gray26")
      if((i-winspacef.antpropx-winsizef.antpropx) > 0){ #if statement to avoid erroring on segments outside the index
        segments(i - winspacef.antpropx, mean((rprops[(i-winspacef.antpropx-winsizef.antpropx):(i-winspacef.antpropx+winsizef.antpropx)])), 
                 i, mean((rprops[(i-winsizef.antpropx):(i+winsizef.antpropx)])), col = "gray26")
      }
    }
  }
  
  #Add and label dotted lines for time
  for(i in seq(from = 0, to = maxtimef.antpropx, by = lspacef.antpropx)){      
    lines(c(i,i), c(dims.antpropx[4],dims.antpropx[6]), type = "l", lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))
    text(i, dims.antpropx[5], labels = paste0(i / 300, "m"), col = rgb(0,0,0, alpha = 0.45))
  }
  
  #Add mean line
  meanline <- mean(rprops[mintimef.antpropx:maxtimef.antpropx], na.rm = TRUE)                                  #get the mean line value
  print(meanline)
  lines(c(dims.antpropx[1],maxtimef.antpropx), c(meanline, meanline), type = "l", lty = 3)       #draw the mean line
  
  #Add legend
  if(legloc.antpropx == 1){
    legend("topleft", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antpropx/binno.antpropx)),paste(floor(antmax.antpropx/binno.antpropx*((2:binno.antpropx)-1))+1, "to", floor(antmax.antpropx/binno.antpropx*(2:binno.antpropx)))),
           fill = 1:binno.antpropx)
  }else if(legloc.antpropx == 2){
    legend("topright", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antpropx/binno.antpropx)),paste(floor(antmax.antpropx/binno.antpropx*((2:binno.antpropx)-1))+1, "to", floor(antmax.antpropx/binno.antpropx*(2:binno.antpropx)))),
           fill = 1:binno.antpropx)
  }else if(legloc.antpropx == 3){
    legend("bottomright", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antpropx/binno.antpropx)),paste(floor(antmax.antpropx/binno.antpropx*((2:binno.antpropx)-1))+1, "to", floor(antmax.antpropx/binno.antpropx*(2:binno.antpropx)))),
           fill = 1:binno.antpropx)
  }else if(legloc.antpropx == 4){
    legend("bottomleft", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.antpropx/binno.antpropx)),paste(floor(antmax.antpropx/binno.antpropx*((2:binno.antpropx)-1))+1, "to", floor(antmax.antpropx/binno.antpropx*(2:binno.antpropx)))),
           fill = 1:binno.antpropx)
  }else{
    print("No legend added; choose 1-4 for topleft, topright, bottomright, or bottomleft.")
  }
  
}


#NET DIRECTION OF Y-MOVEMENT (needs updating)
#trajs.antnety is the data
#winspace.antnety is the "window spacing" for how far apart each averaged point should be
#winsize.antnety is the "window size" for averaging in seconds
#binno.antnety is the "bin number" for how many categories for coloring purposes
#lspace.antnety is the "line spacing" for graphical purposes (3000 frames = 10 minutes)

#Wrapper function for Antnety.f
Antnety <- function(dat, winspace = 10, winsize = 10, 
                    mintime = 0, maxtime,
                    binno = 6, lspace = 3000){
  Antnety.f(dat,winspace,winsize,mintime,maxtime,binno,lspace)
}

Antnety.f <- function(trajs.antnety, winspace.antnety, winsize.antnety, 
                      mintime.antnety, maxtime.antnety, 
                      binno.antnety, lspace.antnety){
  
  if(missing(maxtime.antnety)){
    maxtime.antnety <- dim(trajs.antnety)[1]                               #Ending frame found if not given as an argument
  }
  antmax.antnety <- max(rowSums(!is.na(trajs.antnety[,,2])))               #Greatest number of trajectories in region during any frame
  
  winspacef.antnety <- winspace.antnety*5                                  #Converting sampling spacing from seconds to frames
  winsizef.antnety <- round(winsize.antnety / 2 * 5)                       #converting second window size to frames and halving in prep for extending to either side
  
  plot(x = mintime.antnety:maxtime.antnety, y = rowSums(trajs.antnety[mintime.antnety:maxtime.antnety,,2], na.rm = TRUE)[1:length(mintime.antnety:maxtime.antnety)], 
       main = paste0("Net y-movement of ants (mean window size ", winsize.antnety, "s every ", winspace.antnety, "s)"), xlab = "Frame Number", ylab = "Sum of the x-component of velocities (cm/sec)", 
       col = floor( (rowSums(!is.na(trajs.antnety[mintime.antnety:maxtime.antnety,,2]))/antmax.antnety*binno.antnety)+1 ))
  dims.antnety <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) #Values for time lines below; dims[1:4] are size of the plot, dims[5] is calculating where to put the text just above the bottom of the frame, dims[6] is where the dotted line ends just above the text
  
  #Add x-axis and mean line
  lines(c(dims.antnety[1],maxtime.antnety), c(0,0), type = "l", lty = 3)    
  lines(c(dims.antnety[1],maxtime.antnety), c(mean(trajs.antnety[mintime.antnety:maxtime.antnety,,2], na.rm = TRUE),mean(trajs.antnety[mintime.antnety:maxtime.antnety,,2], na.rm = TRUE)), type = "l", lty = 3)    
  
  #Code for placing average points and lines; if/else if/else for when the window would go below 0, above maxtime, or otherwise is in the middle respectively
  for(i in seq(from = mintime.antnety, to = maxtime.antnety, by = winspacef.antnety)){       
    if((i - winsizef.antnety) < 1){
      points(x = i, y = mean(rowSums(trajs.antnety[(1):(i+winsizef.antnety),,2], na.rm = TRUE)), pch = 18, col = "gray26")
      if(i != 0){ #if statement to avoid erroring on first segment with a 0 index
        segments(i - winspacef.antnety, mean(rowSums(trajs.antnety[(1):(i-winspacef.antnety+winsizef.antnety),,2], na.rm = TRUE)), 
                 i, mean(rowSums(trajs.antnety[(1):(i+winsizef.antnety),,2], na.rm = TRUE)), col = "gray26")
      }
    }else if((i + winsizef.antnety) > maxtime.antnety){
      points(x = i, y = mean(rowSums(trajs.antnety[(i-winsizef.antnety):(maxtime.antnety),,2], na.rm = TRUE)), pch = 18, col = "gray26")
      segments(i - winspacef.antnety, mean(rowSums(trajs.antnety[(i-winspacef.antnety-winsizef.antnety):(maxtime.antnety),,2], na.rm = TRUE)), 
               i, mean(rowSums(trajs.antnety[(i-winsizef.antnety):(maxtime.antnety),,2], na.rm = TRUE)), col = "gray26")
    }else{
      points(x = i, y = mean(rowSums(trajs.antnety[(i-winsizef.antnety):(i+winsizef.antnety),,2], na.rm = TRUE)), pch = 18, col = "gray26")
      if((i-winspacef.antnety-winsizef.antnety) > 0){ #if statement to avoid erroring on segments outside the index
        segments(i - winspacef.antnety, mean(rowSums(trajs.antnety[(i-winspacef.antnety-winsizef.antnety):(i-winspacef.antnety+winsizef.antnety),,2], na.rm = TRUE)), 
                 i, mean(rowSums(trajs.antnety[(i-winsizef.antnety):(i+winsizef.antnety),,2], na.rm = TRUE)), col = "gray26")
      }
    }
  }
  
  #Adds and labels dotted lines every 10 minutes
  for(i in seq(from = 0, to = maxtime.antnety, by = lspace.antnety)){      
    lines(c(i,i), c(dims.antnety[4],dims.antnety[6]), type = "l", lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))
    text(i, dims.antnety[5], labels = paste0(i / 300, "m"), col = rgb(0,0,0, alpha = 0.45))
  }
  
  #Adds legend
  legend("topleft", inset = 0.03, title = "Ants in region",
         legend=c(paste("0 to", floor(antmax.antnety/binno.antnety)),paste(floor(antmax.antnety/binno.antnety*((2:binno.antnety)-1))+1, "to", floor(antmax.antnety/binno.antnety*(2:binno.antnety)))),
         fill = 1:binno.antnety)
}


