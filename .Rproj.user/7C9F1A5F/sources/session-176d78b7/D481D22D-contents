##ANDHEAD
#Change in heading variance with time

#dat is the data (the file name without suffix; must begin with a letter)
#winspace and winsize are the "window spacing" and "window size" for averaging function in seconds
#mintime and maxtime are the start and end times for the graph in minutes 
#binno is the "bin number" for how many categories for coloring purposes
#lspace is the "line spacing" for graphical purposes in minutes
#legloc is the "legend location" (1 = topleft, 2 = topright, 3 = bottomright, 4 = bottomleft, any other number for no legend)
#keepscale is whether to scale the y-axis to the whole data set or just the selected region


Anthead <- function(dat = trajs, winspace = 20, winsize = 20, 
                    mintime = 0, maxtime,
                    binno = 6, lspace = 10, legloc = 1, keepscale = FALSE){
  Anthead.f(dat,winspace,winsize,mintime,maxtime,binno,lspace,legloc,keepscale)
}


Anthead.f <- function(trajs.anthead, winspace.anthead, winsize.anthead, 
                      mintime.anthead, maxtime.anthead, 
                      binno.anthead, lspace.anthead, legloc.anthead, keepscale.anthead){
  
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.anthead <- max(rowSums(!is.na(trajs.anthead[,,1])))
  
  #Ending frame found if not given as an argument
  if(missing(maxtime.anthead)){
    maxtimef.anthead <- dim(trajs.anthead)[1]
  }else{
    maxtimef.anthead <- maxtime.anthead*300                                #Converting argument from minutes to frames
  }
  
  #Converting input arguments to frames
  winspacef.anthead <- winspace.anthead*5
  winsizef.anthead <- round(winsize.anthead * 5 / 2)                       #Halved since the value will be used to extend to either side of a point
  mintimef.anthead <- mintime.anthead*300
  lspacef.anthead <- lspace.anthead*300
  
  
  #Main plotting function
  if(keepscale.anthead == TRUE){                                          #If keeping the scale, find out the appropriate scale for the full data set then plot
    rsums <- rowSums(trajs.anthead[,,1], na.rm = TRUE)
    plot(x = mintimef.anthead:maxtimef.anthead, y = rsums[mintimef.anthead:maxtimef.anthead], 
         ylim = c(min(rsums), max(rsums)),
         main = paste0("Variance of ant heading (mean window size ", winsize.anthead, "s every ", winspace.anthead, "s)"), xlab = "Frame Number", ylab = "Sum of the x-component of velocities (cm/sec)", 
         col = floor( (rowSums(!is.na(trajs.anthead[mintimef.anthead:maxtimef.anthead,,1]))/antmax.anthead*binno.anthead)+1 ))
    dims.anthead <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) #Values for time lines below; dims[1:4] are size of the plot, dims[5] is calculating where to put the text just above the bottom of the frame, dims[6] is where the dotted line ends just above the text
    
  }else{
    plot(x = mintimef.anthead:maxtimef.anthead, y = rowVars(trajs.anthead[mintimef.anthead:maxtimef.anthead,,3], na.rm = TRUE)[1:length(mintimef.anthead:maxtimef.anthead)], 
         main = paste0("Variance of ant heading (mean window size ", winsize.anthead, "s every ", winspace.anthead, "s)"), xlab = "Frame Number", ylab = "Variance of ant headings", 
         col = floor( (rowSums(!is.na(trajs.anthead[mintimef.anthead:maxtimef.anthead,,1]))/antmax.anthead*binno.anthead)+1 ))
    dims.anthead <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) #Values for time lines below; dims[1:4] are size of the plot, dims[5] is calculating where to put the text just above the bottom of the frame, dims[6] is where the dotted line ends just above the text
  }
  
  
  #Add x-axis and mean variance line
  lines(c(dims.anthead[1],maxtimef.anthead), c(0,0), type = "l", lty = 3)                                    #draw x-axis
  
  meanline <- mean(rowVars(trajs.anthead[mintimef.anthead:maxtimef.anthead,,3], na.rm = TRUE), na.rm = TRUE) #get the mean variance line value
  lines(c(dims.anthead[1],maxtimef.anthead), c(meanline, meanline), type = "l", lty = 3)                     #draw the line

  #Code for placing average points and lines; if/else if/else for when the window would go below 0, above maxtimef, or otherwise is in the middle respectively
  for(i in seq(from = mintimef.anthead, to = maxtimef.anthead, by = winspacef.anthead)){       
    if((i - winsizef.anthead) < 1){
      points(x = i, y = mean(rowVars(trajs.anthead[(1):(i+winsizef.anthead),,3], na.rm = TRUE)), pch = 18, col = "gray26")
      if(i != 0){ #if statement to avoid erroring on first segment with a 0 index
        segments(i - winspacef.anthead, mean(rowVars(trajs.anthead[(1):(i-winspacef.anthead+winsizef.anthead),,3], na.rm = TRUE)), 
                 i, mean(rowVars(trajs.anthead[(1):(i+winsizef.anthead),,3], na.rm = TRUE)), col = "gray26")
      }
    }else if((i + winsizef.anthead) > maxtimef.anthead){
      points(x = i, y = mean(rowVars(trajs.anthead[(i-winsizef.anthead):(maxtimef.anthead),,3], na.rm = TRUE)), pch = 18, col = "gray26")
      segments(i - winspacef.anthead, mean(rowVars(trajs.anthead[(i-winspacef.anthead-winsizef.anthead):(maxtimef.anthead),,3], na.rm = TRUE)), 
               i, mean(rowVars(trajs.anthead[(i-winsizef.anthead):(maxtimef.anthead),,3], na.rm = TRUE)), col = "gray26")
    }else{
      points(x = i, y = mean(rowVars(trajs.anthead[(i-winsizef.anthead):(i+winsizef.anthead),,3], na.rm = TRUE)), pch = 18, col = "gray26")
      if((i-winspacef.anthead-winsizef.anthead) > 0){ #if statement to avoid erroring on segments outside the index
        segments(i - winspacef.anthead, mean(rowVars(trajs.anthead[(i-winspacef.anthead-winsizef.anthead):(i-winspacef.anthead+winsizef.anthead),,3], na.rm = TRUE)), 
                 i, mean(rowVars(trajs.anthead[(i-winsizef.anthead):(i+winsizef.anthead),,3], na.rm = TRUE)), col = "gray26")
      }
    }
  }
  
  #Add and label dotted lines for time
  for(i in seq(from = 0, to = maxtimef.anthead, by = lspacef.anthead)){      
    lines(c(i,i), c(dims.anthead[4],dims.anthead[6]), type = "l", lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))
    text(i, dims.anthead[5], labels = paste0(i / 300, "m"), col = rgb(0,0,0, alpha = 0.45))
  }
  
  #Add legend
  if(legloc.anthead == 1){
    legend("topleft", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.anthead/binno.anthead)),paste(floor(antmax.anthead/binno.anthead*((2:binno.anthead)-1))+1, "to", floor(antmax.anthead/binno.anthead*(2:binno.anthead)))),
           fill = 1:binno.anthead)
  }else if(legloc.anthead == 2){
    legend("topright", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.anthead/binno.anthead)),paste(floor(antmax.anthead/binno.anthead*((2:binno.anthead)-1))+1, "to", floor(antmax.anthead/binno.anthead*(2:binno.anthead)))),
           fill = 1:binno.anthead)
  }else if(legloc.anthead == 3){
    legend("bottomright", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.anthead/binno.anthead)),paste(floor(antmax.anthead/binno.anthead*((2:binno.anthead)-1))+1, "to", floor(antmax.anthead/binno.anthead*(2:binno.anthead)))),
           fill = 1:binno.anthead)
  }else if(legloc.anthead == 4){
    legend("bottomleft", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax.anthead/binno.anthead)),paste(floor(antmax.anthead/binno.anthead*((2:binno.anthead)-1))+1, "to", floor(antmax.anthead/binno.anthead*(2:binno.anthead)))),
           fill = 1:binno.anthead)
  }else{
    print("No legend added; choose 1-4 for topleft, topright, bottomright, or bottomleft.")
  }
  
}