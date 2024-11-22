#Antlocs
#Graph a frame or set of frames with the ants and their associated velocity vectors
#Dat is the data

#mtime (minute-time) is the frame to display (entered in minutes)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s); anywhere you see "frate*60" is converting frames to minutes or back

#border is adding a graphical edge around the dots so the arrows don't leave the plot
#ascale (arrow scale) adjusts the size of the vector arrows
#plotitle (in Antlocs.f only) passes the name of the file being graphed so it can be read from the wrapper function

#Options to make the colors of the points correspond with heading (red left, blue right, black for little/no?) and add or remove axis labels

Antlocs <- function(dat = trajs, 
                    mtime = 0, frate = 5, 
                    border = 1.6, ascale = 2){
  Antlocs.f(dat, mtime, frate, border, ascale, deparse(substitute(dat)) )
}

Antlocs.f <- function(dat.antlocs,
                      mtime.antlocs, frate.antlocs,
                      border.antlocs, ascale.antlocs,
                      plotitle.antlocs){
  ftime.antlocs <- round(mtime.antlocs * frate.antlocs * 60)               #Converting minutes to frames
  
  #Paring down the data to the active trajectories in a given frame
  #(all filtered by xtemp > 0 just to avoid any length mismatch shenanigans)
  xtemp <- dat.antlocs$x[,ftime.antlocs]
  
  ytemp <- dat.antlocs$y[,ftime.antlocs]
  ytemp <- ytemp[xtemp > 0]
  
  xvtemp <- dat.antlocs$xvel[,ftime.antlocs]
  xvtemp <- xvtemp[xtemp > 0]
  
  yvtemp <- dat.antlocs$yvel[,ftime.antlocs]
  yvtemp <- yvtemp[xtemp > 0]
  
  sindex <- dat.antlocs$speed[,ftime.antlocs]
  sindex <- sindex[xtemp > 0]
  
  xtemp <- xtemp[xtemp > 0]
  
  #Plotting function
  plot(x = xtemp, y = ytemp, 
       xlim = c(min(xtemp) - border.antlocs, max(xtemp) + border.antlocs),
       ylim = c(min(ytemp) - border.antlocs, max(ytemp) + border.antlocs),
       main = paste0(plotitle.antlocs, " at ", mtime.antlocs, "m")
       )

  arrows(xtemp[sindex > 0.01], ytemp[sindex > 0.01],
         xtemp[sindex > 0.01] + xvtemp[sindex > 0.01]*ascale.antlocs, ytemp[sindex > 0.01] + yvtemp[sindex > 0.01]*ascale.antlocs)
}

