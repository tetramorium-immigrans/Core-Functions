#List of functions
#Spacevecs: the spatial representation of the velocity vectors
#Mspacevecs: multiple spactial representation of velocity vectors

#Options to make the colors of the points correspond with heading (red left, blue right, black for little/no?) and add or remove axis labels

Spacevecs <- function(dat1 = locs, dat2 = trajs,
                      stime = 0,
                      border = 1.6, ascale = 1){
  Spacevecs.f(dat1, dat2, stime, border, ascale)
}

Spacevecs.f <- function(dat1.spacevecs, dat2.spacevecs, 
                        time.spacevecs,
                        border.spacevecs, ascale.spacevecs){
  sframe.spacevecs <- round(time.spacevecs * 300)                 #Converting minutes to frames
  plot(NULL, xlim=c(min(dat1.spacevecs[,,1], na.rm = TRUE)-border.spacevecs, max(dat1.spacevecs[,,1], na.rm = TRUE)+border.spacevecs), ylim=c(min(dat1.spacevecs[,,2], na.rm = TRUE)-border.spacevecs,max(dat1.spacevecs[,,2], na.rm = TRUE)+border.spacevecs), 
       main = paste0(time.spacevecs, " minutes (frame ", sframe.spacevecs, ")"), xlab = "x position (cm)", ylab = "y position (cm)")
  points(dat1.spacevecs[sframe.spacevecs,,1],dat1.spacevecs[sframe.spacevecs,,2])
  options(warn = -1)                                              #Turning off warnings due to arrow function misbehaving in zero-length vectors
  arrows(dat1.spacevecs[sframe.spacevecs,,1],dat1.spacevecs[sframe.spacevecs,,2], 
         dat1.spacevecs[sframe.spacevecs,,1]+dat2.spacevecs[sframe.spacevecs,,1]*ascale.spacevecs, 
         dat1.spacevecs[sframe.spacevecs,,2]+dat2.spacevecs[sframe.spacevecs,,2]*ascale.spacevecs,length = 0.05)
  options(warn = 0)
}


Mspacevecs <- function(dat1 = locs, dat2 = trajs,
                       times = 0,
                       border = 1.6, ascale = 1){
  Mspacevecs.f(dat1, dat2, times, border, ascale)
}

Mspacevecs.f <- function(dat1.mspacevecs, dat2.mspacevecs,
                         times.mspacevecs,
                         border.mspacevecs, ascale.mspacevecs){
  
  nf <- par(no.readonly=TRUE)                                #Save graphical parameters
  par(mfrow = c(floor(sqrt(length(times.mspacevecs))), ceiling(sqrt(length(times.mspacevecs)))))
  sframe.mspacevecs <- round(times.mspacevecs * 300)                 #Converting minutes to frames
  
  for(i in 1:length(times.mspacevecs)){
    Spacevecs(dat1.mspacevecs, dat2.mspacevecs, times.mspacevecs[i], border.mspacevecs, ascale.mspacevecs)
  }

  par(nf)                                                    #Set graphical parameters back to what they were
}