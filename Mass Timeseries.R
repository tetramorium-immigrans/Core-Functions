#Mass functions

Antno.mass <- function(dat = masstrajs, xdim, ydim, prop = FALSE,
                       binno = 6, lspace = 3000, legloc = 1){
  
  if(missing(xdim)){
    xdim <- ceiling(sqrt(length(dat$Data)))
  }
  
  if(missing(ydim)){
    ydim <- ceiling(sqrt(length(dat$Data)))
  }
  
  nf <- par()
  par(mfrow = c(ydim,xdim))
  
  for(i in 1:length(dat$Data)){
    if(i > (xdim * ydim)){break}                                          #stops loop if it goes beyond the designated grid size
    trajtemp <- Process.single(dat$Data[i])
    Antno(trajtemp, prop, binno, lspace, legloc, titles = dat$Names[i])
    
    #Antno(data.frame(dat[i]), prop, binno, lspace, legloc)
  }
  
  par(mfrow = c(1,1))
  
}

Antmeans.mass <- function(dat = masstrajs, xdim, ydim,
                       binno = 6, lspace = 3000, legloc = 1){
  
  if(missing(xdim)){
    xdim <- ceiling(sqrt(length(dat$Data)))
  }
  
  if(missing(ydim)){
    ydim <- ceiling(sqrt(length(dat$Data)))
  }
  
  nf <- par()
  par(mfrow = c(ydim,xdim))
  
  for(i in 1:length(dat$Data)){
    if(i > (xdim * ydim)){break}                                          #stops loop if it goes beyond the designated grid size
    trajtemp <- Process.single(dat$Data[i])
    Antmeans(trajtemp, binno, lspace, legloc, titles = dat$Names[i])
    
  }
  
  par(mfrow = c(1,1))
  
}

Antnetx.mass <- function(dat = masstrajs, xdim, ydim, winspace = 20, winsize = 20,
                         mintime = 0, maxtime,
                         binno = 6, lspace = 3000, legloc = 1, keepscale = FALSE){
  
  if(missing(xdim)){
    xdim <- ceiling(sqrt(length(dat$Data)))
  }
  
  if(missing(ydim)){
    ydim <- ceiling(sqrt(length(dat$Data)))
  }
  
  nf <- par()
  par(mfrow = c(ydim,xdim))
  
  for(i in 1:length(dat$Data)){
    if(i > (xdim * ydim)){break}                                          #stops loop if it goes beyond the designated grid size
    trajtemp <- Process.single(dat$Data[i])
    Antnetx(dat = trajtemp, winspace = winspace, winsize = winsize,
            mintime = mintime, maxtime = maxtime,
            binno = binno, lspace = lspace, legloc = legloc, keepscale = keepscale)
    
  }
  
  par(mfrow = c(1,1))
  
}

Antpropx.mass <- function(dat = masstrajs, xdim, ydim, winspace = 20, winsize = 20,
                          mintime = 0, maxtime,
                          binno = 6, lspace = 3000, legloc = 1){
  
  if(missing(xdim)){
    xdim <- ceiling(sqrt(length(dat$Data)))
  }
  
  if(missing(ydim)){
    ydim <- ceiling(sqrt(length(dat$Data)))
  }
  
  nf <- par()
  par(mfrow = c(ydim,xdim))
  
  for(i in 1:length(dat$Data)){
    if(i > (xdim * ydim)){break}                                          #stops loop if it goes beyond the designated grid size
    trajtemp <- Process.single(dat$Data[i])
    Antpropx(dat = trajtemp, winspace = winspace, winsize = winsize,
             mintime = mintime, maxtime = maxtime,
             binno = binno, lspace = lspace, legloc = legloc)
    
  }
  
  par(mfrow = c(1,1))
  
}