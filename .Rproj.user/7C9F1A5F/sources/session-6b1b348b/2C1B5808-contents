#Mass functions

Antno.mass <- function(dat = masstrajs, xdim, ydim, prop = FALSE,
                       binno = 6, lspace = 3000, legloc = 1){
  
  if(missing(xdim)){
    xdim <- ceiling(sqrt(length(dat)))
  }
  
  if(missing(ydim)){
    ydim <- ceiling(sqrt(length(dat)))
  }
  
  nf <- par()
  par(mfrow = c(ydim,xdim))
  
  for(i in 1:length(dat)){
    if(i > (xdim * ydim)){break}                                          #stops loop if it goes beyond the designated grid size
    trajtemp <- Process.single(dat[i])
    Antno(trajtemp, prop, binno, lspace, legloc)
  }
  
  par(mfrow = c(1,1))
  
}