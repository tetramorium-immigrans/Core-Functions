##Massfunction


Mass.function <- function(fun, dat.mf = masstrajs, datlocs.mf = masslocs,
                         xdim, ydim,
                         mintime.mf = 0, maxtime.mf, frate.mf = 5, titles.mf, ...){
  
  if(missing(xdim)){
    xdim <- ceiling(sqrt(length(dat.mf$Data)))
  }
  
  if(missing(ydim)){
    ydim <- ceiling(sqrt(length(dat.mf$Data)))
  }
  
  if(missing(titles.mf)){
    titles.mf <- ''
  }else{
    titles.mf <- paste0(titles.mf, ',')
  }
  
  nf <- par()
  par(mfrow = c(ydim,xdim))
  
  for(i in 1:length(dat.mf$Data)){
   if(i > (xdim * ydim)){
     print("Filled designated grid size; stopping function")
     break                                          #stops loop if it goes beyond the designated grid size
   }

   fun(dat = Extract(dat.mf$Names[i], dat.ex = dat.mf),
       datlocs = Extract(datlocs.mf$Names[i], dat.ex = datlocs.mf),
       # dat = Extract(paste0("'",dat.mf$Names[i],"'"), dat.ex = dat.mf),
       # datlocs = Extract(paste0("'",datlocs.mf$Names[i],"'"), dat.ex = datlocs.mf),
       mintime = mintime.mf, maxtime = maxtime.mf, frate = frate.mf, titles = paste(titles.mf, dat.mf$Names[i]), ...)
   
   # fun.mf(dat = array(unlist(dat.mf$Data[i]), dim = as.vector(sapply(dat.mf$Data[i], dim))),
   #     datlocs = array(unlist(datlocs.mf$Data[i]), dim = as.vector(sapply(datlocs.mf$Data[i], dim))),
   #     mintime = mintime.mf, maxtime = maxtime.mf, frate = frate.mf, titles = paste(titles.mf, dat.mf$Names[i]), ...)
   
  }
  
  par(mfrow = c(1,1))
  
}

