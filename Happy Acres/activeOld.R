#Active

#dat takes either trajs or locs
#rang (range) is a sequence of frames to check (usually passed as mintimef:maxtimef)

Active <- function(dat, rang){
  activetrajs <- unique(unlist(sapply(rang, FUN = function(x){which(!is.na(dat[x,,1]))})))
  return(activetrajs)
}

