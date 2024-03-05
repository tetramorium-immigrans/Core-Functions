#Extract
#Extracts a trajs- or locs-form file from a masstrajs- or masslocs-form file

Extract <- function(file.ex, dat.ex = masstrajs){
  datnum <- which(dat.ex$Names == file.ex)
  if(datnum == 0){
    print("Invalid file name")
    break
  }else{
    return(array(unlist(dat.ex$Data[datnum]), dim = as.vector(sapply(dat.ex$Data[datnum], dim))))
  }
  
}

