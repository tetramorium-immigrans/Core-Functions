#Extract
#Solves a probem where referencing the index of a list of lists is giving me issues, so have to unlist and reform the data so it works
#UNUSED

Extract <- function(file.ex, dat.ex = masstrajs){
  datnum <- which(names(dat.ex) == file.ex)
  if(datnum == 0){
    print("Invalid file name")
    break
  }else{
    return(array(unlist(dat.ex$Data[datnum]), dim = as.vector(sapply(dat.ex$Data[datnum], dim))))
  }
  
}

