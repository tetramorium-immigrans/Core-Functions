##Legendno
#Adds a legend to the graph

#Type is the legend type.  1 = number of ants in region.
#binno (bin number) is the number of colors used to code for relative numbers on the trail
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#antmax is the maximum number of ants being examined (functions may specify this to be from all the data or merely a subset)

Legendno <- function(type = 1, binno = 6, legloc = 1, antmax){
  if(legloc == 1){
    legendout <- legend("topleft", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax/binno)),paste(floor(antmax/binno*((2:binno)-1))+1, "to", floor(antmax/binno*(2:binno)))),
           fill = 1:binno)
  }else if(legloc == 2){
    legend("topright", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax/binno)),paste(floor(antmax/binno*((2:binno)-1))+1, "to", floor(antmax/binno*(2:binno)))),
           fill = 1:binno)
  }else if(legloc == 3){
    legendout <- legend("bottomright", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax/binno)),paste(floor(antmax/binno*((2:binno)-1))+1, "to", floor(antmax/binno*(2:binno)))),
           fill = 1:binno)
  }else if(legloc == 4){
    legendout <- legend("bottomleft", inset = 0.03, title = "Ants in region",
           legend=c(paste("0 to", floor(antmax/binno)),paste(floor(antmax/binno*((2:binno)-1))+1, "to", floor(antmax/binno*(2:binno)))),
           fill = 1:binno)
  }else{
    #print("No legend added; choose 1-4 for topleft, topright, bottomright, or bottomleft.")
  }
  #return(legendout)
}

