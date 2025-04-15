##Direction
#Function for restricting data set to only outbound or inbound ants
#(Outbound/inbound is determined by the sum of the x-components of velocity being either negative (outbound) or inbound (positive))

#trajs = data list
#output = whether it should filter for outbound (1) or inbound (2)

Direction <- function(trajs, output = 0){
  #trajreturn <- list()
  trajreturn <- trajs
  
  if(output == 0){
    return(trajreturn)
  }else if(output == 1){
    filtered <- rowSums(trajs$xvel) < 0
  }else if(output == 2){
    filtered <- rowSums(trajs$xvel) > 0
  }
  
  #Wanted to do this in a neater way, but was running into subsetting issues I couldn't solve
  if("x" %in% names(trajs)){trajreturn$x <- trajs$x[filtered,]}
  if("y" %in% names(trajs)){trajreturn$y <- trajs$y[filtered,]}
  if("speed" %in% names(trajs)){trajreturn$speed <- trajs$speed[filtered,]}
  if("heading" %in% names(trajs)){trajreturn$heading <- trajs$heading[filtered,]}
  if("xvel" %in% names(trajs)){trajreturn$xvel <- trajs$xvel[filtered,]}
  if("yvel" %in% names(trajs)){trajreturn$yvel <- trajs$yvel[filtered,]}
  if("pathlength" %in% names(trajs)){trajreturn$pathlength <- trajs$pathlength[filtered,]}
  if("distance" %in% names(trajs)){trajreturn$distance <- trajs$distance[filtered,]}
  if("msd" %in% names(trajs)){trajreturn$msd <- trajs$msd[filtered,]}
  
  return(trajreturn)
  
}
