##rm0
#Removes trajectories with a median speed of 0 from the dataset (typically dead
#ants or inconsequential objects).
#Expects a (time x trajectory x 4) trajs-style data frame

rm0 <- function(trajs){
  trajsm <- colMedians(trajs[,,4], na.rm = TRUE)
  trajs <- trajs[,!trajsm == 0,]
  return(trajs)
}