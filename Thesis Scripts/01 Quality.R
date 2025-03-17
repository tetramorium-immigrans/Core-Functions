##THESIS SCRIPTS - QUALITY COMPARISONS##


##SPEED##

massspeed.a <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE)))
massspeedvar.a <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE, fn = "vari")))

massspeed.c <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE)))
massspeedvar.c <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE, fn = "vari")))

mean(massspeed.a)
# [1] 0.5600088
mean(massspeed.c)
# [1] 0.5352712
var(massspeed.a)
#[1] 0.03806622
var(massspeed.c)
#[1] 0.03539959

mean(massspeedvar.a)
# [1] 0.2075509
mean(massspeedvar.c)
# [1] 0.1787862
var(massspeedvar.a)
#[1] 0.02249649
var(massspeedvar.c)
#[1] 0.02083556

t.test(massspeed.a)
#   0.5575160 0.5625016
t.test(massspeed.c)
#   0.5334287 0.5371138
t.test(massspeedvar.a)
#   0.2056345 0.2094672
t.test(massspeedvar.c)
#   0.1773726 0.1801998

max(massspeed.a)
# [1] 1.381232
max(massspeed.c)
# [1] 1.329981

#Histogram - Mean

p1 <- hist(massspeed.a, plot = FALSE)
p2 <- hist(massspeed.c, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of mean trajectory speeds by quality", xlab = "Speed (cm/s)")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of mean trajectory speeds by quality", xlab = "Speed (cm/s)")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.15, legend = 
         c(paste("A Quality, n =", length(massspeed.a), "trajectories"),
           paste("C Quality, n =", length(massspeed.c), "trajectories"),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))

#Histogram - Variance

p1 <- hist(massspeedvar.a, plot = FALSE, breaks = seq(from = 0, to = 2.5, by = 0.1))
p2 <- hist(massspeedvar.c, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of variance of trajectory speeds by quality", xlab = "(cm/s)^2")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of variance of trajectory speeds by quality", xlab = "(cm/s)^2")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.3, legend = 
         c(paste("A Quality, n =", length(massspeed.a), "trajectories"),
           paste("C Quality, n =", length(massspeed.c), "trajectories"),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##X VELOCITY##

massx.a <- unname(unlist(Mass.function(antvel, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE)))
massxvar.a <- unname(unlist(Mass.function(antvel, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE, fn = "vari")))

massx.c <- unname(unlist(Mass.function(antvel, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE)))
massxvar.c <- unname(unlist(Mass.function(antvel, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE, fn = "vari")))

mean(massx.a)
# [1] -0.001557435
mean(massx.c)
# [1] -0.0007479007
var(massx.a)
#[1] 0.02768127
var(massx.c)
#[1] 0.03376059

mean(massxvar.a)
# [1] 0.3090913
mean(massxvar.c)
# [1] 0.2618449
var(massxvar.a)
#[1] 0.04528812
var(massxvar.c)
#[1] 0.03307815

t.test(massx.a)
#   -0.0036831563  0.0005682866
t.test(massx.c)
#   -0.002547298  0.001051496
t.test(massxvar.a)
#   0.3063723 0.3118102
t.test(massxvar.c)
#   0.2600637 0.2636260

max(massx.a)
# [1] 0.7805237
max(massx.c)
# [1] 1.312993

#Histogram - Mean

p1 <- hist(massx.a, plot = FALSE)
p2 <- hist(massx.c, plot = FALSE, breaks = seq(from = -1.5, to = 1.5, by = 0.1))

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of mean trajectory x-velocities by quality", xlab = "Speed (cm/s)")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of mean trajectory x-velocities by quality", xlab = "Speed (cm/s)")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.15, legend = 
         c(paste("A Quality, n =", length(massx.a), "trajectories"),
           paste("C Quality, n =", length(massx.c), "trajectories"),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


#Histogram - Variance

p1 <- hist(massxvar.a, plot = FALSE)
p2 <- hist(massxvar.c, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of variance of trajectory x-velocities by quality", xlab = "(cm/s)^2")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of variance of trajectory x-velocities by quality", xlab = "(cm/s)^2")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.3, legend = 
         c(paste("A Quality, n =", length(massx.a), "trajectories"),
           paste("C Quality, n =", length(massx.c), "trajectories"),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##PERIODICITY##

massp.a <- unname(unlist(Mass.function(antphase, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE)))
masspvar.a <- unname(unlist(Mass.function(antphase, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE, fn = "vari")))

massp.c <- unname(unlist(Mass.function(antphase, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE)))
masspvar.c <- unname(unlist(Mass.function(antphase, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE, fn = "vari")))

mean(massp.a)
# [1] 
mean(massp.c)
# [1] 
var(massp.a)
#[1] 
var(massp.c)
#[1] 

mean(masspvar.a)
# [1] 
mean(masspvar.c)
# [1] 
var(masspvar.a)
#[1] 
var(masspvar.c)
#[1] 

t.test(massp.a)
#   
t.test(massp.c)
#   
t.test(masspvar.a)
#   
t.test(masspvar.c)
#   

max(massp.a)
# [1] 
max(massp.c)
# [1] 

#Histogram - Mean

p1 <- hist(massp.a, plot = FALSE)
p2 <- hist(massp.c, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of time between maxima by quality", xlab = "Time (frames)")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of time between maxima by quality", xlab = "Time (frames)")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.2, legend = 
         c(paste("A Quality, n =", length(massp.a), "peak differences"),
           paste("C Quality, n =", length(massp.c), "peak differences"),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


#Histogram - Variance

p1 <- hist(masspvar.a, plot = FALSE)
p2 <- hist(masspvar.c, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of variance of time between maxima by quality", xlab = "(frames)^2")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of variance of time between maxima by quality", xlab = "(frames)^2")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.15, legend = 
         c(paste("A Quality, n =", length(masspvar.a), "videos"),
           paste("C Quality, n =", length(masspvar.c), "videos"),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##TORTUOSITY##