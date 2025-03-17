##THESIS SCRIPTS - DISTANCE COMPARISONS##


##SPEED##

massspeed.20 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.20, graph = FALSE, datreturn = TRUE)))
massspeedvar.20 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.20, graph = FALSE, datreturn = TRUE, fn = "vari")))

massspeed.300 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.300, graph = FALSE, datreturn = TRUE)))
massspeedvar.300 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.300, graph = FALSE, datreturn = TRUE, fn = "vari")))

mean(massspeed.20)
#[1] 0.5178058
mean(massspeed.30)
#[1] 0.6024784
var(massspeed.20)
#[1] 0.03233024
var(massspeed.30)
#[1] 0.04077036

mean(massspeedvar.20)
#[1] 0.1922439
mean(massspeedvar.30)
#[1] 0.1832995
var(massspeedvar.20)
#[1] 0.02188154
var(massspeedvar.30)
#[1] 1.329981

max(massspeed.20)
#[1] 1.329981
max(massspeed.30)
#[1] 1.381232

t.test(massspeed.20)
#   0.5161180 0.5194937
t.test(massspeed.30)
#   0.5996793 0.6052774
t.test(massspeedvar.20)
#   0.1908554 0.1936325
t.test(massspeedvar.30)
#   0.1812874 0.1853116

#Histogram - Mean

p1 <- hist(massspeed.20, plot = FALSE)
p2 <- hist(massspeed.300, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of mean trajectory speeds by distance", xlab = "Speed (cm/s)")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of mean trajectory speeds by distance", xlab = "Speed (cm/s)")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.2, legend = 
         c(paste("A Distance, n =", length(massspeed.20)),
           paste("C Distance, n =", length(massspeed.300)),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))

#Histogram - Variance

p1 <- hist(massspeedvar.20, plot = FALSE, breaks = seq(from = 0, to = 2.5, by = 0.1))
p2 <- hist(massspeedvar.300, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of variance of trajectory speeds by distance", xlab = "Speed^2 (cm/s)^2")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of variance of trajectory speeds by distance", xlab = "Speed^2 (cm/s)^2")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.3, legend = 
         c(paste("A Distance, n =", length(massspeed.20)),
           paste("C Distance, n =", length(massspeed.300)),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##X VELOCITY##

massx.20 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.20, graph = FALSE, datreturn = TRUE)))
massxvar.20 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.20, graph = FALSE, datreturn = TRUE, fn = "vari")))

massx.300 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.300, graph = FALSE, datreturn = TRUE)))
massxvar.300 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.300, graph = FALSE, datreturn = TRUE, fn = "vari")))

mean(massx.20)
#[1] -0.001521709
mean(massx.30)
#[1] -1.34e-05
var(massx.20)
#[1] 0.02506048
var(massx.30)
#[1] 0.04557609

mean(massxvar.20)
#[1] 0.2621196
mean(massxvar.30)
#[1] 0.3168624
var(massxvar.20)
#[1] 0.03390569
var(massxvar.30)
#[1] 0.04524727

max(massx.20)
#[1] 1.312993
max(massx.30)
#[1] 0.7917934

t.test(massx.20)
#   -3.007701e-03 -3.571653e-05
t.test(massx.30)
#   -0.002972802  0.002946002
t.test(massxvar.20)
#   0.2603911 0.2638481
t.test(massxvar.30)
#   0.3139137 0.3198111

#Histogram graphing - Distance x-velocity

p1 <- hist(massx.20, plot = FALSE)
p2 <- hist(massx.300, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of mean trajectory x-velocities by distance", xlab = "Speed (cm/s)")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of mean trajectory x-velocities by distance", xlab = "Speed (cm/s)")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.2, legend = 
         c(paste("A Distance, n =", length(massx.20)),
           paste("C Distance, n =", length(massx.300)),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


#Histogram graphing - Distance x-velocity

p1 <- hist(massxvar.20, plot = FALSE, breaks = seq(from = 0, to = 2.5, by = 0.1))
p2 <- hist(massxvar.300, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of variance of trajectory x-velocities by distance", xlab = "Speed^2 (cm/s)^2")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of variance of trajectory x-velocities by distance", xlab = "Speed^2 (cm/s)^2")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.3, legend = 
         c(paste("A Distance, n =", length(massx.20)),
           paste("C Distance, n =", length(massx.300)),
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
         c(paste("A Quality, n =", length(massp.a)),
           paste("C Quality, n =", length(massp.c)),
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

legend("topright", inset = 0.2, legend = 
         c(paste("A Quality, n =", length(masspvar.a)),
           paste("C Quality, n =", length(masspvar.c)),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))

##TORTUOSITY##