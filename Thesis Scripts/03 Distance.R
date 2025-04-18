##THESIS SCRIPTS - DISTANCE COMPARISONS##


##(a) MEAN OF SPEED##

massspeed.20 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.20, graph = FALSE, datreturn = TRUE)))
massspeed.30 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.30, graph = FALSE, datreturn = TRUE)))

mean(massspeed.20)
#[1] 0.5202421
var(massspeed.20)
#[1] 0.03306349
t.test(massspeed.20)
#   0.5184828 0.5220014

mean(massspeed.30)
#[1] 0.6024784
var(massspeed.30)
#[1] 0.04077036
t.test(massspeed.30)
#   0.5996793 0.6052774

cohensD(massspeed.20, massspeed.30)
# [1] 0.4359239

#Histogram

p1 <- hist(massspeed.20, plot = FALSE)
p2 <- hist(massspeed.30, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of mean trajectory speeds by distance", xlab = "Speed (cm/s)")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of mean trajectory speeds by distance", xlab = "Speed (cm/s)")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.2, legend = 
         c(paste("20cm, n =", length(massspeed.20)),
           paste("30cm, n =", length(massspeed.30)),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##(b) VARIANCE OF SPEED##

massspeedvar.20 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.20, graph = FALSE, datreturn = TRUE, fn = "vari")))
massspeedvar.30 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.30, graph = FALSE, datreturn = TRUE, fn = "vari")))

mean(massspeedvar.20)
#[1] 0.1843791
var(massspeedvar.20)
#[1] 0.02030129
t.test(massspeedvar.20)
#   0.1830006 0.1857577

mean(massspeedvar.30)
#[1] 0.1832995
var(massspeedvar.30)
#[1] 0.02106841
t.test(massspeedvar.30)
#   0.1812874 0.1853116

cohensD(massspeedvar.20, massspeedvar.30)
# [1] 0.007531074

cliff.delta(massspeedvar.20, massspeedvar.30)
# [1] 0.01741075
2*qnorm(-1/(cliff.delta(massspeedvar.20, massspeedvar.30)$estimate - 2))
# [1] 0.02201322

#Histogram

p1 <- hist(massspeedvar.20, plot = FALSE, breaks = seq(from = 0, to = 2.5, by = 0.1))
p2 <- hist(massspeedvar.30, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of variance of trajectory speeds by distance", xlab = "(cm/s)^2")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of variance of trajectory speeds by distance", xlab = "(cm/s)^2")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.3, legend = 
         c(paste("20cm, n =", length(massspeed.20)),
           paste("30cm, n =", length(massspeed.30)),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##(c) MEAN OF X VELOCITY##

massx.20 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.20, graph = FALSE, datreturn = TRUE)))
massx.30 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.30, graph = FALSE, datreturn = TRUE)))

mean(massx.20)
#[1] -0.001555704
var(massx.20)
#[1] 0.02501529
t.test(massx.20)
#   -3.086001e-03 -2.540635e-05

mean(massx.30)
#[1] -1.34e-05
var(massx.30)
#[1] 0.04557609
t.test(massx.30)
#   -0.002972802  0.002946002

cohensD(massx.20, massx.30)
# [1] 0.008655516

#Histogram

p1 <- hist(massx.20, plot = FALSE, breaks = seq(from = -1.5, to = 1.5, by = 0.1))
p2 <- hist(massx.30, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of mean trajectory x-velocities by distance", xlab = "X-velocity (cm/s)")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of mean trajectory x-velocities by distance", xlab = "X-velocity (cm/s)")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.2, legend = 
         c(paste("20cm, n =", length(massx.20)),
           paste("30cm, n =", length(massx.30)),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##(d) VARIANCE OF X-VELOCITY##

massxvar.20 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.20, graph = FALSE, datreturn = TRUE, fn = "vari")))
massxvar.30 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.30, graph = FALSE, datreturn = TRUE, fn = "vari")))

mean(massxvar.20)
#[1] 0.2621474
var(massxvar.20)
#[1] 0.03424805
t.test(massxvar.20)
#   0.2603569 0.2639380

mean(massxvar.30)
#[1] 0.3168624
var(massxvar.30)
#[1] 0.04524727
t.test(massxvar.30)
#   0.3139137 0.3198111

cohensD(massxvar.20, massxvar.30)
# [1] 0.2812328

cliff.delta(massxvar.20, massxvar.30)
# [1] -0.162374
2*qnorm(-1/(cliff.delta(massxvar.20, massxvar.30)$estimate - 2))
# [1] -0.188503

#Histogram

p1 <- hist(massxvar.20, plot = FALSE)
p2 <- hist(massxvar.30, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of variance of trajectory x-velocities by distance", xlab = "(cm/s)^2")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of variance of trajectory x-velocities by distance", xlab = "(cm/s)^2")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.3, legend = 
         c(paste("20cm, n =", length(massx.20)),
           paste("30cm, n =", length(massx.30)),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##(e) INVERSE TORTUOSITY##

masst.20 <- unname(unlist(Mass.function(anttort, dat.mf = massdat.20, graph = FALSE, datreturn = TRUE)))
masst.30 <- unname(unlist(Mass.function(anttort, dat.mf = massdat.30, graph = FALSE, datreturn = TRUE)))

mean(masst.20)
# [1] 0.2330694
var(masst.20)
# [1] 0.03673245
t.test(masst.20)
#   0.2312150 0.2349237

mean(masst.30)
# [1] 0.2549448
var(masst.30)
# [1] 0.04511302
t.test(masst.30)
#   0.2520005 0.2578891

cohensD(masst.20, masst.30)
# [1] 0.1100981

cliff.delta(masst.20, masst.30)
# [1] -0.04488203
2*qnorm(-1/(cliff.delta(masst.20, masst.30)$estimate - 2))
# [1] -0.05502359

#Histogram

p1 <- hist(masst.20, plot = FALSE)
p2 <- hist(masst.30, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of trajectory inverse tortuosity by distance", xlab = "Inverse tortuosity")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of trajectory inverse tortuosity by distance", xlab = "Inverse tortuosity")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.2, legend = 
         c(paste("20cm, n =", length(masst.20), "trajectories"),
           paste("30cm, n =", length(masst.30), "trajectories"),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##(f) PERIODICITY##

massp.20 <- unname(unlist(Mass.function(antphase, dat.mf = massdat.20, graph = FALSE, datreturn = TRUE)))
massp.30 <- unname(unlist(Mass.function(antphase, dat.mf = massdat.30, graph = FALSE, datreturn = TRUE)))

mean(massp.20)
# [1] 584.3341
var(massp.20)
#[1] 40706.76
t.test(massp.20)
#   574.8284 593.8398

mean(massp.30)
# [1] 572.0776
var(massp.30)
#[1] 36656.41
t.test(massp.30)
# 558.3256 585.8297

cohensD(massp.20, massp.30)
# [1] 0.06167877

#Histogram - Mean

p1 <- hist(massp.20, plot = FALSE)
p2 <- hist(massp.30, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of time between maxima by distance", xlab = "Time (frames)")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of time between maxima by distance", xlab = "Time (frames)")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.2, legend = 
         c(paste("20cm, n =", length(massp.20), "peak differences"),
           paste("30cm, n =", length(massp.30), "peak differences"),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))