##THESIS SCRIPTS - SIZE COMPARISONS##


##(a) MEAN OF SPEED##

massspeed.150 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.150, graph = FALSE, datreturn = TRUE, trajreturn = TRUE)))
massspeed.300 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.300, graph = FALSE, datreturn = TRUE, trajreturn = TRUE)))

mean(massspeed.150)
#[1] 0.5251604
var(massspeed.150)
#[1] 0.03647086
t.test(massspeed.150)
#  0.5229566 0.5273643

mean(massspeed.300)
#[1] 0.6012006
var(massspeed.300)
#[1] 0.04018909
t.test(massspeed.300)
#  0.5990103 0.6033910

cohensD(massspeed.150, massspeed.300)
# [1] 0.3878817

#Histogram

p1 <- hist(massspeed.150, plot = FALSE)
p2 <- hist(massspeed.300, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of mean trajectory speeds by size", xlab = "Speed (cm/s)")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of mean trajectory speeds by size", xlab = "Speed (cm/s)")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.2, legend = 
         c(paste("150 Ants, n =", length(massspeed.150)),
           paste("300 Ants, n =", length(massspeed.300)),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##(b) VARIANCE OF SPEED##

massspeedvar.150 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.150, graph = FALSE, datreturn = TRUE, trajreturn = TRUE, fn = "vari")))
massspeedvar.300 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.300, graph = FALSE, datreturn = TRUE, trajreturn = TRUE, fn = "vari")))

mean(massspeedvar.150)
#[1] 0.1703201
var(massspeedvar.150)
#[1] 0.01899603
t.test(massspeedvar.150)
#  0.1687296 0.1719106

mean(massspeedvar.300)
#[1] 0.2057526
var(massspeedvar.300)
#[1] 0.02270975
t.test(massspeedvar.300)
#  0.2041061 0.2073991

cohensD(massspeedvar.150, massspeedvar.300)
# [1] 0.2447738

cliff.delta(massspeedvar.150, massspeedvar.300)
# [1] -0.1932429  
2*qnorm(-1/(cliff.delta(massspeedvar.150, massspeedvar.300)$estimate - 2))
# [1] -0.2213055

#Histogram

p1 <- hist(massspeedvar.150, plot = FALSE, breaks = seq(from = 0, to = 2.5, by = 0.1))
p2 <- hist(massspeedvar.300, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of variance of trajectory speeds by size", xlab = "(cm/s)^2")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of variance of trajectory speeds by size", xlab = "(cm/s)^2")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.3, legend = 
         c(paste("150 Ants, n =", length(massspeed.150)),
           paste("300 Ants, n =", length(massspeed.300)),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##(c) MEAN OF X-VELOCITY##

massx.150 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.150, graph = FALSE, datreturn = TRUE, trajreturn = TRUE)))
massx.300 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.300, graph = FALSE, datreturn = TRUE, trajreturn = TRUE)))

mean(massx.150)
#[1] -0.003374316
var(massx.150)
#[1] 0.02898884
t.test(massx.150)
#   -0.005339141 -0.001409490

mean(massx.300)
#[1] -0.002214559
var(massx.300)
#[1] 0.03776655
t.test(massx.300)
#   -4.337859e-03 -9.125763e-05

cohensD(massx.150, massx.300)
# [1] 0.00632535

#Histogram

p1 <- hist(massx.150, plot = FALSE)
p2 <- hist(massx.300, plot = FALSE, breaks = seq(from = -1.5, to = 1.5, by = 0.1))

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of mean trajectory x-velocities by size", xlab = "X-velocity (cm/s)")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of mean trajectory x-velocities by size", xlab = "X-velocity (cm/s)")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.2, legend = 
         c(paste("150 Ants, n =", length(massx.150)),
           paste("300 Ants, n =", length(massx.300)),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##(d) VARIANCE OF X-VELOCITY##

massxvar.150 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.150, graph = FALSE, datreturn = TRUE, trajreturn = TRUE, fn = "vari")))
massxvar.300 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.300, graph = FALSE, datreturn = TRUE, trajreturn = TRUE, fn = "vari")))

mean(massxvar.150)
#[1] 0.2616854
var(massxvar.150)
#[1] 0.04180344
t.test(massxvar.150)
#   0.2593259 0.2640449

mean(massxvar.300)
#[1] 0.3207045
var(massxvar.300)
#[1] 0.04348029
t.test(massxvar.300)
#   0.3184262 0.3229828

cohensD(massxvar.150, massxvar.300)
# [1] 0.2856548

cliff.delta(massxvar.150, massxvar.300)
# [1] -0.2045929  
2*qnorm(-1/(cliff.delta(massxvar.150, massxvar.300)$estimate - 2))
# [1] -0.2331497

#Histogram

p1 <- hist(massxvar.150, plot = FALSE, breaks = seq(from = 0, to = 3, by = 0.1))
p2 <- hist(massxvar.300, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of variance of trajectory x-velocities by size", xlab = "(cm/s)^2", xlim = c(0,2))
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of variance of trajectory x-velocities by size", xlab = "(cm/s)^2", xlim = c(0,2))
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.3, legend = 
         c(paste("150 Ants, n =", length(massx.150)),
           paste("300 Ants, n =", length(massx.300)),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##(e) INVERSE TORTUOSITY##

masst.150 <- unname(unlist(Mass.function(anttort, dat.mf = massdat.150, graph = FALSE, datreturn = TRUE)))
masst.300 <- unname(unlist(Mass.function(anttort, dat.mf = massdat.300, graph = FALSE, datreturn = TRUE)))

mean(masst.150)
# [1] 0.2314068
var(masst.150)
# [1] 0.03931341
t.test(masst.150)
#   0.2291187 0.2336949

mean(masst.300)
# [1] 0.2481497
var(masst.300)
# [1] 0.03969259
t.test(masst.300)
#   0.2459730 0.2503265

cohensD(masst.150, masst.300)
# [1] 0.08422856

cliff.delta(masst.150, masst.300)
# [1] -0.05892918
2*qnorm(-1/(cliff.delta(masst.150, masst.300)$estimate - 2))
# [1] -0.07175829

#Histogram

p1 <- hist(masst.150, plot = FALSE)
p2 <- hist(masst.300, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of trajectory inverse tortuosity by size", xlab = "Inverse tortuosity")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of trajectory inverse tortuosity by size", xlab = "Inverse tortuosity")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.2, legend = 
         c(paste("150 ants, n =", length(masst.150), "trajectories"),
           paste("300 ants, n =", length(masst.300), "trajectories"),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##(f)PERIODICITY##

massp.150 <- unname(unlist(Mass.function(antphase, dat.mf = massdat.150, graph = FALSE, datreturn = TRUE)))
massp.300 <- unname(unlist(Mass.function(antphase, dat.mf = massdat.300, graph = FALSE, datreturn = TRUE)))

mean(massp.150)
# [1] 581.903
var(massp.150)
#[1] 40220.98
t.test(massp.150)
#   571.6909 592.1150

mean(massp.300)
# [1] 578.7641
var(massp.300)
#[1] 38466.96
t.test(massp.300)
# 566.5688 590.9593

cohensD(massp.150, massp.300)
# [1] 0.01579023

#Histogram

p1 <- hist(massp.150, plot = FALSE)
p2 <- hist(massp.300, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of time between maxima by size", xlab = "Time (frames)")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of time between maxima by size", xlab = "Time (frames)")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.2, legend = 
         c(paste("150 Ants, n =", length(massp.150), "peak differences"),
           paste("300 Ants, n =", length(massp.300), "peak differences"),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


