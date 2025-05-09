##THESIS SCRIPTS - QUALITY COMPARISONS##


##(a) MEAN OF SPEED##

massspeed.a <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE, trajreturn = TRUE)))
massspeed.c <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE, trajreturn = TRUE)))

mean(massspeed.a)
# [1] 0.5822641
var(massspeed.a)
#[1] 0.04290731
t.test(massspeed.a)
#   0.5795810 0.5849471

mean(massspeed.c)
# [1] 0.5550445
var(massspeed.c)
#[1] 0.0377725
t.test(massspeed.c)
#   0.5530937 0.5569953

cohensD(massspeed.a, massspeed.c)
# [1] 0.1366125

#Histogram

p1 <- hist(massspeed.a, plot = FALSE)
p2 <- hist(massspeed.c, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of mean trajectory speed by quality", xlab = "Speed (cm/s)")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of mean trajectory speed by quality", xlab = "Speed (cm/s)")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.15, legend = 
         c(paste("A Quality, n =", length(massspeed.a), "trajectories"),
           paste("C Quality, n =", length(massspeed.c), "trajectories"),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##(b) VARIANCE OF SPEED##

massspeedvar.a <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE, trajreturn = TRUE, fn = "vari")))
massspeedvar.c <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE, trajreturn = TRUE, fn = "vari")))

mean(massspeedvar.a)
# [1] 0.2069973
var(massspeedvar.a)
#[1] 0.02126578
t.test(massspeedvar.a)
#   0.2051085 0.2088862

mean(massspeedvar.c)
# [1] 0.1781986
var(massspeedvar.c)
#[1] 0.02095696
t.test(massspeedvar.c)
#   0.1767455 0.1796517

cohensD(massspeedvar.a, massspeedvar.c)
# [1] 0.1983863

cliff.delta(massspeedvar.a, massspeedvar.c)
# [1] 0.1502535 
2*qnorm(-1/(cliff.delta(massspeedvar.a, massspeedvar.c)$estimate - 2))
# [1] 0.2039645

#Histogram

p1 <- hist(massspeedvar.a, plot = FALSE, breaks = seq(from = 0, to = 2.5, by = 0.1))
p2 <- hist(massspeedvar.c, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of variance of trajectory speed by quality", xlab = "(cm/s)^2")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of variance of trajectory speed by quality", xlab = "(cm/s)^2")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.3, legend = 
         c(paste("A Quality, n =", length(massspeed.a), "trajectories"),
           paste("C Quality, n =", length(massspeed.c), "trajectories"),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##(c) MEAN OF X-VELOCITY##

massx.a <- unname(unlist(Mass.function(antvel, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE, trajreturn = TRUE)))
massx.c <- unname(unlist(Mass.function(antvel, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE, trajreturn = TRUE)))

mean(massx.a)
# [1] -0.003747602
var(massx.a)
#[1] 0.02961415
t.test(massx.a)
#   -0.005976614 -0.001518590

mean(massx.c)
# [1] -0.002171326
var(massx.c)
#[1] 0.03602116
t.test(massx.c)
#   -0.0040763552 -0.0002662971

cohensD(massx.a, massx.c)
# [1] 0.008597089

#Histogram

p1 <- hist(massx.a, plot = FALSE)
p2 <- hist(massx.c, plot = FALSE, breaks = seq(from = -1.5, to = 1.5, by = 0.1))

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of mean trajectory x-velocity by quality", xlab = "X-velocity (cm/s)")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of mean trajectory x-velocity by quality", xlab = "X-velocity (cm/s)")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.15, legend = 
         c(paste("A Quality, n =", length(massx.a), "trajectories"),
           paste("C Quality, n =", length(massx.c), "trajectories"),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))

##(d) VARIANCE OF X-VELOCITY##

massxvar.a <- unname(unlist(Mass.function(antvel, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE, trajreturn = TRUE, fn = "vari")))
massxvar.c <- unname(unlist(Mass.function(antvel, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE, trajreturn = TRUE, fn = "vari")))

mean(massxvar.a)
# [1] 0.327811
var(massxvar.a)
#[1] 0.05229401
t.test(massxvar.a)
#   0.3248489 0.3307730

mean(massxvar.c)
# [1] 0.2717859
var(massxvar.c)
#[1] 0.03713073
t.test(massxvar.c)
#   0.2698518 0.2737201

cohensD(massxvar.a, massxvar.c)
# [1] 0.2707437

cliff.delta(massxvar.a, massxvar.c)
# [1] 0.1433382  
2*qnorm(-1/(cliff.delta(massxvar.a, massxvar.c)$estimate - 2))
# [1] 0.19382

#Histogram

p1 <- hist(massxvar.a, plot = FALSE, breaks = seq(from = 0, to = 3, by = 0.1))
p2 <- hist(massxvar.c, plot = FALSE, breaks = seq(from = 0, to = 3, by = 0.1))

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of variance of trajectory x-velocity by quality", xlab = "(cm/s)^2", xlim = c(0,2))
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of variance of trajectory x-velocity by quality", xlab = "(cm/s)^2", xlim = c(0,2))
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.3, legend = 
         c(paste("A Quality, n =", length(massx.a), "trajectories"),
           paste("C Quality, n =", length(massx.c), "trajectories"),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##(e) INVERSE TORTUOSITY##

masst.a <- unname(unlist(Mass.function(anttort, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE)))
masst.c <- unname(unlist(Mass.function(anttort, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE)))

mean(masst.a)
# [1] 0.2191203
var(masst.a)
# [1] 0.03372442
t.test(masst.a)
#   0.2167416 0.2214989

mean(masst.c)
# [1] 0.2529161
var(masst.c)
# [1] 0.04267303
t.test(masst.c)
#   0.2508426 0.2549896

cohensD(masst.a, masst.c)
# [1] 0.170444

cliff.delta(masst.a, masst.c)
# [1] -0.08438792 
2*qnorm(-1/(cliff.delta(masst.a, masst.c)$estimate - 2))
# [1] -0.1015262

#Histogram

p1 <- hist(masst.a, plot = FALSE)
p2 <- hist(masst.c, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of trajectory inverse tortuosity by quality", xlab = "Inverse tortuosity")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of trajectory inverse tortuosity by quality", xlab = "Inverse tortuosity")
  plot(p1, col=rgb(1,0,0, 1/4), add=T)
}

legend("topright", inset = 0.2, legend = 
         c(paste("A Quality, n =", length(masst.a), "trajectories"),
           paste("C Quality, n =", length(masst.c), "trajectories"),
           "Overlap"),
       fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##(f) PERIODICITY##

massp.a <- unname(unlist(Mass.function(antphase, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE)))
massp.c <- unname(unlist(Mass.function(antphase, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE)))

mean(massp.a)
# [1] 576.5529
var(massp.a)
#[1] 41790.47
t.test(massp.a)
#   563.2384 589.8673

mean(massp.c)
# [1] 583.0045
var(massp.c)
#[1] 38192.42
t.test(massp.c)
#   573.3363 592.6726

cohensD(massp.a, massp.c)
# [1] 0.03245761

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



##CREATING POOLED SETS OF DATA##

massdat.0 <- c(massdat.a, massdat.c)
massspeed.0 <- c(massspeed.a, massspeed.c)
massspeedvar.0 <- c(massspeedvar.a, massspeedvar.c)
massx.0 <- c(massx.a, massx.c)
massxvar.0 <- c(massxvar.a, massxvar.c)
masst.0 <- c(masst.a, masst.c)
massp.0 <- c(massp.a, massp.c)


#Histogram - Variance

# p1 <- hist(masspvar.a, plot = FALSE)
# p2 <- hist(masspvar.c, plot = FALSE)
# 
# if(max(p1$counts) > max(p2$counts)){
#   plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of variance of time between maxima by quality", xlab = "(frames)^2")
#   plot(p2, col=rgb(0,0,1, 1/4), add=T)
# }else{
#   plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of variance of time between maxima by quality", xlab = "(frames)^2")
#   plot(p1, col=rgb(1,0,0, 1/4), add=T)
# }
# 
# legend("topright", inset = 0.15, legend = 
#          c(paste("A Quality, n =", length(masspvar.a), "videos"),
#            paste("C Quality, n =", length(masspvar.c), "videos"),
#            "Overlap"),
#        fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))

