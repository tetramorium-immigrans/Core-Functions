##THESIS SCRIPTS - QUALITY COMPARISONS##


##(a) MEAN OF SPEED##

massspeed.a <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE)))
massspeed.c <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE)))

mean(massspeed.a)
# [1] 0.5605528
var(massspeed.a)
#[1] 0.03871082
t.test(massspeed.a)
#   0.5580044 0.5631013

mean(massspeed.c)
# [1] 0.5391525
var(massspeed.c)
#[1] 0.03592545
t.test(massspeed.c)
#   0.537250 0.541055

cohensD(massspeed.a, massspeed.c)
# [1] 0.1112991

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

massspeedvar.a <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE, fn = "vari")))
massspeedvar.c <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE, fn = "vari")))

mean(massspeedvar.a)
# [1] 0.2021376
var(massspeedvar.a)
#[1] 0.02188973
t.test(massspeedvar.a)
#   0.2002212 0.2040540

mean(massspeedvar.c)
# [1] 0.1731485
var(massspeedvar.c)
#[1] 0.01943469
t.test(massspeedvar.c)
#   0.1717492 0.1745478

cohensD(massspeedvar.a, massspeedvar.c)
# [1] 0.2031847

cliff.delta(massspeedvar.a, massspeedvar.c)
# [1] 0.2031847
2*qnorm(-1/(cliff.delta(massspeedvar.a, massspeedvar.c)$estimate - 2))
# [1] 0.1988256

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

massx.a <- unname(unlist(Mass.function(antvel, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE)))
massx.c <- unname(unlist(Mass.function(antvel, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE)))

mean(massx.a)
# [1] -0.001554159
var(massx.a)
#[1] 0.02768127
t.test(massx.a)
#   -0.0037057190  0.0005974018

mean(massx.c)
# [1] -0.0007479647
var(massx.c)
#[1] 0.03376059
t.test(massx.c)
#   -0.002605537  0.001109607

cohensD(massx.a, massx.c)
# [1] 0.00452439

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

massxvar.a <- unname(unlist(Mass.function(antvel, dat.mf = massdat.a, graph = FALSE, datreturn = TRUE, fn = "vari")))
massxvar.c <- unname(unlist(Mass.function(antvel, dat.mf = massdat.c, graph = FALSE, datreturn = TRUE, fn = "vari")))

mean(massxvar.a)
# [1] 0.3106547
var(massxvar.a)
#[1] 0.04595454
t.test(massxvar.a)
#   0.3078781 0.3134314

mean(massxvar.c)
# [1] 0.2617055
var(massxvar.c)
#[1] 0.03314148
t.test(massxvar.c)
#   0.2598782 0.2635328

cohensD(massxvar.a, massxvar.c)
# [1] 0.2512731

cliff.delta(massxvar.a, massxvar.c)
# [1] 0.1353804
2*qnorm(-1/(cliff.delta(massxvar.a, massxvar.c)$estimate - 2))
# [1] 0.1822452

#Histogram

p1 <- hist(massxvar.a, plot = FALSE)
p2 <- hist(massxvar.c, plot = FALSE)

if(max(p1$counts) > max(p2$counts)){
  plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of variance of trajectory x-velocity by quality", xlab = "(cm/s)^2")
  plot(p2, col=rgb(0,0,1, 1/4), add=T)
}else{
  plot(p2, col=rgb(0,0,1, 1/4), main = "Distribution of variance of trajectory x-velocity by quality", xlab = "(cm/s)^2")
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
# [1] -0.08438766
2*qnorm(-1/(cliff.delta(masst.a, masst.c)$estimate - 2))
# [1] -0.1015259

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

