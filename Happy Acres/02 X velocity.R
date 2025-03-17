##THESIS SCRIPTS - X-VELOCITY##

##X VELOCITY BY QUALITY##

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

#Histogram graphing - Quality x-vel
p2 <- hist(massx.a, plot = FALSE)
p1 <- hist(massx.c, plot = FALSE, breaks = seq(from = -1.5, to = 1.5, by = 0.1))

plot(p1, col=rgb(0,0,1, 1/4), main = "Distribution of mean x-component of velocity", xlab = "Velocity (cm/s)", xlim = c(-1, 1))
plot(p2, col=rgb(1,0,0, 1/4), add=T)
legend("topright", inset = 0.25, legend = c("A videos", "C videos", "Overlap"), fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))

#Histogram graphing - Quality variance
p2 <- hist(massxvar.a, plot = FALSE)
p1 <- hist(massxvar.c, plot = FALSE)

plot(p1, col=rgb(0,0,1, 1/4), main = "Distribution of variance of x-component of velocity", xlab = "Variance of velocity (cm/s)^2")
plot(p2, col=rgb(1,0,0, 1/4), add=T)
legend("topright", inset = 0.25, legend = c("A videos", "C videos", "Overlap"), fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))



##X VELOCITY BY SIZE##

massx.150 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.150, graph = FALSE, datreturn = TRUE)))
massxvar.150 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.150, graph = FALSE, datreturn = TRUE, fn = "vari")))

massx.300 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.300, graph = FALSE, datreturn = TRUE)))
massxvar.300 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.300, graph = FALSE, datreturn = TRUE, fn = "vari")))

mean(massx.150)
#[1] -0.001513446
mean(massx.300)
#[1] -0.0006606323
var(massx.150)
#[1] 0.02750016
var(massx.300)
#[1] 0.03484058

mean(massxvar.150)
#[1] 0.2509312
mean(massxvar.300)
#[1] 0.3029104
var(massxvar.150)
#[1] 0.03718373
var(massxvar.300)
#[1] 0.0376668

t.test(massx.150)
#   -0.0034271555  0.0004002642
t.test(massx.300)
#   -0.002623387  0.001302122
t.test(massxvar.150)
#   0.2487059 0.2531564
t.test(massxvar.300)
#   0.3008696 0.3049513

max(massx.150)
#[1] 0.7680766
max(massx.300)
#[1] 1.312993

#Histogram graphing - Size x-vel
p2 <- hist(massx.150, plot = FALSE)
p1 <- hist(massx.300, plot = FALSE,  breaks = seq(from = -1.5, to = 1.5, by = 0.1))

plot(p1, col=rgb(0,0,1, 1/4), main = "Distribution of mean x-component of velocity", xlab = "Velocity (cm/s)", xlim = c(-1, 1))
plot(p2, col=rgb(1,0,0, 1/4), add=T)
legend("topright", inset = 0.25, legend = c("150 videos", "300 videos", "Overlap"), fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))

#Histogram graphing - Size variance
p1 <- hist(massxvar.150, plot = FALSE)
p2 <- hist(massxvar.300, plot = FALSE)

plot(p1, col=rgb(0,0,1, 1/4), main = "Distribution of variance of x-component of velocity", xlab = "Variance of velocity (cm/s)^2")
plot(p2, col=rgb(1,0,0, 1/4), add=T)
legend("topright", inset = 0.25, legend = c("150 videos", "300 videos", "Overlap"), fill = c(rgb(0,0,1,1/4), rgb(1,0,0,1/4), rgb(1,0,1, 1/2)))


##X VELOCITY BY DISTANCE##

massx.20 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.20, graph = FALSE, datreturn = TRUE)))
massxvar.20 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.20, graph = FALSE, datreturn = TRUE, fn = "vari")))

massx.30 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.30, graph = FALSE, datreturn = TRUE)))
massxvar.30 <- unname(unlist(Mass.function(antvel, dat.mf = massdat.30, graph = FALSE, datreturn = TRUE, fn = "vari")))

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

#Histogram graphing - Distance speed
p1 <- hist(massx.20, plot = FALSE, breaks = seq(from = -1.5, to = 1.5, by = 0.1))
p2 <- hist(massx.30, plot = FALSE)

plot(p1, col=rgb(0,0,1, 1/4), main = "Distribution of mean x-component of velocity", xlab = "Velocity (cm/s)", xlim = c(-1,1))
plot(p2, col=rgb(1,0,0, 1/4), add=T)
legend("topright", inset = 0.25, legend = c("20cm videos", "30cm videos", "Overlap"), fill = c(rgb(0,0,1,1/4), rgb(1,0,0,1/4), rgb(1,0,1, 1/2)))

#Histogram graphing - Distance variance
p1 <- hist(massxvar.20, plot = FALSE)
p2 <- hist(massxvar.30, plot = FALSE)

plot(p1, col=rgb(1,0,0, 1/4), main = "Distribution of variance of x-component of velocity", xlab = "Variance of Velocity (cm/s)^2", xlim = c(0,2))
plot(p2, col=rgb(0,0,1, 1/4), add=T)
legend("topright", inset = 0.25, legend = c("20cm videos", "30cm videos", "Overlap"), fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))