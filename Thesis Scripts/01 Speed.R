##THESIS SCRIPTS - SPEED##

##SPEED BY QUALITY##

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

#Histogram graphing - Quality speed
p2 <- hist(massspeed.a, plot = FALSE)
p1 <- hist(massspeed.c, plot = FALSE)

plot(p1, col=rgb(0,0,1, 1/4), xlim=c(0,1.4), main = "Distribution of mean trajectory speeds", xlab = "Speed (cm/s)")
plot(p2, col=rgb(1,0,0, 1/4), xlim=c(0,1.4), add=T)
legend("topright", inset = 0.25, legend = c("A videos", "C videos", "Overlap"), fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))

#Histogram graphing - Quality variance
p2 <- hist(massspeedvar.a, plot = FALSE, breaks = seq(from = 0, to = 2.5, by = 0.1), xlim = c(0, 1.4))
p1 <- hist(massspeedvar.c, plot = FALSE)

plot(p1, col=rgb(0,0,1, 1/4), xlim=c(0,1.4), main = "Distribution of variance of trajectory speeds", xlab = "Variance of speed (cm/s)^2")
plot(p2, col=rgb(1,0,0, 1/4), xlim=c(0,1.4), add=T)
legend("topright", inset = 0.25, legend = c("A videos", "C videos", "Overlap"), fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))



##SPEED BY SIZE##

massspeed.150 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.150, graph = FALSE, datreturn = TRUE)))
massspeedvar.150 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.150, graph = FALSE, datreturn = TRUE, fn = "vari")))

massspeed.300 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.300, graph = FALSE, datreturn = TRUE)))
massspeedvar.300 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.300, graph = FALSE, datreturn = TRUE, fn = "vari")))

mean(massspeed.150)
#[1] 0.5079424
mean(massspeed.300)
#[1] 0.5747192
var(massspeed.150)
#[1] 0.03350699
var(massspeed.300)
#[1] 0.03701552

mean(massspeedvar.150)
#[1] 0.1664908
mean(massspeedvar.300)
#[1] 0.2084798
var(massspeedvar.150)
#[1] 0.01942356
var(massspeedvar.300)
#[1] 0.02268624

max(massspeed.150)
#[1] 1.381232
max(massspeed.300)
#[1] 1.329981

t.test(massspeed.150)
#0.5058300 0.5100548
t.test(massspeed.300)
#0.5726961 0.5767423
t.test(massspeedvar.150)
#0.1648825 0.1680992
t.test(massspeedvar.300)
#0.2068960 0.2100636

#Histogram graphing - Size speed
p2 <- hist(massspeed.150, plot = FALSE)
p1 <- hist(massspeed.300, plot = FALSE)

plot(p1, col=rgb(0,0,1, 1/4), xlim=c(0,1.4), main = "Distribution of mean trajectory speeds", xlab = "Speed (cm/s)")
plot(p2, col=rgb(1,0,0, 1/4), xlim=c(0,1.4), add=T)
legend("topright", inset = 0.25, legend = c("150 videos", "300 videos", "Overlap"), fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))

#Histogram graphing - Size variance
p2 <- hist(massspeedvar.150, plot = FALSE, breaks = seq(from = 0, to = 2.5, by = 0.1), xlim = c(0, 1.4))
p1 <- hist(massspeedvar.300, plot = FALSE)

plot(p1, col=rgb(0,0,1, 1/4), xlim=c(0,1.4), main = "Distribution of variance of trajectory speeds", xlab = "Speed (cm/s)^2")
plot(p2, col=rgb(1,0,0, 1/4), xlim=c(0,1.4), add=T)
legend("topright", inset = 0.25, legend = c("150 videos", "300 videos", "Overlap"), fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))


##SPEED BY DISTANCE##

massspeed.20 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.20, graph = FALSE, datreturn = TRUE)))
massspeedvar.20 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.20, graph = FALSE, datreturn = TRUE, fn = "vari")))

massspeed.30 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.30, graph = FALSE, datreturn = TRUE)))
massspeedvar.30 <- unname(unlist(Mass.function(antspeed, dat.mf = massdat.30, graph = FALSE, datreturn = TRUE, fn = "vari")))

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

#Histogram graphing - Distance speed
p1 <- hist(massspeed.20, plot = FALSE)
p2 <- hist(massspeed.30, plot = FALSE)

plot(p1, col=rgb(0,0,1, 1/4), main = "Distribution of mean trajectory speeds", xlab = "Speed (cm/s)")
plot(p2, col=rgb(1,0,0, 1/4), add=T)
legend("topright", inset = 0.25, legend = c("20cm videos", "30cm videos", "Overlap"), fill = c(rgb(0,0,1,1/4), rgb(1,0,0,1/4), rgb(1,0,1, 1/2)))

#Histogram graphing - Distance variance
p1 <- hist(massspeedvar.20, plot = FALSE)
p2 <- hist(massspeedvar.30, plot = FALSE)

plot(p1, col=rgb(0,0,1, 1/4), main = "Distribution of variance of trajectory speeds", xlab = "Speed (cm/s)^2")
plot(p2, col=rgb(1,0,0, 1/4), add=T)
legend("topright", inset = 0.25, legend = c("20cm videos", "30cm videos", "Overlap"), fill = c(rgb(1,0,0,1/4), rgb(0,0,1,1/4), rgb(1,0,1, 1/2)))