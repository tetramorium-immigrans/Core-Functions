par(mfrow = c(2,2))
trajs <- Import.f(a2023_06_14R3)
Antno(prop = TRUE, legloc = 5)
trajs <- Import.f(b2023_06_28L)
Antno(prop = TRUE, legloc = 5)
trajs <- Import.f(b2023_06_28R)
Antno(prop = TRUE, legloc = 5)
trajs <- Import.f(c2023_07_01R)
Antno(prop = TRUE, legloc = 5)

par(mfrow = c(2,2))
trajs <- Import.f(a2023_06_14R3)
Antsums(legloc = 5)
trajs <- Import.f(b2023_06_28L)
Antsums(legloc = 5)
trajs <- Import.f(b2023_06_28R)
Antsums(legloc = 5)
trajs <- Import.f(c2023_07_01R)
Antsums(legloc = 5)

par(mfrow = c(2,2))
trajs <- Import.f(a2023_06_14R3)
Antmeans(legloc = 5)
trajs <- Import.f(b2023_06_28L)
Antmeans(legloc = 5)
trajs <- Import.f(b2023_06_28R)
Antmeans(legloc = 5)
trajs <- Import.f(c2023_07_01R)
Antmeans(legloc = 5)

par(mfrow = c(2,2))
trajs <- Import.f(a2023_06_14R3)
Antnetx(legloc = 5)
trajs <- Import.f(b2023_06_28L)
Antnetx(legloc = 5)
trajs <- Import.f(b2023_06_28R)
Antnetx(legloc = 5)
trajs <- Import.f(c2023_07_01R)
Antnetx(legloc = 5)

par(mfrow = c(2,2))
trajs <- Import.f(a2023_06_14R3)
Antpropx(legloc = 5)
trajs <- Import.f(b2023_06_28L)
Antpropx(legloc = 5)
trajs <- Import.f(b2023_06_28R)
Antpropx(legloc = 5)
trajs <- Import.f(c2023_07_01R)
Antpropx(legloc = 5)

par(mfrow = c(2,2))
trajs <- Import.f(a2023_06_14R3)
Antpropx(legloc = 5, outbound = 0)
trajs <- Import.f(b2023_06_28L)
Antpropx(legloc = 5, outbound = 0)
trajs <- Import.f(b2023_06_28R)
Antpropx(legloc = 5, outbound = 0)
trajs <- Import.f(c2023_07_01R)
Antpropx(legloc = 5, outbound = 0)

trajsp <- rowMeans(trajs[,,4], na.rm = TRUE)
trajsp[is.nan(trajsp)] <- rowSums(trajs[,,4])[is.nan(trajsp)]

trajsp <- rowSums(trajs[,,4], na.rm = TRUE)


trajsp[is.na(trajsp)] <- 0


acf(rowMeans(trajs70[,,1], na.rm = TRUE), lag.max = 30000, na.action = na.pass)

trajs70sp <- trajs70[900:1200,,1]
acf(rowSums(trajs70[,,1], na.rm = TRUE), lag.max = 300, na.action = na.pass)

acf(rowSums(trajs70[900:1200,,1] < 0, na.rm = TRUE) / rowSums(!is.na(trajs70[900:1200,,1]), na.rm = TRUE), lag.max = 30000, na.action = na.pass)

owd <- getwd()
setwd("D:\\Processed\\")
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.delim)
setwd(owd)