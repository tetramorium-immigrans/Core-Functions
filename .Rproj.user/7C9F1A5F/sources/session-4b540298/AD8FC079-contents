##THESIS SCRIPTS - COUNT VS. INSTANTANEOUS SPEED##
#Note: assumes that 01_Quality.R has been run first

#Calculate in each frame the # of trajectories and average of their instantaneous speeds
timenum <- unlist(pbsapply(1:length(massdat.0), function(z){
  antno(massdat.0[[z]], datreturn = TRUE, graph = FALSE)
  }))

timespeed <- unlist(pbsapply(1:length(massdat.0), function(z){
  antspeed(massdat.0[[z]], bytime = TRUE, datreturn = TRUE, graph = FALSE)
  }))

# timenumbackup <- timenum
# timespeedbackup <- timespeed
# timenum <- timenumbackup

#Remove frames where there are no ants to avoid NaNs
timespeed <- timespeed[which(timenum != 0)]
timenum <- timenum[which(timenum != 0)]

timenum <- timenum[which(timespeed != 0)]
timespeed <- timespeed[which(timespeed != 0)]

#Kept getting NaN errors in lm so replaced NaNs with NAs manually
timenum[is.nan(timenum)] <- NA
timespeed[is.nan(timespeed)] <- NA

#Put into data frame
dat <- data.frame(timenum, timespeed)

plot(dat$timenum, dat$timespeed) #Looks like a solid mess

#Apply by factor mean and median to see if there's a shape to the solid mess
meantest <- with(dat, tapply(timespeed, timenum, mean))
mediantest <- with(dat, tapply(timespeed, timenum, median))
#Looks like there's either a two-part curve or an asymptotic approach to around 0.52-0.53 cm/s

#Modeling various root functions to fit the curve
plot(meantest)
s <- 2:76
meantesttruncate <- meantest[s]

lm1 <- lm(meantesttruncate ~ I(s^(1/1)))
lm2 <- lm(meantesttruncate ~ I(s^(1/2)))
lm3 <- lm(meantesttruncate ~ I(s^(1/3)))
lm4 <- lm(meantesttruncate ~ I(s^(1/4)))
lm5 <- lm(meantesttruncate ~ I(s^(1/5)))
lm6 <- lm(meantesttruncate ~ I(s^(1/6)))
lm7 <- lm(meantesttruncate ~ I(s^(1/7))) #Best according to aic, although basically indistinguishable from 6, 8, and 9 and hardly different from 5
lm8 <- lm(meantesttruncate ~ I(s^(1/8)))
lm9 <- lm(meantesttruncate ~ I(s^(1/9)))

aictab(cand.set = list(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9))

lines(s, predict(lm1, list(meantesttruncate = s)), col = "red")
lines(s, predict(lm2, list(meantesttruncate = s)), col = "orange")
lines(s, predict(lm3, list(meantesttruncate = s)), col = "yellow")
lines(s, predict(lm4, list(meantesttruncate = s)), col = "green")
lines(s, predict(lm5, list(meantesttruncate = s)), col = "blue")
lines(s, predict(lm6, list(meantesttruncate = s)), col = "orchid4")
lines(s, predict(lm7, list(meantesttruncate = s)), col = "darkmagenta")



#---
plot(meantest)
preds <- predict(lm7, list(meantesttruncate = s), interval = 'confidence')

lines(s, predict(lm7, list(meantesttruncate = s)), col = "red")
lines(s, preds[ ,3], lty = 'dashed', col = 'blue')
lines(s, preds[ ,2], lty = 'dashed', col = 'blue')

#polygon(c(rev(s), s), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey', border = NA)
#lines(s, predict(lm7, list(meantesttruncate = s)), col = "red")

#---
#Try having nls first fit only power then everything

lmnls <- nls(meantesttruncate ~ 0.142344 + 0.22*(s^power), start = list(power = 1/7))
lines(s, predict(lmnls, list(meantesttruncate = s)), col = "green")
summary(lmnls)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# power 0.146831   0.001209   121.4   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.0141 on 74 degrees of freedom
# 
# Number of iterations to convergence: 2 
# Achieved convergence tolerance: 1.217e-06

lmnls <- nls(meantesttruncate ~ inter + slope*(s^power), start = list(inter = lm7$coefficients[1], slope = lm7$coefficients[2], power = 1/7))
lines(s, predict(lmnls, list(meantesttruncate = s)), col = "green")
summary(lmnls)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)
# inter  0.12171    0.22772   0.534    0.595
# slope  0.24270    0.21466   1.131    0.262
# power  0.13471    0.08459   1.593    0.116
# 
# Residual standard error: 0.01425 on 72 degrees of freedom
# 
# Number of iterations to convergence: 8 
# Achieved convergence tolerance: 4.933e-06

#Looks like by adding more variables I'm dramatically increasing my uncertainty in the estimates.  That makes sense.  There's a lot of combinations that could work