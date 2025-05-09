##THESIS SCRIPTS - RATIO OF INBOUND/OUTBOUND VS SPEED##
#Note: assumes that 01_Quality.R has been run first

#Calculate the speed and ratio

timespeed <- unlist(pbsapply(1:length(massdat.0), function(z){
  antspeed(massdat.0[[z]], bytime = TRUE, datreturn = TRUE, graph = FALSE)
}))

timeratio <- unlist(pbsapply(1:length(massdat.0), function(z){
  antpropx(massdat.0[[z]], datreturn = TRUE, graph = FALSE, bytime = TRUE)
})) 

trajratio <- unlist(pbsapply(1:length(massdat.0), function(z){
  antpropx(massdat.0[[z]], datreturn = TRUE, graph = FALSE, bytime = FALSE)
})) 

#Remove NaNs (caused by 0-ant frames)
timespeedtrunc <- timespeed[!is.nan(timeratio)]
trajratiotrunc <- trajratio[!is.nan(timeratio)]
timeratiotrunc <- timeratio[!is.nan(timeratio)]

#Put into data frame
dat <- data.frame(timespeedtrunc, abs(timeratiotrunc), abs(trajratiotrunc))


plot(dat$timespeed, dat$trajratio)
#Looks like minarets...

plot(abs(timeratio), timespeed)
plot(abs(trajratio), timespeed)

#modeling timeratio
plot(abs(dat$timeratiotrunc), dat$timespeedtrunc)

lm1 <- lm(timespeedtrunc ~ I(abs(timeratiotrunc)))
summary(lm1)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.4977 -0.1265 -0.0198  0.1117  5.9964 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)             0.4976838  0.0002185 2277.238   <2e-16 ***
#   I(abs(timeratiotrunc)) -0.0010977  0.0007244   -1.515     0.13    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1807 on 1441857 degrees of freedom
# Multiple R-squared:  1.592e-06,	Adjusted R-squared:  8.987e-07 
# F-statistic: 2.296 on 1 and 1441857 DF,  p-value: 0.1297

s = seq(from = 0, to = max(abs(timeratiotrunc)), length.out = 100)
preds <- predict(lm1, newdata = list(timeratiotrunc = s), interval = 'confidence')

lines(s, preds[ ,1], col = "red")
lines(s, preds[ ,3], lty = 'dashed', col = 'blue')
lines(s, preds[ ,2], lty = 'dashed', col = 'blue')

lmnls <- nls(timespeedtrunc ~ inter + slope*(timeratiotrunc^power), start = list(inter = lm1$coefficients[1], slope = lm1$coefficients[2], power = 1)) 
summary(lmnls)

#modeling trajratiotrunc
trt <- dat$abs.trajratiotrunc.

plot(trt, dat$timespeedtrunc)

lm1 <- lm(timespeedtrunc ~ I(trt))
summary(lm1)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.5038 -0.1270 -0.0208  0.1107  6.0752 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.5038483  0.0001653 3048.59   <2e-16 ***
#   I(abs(trajratiotrunc)) -0.0849637  0.0009192  -92.44   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1802 on 1441857 degrees of freedom
# Multiple R-squared:  0.005891,	Adjusted R-squared:  0.00589 
# F-statistic:  8544 on 1 and 1441857 DF,  p-value: < 2.2e-16



s = seq(from = 0, to = max(trt), length.out = 100)
preds <- predict(lm1, newdata = list(trt = s), interval = 'confidence')

lines(s, preds[ ,1], col = "red")
lines(s, preds[ ,3], lty = 'dashed', col = 'blue')
lines(s, preds[ ,2], lty = 'dashed', col = 'blue')

lmnls <- nls(timespeedtrunc ~ inter + slope*(trajratiotrunc^power), start = list(inter = lm1$coefficients[1], slope = lm1$coefficients[2], power = 1)) 
summary(lmnls)

#--Trying by-factor

factortime <- with(dat, tapply(timespeedtrunc, abs.timeratiotrunc., mean))
sorttime <- sort(unique(dat$abs.timeratiotrunc.))


plot(sorttime, factortime)

lm1 <- lm(factortime ~ I(sorttime))
summary(lm1)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.28103 -0.03715 -0.00472  0.02457  0.66066 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.500953   0.005523   90.70   <2e-16 ***
#   I(sorttime) 0.224057   0.013977   16.03   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.09452 on 894 degrees of freedom
# Multiple R-squared:  0.2233,	Adjusted R-squared:  0.2224 
# F-statistic:   257 on 1 and 894 DF,  p-value: < 2.2e-16

s = seq(from = 0, to = max(sorttime), length.out = 100)
preds <- predict(lm1, newdata = list(sorttime = s), interval = 'confidence')

lines(s, preds[ ,1], col = "red")
lines(s, preds[ ,3], lty = 'dashed', col = 'blue')
lines(s, preds[ ,2], lty = 'dashed', col = 'blue')

#--

factortime2 <- with(dat, tapply(timespeedtrunc, abs.trajratiotrunc., mean))
sorttime2 <- sort(unique(dat$abs.trajratiotrunc))


plot(sorttime2, factortime2)

lm2 <- lm(factortime2 ~ I(sorttime2))
summary(lm2)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.26183 -0.04244 -0.00522  0.02817  0.51288 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.540534   0.005435  99.450   <2e-16 ***
#   I(sorttime2) -0.016951   0.018006  -0.941    0.347    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.07691 on 457 degrees of freedom
# Multiple R-squared:  0.001936,	Adjusted R-squared:  -0.0002484 
# F-statistic: 0.8862 on 1 and 457 DF,  p-value: 0.347

s = seq(from = 0, to = max(sorttime2), length.out = 100)
preds <- predict(lm2, newdata = list(sorttime2 = s), interval = 'confidence')

lines(s, preds[ ,1], col = "red")
lines(s, preds[ ,3], lty = 'dashed', col = 'blue')
lines(s, preds[ ,2], lty = 'dashed', col = 'blue')
