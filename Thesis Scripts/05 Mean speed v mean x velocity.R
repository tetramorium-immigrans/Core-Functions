##THESIS SCRIPTS - MEAN SPEED VS. MEAN X-VELOCITY##
#Note: assumes that 01_Quality.R has been run first

#Calculate the mean trajectory speeds and x-velocities
trajspeed <- unlist(pbsapply(1:length(massdat.0), function(z){
  antspeed(massdat.0[[z]], datreturn = TRUE, graph = FALSE)
})) 

trajx <- unlist(pbsapply(1:length(massdat.0), function(z){
  antvel(massdat.0[[z]], datreturn = TRUE, graph = FALSE)
})) 

trajy <- unlist(pbsapply(1:length(massdat.0), function(z){                      #Checked y as well but it didn't have much of an appearance
  antvel(massdat.0[[z]], datreturn = TRUE, graph = FALSE, xvel = FALSE)
})) 

#Put into data frame
dat <- data.frame(trajspeed, trajx, trajy)

plot(dat$trajspeed, dat$trajx) #Arrowhead
plot(dat$trajspeed, abs(dat$trajx), xlab = "Mean speed of ants in frame", ylab = "Absolute value of mean x-velocity of ants in frame") #Okay, fixed to compare

lm1 <- lm(abs(trajx) ~ I(trajspeed))
lm2 <- lm(abs(trajx) ~ I(trajspeed^2))
lm3 <- lm(abs(trajx) ~ I(trajspeed^3))
summary(lm1)
summary(lm2)
summary(lm3)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.31063 -0.08295 -0.03059  0.05844  1.26393 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -0.023706   0.001505  -15.75   <2e-16 ***
#   I(trajspeed)  0.259693   0.002595  100.08   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1234 on 61028 degrees of freedom
# Multiple R-squared:  0.141,	Adjusted R-squared:  0.141 
# F-statistic: 1.002e+04 on 1 and 61028 DF,  p-value: < 2.2e-16

lmnls <- nls(abs(trajx) ~ inter + slope*(trajspeed^power), start = list(inter = lm2$coefficients[1], slope = lm2$coefficients[2], power = 1))
summary(lmnls)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# inter -0.0018402  0.0008255  -2.229  0.02580 *  
#   slope  0.0101753  0.0035770   2.845  0.00445 ** 
#   power  7.9049838  1.7920583   4.411 1.03e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1782 on 61027 degrees of freedom
# 
# Number of iterations to convergence: 11 
# Achieved convergence tolerance: 7.429e-06

s = seq(from = 0, to = max(trajspeed), length.out = 100)
preds <- predict(lmnls, list(trajx = s), interval = 'confidence')

lines(s, predict(lmnls, list(trajx = s)), col = "red")
lines(s, preds[ ,3], lty = 'dashed', col = 'blue')
lines(s, preds[ ,2], lty = 'dashed', col = 'blue')

#Okay, so basically no relationship.  My visual inspection agrees; ants that are headed in or out are not moving faster than those going every which way
#Caveat: I'm still iffy about how x-vel is calculated

plot(dat$trajspeed, abs(dat$trajy), xlab = "Mean speed of ants in frame", ylab = "Mean y-velocity of ants in frame") #How does y-vel look compared to x-vel?
#Flatter little porcupine of a graph.  Basically confirms that most of the movement is in the x direction rather than the y
#The even looser relationship makes me think there might be something to the x, but it's just not that reliable

