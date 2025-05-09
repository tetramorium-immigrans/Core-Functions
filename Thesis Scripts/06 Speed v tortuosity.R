##THESIS SCRIPTS - TRAJECTORY INVERSE TORTUOSITY VS TRAJECTORY MEAN SPEED##
#Note: assumes that 01_Quality.R has been run first

#Calculate the mean trajectory speeds and x-velocities
trajspeed <- unlist(pbsapply(1:length(massdat.0), function(z){
  antspeed(massdat.0[[z]], datreturn = TRUE, graph = FALSE)
})) 

trajt <- unlist(pbsapply(1:length(massdat.0), function(z){
  anttort(massdat.0[[z]], datreturn = TRUE, graph = FALSE)
})) 

#Put into data frame
dat <- data.frame(trajspeed, trajt)


lm1 <- lm(trajspeed ~ I(trajt))
summary(lm1)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.51239 -0.15090 -0.01195  0.13993  0.93549 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.543505   0.001262  430.64   <2e-16 ***
#   I(trajt)    0.090549   0.004046   22.38   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1989 on 61028 degrees of freedom
# Multiple R-squared:  0.00814,	Adjusted R-squared:  0.008123 
# F-statistic: 500.8 on 1 and 61028 DF,  p-value: < 2.2e-16

#Looking like no relationship, which is actually kind of interesting.  How much they're turning has little effect on speed

lmnls <- nls(trajt ~ inter + slope*(trajspeed^power), start = list(inter = lm1$coefficients[1], slope = lm1$coefficients[2], power = 1))
summary(lmnls)

# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# inter  0.36200    0.04704   7.696 1.42e-14 ***
#   slope -0.09330    0.04542  -2.054  0.03999 *  
#   power -0.39697    0.14383  -2.760  0.00578 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.198 on 61027 degrees of freedom
# 
# Number of iterations to convergence: 12 
# Achieved convergence tolerance: 6.294e-06

plot(dat$trajspeed, dat$trajt, xlab = "Mean speed of trajectories", ylab = "Mean inverse tortuosity of trajectories")

lm0 <- lm(abs(trajx) ~ I(trajspeed^-0.39697))

summary(lm0)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.21611 -0.09467 -0.03247  0.06525  0.61096 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            0.422062   0.003223  130.95   <2e-16 ***
#   I(trajspeed^-0.39697) -0.229963   0.002437  -94.34   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.128 on 61028 degrees of freedom
# Multiple R-squared:  0.1273,	Adjusted R-squared:  0.1273 
# F-statistic:  8901 on 1 and 61028 DF,  p-value: < 2.2e-16

s = seq(from = 0, to = max(trajspeed), length.out = 100)
preds <- predict(lm0, newdata = list(trajspeed = s), interval = 'confidence')

lines(s, preds[ ,1], col = "red")
lines(s, preds[ ,3], lty = 'dashed', col = 'blue')
lines(s, preds[ ,2], lty = 'dashed', col = 'blue')

