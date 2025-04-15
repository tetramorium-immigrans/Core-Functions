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
plot(dat$trajspeed, abs(dat$trajx)) #Okay, fixed to compare

lm1 <- lm(trajx ~ I(trajspeed))
summary(lm1)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.06904 -0.06951  0.00145  0.06155  1.31399 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  -0.0009299  0.0021729  -0.428    0.669
# I(trajspeed) -0.0002202  0.0037459  -0.059    0.953
# 
# Residual standard error: 0.1782 on 61028 degrees of freedom
# Multiple R-squared:  5.664e-08,	Adjusted R-squared:  -1.633e-05 
# F-statistic: 0.003457 on 1 and 61028 DF,  p-value: 0.9531

#Okay, so basically no relationship.  My visual inspection agrees; ants that are headed in or out are not moving faster than those going every which way
#Caveat: I'm still iffy about how x-vel is calculated

plot(dat$trajspeed, abs(dat$trajy)) #How does y-vel look compared to x-vel?
#Flatter little porcupine of a graph.  Basically confirms that most of the movement is in the x direction rather than the y
#The even looser relationship makes me think there might be something to the x, but it's just not that reliable