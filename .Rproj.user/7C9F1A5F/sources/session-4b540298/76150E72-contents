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



lm1 <- lm(trajt ~ I(trajspeed))
summary(lm1)

lm2 <- lm(trajspeed ~ I(trajt))
summary(lm2)

#Looking like no relationship, which is actually kind of interesting.  How much they're turning has little effect on speed

lmnls <- nls(trajt ~ inter + slope*(trajspeed^power), start = list(inter = lm2$coefficients[1], slope = lm2$coefficients[2], power = 1))
summary(lmnls)

plot(dat$trajspeed, dat$trajt, xlab = "Mean speed of trajectories", ylab = "Mean inverse tortuosity of trajectories")

s = seq(from = 0, to = max(trajspeed), length.out = 100)
preds <- predict(lm1, list(trajspeed = s), interval = 'confidence')

lines(s, predict(lm1, list(trajspeed = s)), col = "red")
lines(s, preds[ ,3], lty = 'dashed', col = 'blue')
lines(s, preds[ ,2], lty = 'dashed', col = 'blue')

