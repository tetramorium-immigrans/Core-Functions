##THESIS SCRIPTS - RATIO OF INBOUND/OUTBOUND VS SPEED##
#Note: assumes that 01_Quality.R has been run first

#Calculate the mean trajectory speeds and x-velocities
trajspeed <- unlist(pbsapply(1:length(massdat.0), function(z){
  antspeed(massdat.0[[z]], datreturn = TRUE, graph = FALSE)
})) 




ioratio <- unlist(pbsapply(1:length(massdat.0), function(z){
  anttort(massdat.0[[z]], datreturn = TRUE, graph = FALSE)
})) 

#Put into data frame
dat <- data.frame(trajspeed, trajt)

plot(dat$trajspeed, dat$trajt) #

lm1 <- lm(trajt ~ I(trajspeed))
summary(lm1)

lm2 <- lm(trajspeed ~ I(trajt))
summary(lm2)

#