#aic

aic <- function(inputdat, delta = 2, fn = "linear",
                graph = FALSE, graphall = FALSE){
  x = unlist(inputdat[1])
  y = unlist(inputdat[2])
  
  #dat <- data.frame(x = inputdat[1], y = inputdat[2])
  
  lmlist <- list()
  
  if(fn == "linear"){
    lmlist[[1]] <- lm(y ~ x)
    lmlist[[2]] <- lm(y ~ x + I(x^2))
    lmlist[[3]] <- lm(y ~ x + I(x^2) + I(x^3))
    lmlist[[4]] <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4))
    lmlist[[5]] <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5))
    lmlist[[6]] <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6))
    lmlist[[7]] <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7))
    lmlist[[8]] <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8))
    lmlist[[9]] <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9))
  }
  
  if(fn == "logarithmic"){
    lmlist[[1]] <- lm(log(y) ~ x)
    lmlist[[2]] <- lm(log(y) ~ x + I(x^2))
    lmlist[[3]] <- lm(log(y) ~ x + I(x^2) + I(x^3))
    lmlist[[4]] <- lm(log(y) ~ x + I(x^2) + I(x^3) + I(x^4))
    lmlist[[5]] <- lm(log(y) ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5))
    lmlist[[6]] <- lm(log(y) ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6))
    lmlist[[7]] <- lm(log(y) ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7))
    lmlist[[8]] <- lm(log(y) ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8))
    lmlist[[9]] <- lm(log(y) ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9))
  }
  
  # lmlist[[1]] <- lm(y ~ x, dat)
  # lmlist[[2]] <- lm(y ~ x + I(x^2), dat)
  # lmlist[[3]] <- lm(y ~ x + I(x^2) + I(x^3), dat)
  # lmlist[[4]] <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4), dat)
  # lmlist[[5]] <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5), dat)
  # lmlist[[6]] <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6), dat)
  # lmlist[[7]] <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7), dat)
  # lmlist[[8]] <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8), dat)
  # lmlist[[9]] <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9), dat)
  
  aics <- aictab(cand.set = lmlist, modnames = c("x^1", "x^2","x^3","x^4","x^5","x^6","x^7","x^8","x^9"))
  
  if(graph == FALSE & graphall == FALSE){return(aics)}
  
  # bestlist <- lmlist[[
  #   aics$K[which(aics$Delta_AICc < delta)] - 2
  # ]]
  
  if(graph == TRUE | graphall == TRUE){
    x0 <- seq(min(dat$x), max(dat$x), length = 100)
    
    if(graphall == FALSE){
      bestlist <- aics$K[which(aics$Delta_AICc < delta)] - 2
      print(bestlist)
      
      for(i in 1:length(bestlist)){
        #lines(x0, predict.lm(lmlist[bestlist][[i]], newdata = data.frame(x = x0)), col = i + 1)
        ytemp <- predict.lm(lmlist[bestlist][[i]], newdata = data.frame(x = x0))
        lines(x0, ytemp, col = i + 1)
        
      }
    }else{
      for(i in 1:length(lmlist)){
        lines(x0, predict.lm(lmlist[[i]], newdata = data.frame(x = x0)), col = i + 1)
      }
    }
    
    # y1 <- predict.lm(lmlist[[1]], newdata = data.frame(x = x0))
    # y2 <- predict.lm(lmlist[[2]], newdata = data.frame(x = x0))
    # y3 <- predict.lm(lmlist[[3]], newdata = data.frame(x = x0))
    # y4 <- predict.lm(lmlist[[4]], newdata = data.frame(x = x0))
    # y5 <- predict.lm(lmlist[[5]], newdata = data.frame(x = x0))
    # y6 <- predict.lm(lmlist[[6]], newdata = data.frame(x = x0))
    # y7 <- predict.lm(lmlist[[7]], newdata = data.frame(x = x0))
    # y8 <- predict.lm(lmlist[[8]], newdata = data.frame(x = x0))
    # y9 <- predict.lm(lmlist[[9]], newdata = data.frame(x = x0))
    # 
    # lines(x0, y1, col = 2)
    # lines(x0, y2, col = 3)
    # lines(x0, y3, col = 4)
    # lines(x0, y4, col = 5)
    # lines(x0, y5, col = 6)
    # lines(x0, y6, col = 7)
    # lines(x0, y7, col = 8)
    # lines(x0, y8, col = 9)
    # lines(x0, y9, col = 10)
  }
  
}

#for which delta under a given value return the K
#consult the model list for which models those go to
#graph those
#add confidence interval around the highest
#legend reflects this graphing