##### Supplemental R script for,
# Chapin KJ, Peixoto PEC, Briffa M. 2019. Further mismeasures of animal contests: a new framework 
#    for assessment strategies. Behavioral Ecology doi:10.1093/beheco/arz081

##### DATA GENERATION
runnum <- 100                 # number of triads
error <- 0.25                 # error rate
ratio<-0.1                    # proportion of individuals adopting mutual assessment
number.MA<-ratio*runnum       # number of individuals adopting mutual assessment
number.SA<-(runnum-number.MA) # number of individuals adopting self assessment

### Define RHP distributions for three groups of individuals
rhp <- data.frame(rnorm(runnum, 0, 1), rnorm(runnum, 0, 1), rnorm(runnum, 0, 1)) # randomly generate RHP values
rhp <- abs(min(rhp)) + rhp                                                       # remove negative values

### Formation of each triad of individuals
rhp <- t(apply(rhp, 1, sort))                                                 # sort so smallest is the focal
colnames(rhp) <- c("loser.RHP", "winner1.RHP", "winner2.RHP")                 # name the columns
strategy<-sample(c(rep('mutual', number.MA), rep('self', number.SA)), runnum) # define the proportion of individuals adopting SA and MA in the population
da <- data.frame(rhp, strategy)                                               # randomly assign assessment strategies to each individual

### Calculate contest duration for each pair in each triad
da$duration1 <- NA; da$duration2 <- NA                
for(ii in 1:length(da[,1])){      # for all simulations...
  if(da$strategy[ii] == "self"){  # if the strategy is "self"
    da$duration1[ii] <- da$loser.RHP[ii] + sample(seq((-1*error), error, 0.001), 1) 
    da$duration2[ii] <- da$loser.RHP[ii] + sample(seq((-1*error), error, 0.001), 1) 
  }
  if(da$strategy[ii] == "mutual"){ # if the strategy is "mutual"
    da$duration1[ii] <- abs(da$loser.RHP[ii] - da$winner1.RHP[ii])+sample(seq((-1*error), error, 0.001), 1)
    da$duration2[ii] <- abs(da$loser.RHP[ii] - da$winner2.RHP[ii])+sample(seq((-1*error), error, 0.001), 1)
  }
}

### RHP correction
da$duration1.1 <- NA; da$duration2.2 <- NA
for(ii in 1:length(da[,1])){ # for all simulations...
  if(da$strategy[ii] == "self"){ # if the strategy is "self"
    da$duration1.1[ii] <- da$duration1[ii]+abs(min(da$duration1, da$duration2))
    da$duration2.2[ii] <- da$duration2[ii]+abs(min(da$duration1, da$duration2))
  }
  if(da$strategy[ii] == "mutual"){ # if the strategy is "mutual"
    da$duration1.1[ii] <- max(da$duration1, da$duration2) - da$duration1[ii]
    da$duration2.2[ii] <- max(da$duration1, da$duration2) - da$duration2[ii]
  }
}
da$durdif <- (da$duration2.2 - da$duration1.1)  # contest duration difference
da$rhpdif <- abs(da$loser.RHP - da$winner1.RHP) # RHP difference for the first pair in each triad

##### PLOTTING
par(mgp=c(2,1,0), mfrow=c(2,2), mar=c(3, 3, 1, 1))

### fig 2a
plot(da$duration1.1 ~ da$rhpdif, las=1, ylab='Contest duration', xlab='RHP Difference', cex=1, pch=16, col='grey', ylim=c(0, 4))
mod1 <- lm(da$duration1.1 ~ da$rhpdif); abline(mod1, lwd=3); summary(mod1)

### fig 2b
plot(da$duration1.1 ~ da$loser.RHP, las=1, ylab = '', xlab='Loser RHP', cex=1, pch=16, col='grey', ylim=c(0, 4))
mod2 <- lm(da$duration1.1 ~ da$loser.RHP); abline(mod2, lwd=3); summary(mod2)

### fig 2c
plot(da$duration1.1 ~ da$winner1.RHP, las=1, ylab = 'Contest duration', xlab = 'Winner RHP', cex=1, pch=16, col='grey', ylim=c(0, 4))
mod3 <- lm(da$duration1.1 ~ da$winner1.RHP); abline(mod3, lwd=3); summary(mod3)

### fig 2d
da$guess <- NA; da$slopes <- NA
plot(1, ylab='', xlab='Winner RHP', type='n', las=1, xlim=c(1,6), ylim=c(0, 4))
for(ii in 1:length(da$loser.RHP)){
  x <- c(da$winner1.RHP[ii], da$winner2.RHP[ii])
  y <- c(da$duration1.1[ii], da$duration2.2[ii])
  mod4 <- lm(y ~ x)
  da$slopes[ii] <- mod4$coefficients[2]
}
m<-mean(da$slopes[da$slopes<0]); s<-sd(da$slopes[da$slopes<0]); l<-length(da$slopes[da$slopes<0])
ci<-qnorm(0.975)*s/sqrt(l); lower<-m-ci; upper<-m+ci

for(ii in 1:length(da$loser.RHP)){
  x <- c(da$winner1.RHP[ii], da$winner2.RHP[ii])
  y <- c(da$duration1.1[ii], da$duration2.2[ii])
  if(da$slopes[ii] <= upper){
    da$guess[ii] <- 'mutual'
    lines(y ~ x, col='grey', lwd=1.5)
    points(y ~ x, col='grey', pch=16, cex=1)
  }else{
    da$guess[ii] <- 'self'
    lines(y ~ x, col='black', lwd=1.5)
    points(y ~ x, col='black', pch=16, cex=1)
  }
}

### Correct Assignment Frequencies
dama<-da[da$strategy=='mutual',] # MA assignment accuracy
success.MA <- dama$strategy == dama$guess
efficiency.MA<-length(subset(success.MA, success.MA == TRUE))/length(success.MA)*100
dasa<-da[da$strategy=='self',] # SA assignment accuracy
success.SA <- dasa$strategy == dasa$guess
efficiency.SA<-length(subset(success.SA, success.SA == TRUE))/length(success.SA)*100 # calculate percentage of correct assessment strategy assignment for SA
efficiency.SA; efficiency.MA
#