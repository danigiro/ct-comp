#Clear all workspace
rm(list = ls(all = TRUE))
libs <- c("thief","data.table","progress","doParallel", "hts")
invisible(lapply(libs, library, character.only = TRUE))

#######################################################################
# global variables
#######################################################################
Tm.5 <- seq(as.POSIXct("2006-01-01 00:00:00"), as.POSIXct("2006-12-31 23:55:00"), by = 60*5)
Tm.60 <- seq(as.POSIXct("2006-01-01 00:00:00"), as.POSIXct("2006-12-31 23:55:00"), by = 60*60) + 1800
train.days <- 14
m = 24
forecast.horizon = 2 # h on the most aggregated series
#######################################################################

#######################################################################
# Compute the number of cores to use in parallel execution
#######################################################################
ncores = 12
cat("Cores that we will use:", ncores,"\n\n\n\n")

meas <- as.data.frame(fread("./raw/meas.csv", header = TRUE, sep = ","))
level1 <- max(as.numeric(substr(colnames(meas), 1, 2))) #total no. of level1 series
level2 <- rle(as.numeric(substr(colnames(meas), 1, 2)))$length
nodes <- list(level1, level2)
temp <- ts(meas)
xx <- hts(temp, nodes, characters = c(2,4))

#################################################################################
#forecast with ETS
#################################################################################
L01 <- aggts(xx, levels = c(0, 1)) #extract L0 and L1 time series, a total of 6 in this case
ptm <- proc.time()

stations = 1:ncol(L01)

dir.create("./results/Results_Time", recursive = TRUE, showWarnings = FALSE)

registerDoParallel(cores=ncores)
results <- foreach(st=stations) %dopar% {
  
  Y <- ts(L01[,st], freq = m)
  
  k.v <- c(1,2,3,4,6,8,12,24)
  k.s <- sum(k.v)
  Mk.v <- m/k.v
  Y.hat <- matrix(NA,k.s * forecast.horizon,365-train.days-forecast.horizon+1)
  Y.obs <- matrix(NA,m*forecast.horizon,365-train.days-forecast.horizon+1)
  Res.insamp <- vector(mode = "list", length = 365-train.days-forecast.horizon+1)
  for(i in 1:(365-train.days-forecast.horizon+1))
    #for(i in 1:10) # Only for testing purposes.
  {
    #  print(i)
    y <- window(Y, start = c(i,1), end = c(i+train.days-1,24))
    yk <- thief::tsaggregates(y, m = m, align = "end")
    frc <- thief:::th.forecast(aggy = yk, h = m * forecast.horizon, usemodel = "ets", forecastfunction = NULL)
    frc$mse[1] <- mean(frc$residuals[[1]]^2)
    
    for(h in 1:forecast.horizon){
      tmp <- NULL
      for (k in 1:length(k.v)){
        tmp <- c(tmp,frc$forecast[[length(k.v)-k+1]][(Mk.v[length(k.v)-k+1] * (h-1)+1):(Mk.v[length(k.v)-k+1] * h)])
      }
      Y.hat[(k.s*(h-1)+1):(k.s*h),i] <- tmp # The i-th column of Y.hat is the vector in eq. (9)
      # of "Reconciling solar forecasts: Temporal hierarchy" by Yang et al. (2017).
      # The first k.s elements are for h=1, the next k.s for h=2, etc.
      Y.obs[,i] <- window(Y, start = c(i+train.days,1), end = c(i+train.days+forecast.horizon-1,24))
      # The i-th column of Y.obs contains the m*forecast.horizon observations
      # for the m bottom level-series in the i-th forecast period. In other words, these
      # are the observed values for the bottom-level series contained (together with
      # higher level series) in Y.hat[,i].
    }
    
    Res.insamp[[i]] <- frc$residuals # The i-th element in the list Res.insamp contains the in-sample residuals
    # (observed - fitted) for all the series in the hierarchy in the i-th training
    # period. For example, there will be m*train.days residuals (here 24*14=336)
    # for the bottom-level series.
  }
  
  file.name <- paste("Results_Time", colnames(L01)[st], "--", 0, "--Baseforec.RData", sep = "")
  save(Y.hat,Y.obs,Res.insamp, file = file.name)
}

pred <- as.data.frame(fread("./raw/pred.csv", header = TRUE, sep = ","))
ptm <- proc.time()

stations = 1:ncol(meas)

registerDoParallel(cores=ncores)
results <- foreach(st=stations) %dopar% {

Y <- ts(meas[,st], freq = m)
NWP <- ts(pred[,st], freq = m)

Y.tilde.struc = Y.tilde.ols = Y.tilde.mse <- ts(rep(NA, length(Y)), freq = m)
k.v <- c(1,2,3,4,6,8,12,24)
k.s <- sum(k.v)
Mk.v <- m/k.v
Y.hat <- matrix(NA,k.s * forecast.horizon,365-train.days-forecast.horizon+1)
Y.obs <- matrix(NA,m*forecast.horizon,365-train.days-forecast.horizon+1)
Res.insamp <- vector(mode = "list", length = 365-train.days-forecast.horizon+1)
for(i in 1:(365-train.days-forecast.horizon+1))
{
#  print(i)
  y <- window(Y, start = c(i,1), end = c(i+train.days-1,24))
  yk <- tsaggregates(y, m = m, align = "end")
  frc <- thief:::th.forecast(aggy = yk, h = m * forecast.horizon, usemodel = "ets", forecastfunction = NULL)
  #replace the most disaggregated level forecasts with NWP results from 3Tier
  frc$forecast[[1]] <- window(NWP, start = c(i+train.days, 1), end = c(i+train.days+forecast.horizon-1,24))
  frc$residuals[[1]] <- window(NWP, start = c(i,1), end = c(i+train.days-1,24)) - y
  frc$mse[1] <- mean(frc$residuals[[1]]^2)

  for(h in 1:forecast.horizon){
    tmp <- NULL
    for (k in 1:length(k.v)){
      tmp <- c(tmp,frc$forecast[[length(k.v)-k+1]][(Mk.v[length(k.v)-k+1] * (h-1)+1):(Mk.v[length(k.v)-k+1] * h)])
    }
    Y.hat[(k.s*(h-1)+1):(k.s*h),i] <- tmp 
# The i-th column of Y.hat is the vector in eq. (9)
# of "Reconciling solar forecasts: Temporal hierarchy" by Yang et al. (2017).
# The first k.s elements are for h=1, the next k.s for h=2, etc.

    Y.obs[,i] <- window(Y, start = c(i+train.days,1), end = c(i+train.days+forecast.horizon-1,24))
# The i-th column of Y.obs contains the m*forecast.horizon observations
# for the m bottom level-series in the i-th forecast period. In other words, these
# are the observed values for the bottom-level series contained (together with
# higher level series) in Y.hat[,i].
  }

  Res.insamp[[i]] <- frc$residuals 
# The i-th element in the list Res.insamp contains the in-sample residuals
# (observed - fitted) for all the series in the hierarchy in the i-th training
# period. For example, there will be m*train.days residuals (here 24*14=336)
# for the bottom-level series.

}

proc.time() - ptm

file.name <- paste("Results_Time", names(meas)[st], "--", st, "--Baseforec.RData", sep = "")
save(Y.hat,Y.obs,Res.insamp, file = file.name)
}
