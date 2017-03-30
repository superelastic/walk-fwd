#install.packages('quantmod')
#install.packages('PerformanceAnalytics')
require(quantmod)
require(PerformanceAnalytics)
library(partitions)
library(iterpc)
#getSymbols(c("SPY", "TLT"), from="2009-01-01")
#returns <- merge(Return.calculate(Ad(SPY)), Return.calculate(Ad(TLT)), join='inner')

getSymbols(c("CSD", "EDV", "VNQ"), from="2009-01-01")
#returns <- merge(Return.calculate(Ad(CSD)), Return.calculate(Ad(EDV)), join='inner')
#returns <- merge(returns, Return.calculate(Ad(VNQ)), join='inner')

df <- read.csv("C:/Users/Russ Fischer/Documents/R/my-projects/walk-forward-kipnis/keating_microcap_returns.csv", header=TRUE)
returns <- merge(Return.calculate(Ad(CSD)), Return.calculate(Ad(EDV)), join='inner')
returns <- merge(returns, Return.calculate(Ad(VNQ)), join='inner')
returns <- merge(returns, (df$Return), join='inner')

returns <- returns[-1,]
configs <- list()
#B = matrix(
#  c(0,0,0,0,0,0,0.2,0.2,0.2,0.2,0.2,0.4,0.4,0.4,0.4,0.6,0.6,0.6,0.8,0.8,1,0,0.2,0.4,0.6,0.8,1,0,0.2,0.4,0.6,0.8,0,0.2,0.4,0.6,0,0.2,0.4,0,0.2,0,1,0.8,0.6,0.4,0.2,0,0.8,0.6,0.4,0.2,0,0.6,0.4,0.2,0,0.4,0.2,0,0.2,0,0)
#  ,
#  nrow=21,
#  ncol=3)

C = t(restrictedparts(10,4))/10
B <- do.call(rbind, lapply(1:nrow(C),function(i) getall(iterpc(table(C[i,]), order=T))))
# CSD EDV BND IVV MUB IYR IGOV

for(i in 1:286) {
  ##  cat(B[i,1],", ")
  ##  cat(B[i,2],", ")
  ##  cat(B[i,3],"\n")
  #  weightCSD <- B[i,1]
  #  weightEDV <- B[i,2]
  #  weightVMNFX <- B[i,3]
  #  config <- Return.portfolio(R = returns, weights=c(weightCSD, weightEDV, weightVMNFX), rebalance_on = "months")
  #  configs[[i]] <- config
  weightCSD <- B[i,1]
  weightEDV <- B[i,2]
  weightVNQ <- B[i,3]
  weightROD <- B[i,4]
  config <- Return.portfolio(R = returns, weights=c(weightCSD,weightEDV,weightVNQ,weightROD), rebalance_on = "months")
  configs[[i]] <- config  
  
  #  weightSPY <- (i-1)*.05
  #  weightTLT <- 1-weightSPY
  #  config <- Return.portfolio(R = returns, weights=c(weightSPY, weightTLT), rebalance_on = "months")
  #  configs[[i]] <- config
}
configs <- do.call(cbind, configs)
cumRets <- cumprod(1+configs)
period <- 72

roll72CumAnn <- (cumRets/lag(cumRets, period))^(252/period) - 1
roll72SD <- sapply(X = configs, runSD, n=period)*sqrt(252)

# creating weights
sd_f_factor <- 1.0
modSharpe <- roll72CumAnn/roll72SD^sd_f_factor
monthlyModSharpe <- modSharpe[endpoints(modSharpe, on="months"),]

findMax <- function(data) {
  return(data==max(data))
}

weights <- t(apply(monthlyModSharpe, 1, findMax))
weights <- weights*1
weights <- xts(weights, order.by=as.Date(rownames(weights)))
weights[is.na(weights)] <- 0
weights$zeroes <- 1-rowSums(weights)
configs$zeroes <- 0

# calculate performance
stratRets <- Return.portfolio(R = configs, weights = weights)
rbind(table.AnnualizedReturns(stratRets), maxDrawdown(stratRets))
charts.PerformanceSummary(stratRets[-600:-2051,], ylog=TRUE)

# print perf stats
rbind(table.AnnualizedReturns(stratRets), maxDrawdown(stratRets))

# compare perf to SPY TLT alone
stratAndComponents <- merge(returns, stratRets, join='inner')
charts.PerformanceSummary(stratAndComponents[-600:-2051,], ylog=TRUE)
rbind(table.AnnualizedReturns(stratAndComponents), maxDrawdown(stratAndComponents))

apply.yearly(stratAndComponents, Return.cumulative)

# weight of SPY in pair 
selectedPort <- apply(monthlyModSharpe, 1, which.max)
selectedPort <- do.call(rbind, selectedPort)

bestPortLabel <- apply(monthlyModSharpe, 1, which.max)
bestPortIndex <- do.call(rbind, bestPortLabel)
strats <- rbind(B[selectedPort,])
stratByMonth <- cbind(selectedPort,strats)

weightSPY <- (selectedPort-1)*.05
align <- cbind(weightSPY, stratRets)
align <- na.locf(align)
chart.TimeSeries(align[,1], date.format="%Y", ylab="Weight SPY", main="Weight of SPY in SPY-TLT pair")

