#install.packages('quantmod')
#install.packages('PerformanceAnalytics')
require(quantmod)
require(PerformanceAnalytics)
library(partitions)
library(iterpc)

getSymbols(c("CWB","JNK","TLT","PCY"), from="2009-08-03", to="2017-03-30")
returns <- merge(Return.calculate(Ad(CWB)), Return.calculate(Ad(JNK)), join='inner')
returns <- merge(returns, Return.calculate(Ad(TLT)), join='inner')
returns <- merge(returns, Return.calculate(Ad(PCY)), join='inner')

#getSymbols(c("CSD", "VNQ"), from="2009-08-03", to="2017-03-30")
#df <- read.csv("C:/Users/Russ Fischer/Documents/R/my-projects/walk-fwd/rodenbach_returns.csv", header=TRUE)
#df2 <- read.csv("C:/Users/Russ Fischer/Documents/R/my-projects/walk-fwd/bondStrat.csv", header=TRUE)
#returns <- merge(Return.calculate(Ad(CSD)), Return.calculate(Ad(VNQ)), join='inner')
#returns <- merge(returns, (df$Return), join='inner')
#returns <- merge(returns, (df2$Return), join='inner')

returns <- returns[-1,]
configs <- list()

C = t(restrictedparts(10,4))/10
B <- do.call(rbind, lapply(1:nrow(C),function(i) getall(iterpc(table(C[i,]), order=T))))

for(i in 1:286) {
  weight_1 <- B[i,1]
  weight_2 <- B[i,2]
  weight_3 <- B[i,3]
  weight_4 <- B[i,4]
  config <- Return.portfolio(R = returns, weights=c(weight_1,weight_2,weight_3,weight_4), rebalance_on = "months")
  configs[[i]] <- config  
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
charts.PerformanceSummary(stratRets[-1:-1200,], ylog=TRUE)

# print perf stats
rbind(table.AnnualizedReturns(stratRets), maxDrawdown(stratRets))

# compare perf to SPY TLT alone
stratAndComponents <- merge(returns, stratRets, join='inner')
charts.PerformanceSummary(stratAndComponents[-1:-1200,], ylog=TRUE)
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

