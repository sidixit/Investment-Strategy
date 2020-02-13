library(quantmod)
library(PerformanceAnalytics)

data.VISA <- getSymbols("V", from = "2014-12-31", to = "2016-12-31", auto.assign = FALSE)
data.VISA <- to.monthly(data.VISA)
ret.VISA <- Return.calculate(data.VISA$data.VISA.Adjusted)


data.UNH <- getSymbols("UNH", from = "2014-12-31", to = "2016-12-31", auto.assign = FALSE)
data.UNH <- to.monthly(data.UNH)
ret.UNH <- Return.calculate(data.UNH$data.UNH.Adjusted)



portfolio <- cbind(ret.VISA, ret.UNH)
portfolio <- portfolio[-1,]
portfolio.ret <- Return.portfolio(portfolio, weights = c(.5,0.5), rebalance_on = "months" )

port.csv <- cbind(index(portfolio.ret), data.frame(portfolio.ret))
names(port.csv) <- c("date", "port.ret")
row.names(port.csv) <- seq(1, nrow(port.csv), by = 1)
write.csv(port.csv, "C:/users/tchan/Desktop/FIS/Project/Hypothetical_Portfolio2(Monthly).csv", row.names = F)

