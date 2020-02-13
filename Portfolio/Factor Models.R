library(quantmod)
library(xts)
library(PerformanceAnalytics)
#Step 1: Create/Load your Porfolio Return
port <- read.csv("C:/users/tchan/Desktop/FIS/Project/Hypothetical_Portfolio2(Monthly).csv")
port$date<-as.yearmon(as.character(port$date),"%b %Y")
head(port)
tail(port)

#Step 2: Load market return
data.GSPC <- getSymbols("^GSPC", from = "2014-12-31", to = "2016-12-31", auto.assign = FALSE)
data.GSPC <- to.monthly(data.GSPC)
mkt.ret <- Return.calculate(data.GSPC$data.GSPC.Adjusted)
mkt.ret <- mkt.ret[-1,]
head(mkt.ret)

#Step 3: Load Risk Free return
rf <- read.csv("C:/Users/tchan/Desktop/FIS/Slides/Lec 6/DGS3MO.csv")
rf$date<-as.Date(rf$DATE,"%Y-%m-%d")
rf$DGS3MO<-as.numeric(as.character(rf$DGS3MO))

rf$DATE <- NULL
rf<-xts(rf$DGS3MO,order.by=rf$date)
names(rf)<-paste("DGS3MO")
rf.monthly<-to.monthly(rf)
rf.monthly<-(1+rf.monthly[,1]/100)^(1/12)-1
rf.sub<-subset(rf.monthly,
               index(rf.monthly) >= as.yearmon("Jan 2015") &
                 index(rf.monthly) <= as.yearmon("Dec 2016"))

#Step 4: Combine all the returns
combo <- cbind(data.frame(mkt.ret),data.frame(rf.sub), port$port.ret)
names(combo)<-paste(c("mkt.ret","rf","port.ret"))
head(combo)

#Step 5: Calculate excess portfolio and market return
combo$exret<-combo$port.ret - combo$rf
combo$exmkt<-combo$mkt.ret - combo$rf
head(combo)

#Step 6: Run Regression of Excess Firm Return on Excess Market Return
CAPM<-lm(exret~exmkt, data = combo)
summary(CAPM)

#Calculate Adjusted Beta
beta <- summary(CAPM)$coefficients[2]
beta
adj.beta<-(2/3)*beta+(1/3)*1
adj.beta

#Market Model
"The CAPM requires that we use expected returns and the "true" market portfolio. A more
common way to calculate beta in practice is to use the market model, because the market
model uses a market proxy without the requirement that this market proxy be the "true"
market portfolio. In addition, the market model does not require the use of a risk-free rate
and, therefore, there is no need to calculate the excess returns of the firm and the market.
That is,"

reg<-lm(port.ret~mkt.ret, data = combo)
summary(reg)

#Notice the alpha and beta for this model and compare it with the CAPM model.
beta.mktmod<-summary(reg)$coefficients[2]
beta.mktmod

adj.beta.mktmod<-(2/3)*beta.mktmod+(1/3)*1
adj.beta.mktmod


