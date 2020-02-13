library(quantmod)
#Step 1: Obtaining US Treasury Yields for Key Maturities
t3mo <- getSymbols('DGS3MO',src='FRED', auto.assign = FALSE)
t6mo <- getSymbols('DGS6MO',src='FRED', auto.assign = FALSE)
t1yr <- getSymbols('DGS1',src='FRED', auto.assign = FALSE)
t2yr <- getSymbols('DGS2',src='FRED', auto.assign = FALSE)
t3yr <- getSymbols('DGS3',src='FRED', auto.assign = FALSE)
t5yr <- getSymbols('DGS5',src='FRED', auto.assign = FALSE)
t7yr <- getSymbols('DGS7',src='FRED', auto.assign = FALSE)
t10yr <- getSymbols('DGS10',src='FRED', auto.assign = FALSE)
t20yr <- getSymbols('DGS20',src='FRED', auto.assign = FALSE)
t30yr <- getSymbols('DGS30',src='FRED', auto.assign = FALSE)

#Step 2: CombineYield Data into One Data Object and Subset Data to Only Include Yields from 1990 to 2013

treasury<-t3mo
treasury<-merge(treasury,t6mo)
treasury<-merge(treasury,t1yr)
treasury<-merge(treasury,t2yr)
treasury<-merge(treasury,t3yr)
treasury<-merge(treasury,t5yr)
treasury<-merge(treasury,t7yr)
treasury<-merge(treasury,t10yr)
treasury<-merge(treasury,t20yr)
treasury<-merge(treasury,t30yr)
extreme<-subset(treasury, index(treasury) >= "1990-01-01" & index(treasury) <= "2016-12-31")
extreme<-na.omit(extreme[,c(1,8,10)])
head(extreme)
tail(extreme)

#Step 3: Identify Examples of Different Shapes of the Yield Curve
extreme$sign.diff<-extreme$DGS30-extreme$DGS3MO
extreme$inverted<-ifelse(extreme$sign.diff==min(extreme$sign.diff),1,0)
inverted<-subset(extreme,extreme$inverted==1)
inverted

extreme$upward<-ifelse(extreme$sign.diff==max(extreme$sign.diff),1,0)
upward<-subset(extreme,extreme$upward==1)
upward

extreme$abs.diff<-abs(extreme$DGS30-extreme$DGS3MO)
extreme$flat<-ifelse(extreme$abs.diff==min(extreme$abs.diff),1,0)
flat<-subset(extreme,extreme$flat==1)
flat$abs.diff2<-abs(flat$DGS30-flat$DGS10)
flat$flat2<-ifelse(flat$abs.diff2==min(flat$abs.diff2),1,0)
flat[,c(-4:-6)]

#Step 4: Extract the Yield Curve on Dates Selected and on December 31, 2016
invert.date<-as.Date("2000-11-24")
normal.date<-as.Date("2010-01-11")
flat.date<-as.Date("2006-03-02")
current.date<-as.Date("2016-12-30")
tyld.curve <- subset(treasury, index(treasury) == invert.date | index(treasury) == flat.date |
                       index(treasury) == normal.date | index(treasury) == current.date)
tyld.curve

#Step 5: Prepare Data for Plotting
tyld.curve<-t(tyld.curve)
rownames(tyld.curve)<-paste(c(0.25,0.5,1,2,3,5,7,10,20,30))
colnames(tyld.curve)<-paste(c("inverted","flat","normal","current"))
tyld.curve

#Step 6:Plot the Yield Curve on the Four Dates Selected
TTM <- c(0.25,0.5,1,2,3,5,7,10,20,30)
y.range<-range(tyld.curve)
plot(x=TTM,y=tyld.curve[,1],type="o",ylim=y.range,
     xlab="Time to Maturity",ylab="Yield (Percent)",
     col="gray60",
     lwd=2,
     main="Shapes of the Treasury Yield Curve")
lines(x=TTM,y=tyld.curve[,2],type="o",lty=3,col="black")
lines(x=TTM,y=tyld.curve[,3],type="o",col="black",lwd=3)
lines(x=TTM,y=tyld.curve[,4],type="o",lty=3,col="gray40",lwd=2)
legend("bottomright",c("Inverted (11/24/2000)",
                       "Flat (08/26/2006)","Normal (01/11/2010)",
                       "December 31, 2013"),
       col=c("gray60","black","black","gray40"),
       lty=c(1,3,1,3),
       lwd=c(2,1,3,2))

####################################################
#####Slope of the US Treasury Yield Curve ##########
####################################################

#Step 1: Obtain Data for Three Month and 30-Year Treasury

slope<-t3mo
slope<-merge(t3mo,t30yr)
slope<-subset(slope, index(slope)>= "2005-01-01" & index(slope)<= "2016-12-31")
slope<-na.omit(slope)
head(slope)

#Step 2: Plot the Raw Yield Data
y.range<-range(slope)
plot(x=index(slope), xlab="Date", y=slope$DGS30, ylab="Yield (Percent)", ylim=y.range,
     type="l", col="black", lty=1, lwd=1, main="Yields on 3-Month and 30-Year Treasuries (2007-2016)")
shade<-par("usr")
rect(as.Date("2007-12-01"), shade[2], as.Date("2009-06-01"), shade[3], col="gray60", lty=0)
box(which="plot",lty=1)
lines(x=index(slope),y=slope$DGS30)
lines(x=index(slope),y=slope$DGS3MO,lty=2,col="blue")
legend("topright", c("3-Month Treasury","30-Year Treasury"),col=c("black","blue"),  lty=c(2,1))

#the last recession was when we see short-term rates drop significantly from over 5% to close to zero. These rates have not recovered through the end of 2013. The long-term rates were also volatile during this period, dropping from a high of approximately 5% down to approximately 2.5%twice: once during the recession and another during 2012-2013.
# slope of the yield curve has steepened since the last recession

#Step 3: Calculate the Slope of the Yield Curve
slope$slope<-(slope$DGS30-slope$DGS3MO)*100
head(slope)

#Step 4: Plot the Slope of theYield Curve

plot(x=index(slope), xlab="Date", y=slope$slope, ylab="Spread (bps)", type="l", lty=1, lwd=1,
     main="Slope of the US Treasury Yield Curve from 2007 to 2016")
shade<-par("usr")
rect(as.Date("2007-12-01"), shade[2], as.Date("2009-06-01"), shade[3], col="gray60", lty=0)
box(which="plot",lty=1)
abline(h=0,lty=1)
lines(x=index(slope),y=slope$slope,lty=1,lwd=1)

#by the end of 2010, real yields onTreasuries were negative, which can be an indication of the severe risk aversion and flight to safety that was observed after the 2008/2009 crisis. Only in 2013 did real yields break the declining trend and end the year at a positive level




######################################################################################
################# Bond Valuation on Coupon Payment Dates #############################
######################################################################################

# Bond Valuation on Coupon Payment Dates
# Pricing Vanilla Bonds(a bond with a fixed rate, fixed maturity, and no embedded options) with Known Yield-to-Maturity
# Step 1: Setup Variables for Bond Characteristics

#The intrinsic value of a bond is the present value of the coupon and principal payments bondholders expect to receive from owning the bond discounted at the appropriate risk-adjusted discount rate.

#Total Capital SA Medium Term Notes (TOTAL MTNs)
coupon=0.0425 # pays annual coupons with an annual rate of 4.25 %.
maturity=4
yield=0.017 #the price for this TOTAL MTN was GBP1097 and its yield was 1.7 %
par=1000 #we will refer to the price or value of a bond in relation to the $ 1000 par value
coupon.freq=1

#Step 2: Create Variables Accounting for the Periodicity of the Coupon Payments
periods=maturity*coupon.freq
periods

coupon.period=coupon/coupon.freq
coupon.period

yield.period=yield/coupon.freq
yield.period


#Step 3: Construct a Vector of Applicable Per Period Coupon Rates

bond.coupon<-rep(coupon.period,times=periods, length.out=NA,each=1)
bond.df<-as.data.frame(bond.coupon)
bond.df

#Step 4: Calculate Periodic Cash Flows, Yield to Maturity, and Present Value of Each Period's Cash Flows
bond.df$cf<-coupon.period*par
bond.df$cf[periods]=bond.df$cf[periods]+par
bond.df$ytm<-yield.period
bond.df$period<-c(1:periods)
bond.df$pv.cf<-bond.df$cf/((1+bond.df$ytm)^bond.df$period)
bond.df

#Step 5: CalculateValue of Bond by Summing the PresentValue of Each Period's Cash Flows
value=sum(bond.df$pv.cf)
value

########################################################
####### Vanilla Bond Pricing Function ##################
########################################################

bondprc<-function(coupon,maturity,yield,par,coupon.freq){
  periods=maturity*coupon.freq
  coupon.period=coupon/coupon.freq
  yield.period=yield/coupon.freq
  bond.coupon<-rep(coupon.period,times=periods,length.out=NA,each=1)
  bond.df<-as.data.frame(bond.coupon)
  for (i in 1:periods) {
    bond.df$cf[i]=par*coupon.period
    bond.df$period[i]=i
    bond.df$yield[i]=yield.period
  }
  bond.df$cf[periods]=bond.df$cf[periods]+par
  bond.df$PV=bond.df$cf/((1+bond.df$yield)^bond.df$period)
  value=sum(bond.df$PV)
  value
}

bondprc(coupon,maturity,yield,par,coupon.freq)


################################################################
###### Finding Bond Yield-to-Maturity with Known Price #########
################################################################

## In this section, we will estimate the yield to maturity of the bond when we know the market price of the bond.

# Step 1: Create a Vector of the Bond's Cash Flows, with the Purchase Price as the First Observation and as a Negative Number
bond.cf<-c(-value, rep(coupon.period*par,periods-1), par+coupon.period*par)
bond.cf  # atleasr one sign change

# Step 2: Create Functions to Iteratively Find the Yield to Maturity
bondval<-function(i,bond.cf,t=seq(along=bond.cf)) {
  sum(bond.cf/(1+i)^t)
}

bond.ytm <- function(bond.cf) { 
  uniroot(bondval,c(0,1), bond.cf=bond.cf)$root
}

# Step 3: Annualize the Periodic Yield to Maturity Calculated Above

YTM<-bond.ytm(bond.cf)*coupon.freq
YTM

###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
#################################################### Bond Valuation function ##############################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################


bondprc<-function(coupon,maturity,yield,par,coupon.freq){
  periods=maturity*coupon.freq
  coupon.period=coupon/coupon.freq
  yield.period=yield/coupon.freq
  bond.coupon<-rep(coupon.period,times=periods,length.out=NA,each=1)
  bond.df<-as.data.frame(bond.coupon)
  for (i in 1:periods) {
    bond.df$cf[i]=par*coupon.period
    bond.df$period[i]=i
    bond.df$yield[i]=yield.period
  }
  bond.df$cf[periods]=bond.df$cf[periods]+par
  bond.df$PV=bond.df$cf/((1+bond.df$yield)^bond.df$period)
  value=sum(bond.df$PV)
  value
}

#Calculating the value of a bond
coupon=0.0425
maturity=4
yield=0.017
par=1000
coupon.freq=1

price <- bondprc(coupon,maturity,yield,par,coupon.freq)
price


###Bond Price volatility
coupon=.10
maturity=20
yield=seq(from = .15, to = 0, by = -0.01)
par=100
coupon.freq=1


pv <- c()

for (i in 1:length(yield)){
  pv <- c(pv, bondprc(coupon,maturity,yield[i],par,coupon.freq))
}

plot(yield, pv, type = "l", col = "red")

###Size of yield change
###Small changes in the yield have symmetric effect on the bond prices. Let us see how change in .01% changes the value
coupon=.10
maturity=20
yield=0.10
par=100
coupon.freq=1

new_yield_1 <- 0.101
new_yield_2 <- 0.099

bondprc(coupon,maturity,new_yield_1,par,coupon.freq)/bondprc(coupon,maturity,yield,par,coupon.freq) - 1

bondprc(coupon,maturity,new_yield_2,par,coupon.freq)/bondprc(coupon,maturity,yield,par,coupon.freq) - 1

###Large change Asymmetric effect
coupon=.10
maturity=20
yield=0.10
par=100
coupon.freq=1

new_yield_1 <- 0.14
new_yield_2 <- 0.06

bondprc(coupon,maturity,new_yield_1,par,coupon.freq)/bondprc(coupon,maturity,yield,par,coupon.freq) - 1

bondprc(coupon,maturity,new_yield_2,par,coupon.freq)/bondprc(coupon,maturity,yield,par,coupon.freq) - 1

####Coupoun Rate vs Bond value
#Fixing the time to maturity andd yield, bond prices volatility is higher if the coupoun rate is lower
coupon=.10
maturity=20
yield=0.10
par=100
coupon.freq=1


bondprc(.10,maturity,.08,par,coupon.freq)/bondprc(.10 ,maturity,.10,par,coupon.freq) - 1
bondprc(.05,maturity,.08,par,coupon.freq)/bondprc(.05 ,maturity,.10,par,coupon.freq) - 1
bondprc(.00,maturity,.08,par,coupon.freq)/bondprc(.00 ,maturity,.10,par,coupon.freq) - 1


####Time to Maturity vs Bond value
coupon=.10
maturity=20
yield=0.10
par=100
coupon.freq=1


bondprc(coupon, 20, .08, par, coupon.freq)/bondprc(coupon, 20, .10, par, coupon.freq) - 1
bondprc(coupon, 10, .08, par, coupon.freq)/bondprc(coupon, 10, .10, par, coupon.freq) - 1
bondprc(coupon, 5, .08, par, coupon.freq)/bondprc(coupon, 5, .10, par, coupon.freq) - 1

###Price value of a basis point "PV01"
#This is equals to price of a bond if the required yield changes by .01%
coupon=.10
maturity=20
yield=0.10
par=100
coupon.freq=1


bondprc(coupon, maturity, .10, par, coupon.freq)
bondprc(coupon, maturity, .1001, par, coupon.freq)
PV01 <- abs(bondprc(coupon, maturity, .10, par, coupon.freq) -  bondprc(coupon, maturity, .1001, par, coupon.freq))
PV01

####Duration
coupon=.10
maturity=20
yield=0.10
par=100
coupon.freq=1


p <- bondprc(coupon, maturity, yield, par, coupon.freq)
p_up <- bondprc(coupon, maturity, yield + .01, par, coupon.freq)
p_down <- bondprc(coupon, maturity, yield - .01, par, coupon.freq)

duration <- (p_down - p_up)/ (2 * p * .01)
duration
duration_percentage_change <- -duration * .01
duration_percentage_change
duration_dollar_change <- duration_percentage_change * p
duration_dollar_change

###Convexity
coupon=.05
maturity=10
yield=0.04
par=100
coupon.freq=1


p <- bondprc(coupon, maturity, yield, par, coupon.freq)
p_up <- bondprc(coupon, maturity, yield + .01, par, coupon.freq)
p_down <- bondprc(coupon, maturity, yield - .01, par, coupon.freq)

convexity <- (p_up + p_down - 2 * p) / (p * (.01^2))
convexity

convexity_percentage_change <- 0.5 * convexity * .01^2
convexity_percentage_change

convexity_dollar_change <- convexity_percentage_change * p
convexity_dollar_change

####Combining both duration and convexity
p
duration_dollar_change
convexity_dollar_change
price_change <- duration_dollar_change + convexity_dollar_change

price <- duration_dollar_change + convexity_dollar_change + p
price