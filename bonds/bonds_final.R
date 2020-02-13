###########################BONDS FINAL ######################################

###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
#################################################### Bond Valuation function ##############################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################

#Total Capital SA Medium Term Notes (TOTAL MTNs)

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
maturity=5
yield=0.017
par=5000
coupon.freq=1

price <- bondprc(coupon,maturity,yield,par,coupon.freq)
price #this is the final yield over 4 years

return = price - par
return

############################################
coupon=0.0425
maturity=5
yield=0.0007
par=5000
coupon.freq=1

price <- bondprc(coupon,maturity,yield,par,coupon.freq)
price #this is the final yield over 4 years

return = price - par
return
##############################################



##3month simulation
couponrate = 4.25 #annualy
couponRateMonthly = 4.25/12
couponRate3Months = couponRateMonthly*3/100


price <- bondprc(coupon,maturity,yield,par,coupon.freq)
price #this is the final yield over 4 years

monthReturn3Months = par*couponRate3Months
monthReturn3Months


#

###Bond Price volatility
#coupon=.10
#maturity=20
yield=seq(from = .15, to = 0, by = -0.01)
#par=100
#coupon.freq=1


pv <- c()

for (i in 1:length(yield)){
  pv <- c(pv, bondprc(coupon,maturity,yield[i],par,coupon.freq))
}

plot(yield, pv, type = "l", col = "red")

###Size of yield change
###Small changes in the yield have symmetric effect on the bond prices. Let us see how change in .01% changes the value
#coupon=.10
#maturity=20
#yield=0.10
#par=100
#coupon.freq=1

new_yield_1 <- 0.101
new_yield_2 <- 0.099

bondprc(coupon,maturity,new_yield_1,par,coupon.freq)/bondprc(coupon,maturity,yield,par,coupon.freq) - 1

bondprc(coupon,maturity,new_yield_2,par,coupon.freq)/bondprc(coupon,maturity,yield,par,coupon.freq) - 1

###Large change Asymmetric effect
#coupon=.10
#maturity=20
#yield=0.10
#par=100
#coupon.freq=1

new_yield_1 <- 0.14
new_yield_2 <- 0.06

bondprc(coupon,maturity,new_yield_1,par,coupon.freq)/bondprc(coupon,maturity,yield,par,coupon.freq) - 1

bondprc(coupon,maturity,new_yield_2,par,coupon.freq)/bondprc(coupon,maturity,yield,par,coupon.freq) - 1

####Coupoun Rate vs Bond value
#Fixing the time to maturity andd yield, bond prices volatility is higher if the coupoun rate is lower
#coupon=.10
#maturity=20
#yield=0.10
#par=100
#coupon.freq=1


bondprc(.10,maturity,.08,par,coupon.freq)/bondprc(.10 ,maturity,.10,par,coupon.freq) - 1
bondprc(.05,maturity,.08,par,coupon.freq)/bondprc(.05 ,maturity,.10,par,coupon.freq) - 1
bondprc(.00,maturity,.08,par,coupon.freq)/bondprc(.00 ,maturity,.10,par,coupon.freq) - 1


####Time to Maturity vs Bond value
#coupon=.10
#maturity=20
#yield=0.10
#par=100
#coupon.freq=1


bondprc(coupon, 20, .08, par, coupon.freq)/bondprc(coupon, 20, .10, par, coupon.freq) - 1
bondprc(coupon, 10, .08, par, coupon.freq)/bondprc(coupon, 10, .10, par, coupon.freq) - 1
bondprc(coupon, 5, .08, par, coupon.freq)/bondprc(coupon, 5, .10, par, coupon.freq) - 1

###Price value of a basis point "PV01"
#This is equals to price of a bond if the required yield changes by .01%
#coupon=.10
#maturity=20
#yield=0.10
#par=100
#coupon.freq=1


bondprc(coupon, maturity, .10, par, coupon.freq)
bondprc(coupon, maturity, .1001, par, coupon.freq)
PV01 <- abs(bondprc(coupon, maturity, .10, par, coupon.freq) -  bondprc(coupon, maturity, .1001, par, coupon.freq))
PV01

####Duration
#coupon=.10
#maturity=20
#yield=0.10
#par=100
#coupon.freq=1


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
#coupon=.05
#maturity=10
#yield=0.04
#par=100
#coupon.freq=1


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