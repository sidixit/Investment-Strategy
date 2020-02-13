# Market Index, #Portfolio, #Bonds, #Options




######## ###########

#Calculate Return


######## Market Index and Portfolio Index#####################

library(quantmod)
library(PerformanceAnalytics)
portfolio<- function(w_s1, w_s2, w_mkt){
  data.V <- getSymbols("V",from="2010-12-31",to="2017-12-05",auto.assign=FALSE)
  data.UNH <- getSymbols("UNH",from="2010-12-31",to="2017-12-05",auto.assign=FALSE)
  data.GSPC <- getSymbols("^GSPC",from="2010-12-31",to="2017-12-05",auto.assign=FALSE)
  
  multi <-cbind(data.V$V.Adjusted, data.UNH$UNH.Adjusted, data.GSPC$GSPC.Adjusted)
  period.ret<-multi[c(1,nrow(multi)),]
  period.ret
  
  rets<-lapply(period.ret,Delt)
  rets
  
  rets<-data.frame(rets)
  rets
  
  rets<-rets[2,]*100
  names(rets)<-paste(c("V","UNH","GSPC"))
  rets
  
  w.V<-i.V/(i.V+i.UNH+i.GSPC)
  w.V
  w.UNH<-i.UNH/(i.V+i.UNH+i.GSPC)
  w.UNH
  w.GSPC<-i.GSPC/(i.V+i.UNH+i.GSPC)
  w.GSPC
  
  port.ret.4asset<- w.V*rets$V + w.UNH*rets$UNH + w.GSPC*rets$GSPC
  port.ret.4asset
}

portfolio(10000,10000,10000)


########Bonds##########################



