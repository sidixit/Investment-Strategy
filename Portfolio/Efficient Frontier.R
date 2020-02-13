data.VISA <- getSymbols("V", from = "2014-12-31", to = "2016-12-31", auto.assign = FALSE)
data.VISA <- to.monthly(data.VISA)
VISA.ret <- Return.calculate(data.VISA$data.VISA.Adjusted)
VISA.ret <- VISA.ret[-1,]
head(VISA.ret)

data.UNH <- getSymbols("UNH", from = "2014-12-31", to = "2016-12-31", auto.assign = FALSE)
data.UNH <- to.monthly(data.UNH)
UNH.ret <- Return.calculate(data.UNH$data.UNH.Adjusted)
UNH.ret <- UNH.ret[-1,]
head(UNH.ret)

Ret.monthly <- cbind(VISA.ret , UNH.ret)
mat.ret<-matrix(Ret.monthly,nrow(Ret.monthly))
colnames(mat.ret)<-c("VISA.Ret", "UNH.Ret")
head(mat.ret)

VCOV<-cov(mat.ret)
VCOV

avg.ret<-matrix(apply(mat.ret,2,mean))
colnames(avg.ret)<-paste("Avg.Ret")
rownames(avg.ret)<-paste(c("VISA","UNH"))
avg.ret

min.ret<-min(avg.ret)
min.ret

max.ret<-max(avg.ret)
max.ret

increments=100
tgt.ret<-seq(min.ret,max.ret,length=increments)
head(tgt.ret)

tail(tgt.ret)

tgt.sd<-rep(0,length=increments)
tgt.sd

wgt<-matrix(0,nrow=increments,ncol=length(avg.ret))
head(wgt)

library(quadprog)
for (i in 1:increments){
  Dmat<-2*VCOV
  dvec<-c(rep(0,length(avg.ret)))
  Amat<-cbind(rep(1,length(avg.ret)),avg.ret,
              diag(1,nrow=ncol(Ret.monthly)))
  bvec<-c(1,tgt.ret[i],rep(0,ncol(Ret.monthly)))
  soln<-solve.QP(Dmat,dvec,Amat,bvec=bvec,meq=2)
  tgt.sd[i]<-sqrt(soln$value)
  wgt[i,]<-soln$solution
}

colnames(wgt)<-paste(c("wgt.VISA","wgt.UNH"))
wgt[1,2]<-0
wgt[nrow(wgt),1]<-0
head(wgt)

tgt.port<-data.frame(cbind(tgt.ret,tgt.sd,wgt))
head(tgt.port)

minvar.port<-subset(tgt.port,tgt.port$tgt.sd==min(tgt.port$tgt.sd))
minvar.port

riskfree = 0.0000583
tgt.port$Sharpe<-(tgt.port$tgt.ret-riskfree)/tgt.port$tgt.sd
head(tgt.port)

tangency.port<-subset(tgt.port,tgt.port$Sharpe==max(tgt.port$Sharpe))
tangency.port

eff.frontier<-subset(tgt.port,tgt.port$tgt.ret>=minvar.port$tgt.ret)
head(eff.frontier)

plot(x=tgt.sd, xlab="Portfolio Risk", y=tgt.ret, ylab="Portfolio Return", col="gray40",
     main="Mean-Variance Efficient Frontier of Two Assets
     Based on the Quadratic Programming Approach")
abline(h=0,lty=1)
points(x=minvar.port$tgt.sd,y=minvar.port$tgt.ret,pch=17,cex=3)
points(x=tangency.port$tgt.sd,y=tangency.port$tgt.ret,pch=19,cex=3)
points(x=eff.frontier$tgt.sd,y=eff.frontier$tgt.ret, col ="red" )


