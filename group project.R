##########time series group project###########

data=read.table("C:/Users/Thomas/MmathStat/Course materials/4th year/time series/group project/projectdata.txt")
y=data[,51] 
y$data=ts(y,start=c(2006,1),frequency=12)
plot(y$data ,ylab="Total electricity consumption (millions of kilowatt -hours (106 kWh))") 
points(y$data ,pch=21,bg=1)
y$trend=filter(y$data ,c(1/24,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1 /12,1/24)) 
y$trend=ts(y$trend ,start=c(2006,1),frequency=12)
lines(y$trend ,col=3,lty=2) 
legend(2008,42,c("Electricity consumption","Trend"),col=c(1,3), pch=c(21,NA),pt.bg=c(1,NA),lty=c(1,2),cex=0.8)
y$mon = c(rep(1:12,10))
ymonf=factor(y$mon) 
time=(1:120)
fit1=lm(y$data~time+ymonf) 
fit2=lm(log(y$data)~time+ymonf)
summary(fit1) #Multiple R-squared: 0.9103, Adjusted R-squared: 0.9002 18 summary(fit2) #Multiple R-squared: 0.9346, Adjusted R-squared: 0.9272
summary(fit2) #Multiple R-squared: 0.9346, Adjusted R-squared: 0.9272

fit2$acf=acf(fit2$residuals,
             lag.max=30,ci.type="ma",main="Autocorrelation Function for Electricity Consumption Adjusted Data") 
fit2$pacf=pacf(fit2$residuals ,lag.max=30,
               main="Partial Autocorrelation Function for Electricity Consumption Adjusted Data")

ar1=arima(fit2$residuals ,order=c(1,0,0)) 
t=0.5941/0.0727 
df=120-1-0-1 
pt(t,df) 
p=2*(1-pt(t,df)) 
tsdiag(ar1,gof.lag = 40) 
plot.default(fit2$residuals -ar1$residuals ,ar1$residuals ,
             xlab="Fitted Values", ylab="Residuals",cex.lab=1.4)

ma2=arima(fit2$residuals ,order=c(0,0,2)) 
# We need to assess the signficance of the highest order term: 
t=0.4313 /0.0957 #4.506792 
df=120-0-2-1 
pt(t,df) 
tsdiag(ma2,gof.lag = 40) 
plot.default(fit2$residuals -ma2$residuals ,ma2$residuals ,
             xlab="Fitted Values", ylab="Residuals",cex.lab=1.4)

ar1ma1=arima(fit2$residuals ,order=c(1,0,1)) 
t=-0.0813 /0.1243 
df=120-1-1-1 
pt(t,df) 
p=2*(1- pt(t,df))

ar2=arima(fit2$residuals ,order=c(2,0,0)) 
t=0.0718 /0.0911 
df=120-2-0-1 
pt(t,df) 
2*(1- pt(t,df))

ma3=arima(fit2$residuals ,order=c(0,0,3)) 
t=0.0717 /0.0853 
df=120-0-3-1 
pt(t,df) 
2*(1- pt(t,df))

#########forecasting###########
######AR(1) Model##############
predicted.trend = fit2$coefficients[1] + fit2$coefficients[2]*(121:126)
season=c(0,fit2$coef[3],fit2$coef[4],fit2$coef[5],fit2$coef[6],fit2$coef[7]) 
predicted.trend.season = predicted.trend+season
ar1F<-predict(ar1,n.ahead=12) 
ar1F$pred=ts(ar1F$pred,start=c(2016,1),frequency=12) 
ar1FT<-ar1F$pred+predicted.trend.season 
ar1F$se=ts(ar1F$se,start=c(2016,1),frequency=12) 
ar1FTU=ar1FT+2*ar1F$se
ar1FTL=ar1FT-2*ar1F$se
plot(y$data,xlim=c(2006,2017),ylab="Total electricity consumption (millions of kilowatt -hours (106 kWh))")
lines(exp(ar1FT),col=2,lty=2)
lines(exp(ar1FTU),col=4,lty=3)
lines(exp(ar1FTL),col=4,lty=3) 
legend(2008,42,c("Electricity consumption","Forecast", "95% confidence intervals"),col=c(1,2,4),lty=c(1,2,3),cex=0.8)
exp(ar1FT)
exp(ar1FTU)
exp(ar1FTL)

####accuracy check###
y=data[1:108,51] 
y$data=ts(y,start=c(2006,1),frequency=12)
y$mon = c(rep(1:12,9))
ymonf=factor(y$mon) 
time=(1:108)
fit2=lm(log(y$data)~time+ymonf)
ar1=arima(fit2$residuals ,order=c(1,0,0))
predicted.trend = fit2$coefficients[1] + fit2$coefficients[2]*(109:120)
season=c(0,fit2$coef[3],fit2$coef[4],fit2$coef[5],fit2$coef[6],fit2$coef[7],
         fit2$coefficients[8],fit2$coefficients[9],fit2$coefficients[10],fit2$coefficients[11],fit2$coefficients[12],fit2$coefficients[13]) 
predicted.trend.season = predicted.trend+season
ar1F<-predict(ar1,n.ahead=12) 
ar1F$pred=ts(ar1F$pred,start=c(2015,1),frequency=12) 
ar1FT<-ar1F$pred+predicted.trend.season 
ar1F$se=ts(ar1F$se,start=c(2015,1),frequency=12) 
ar1FTU=ar1FT+2*ar1F$se
ar1FTL=ar1FT-2*ar1F$se
plot(y$data,xlim=c(2006,2017),ylab="Total electricity consumption (millions of kilowatt -hours (106 kWh))")
lines(exp(ar1FT),col=2,lty=2)
lines(exp(ar1FTU),col=4,lty=3)
lines(exp(ar1FTL),col=4,lty=3) 
legend(2008,42,c("Electricity consumption","Forecast", "95% confidence intervals"),col=c(1,2,4),lty=c(1,2,3),cex=0.8)
exp(ar1FT)
exp(ar1FTL)
exp(ar1FTU)
######MA(2)###################
predicted.trend = fit2$coefficients[1] + fit2$coefficients[2]*(121:126)
season=c(0,fit2$coef[3],fit2$coef[4],fit2$coef[5],fit2$coef[6],fit2$coef[7]) 
predicted.trend.season = predicted.trend+season
ma2=arima(fit2$residuals ,order=c(0,0,2))
ma2F<-predict(ma2,n.ahead=6) 
ma2F$pred=ts(ma2F$pred,start=c(2016,1),frequency=12) 
ma2FT<-ma2F$pred+predicted.trend.season 
ma2F$se=ts(ma2F$se,start=c(2016,1),frequency=12) 
ma2FTU=ma2FT+2*ma2F$se
ma2FTL=ma2FT-2*ma2F$se
plot(y$data,xlim=c(2006,2017),ylab="Total electricity consumption (millions of kilowatt -hours (106 kWh))")
lines(exp(ma2FT),col=2,lty=2)
lines(exp(ma2FTU),col=4,lty=3)
lines(exp(ma2FTL),col=4,lty=3) 
legend(2008,42,c("Electricity consumption","Forecast", "95% confidence intervals"),col=c(1,2,4),lty=c(1,2,3),cex=0.8)
exp(ma2FT)
exp(ma2FTU)
exp(ma2FTL)
######model accuracy#######
y=data[1:108,51]
y$data=ts(y,start=c(2006,1),frequency=12)
y$mon = c(rep(1:12,9))
ymonf=factor(y$mon) 
time=(1:108)
fit2=lm(log(y$data)~time+ymonf)
ma2=arima(fit2$residuals ,order=c(0,0,2))
predicted.trend = fit2$coefficients[1] + fit2$coefficients[2]*(109:120)
season=c(0,fit2$coef[3],fit2$coef[4],fit2$coef[5],fit2$coef[6],fit2$coef[7],
         fit2$coefficients[8],fit2$coefficients[9],fit2$coefficients[10],fit2$coefficients[11],fit2$coefficients[12],fit2$coefficients[13])
ma2F<-predict(ma2,n.ahead=12) 
ma2F$pred=ts(ma2F$pred,start=c(2015,1),frequency=12) 
ma2FT<-ma2F$pred+predicted.trend.season 
ma2F$se=ts(ma2F$se,start=c(2015,1),frequency=12) 
ma2FTU=ma2FT+2*ma2F$se
ma2FTL=ma2FT-2*ma2F$se
plot(y$data,xlim=c(2006,2016),ylab="Total electricity consumption (millions of kilowatt -hours (106 kWh))")
lines(exp(ma2FT),col=2,lty=2)
lines(exp(ma2FTU),col=4,lty=3)
lines(exp(ma2FTL),col=4,lty=3) 
legend(2008,42,c("Electricity consumption","Forecast", "95% confidence intervals"),col=c(1,2,4),lty=c(1,2,3),cex=0.8)
exp(ma2FT)
exp(ma2FTU)
exp(ma2FTL)


##########essential code for report##########
#########AR1 2016 predictions#############
predicted.trend = fit2$coefficients[1] + fit2$coefficients[2]*(121:126)
season=c(0,fit2$coef[3],fit2$coef[4],fit2$coef[5],fit2$coef[6],fit2$coef[7]) 
predicted.trend.season = predicted.trend+season
ar1F<-predict(ar1,n.ahead=12) 
ar1F$pred=ts(ar1F$pred,start=c(2016,1),frequency=12) 
ar1FT<-ar1F$pred+predicted.trend.season 
ar1F$se=ts(ar1F$se,start=c(2016,1),frequency=12) 
ar1FTU=ar1FT+2*ar1F$se
ar1FTL=ar1FT-2*ar1F$se
############AR1 2015 predictions###############
y=data[1:108,51] 
y$data=ts(y,start=c(2006,1),frequency=12)
y$mon = c(rep(1:12,9))
ymonf=factor(y$mon) 
time=(1:108)
fit2=lm(log(y$data)~time+ymonf)
ar1=arima(fit2$residuals ,order=c(1,0,0))
predicted.trend = fit2$coefficients[1] + fit2$coefficients[2]*(109:120)
season=c(0,fit2$coef[3],fit2$coef[4],fit2$coef[5],fit2$coef[6],fit2$coef[7],
         fit2$coefficients[8],fit2$coefficients[9],fit2$coefficients[10],fit2$coefficients[11],fit2$coefficients[12],fit2$coefficients[13]) 
predicted.trend.season = predicted.trend+season
ar1F<-predict(ar1,n.ahead=12) 
ar1F$pred=ts(ar1F$pred,start=c(2015,1),frequency=12) 
ar1FT<-ar1F$pred+predicted.trend.season 
ar1F$se=ts(ar1F$se,start=c(2015,1),frequency=12) 
ar1FTU=ar1FT+2*ar1F$se
ar1FTL=ar1FT-2*ar1F$se
###############MA2 2016 predictions################
predicted.trend = fit2$coefficients[1] + fit2$coefficients[2]*(121:126)
season=c(0,fit2$coef[3],fit2$coef[4],fit2$coef[5],fit2$coef[6],fit2$coef[7]) 
predicted.trend.season = predicted.trend+season
ma2=arima(fit2$residuals ,order=c(0,0,2))
ma2F<-predict(ma2,n.ahead=6) 
ma2F$pred=ts(ma2F$pred,start=c(2016,1),frequency=12) 
ma2FT<-ma2F$pred+predicted.trend.season 
ma2F$se=ts(ma2F$se,start=c(2016,1),frequency=12) 
ma2FTU=ma2FT+2*ma2F$se
ma2FTL=ma2FT-2*ma2F$se
#################MA2 2015 predictions#############
y=data[1:108,51]
y$data=ts(y,start=c(2006,1),frequency=12)
y$mon = c(rep(1:12,9))
ymonf=factor(y$mon) 
time=(1:108)
fit2=lm(log(y$data)~time+ymonf)
ma2=arima(fit2$residuals ,order=c(0,0,2))
predicted.trend = fit2$coefficients[1] + fit2$coefficients[2]*(109:120)
season=c(0,fit2$coef[3],fit2$coef[4],fit2$coef[5],fit2$coef[6],fit2$coef[7],
         fit2$coefficients[8],fit2$coefficients[9],fit2$coefficients[10],fit2$coefficients[11],fit2$coefficients[12],fit2$coefficients[13])
ma2F<-predict(ma2,n.ahead=12) 
ma2F$pred=ts(ma2F$pred,start=c(2015,1),frequency=12) 
ma2FT<-ma2F$pred+predicted.trend.season 
ma2F$se=ts(ma2F$se,start=c(2015,1),frequency=12) 
ma2FTU=ma2FT+2*ma2F$se
ma2FTL=ma2FT-2*ma2F$se



