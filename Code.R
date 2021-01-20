library(Quandl)
library(dplyr)
library(zoo)
library(ggplot2)
library(corrplot)
library(tseries)
library(TSA)
library(forecast)
library(PerformanceAnalytics)
library(TTR)
library(MASS)
library(LaplacesDemon)
library(NormalLaplace)
library(quantmod)
library(tidyr)
library(reshape2)

Quandl.api_key("PqwdR9zg7uFyWWv4enza")

setwd('C:/Users/HP/Desktop/WQU/Capstone Project/Code and Data')

# USA_YC = Quandl("YC/USA",type = 'raw')
# GBR_YC = Quandl("YC/GBR",type = 'raw')
# CAN_YC = Quandl("YC/CAN",type = 'raw')
# FRA_YC = Quandl("YC/FRA",type = 'raw')
# JPN_YC = Quandl("YC/JPN",type = 'raw')
# GRC_YC = Quandl("YC/GRC",type = 'raw')
# BEL_YC = Quandl("YC/BEL",type = 'raw')
# SWISS_YC = Quandl("YC/CHE",type = 'raw')
# 
# save(USA_YC,GBR_YC,CAN_YC,FRA_YC,JPN_YC,GRC_YC,BEL_YC,SWISS_YC,file = "YieldCurve.RData")

# US_GDP = Quandl("FRED/GDP",type = 'raw')
# US_CPI = Quandl("RATEINF/CPI_USA",type = 'raw')
# US_Oil_WTI = Quandl("FRED/WTISPLC",type = 'raw')
# # US_Gold = Quandl("FRED/GOLDPMGBD228NLBM",type = 'raw')
# US_Gold = Quandl("LBMA/GOLD",type = 'raw')
# US_UNEM = Quandl("FRED/UNRATE",type = 'raw')
# US_IND_PRO = Quandl("FRED/INDPRO",type = 'raw')
# US_CAP_UTIL = Quandl("FRED/CAPUTLB50001SQ",type = 'raw')
# US_SP500 = read.csv(file = 'GSPC.csv')

# save(US_GDP,US_CPI,US_Oil_WTI,US_Gold,US_UNEM,US_IND_PRO,US_CAP_UTIL,US_SP500,file = 'EconomicIndicators.RData')
#####################################
load('YieldCurve.RData')
load('EconomicIndicators.RData')
###########################################################################

#Data Prepreocessing
#USA
USA_YC_OOS = USA_YC[240:1,c(1,5:12)]
USA_YC_OOS[,2:9] = USA_YC_OOS[,2:9]/100 
rownames(USA_YC_OOS) = 1:nrow(USA_YC_OOS)

USA_YC = USA_YC[5242:241,c(1,5:12)]
USA_YC[,2:9] = USA_YC[,2:9]/100
rownames(USA_YC) = 1:nrow(USA_YC)

USA_YC[is.na(USA_YC[,9]),9] = USA_YC[is.na(USA_YC[,9]),8]

x = c(1,2,3,5,7,10,20,30)
# y = USA_YC[1,-1]
# spline(x,y,n=30)

USA_YC_SPline = matrix(rep(0,30*nrow(USA_YC)),nrow = nrow(USA_YC),ncol = 30)
for (i in 1:nrow(USA_YC)) {
  y = USA_YC[i,-1]
  USA_YC_SPline[i,] = spline(x,y,n=30)$y
}

plot(x,USA_YC[500,-1],type = 'p',xlab = 'Maturity',
     ylab = 'Yield', main = 'Fitiing Spline',col = 'red')
lines(c(1:30), USA_YC_SPline[500,])


#PCA of USA - Full data 
date_USA = USA_YC$Date
y.pca = prcomp(USA_YC_SPline,scale = T,center = T)
cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)

#Taking the first 3 factor loadings - Full data
factor_loadings_USA = y.pca$rotation[,1:3]
PC_USA = y.pca$x[,1:3]
scale = y.pca$scale
center = y.pca$center

PCA_YC_USA = PC_USA%*%t(factor_loadings_USA)
PCA_YC_USA = apply(PCA_YC_USA,1,function(x) x*scale+center)
PCA_YC_USA = t(PCA_YC_USA)

#Plots - Output
#1. Explained variance
plot(c(1:10),(cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)*100)[1:10],ylim = c(90,100),type = 'b',xlab = 'Principal Components',
     ylab = 'Percentage of cumulative varinace explained', main = 'Cumlative variance explained by Pricipal Components',col = 'red',
     xaxt = 'n')
axis(side = 1, at = c(1:10))
abline(h = 100,col = 'gray',lty=2)


#2. Time Series of principal components
a = data.frame(Date = rep(date_USA,3),Values = c(PC_USA[,1],PC_USA[,2],PC_USA[,3]), 
               PC = c(rep('PC 1',length(date_USA)),rep('PC 2',length(date_USA)),rep('PC 3',length(date_USA))))
ggplot(data = a) +
  geom_line(aes(x = Date, y = Values, colour = PC)) +
  geom_hline(yintercept = 0,linetype = 'dashed', color = 'grey') +
  scale_x_date(date_breaks = '2 years') +
  labs(title = "Evolution of Principal Components over time", x = "Date", y = "", color = "Principal Components") +
  scale_color_manual(labels = c("PC 1", "PC 2", 'PC 3'), values = c("red", "green",'blue')) +
  theme_bw() +
  theme(legend.position = "top",plot.title = element_text(hjust = 0.5))

#3. PCA Fit on 4 random YCs
rand_rows = c(1035,2467,3182,4756)
rand_dates = date_USA[rand_rows]
b = data.frame(Maturity = c(1:30))
for (i in 1:4) {
  b = cbind(b,data.frame(a = USA_YC_SPline[rand_rows[i],]),data.frame(b = PCA_YC_USA[rand_rows[i],]))
  names(b)[(i*2):(i*2+1)] <- c(paste('Observed-',rand_dates[i],sep = ''),paste(rand_dates[i],'-PCA',sep = ''))
}

ggplot(data = b) +
  geom_line(aes(x=b[,1], y=b[,2]), color = 'red') +
  geom_point(aes(x=b[,1], y=b[,3]), color = 'red' ) +
  geom_line(aes(x=b[,1], y=b[,4]), color = 'green') +
  geom_point(aes(x=b[,1], y=b[,5]), color = 'green') +
  geom_line(aes(x=b[,1], y=b[,6]), color = 'blue') +
  geom_point(aes(x=b[,1], y=b[,7]),  color = 'blue') +
  geom_line(aes(x=b[,1], y=b[,8]), color = 'orange') +
  geom_point(aes(x=b[,1], y=b[,9]),  color = 'orange') +
  geom_label(label='24-02-2004', x=10.1,y=0.045,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='10-11-2009', x=20.1,y=0.045,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='17-09-2012', x=8.1,y=0.02,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='07-01-2019', x=15.1,y=0.03,label.size = 0.2,color = "black",fill=NA) +
  labs(title = "Randomly selected yield curves and PC fit", x = "Maturity", y = "Rate") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


#4. Correlation among PCs
corrplot(cor(PC_USA))

#5. Effect of individual PC on Yield Curves
row_num = 1000
c = data.frame(Maturity = c(1:30), YC = USA_YC_SPline[row_num,])
PC_C = PC_USA[row_num,]

PC_C_Shocked = c(PC_USA[row_num,1]+sd(PC_USA[,1]),PC_USA[row_num,2],PC_USA[row_num,3])
YC_Shocked = t(PC_C_Shocked%*%t(factor_loadings_USA))*scale+center

c = cbind(c,YC_Level = YC_Shocked)

PC_C_Shocked = c(PC_USA[row_num,1],PC_USA[row_num,2]+sd(PC_USA[,2]),PC_USA[row_num,3])
YC_Shocked = t(PC_C_Shocked%*%t(factor_loadings_USA))*scale+center

c = cbind(c,YC_Slope = YC_Shocked)

PC_C_Shocked = c(PC_USA[row_num,1],PC_USA[row_num,2],PC_USA[row_num,3]+sd(PC_USA[,3]))
YC_Shocked = t(PC_C_Shocked%*%t(factor_loadings_USA))*scale+center

c = cbind(c,YC_Curvature = YC_Shocked)

#PC1
ggplot(data = c) +
  geom_line(aes(x=c[,1], y=c[,2]), color = 'red') +
  geom_line(aes(x=c[,1], y=c[,3]), color = 'green') +
  labs(title = "1 standard deviation shock to 1st PC on 02-01-2004 yields", x = "Maturity", y = "Rate") +
  geom_label(label='Actual Yields', x=5,y=0.04,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='Shocked PC Yields', x=9,y=0.02,label.size = 0.2,color = "black",fill=NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
#PC2
ggplot(data = c) +
  geom_line(aes(x=c[,1], y=c[,2]), color = 'red') +
  geom_line(aes(x=c[,1], y=c[,4]), color = 'green') +
  labs(title = "1 standard deviation shock to 2nd PC on 02-01-2004 yields", x = "Maturity", y = "Rate") +
  geom_label(label='Actual Yields', x=5,y=0.04,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='Shocked PC Yields', x=7,y=0.02,label.size = 0.2,color = "black",fill=NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
#PC3
ggplot(data = c) +
  geom_line(aes(x=c[,1], y=c[,2]), color = 'red') +
  geom_line(aes(x=c[,1], y=c[,5]), color = 'green') +
  labs(title = "1 standard deviation shock to 3rd PC on 02-01-2004 yields", x = "Maturity", y = "Rate") +
  geom_label(label='Actual Yields', x=10.5,y=0.04,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='Shocked PC Yields', x=3,y=0.04,label.size = 0.2,color = "black",fill=NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


#6. Compare factor loadings Full data vs 2000-2008 end vs 2009 start-Data End
USA_YC_1Half = USA_YC_SPline[1:2251,]
USA_YC_2Half = USA_YC_SPline[2252:5002,]

Expl_Var = data.frame(Full = cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2))

#y.pca = prcomp(USA_YC_SPline,scale = T,center = T)
sd_full = y.pca$sdev[1:10]

y.pca = prcomp(USA_YC_1Half,scale = T,center = T)
sd_half1 = y.pca$sdev[1:10] 
Expl_Var$Half1 = cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)
factor_loadings_Half1 = y.pca$rotation[,1:3]
PC_USA_Half1 = y.pca$x[,1:3]

y.pca = prcomp(USA_YC_2Half,scale = T,center = T)
sd_half2 = y.pca$sdev[1:10] 
Expl_Var$Half2 = cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)
factor_loadings_Half2 = y.pca$rotation[,1:3]
PC_USA_Half2 = y.pca$x[,1:3]

#Comparing eigen values
ggplot(data = data.frame(Full = sd_full,Half1 = sd_half1,Half2 = sd_half2)) +
  geom_line(aes(x=c(1:10), y=Full),color = 'red') +
  geom_line(aes(x=c(1:10), y=Half1),color = 'green') +
  geom_line(aes(x=c(1:10), y=Half2),color = 'blue') +
  geom_point(aes(x=c(1:10), y=Full),color = 'red') +
  geom_point(aes(x=c(1:10), y=Half1),color = 'green') +
  geom_point(aes(x=c(1:10), y=Half2),color = 'blue') +
  xlim(0,5) +
  labs(title = "Comparison of eigenvalues on different windows", x = "Eigen Vallues", 
       y = "Value") +
  geom_label(label='2000-2019', x=2,y=1,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='2000-2009', x=2,y=2,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='2009-2019', x=2,y=3,label.size = 0.2,color = "black",fill=NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


#Explained variance chart
ggplot(data = Expl_Var) +
  geom_line(aes(x=c(1:30), y=Full),color = 'red') +
  geom_line(aes(x=c(1:30), y=Half1),color = 'green') +
  geom_line(aes(x=c(1:30), y=Half2),color = 'blue') +
  geom_point(aes(x=c(1:30), y=Full),color = 'red') +
  geom_point(aes(x=c(1:30), y=Half1),color = 'green') +
  geom_point(aes(x=c(1:30), y=Half2),color = 'blue') +
  xlim(0,5) +
  labs(title = "Comparison of cumulative explained variance on different windows", x = "Principal Component", 
       y = "Percentage of cumulative variance explained") +
  geom_label(label='2000-2019', x=.65,y=0.95,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='2000-2009', x=.6,y=0.9,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='2009-2019', x=.6,y=0.8,label.size = 0.2,color = "black",fill=NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Factor loadings comparison

#Direction 1
ggplot(data.frame(Maturity = c(1:30), Full = factor_loadings_USA[,1], Half1 = factor_loadings_Half1[,1],Half2 = factor_loadings_Half2[,1])) +
  geom_line(aes(x = Maturity, y = Full), color = 'red') +
  geom_line(aes(x = Maturity, y = Half1), color = 'blue') +
  geom_line(aes(x = Maturity, y = Half2), color = 'green') +
  labs(title = "First Principal Direction compariosn - Full data and data split in 2 windows", x = "Maturity", 
       y = "Value") +
  geom_label(label='2000-2019', x=2,y=-0.18,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='2000-2009', x=2,y=-0.12,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='2009-2019', x=5,y=-0.05,label.size = 0.2,color = "black",fill=NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Direction2
ggplot(data.frame(Maturity = c(1:30), Full = factor_loadings_USA[,2], Half1 = factor_loadings_Half1[,2],Half2 = factor_loadings_Half2[,2])) +
  geom_line(aes(x = Maturity, y = Full), color = 'red') +
  geom_line(aes(x = Maturity, y = Half1), color = 'blue') +
  geom_line(aes(x = Maturity, y = Half2), color = 'green') +
  labs(title = "First Principal Direction compariosn - Full data and data split in 2 windows", x = "Maturity", 
       y = "Value") +
  geom_label(label='2000-2019', x=12,y=0.1,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='2000-2009', x=12,y=-0.1,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='2009-2019', x=5,y=-0.4,label.size = 0.2,color = "black",fill=NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Direction3
ggplot(data.frame(Maturity = c(1:30), Full = factor_loadings_USA[,3], Half1 = factor_loadings_Half1[,3],Half2 = factor_loadings_Half2[,3])) +
  geom_line(aes(x = Maturity, y = Full), color = 'red') +
  geom_line(aes(x = Maturity, y = Half1), color = 'blue') +
  geom_line(aes(x = Maturity, y = Half2), color = 'green') +
  labs(title = "First Principal Direction compariosn - Full data and data split in 2 windows", x = "Maturity", 
       y = "Value") +
  geom_label(label='2000-2019', x=12,y=0.1,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='2000-2009', x=12,y=-0.1,label.size = 0.2,color = "black",fill=NA) +
  geom_label(label='2009-2019', x=5,y=-0.4,label.size = 0.2,color = "black",fill=NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


#Principal Components Comparison
#PC1
ggplot() +
  geom_line(data = data.frame(Date = date_USA,PC1 = PC_USA[,1]),aes(x = Date, y = PC1),col = 'red') +
  geom_line(data = data.frame(Date = date_USA[1:2251],PC1 = PC_USA_Half1[,1]),aes(x = Date, y = PC1),col = 'blue') +
  geom_line(data = data.frame(Date = date_USA[2252:5002],PC1 = PC_USA_Half2[,1]),aes(x = Date, y = PC1),col = 'green') 

#PC2
ggplot() +
  geom_line(data = data.frame(Date = date_USA,PC1 = PC_USA[,2]),aes(x = Date, y = PC1),col = 'red') +
  geom_line(data = data.frame(Date = date_USA[1:2251],PC1 = PC_USA_Half1[,2]),aes(x = Date, y = PC1),col = 'blue') +
  geom_line(data = data.frame(Date = date_USA[2252:5002],PC1 = PC_USA_Half2[,2]),aes(x = Date, y = PC1),col = 'green') 

#PC3
ggplot() +
  geom_line(data = data.frame(Date = date_USA,PC1 = PC_USA[,3]),aes(x = Date, y = PC1),col = 'red') +
  geom_line(data = data.frame(Date = date_USA[1:2251],PC1 = -PC_USA_Half1[,3]),aes(x = Date, y = PC1),col = 'blue') +
  geom_line(data = data.frame(Date = date_USA[2252:5002],PC1 = -PC_USA_Half2[,3]),aes(x = Date, y = PC1),col = 'green') 


###########################################################################################################################
#####Time Series Principal Components#################################

#Take only 2013 - 2018 data and 2019 data as out of sample

USA_YC_SPline_TS = USA_YC_SPline[3253:5002,]
y.pca = prcomp(USA_YC_SPline_TS,scale = T,center = T)
y.pca$sdev[1:10] 
cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)
factor_loadings_TS = y.pca$rotation[,1:3]
PC_USA_TS = y.pca$x[,1:3]
scale = y.pca$scale
center = y.pca$center


# PC_USA_Train = PC_USA[1:1500,]
# PC_USA_OOS = PC_USA[1501:1750,]
date_TS = date_USA[3253:5002]

plot(date_TS,PC_USA_TS[,1],type = 'l',main ='PC #1',xlab= 'Date', ylab = 'Value')
abline(reg=lm(PC_USA_TS[,1]~date_TS), col = 'blue')
#PC 1
Mov_Avg =  sapply(SMA(PC_USA_TS[,1],n=50), function(x) ifelse(is.na(x),0,x))
a = PC_USA_TS[,1] - sapply(SMA(PC_USA_TS[,1],n=50), function(x) ifelse(is.na(x),0,x))

plot(date_TS,a,type = 'l',main ='PC #1',xlab= 'Date', ylab = 'Value')
abline(reg=lm(a~date_TS), col = 'blue')
#lines(date_train,SMA(PC_USA_Train[,1],n=50))

#a = PC_USA_Train[,1] - sapply(SMA(PC_USA_Train[,1],n=50), function(x) ifelse(is.na(x),0,x))

PC1_Train = a[1:1500]
PC1_Test = a[-c(1:1500)]

acf(PC1_Train)
pacf(PC1_Train)

# acf(PC1_Train^2)
# pacf(PC1_Train^2)

adf.test(PC1_Train)
#adf.test(diff(a))

# acf(diff(PC_USA_Train[,1]))
# pacf(diff(PC_USA_Train[,1]))

auto.arima(PC1_Train)

plot(PC1_Train)
#plot(diff(PC_USA_Train[,1],lag = 300))
PC1_Model = arima(PC1_Train,order = c(2,0,0))
summary(PC1_Model)
PC1_pred <- predict(PC1_Model, n.ahead = 100)
plot(c(PC1_Train,PC1_Test), col = 'blue')
lines(c(rep(NA,length(PC1_Train)),PC1_pred$pred), col = 'red')

plot(c(PC_USA_Train[,1],PC1_pred$pred))
lines(c(rep(0,length(PC_USA_Train[,1]))))

McLeod.Li.test(PC1_Model)

acf(PC1_Model$residuals)

plot(date_TS[1:1500],PC1_Model$residuals)

qqnorm(PC1_Model$residuals,xlab = 'Theoritical Quantiles', ylab = 'Sample Quantiles', main = 'QQPlot of PCA 1 Model')
qqline(PC1_Model$residuals, distribution = qnorm,probs = c(0.25, 0.75), qtype = 7)

#plot.ts(SMA(PC_USA_Train[,1], n=200))

# acf(diff(PC_USA_Train[,1]))
# pacf(diff(PC_USA_Train[,1]))

plot(date_TS[1:1600],c(PC1_Train,PC1_Test[1:100])+Mov_Avg[1:1600], type = 'p', col = 'blue', cex = .5)
lines(date_TS[1501:1600],PC1_pred$pred+Mov_Avg[1501:1600],col= 'red', lty = 2, lwd = 2)

#PC2
plot(date_TS,PC_USA_TS[,2],type = 'l',main ='PC #2',xlab= 'Date', ylab = 'Value')
abline(reg=lm(PC_USA_TS[,2]~date_TS), col = 'blue')

Mov_Avg =  sapply(SMA(PC_USA_TS[,2],n=50), function(x) ifelse(is.na(x),0,x))
a = PC_USA_TS[,2] - sapply(SMA(PC_USA_TS[,2],n=50), function(x) ifelse(is.na(x),0,x))

plot(date_TS,a,main ='PC #2',xlab= 'Date', ylab = 'Value', type ='l')
abline(reg=lm(a~date_TS), col = 'blue')
#lines(date_train,SMA(PC_USA_Train[,1],n=50))

#a = PC_USA_Train[,1] - sapply(SMA(PC_USA_Train[,1],n=50), function(x) ifelse(is.na(x),0,x))

PC2_Train = a[1:1500]
PC2_Test = a[-c(1:1500)]

acf(PC2_Train)
pacf(PC2_Train)

adf.test(PC2_Train)
#adf.test(diff(PC_USA_Train[,2]))

auto.arima(PC2_Train)
plot(PC2_Train)
#plot(diff(PC_USA_Train[,1],lag = 300))
PC2_Model = arima(PC2_Train,order = c(1,0,0))
summary(PC2_Model)
PC2_pred <- predict(PC2_Model, n.ahead = 100)
plot(c(PC2_Train,PC2_Test))
lines(c(rep(0,length(PC2_Train)),PC2_pred$pred), col = 'red')

McLeod.Li.test(PC2_Model)

acf(PC2_Model$residuals)

plot(date_TS[1:1500],PC2_Model$residuals)

qqnorm(PC2_Model$residuals,xlab = 'Theoritical Quantiles', ylab = 'Sample Quantiles', main = 'QQPlot of PCA 1 Model')
qqline(PC2_Model$residuals, distribution = qnorm,probs = c(0.25, 0.75), qtype = 7)

plot(date_TS[1:1600],c(PC2_Train,PC2_Test[1:100])+Mov_Avg[1:1600], type = 'p', col = 'blue', cex = .5)
lines(date_TS[1501:1600],PC2_pred$pred+Mov_Avg[1501:1600],col= 'red', lty = 2, lwd = 2)

#PC 3

plot(date_TS,PC_USA_TS[,3],type = 'l',main ='PC #3',xlab= 'Date', ylab = 'Value')
abline(reg=lm(PC_USA_TS[,3]~date_TS), col = 'blue')

Mov_Avg =  sapply(SMA(PC_USA_TS[,3],n=50), function(x) ifelse(is.na(x),0,x))
a = PC_USA_TS[,3] - sapply(SMA(PC_USA_TS[,3],n=50), function(x) ifelse(is.na(x),0,x))

plot(date_TS,a,main ='PC #3',xlab= 'Date', ylab = 'Value',type ='l')
abline(reg=lm(a~date_TS), col = 'blue')
#lines(date_train,SMA(PC_USA_Train[,1],n=50))

#a = PC_USA_Train[,1] - sapply(SMA(PC_USA_Train[,1],n=50), function(x) ifelse(is.na(x),0,x))

PC3_Train = a[1:1500]
PC3_Test = a[-c(1:1500)]

acf(PC3_Train)
pacf(PC3_Train)

adf.test(PC3_Train)
#adf.test(diff(PC_USA_Train[,2]))

auto.arima(PC3_Train)
plot(PC3_Train)
#plot(diff(PC_USA_Train[,1],lag = 300))
PC3_Model = arima(PC3_Train,order = c(1,0,1))
summary(PC3_Model)
PC3_pred <- predict(PC3_Model, n.ahead = 100)
plot(c(PC3_Train,PC3_Test))
lines(c(rep(0,length(PC3_Train)),PC3_pred$pred), col = 'red')

McLeod.Li.test(PC3_Model)

acf(PC3_Model$residuals)

plot(date_train,PC3_Model$residuals)

qqnorm(PC3_Model$residuals,xlab = 'Theoritical Quantiles', ylab = 'Sample Quantiles', main = 'QQPlot of PCA 1 Model')
qqline(PC3_Model$residuals, distribution = qnorm,probs = c(0.25, 0.75), qtype = 7)


plot(date_TS[1:1600],c(PC3_Train,PC3_Test[1:100])+Mov_Avg[1:1600],  type = 'p', col = 'blue', cex = .5)
lines(date_TS[1501:1600],PC3_pred$pred+Mov_Avg[1501:1600],col= 'red', lty = 2, lwd = 2)

#Recombination
PC_Pred = cbind(PC1_pred$pred,PC2_pred$pred,PC3_pred$pred)
PCA_YC_Pred = PC_Pred%*%t(factor_loadings_TS)
PCA_YC_Pred = apply(PCA_YC_Pred,1,function(x) x*scale+center)
PCA_YC_Pred = t(PCA_YC_Pred)

#Test 10th year maturity
plot(date_TS[1501:1600],USA_YC_SPline_TS[1501:1600,10],ylim = c(0,0.05))
lines(date_TS[1501:1600],PCA_YC_Pred[,10], col='red')

#Test 20th year maturity
plot(date_TS[1501:1600],USA_YC_SPline_TS[1501:1600,20],ylim = c(0,0.05))
lines(date_TS[1501:1600],PCA_YC_Pred[,20], col='red')

##############Frequency Distribution of Yields and Percentage Change in Yields################################
date_USA = USA_YC$Date
y.pca = prcomp(USA_YC_SPline,scale = T,center = T)
cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)
factor_loadings_YC = y.pca$rotation[,1:3]
PC_YC = y.pca$x[,1:3]
scale_YC = y.pca$scale
center_YC = y.pca$center

hist(PC_YC[,1], breaks = 50)
hist(PC_YC[,2], breaks = 50)
hist(PC_YC[,3], breaks = 50)

PC_PCYC = cbind(diff(PC_YC[,1])/PC_YC[-length(PC_YC[,1]),1],diff(PC_YC[,2])/PC_YC[-length(PC_YC[,2]),2],
                diff(PC_YC[,3])/PC_YC[-length(PC_YC[,3]),3])

PCYC_USA = apply(USA_YC_SPline,2,function(x) diff(x)/x[-length(x)])
y.pca = prcomp(PCYC_USA,scale = T,center = T)
cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)
factor_loadings_PCYC = y.pca$rotation[,1:3]
PC_PCYC = y.pca$x[,1:3]
scale_PCYC = y.pca$scale
center_PCYC = y.pca$center

hist(PC_PCYC[,1], breaks = 50)
hist(PC_PCYC[,2], breaks = 50)
hist(PC_PCYC[,3], breaks = 50)

qqnorm(PC_PCYC[,1],xlab = 'Theoritical Quantiles', ylab = 'Sample Quantiles', main = 'QQPlot of YCPC PCA 1')
qqline(PC_PCYC[,1], distribution = qnorm,probs = c(0.25, 0.75), qtype = 7)

qqnorm(PC_PCYC[,2],xlab = 'Theoritical Quantiles', ylab = 'Sample Quantiles', main = 'QQPlot of YCPC PCA 2')
qqline(PC_PCYC[,2], distribution = qnorm,probs = c(0.25, 0.75), qtype = 7)

qqnorm(PC_PCYC[,3],xlab = 'Theoritical Quantiles', ylab = 'Sample Quantiles', main = 'QQPlot of YCPC PCA 3')
qqline(PC_PCYC[,3], distribution = qnorm,probs = c(0.25, 0.75), qtype = 7)

h = hist(PC_PCYC[,1], breaks = 50)
lines(seq(min(PC_PCYC[,1]),max(PC_PCYC[,1]),length = 400), 
      dnorm(seq(min(PC_PCYC[,1]),max(PC_PCYC[,1]),length = 400), 
            mean = mean(PC_PCYC[,1]), 
            sd = sd(PC_PCYC[,1]))*diff(h$mids[1:2])*length(PC_PCYC[,1]), col = 'red')

h = hist(PC_PCYC[,2], breaks = 50)
lines(seq(min(PC_PCYC[,2]),max(PC_PCYC[,2]),length = 400), 
      dnorm(seq(min(PC_PCYC[,2]),max(PC_PCYC[,2]),length = 400), 
            mean = mean(PC_PCYC[,2]), 
            sd = sd(PC_PCYC[,2]))*diff(h$mids[1:2])*length(PC_PCYC[,2]), col = 'red')

h = hist(PC_PCYC[,3], breaks = 50)
lines(seq(min(PC_PCYC[,3]),max(PC_PCYC[,3]),length = 400), 
      dnorm(seq(min(PC_PCYC[,3]),max(PC_PCYC[,3]),length = 400), 
            mean = mean(PC_PCYC[,3]), 
            sd = sd(PC_PCYC[,3]))*diff(h$mids[1:2])*length(PC_PCYC[,3]), col = 'red')

print(paste("PCYC PC 1 : Mean=",round(mean(PC_PCYC[,1]),2),", St Dev=",round(sd(PC_PCYC[,1]),2),", Skewness=", round(skewness(PC_PCYC[,1]),2),
            "Kurtosis= ", round(kurtosis(PC_PCYC[,1],method = 'excess'),2)))

print(paste("PCYC PC 2 : Mean=",round(mean(PC_PCYC[,2]),2),", St Dev=",round(sd(PC_PCYC[,2]),2),", Skewness=", round(skewness(PC_PCYC[,2]),2),
            "Kurtosis= ", round(kurtosis(PC_PCYC[,2],method = 'excess'),2)))

print(paste("PCYC PC 3 : Mean=",round(mean(PC_PCYC[,3]),2),", St Dev=",round(sd(PC_PCYC[,2]),2),", Skewness=", round(skewness(PC_PCYC[,3]),2),
            "Kurtosis= ", round(kurtosis(PC_PCYC[,3],method = 'excess'),2)))

#Simulate using Normal
PCA1_Norm = rnorm(1000, mean = mean(PC_PCYC[,1]), sd = sd(PC_PCYC[,1]))
Q_PC1 = quantile(PC_PCYC[,1], probs = c(1:9)/10)
Q_PC1 = rbind(Q_PC1,quantile(PCA1_Norm, probs = c(1:9)/10))

PCA2_Norm = rnorm(1000, mean = mean(PC_PCYC[,2]), sd = sd(PC_PCYC[,2]))
Q_PC2 = quantile(PC_PCYC[,2], probs = c(1:9)/10)
Q_PC2 = rbind(Q_PC2,quantile(PCA2_Norm, probs = c(1:9)/10))

PCA3_Norm = rnorm(1000, mean = mean(PC_PCYC[,3]), sd = sd(PC_PCYC[,3]))
Q_PC3 = quantile(PC_PCYC[,3], probs = c(1:9)/10)
Q_PC3 = rbind(Q_PC3,quantile(PCA3_Norm, probs = c(1:9)/10))

PCA_Norm = cbind(PCA1_Norm,PCA2_Norm,PCA3_Norm)
PCYC_Norm = PCA_Norm%*%t(factor_loadings_PCYC)
PCYC_Norm = apply(PCYC_Norm,1,function(x) x*scale_PCYC+center_PCYC)
PCYC_Norm = t(PCYC_NOrm)

#Simulating yield curves using last date actual yield curve
YC_Norm = t(apply(PCYC_Norm + 1, 2, function(x) x*USA_YC_SPline[5002,]))

#Laplace distribution

bestfit = function(x) {
  loc_array = seq(from = mean(x)-2,to = mean(x)+2, length.out = 100)
  sca_array = seq(from = .001,to = 10, length.out = 100)
  sumsq_min = 1000000
  sca_opt = 0
  loc_opt = 0
  for (i in 1:100) {
    for (j in 1:100) {
      sumsq = sum((quantile(rlaplace(1000, location = loc_array[i], scale = sca_array[j]),probs = c(1:99)/100) - quantile(x,probs = c(1:99)/100))^2)
      if (sumsq<sumsq_min) {
        sumsq_min = sumsq
        sca_opt = sca_array[j]
        loc_opt = loc_array[i]
      }
    }
  }
  return(c(loc_opt,sca_opt))
}

param1 = bestfit(PC_PCYC[,1])
h = hist(PC_PCYC[,1], breaks = 50)
lines(seq(min(PC_PCYC[,1]),max(PC_PCYC[,1]),length = 400), 
      dlaplace(seq(min(PC_PCYC[,1]),max(PC_PCYC[,1]),length = 400), 
               location = param1[1], scale = param1[2])*diff(h$mids[1:2])*length(PC_PCYC[,1]), col = 'red')

qqplot(qlaplace(ppoints(1000),scale = param1[2], location = param1[1]), PC_PCYC[,1], 
       main = "Laplace Q-Q plot",xlab = "Theoretical quantiles", ylab = "Sample quantiles")
abline(c(0,1), col = "red", lwd = 2)

param2 = bestfit(PC_PCYC[,2])
h = hist(PC_PCYC[,2], breaks = 50)
lines(seq(min(PC_PCYC[,2]),max(PC_PCYC[,2]),length = 400), 
      dlaplace(seq(min(PC_PCYC[,2]),max(PC_PCYC[,2]),length = 400), 
               location = param2[1], scale = param2[2])*diff(h$mids[1:2])*length(PC_PCYC[,2]), col = 'red')
qqplot(qlaplace(ppoints(1000),scale = param2[2], location = param2[1]), PC_PCYC[,2], 
       main = "Laplace Q-Q plot",xlab = "Theoretical quantiles", ylab = "Sample quantiles")
abline(c(0,1), col = "red", lwd = 2)

param3 = bestfit(PC_PCYC[,3])
h = hist(PC_PCYC[,3], breaks = 50)
lines(seq(min(PC_PCYC[,3]),max(PC_PCYC[,3]),length = 400), 
      dlaplace(seq(min(PC_PCYC[,3]),max(PC_PCYC[,3]),length = 400), 
               location = param3[1], scale = param3[2])*diff(h$mids[1:2])*length(PC_PCYC[,3]), col = 'red')
qqplot(qlaplace(ppoints(1000),scale = param3[2], location = param3[1]), PC_PCYC[,3], 
       main = "Laplace Q-Q plot",xlab = "Theoretical quantiles", ylab = "Sample quantiles")
abline(c(0,1), col = "red", lwd = 2)

PCA_Laplace = cbind(rlaplace(1000,location = param1[1],scale = param1[2]),
                 rlaplace(1000,location = param2[1],scale = param2[2]),
                 rlaplace(1000,location = param3[1],scale = param3[2]))
PCYC_Laplace = PCA_Laplace%*%t(factor_loadings_PCYC)
PCYC_Laplace = apply(PCYC_Laplace,1,function(x) x*scale_PCYC+center_PCYC)
PCYC_Laplace = t(PCYC_Laplace)

YC_Laplace = t(apply(PCYC_Laplace + 1, 1, function(x) x*USA_YC_SPline[5002,]))

Lap_Plot = YC_Laplace
Lap_Plot = as.data.frame(Lap_Plot)
colnames(Lap_Plot) = c(1:30)
Lap_Plot$Sims = rownames(Lap_Plot)
Lap_Plot = melt(Lap_Plot, id.vars="Sims")
Lap_Plot$Maturity = as.numeric(gsub("time", "", Lap_Plot$variable))

ggplot(Lap_Plot, aes(x=Maturity, y=value, group=Sims)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  geom_line(size=0.2, alpha=0.1)
#############################################################################################################################
######## Create single factor portfolios########################################################################

Date_USA = USA_YC$Date
y.pca = prcomp(USA_YC_SPline,scale = T,center = T)
cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)
factor_loadings_USA = y.pca$rotation[,1:3]
PC_USA = y.pca$x[,1:3]
scale = y.pca$scale
center = y.pca$center
# PCA_YC_USA = PC_USA%*%t(factor_loadings_USA)
# PCA_YC_USA = apply(PCA_YC_USA,1,function(x) x*scale+center)
# PCA_YC_USA = t(PCA_YC_USA)

#Regressing against maturities
#PCA1
PCA1_Train = PC_USA[1:4752,1] 
PCA1_Test = PC_USA[4753:5002,1]
Maturities = cbind(USA_YC$`3-Year`,USA_YC$`5-Year`,USA_YC$`10-Year`)
Mat_Train = Maturities[1:4752,]
Mat_Test = Maturities[4753:5002,]
Factor_Model1 = lm(PCA1_Train ~ Mat_Train)
summary(Factor_Model1)

plot(Date_USA[1:4752],Factor_Model1$residuals,
     #ylim = c(90,100),
     type = 'l', xlab = 'Date',ylab = '', main = 'Regression Residuals',col = 'black')

PCA1_Model = Factor_Model1$coefficients[1] + Factor_Model1$coefficients[2]*Mat_Train[,1] + Factor_Model1$coefficients[3]*Mat_Train[,2] + Factor_Model1$coefficients[4]*Mat_Train[,3]

ggplot() +
  geom_point(data = data.frame(Date = date_USA[1:4752],PC1 = PCA1_Train),aes(x = Date, y = PC1),col = 'blue',shape = 1) +
  geom_line(data = data.frame(Date = date_USA[1:4752],PC1_Model = PCA1_Model),aes(x = Date, y = PC1_Model),col = 'red',size = 0.05) 

PCA1_Model_OOS = Factor_Model1$coefficients[1] + Factor_Model1$coefficients[2]*Mat_Test[,1] + Factor_Model1$coefficients[3]*Mat_Test[,2] + Factor_Model1$coefficients[4]*Mat_Test[,3]

ggplot() +
  geom_point(data = data.frame(Date = date_USA[4753:5002],PC1 = PCA1_Test),aes(x = Date, y = PC1),col = 'blue',shape = 1) +
  geom_line(data = data.frame(Date = date_USA[4753:5002],PC1_Model_OOS = PCA1_Model_OOS),aes(x = Date, y = PC1_Model_OOS),col = 'red',size = 0.05) 

plot(Date_USA[4753:5002],PCA1_Test - PCA1_Model_OOS,
     #ylim = c(90,100),
     type = 'l', xlab = 'Date',ylab = '', main = 'Regression Residuals',col = 'black')

#PCA2
PCA2_Train = PC_USA[1:4752,2] 
PCA2_Test = PC_USA[4753:5002,2]
Maturities = cbind(USA_YC$`3-Year`,USA_YC$`5-Year`,USA_YC$`10-Year`)
Mat_Train = Maturities[1:4752,]
Mat_Test = Maturities[4753:5002,]
Factor_Model2 = lm(PCA2_Train ~ Mat_Train)
summary(Factor_Model2)

plot(Date_USA[1:4752],Factor_Model2$residuals,
     #ylim = c(90,100),
     type = 'l', xlab = 'Date',ylab = '', main = 'Regression Residuals',col = 'black')

PCA2_Model = Factor_Model2$coefficients[1] + Factor_Model2$coefficients[2]*Mat_Train[,1] + Factor_Model2$coefficients[3]*Mat_Train[,2] + Factor_Model2$coefficients[4]*Mat_Train[,3]

ggplot() +
  geom_point(data = data.frame(Date = date_USA[1:4752],PC2 = PCA2_Train),aes(x = Date, y = PC2),col = 'blue',shape = 1) +
  geom_line(data = data.frame(Date = date_USA[1:4752],PC2_Model = PCA2_Model),aes(x = Date, y = PC2_Model),col = 'red',size = 0.05) 

PCA2_Model_OOS = Factor_Model2$coefficients[1] + Factor_Model2$coefficients[2]*Mat_Test[,1] + Factor_Model2$coefficients[3]*Mat_Test[,2] + Factor_Model2$coefficients[4]*Mat_Test[,3]

ggplot() +
  geom_point(data = data.frame(Date = date_USA[4753:5002],PC2 = PCA2_Test),aes(x = Date, y = PC2),col = 'blue',shape = 1) +
  geom_line(data = data.frame(Date = date_USA[4753:5002],PC2_Model_OOS = PCA2_Model_OOS),aes(x = Date, y = PC2_Model_OOS),col = 'red',size = 0.05) 

plot(Date_USA[4753:5002],PCA2_Test - PCA2_Model_OOS,
     #ylim = c(90,100),
     type = 'l', xlab = 'Date',ylab = '', main = 'Regression Residuals',col = 'black')


# PCA1_Model_onPCA2 = Factor_Model1$coefficients[1] + Factor_Model1$coefficients[2]*PC_USA[,2] + Factor_Model1$coefficients[3]*PC_USA[,2] + Factor_Model1$coefficients[4]*PC_USA[,2]
# ggplot() +
#   geom_point(data = data.frame(Date = date_USA,PC2 = PC_USA[,2]),aes(x = Date, y = PC2),col = 'blue',shape = 1) +
#   geom_line(data = data.frame(Date = date_USA,PC1_Model_onPC2 = PCA1_Model_onPCA2),aes(x = Date, y = PC1_Model_onPC2),col = 'red',size = 0.05) 


#Increasing number of maturitues doesnt increase Rsquare
PCA1_Train = PC_USA[1:4752,1] 
Maturities = cbind(USA_YC$`3-Year`,USA_YC$`5-Year`,USA_YC$`7-Year`,USA_YC$`10-Year`)
Mat_Train = Maturities[1:4752,]
Factor_Model1 = lm(PCA1_Train ~ Mat_Train)
summary(Factor_Model1)
#Decreasing number of maturitues doesnt decreases Rsquare
PCA1_Train = PC_USA[1:4752,1] 
Maturities = cbind(USA_YC$`3-Year`,USA_YC$`5-Year`)
Mat_Train = Maturities[1:4752,]
Factor_Model1 = lm(PCA1_Train ~ Mat_Train)
summary(Factor_Model1)

###################################################################################################
########################  Regress factors against macro-economic variables ########################
#Data Preprocessing
US_CAP_UTIL = US_CAP_UTIL[215:1,]
rownames(US_CAP_UTIL) = 1:nrow(US_CAP_UTIL)

US_CPI = US_CPI[1295:1,]
rownames(US_CPI) = 1:nrow(US_CPI)

US_GDP = US_GDP[295:1,]
rownames(US_GDP) = 1:nrow(US_GDP)

US_IND_PRO = US_IND_PRO[1223:1,]
rownames(US_IND_PRO) = 1:nrow(US_IND_PRO)

US_Oil_WTI = US_Oil_WTI[898:1,]
rownames(US_Oil_WTI) = 1:nrow(US_Oil_WTI)

US_SP500 = US_SP500[10087:1,]
rownames(US_SP500) = 1:nrow(US_SP500)

US_UNEM = US_UNEM[875:1,]
rownames(US_UNEM) = 1:nrow(US_UNEM)

US_Gold = US_Gold[13396:1,1:2]
rownames(US_Gold) = 1:nrow(US_Gold)

US_YC_PCA = data.frame(Date = date_USA, PC_USA)

#Cutoff Date - 1st Jan 2000 - 31 Dec 2019
US_CAP_UTIL = US_CAP_UTIL[133:213,]
rownames(US_CAP_UTIL) = 1:nrow(US_CAP_UTIL)

US_CPI = US_CPI[1045:1285,]
rownames(US_CPI) = 1:nrow(US_CPI)

US_GDP = US_GDP[213:293,]
rownames(US_GDP) = 1:nrow(US_GDP)

US_IND_PRO = US_IND_PRO[973:1213,]
rownames(US_IND_PRO) = 1:nrow(US_IND_PRO)

US_Oil_WTI = US_Oil_WTI[649:889,]
rownames(US_Oil_WTI) = 1:nrow(US_Oil_WTI)

US_SP500 = US_SP500[4815:9846,]
rownames(US_SP500) = 1:nrow(US_SP500)
US_SP500 = US_SP500[,c(1,6)]
US_SP500$Date = as.Date(US_SP500$Date)

US_UNEM = US_UNEM[625:865,]
rownames(US_UNEM) = 1:nrow(US_UNEM)

US_Gold = US_Gold[8090:13144,]
rownames(US_Gold) = 1:nrow(US_Gold)

########### Quaterly Returns %############################
QtrChange = function(x) {
  a = xts(x[,2],x[,1])
  return(quarterlyReturn(a))
}

CAP_UTIL = as.data.frame(QtrChange(US_CAP_UTIL))*100
CAP_UTIL$Date = rownames(CAP_UTIL)
CAP_UTIL = CAP_UTIL[-1,]

CPI = as.data.frame(QtrChange(US_CPI))*100
CPI$Date = rownames(CPI)
CPI = CPI[-nrow(CPI),]

GDP = as.data.frame(QtrChange(US_GDP))*100
GDP$Date = rownames(GDP)
GDP = GDP[-1,]

IND_PRO = as.data.frame(QtrChange(US_IND_PRO))*100
IND_PRO$Date = rownames(IND_PRO)
IND_PRO = IND_PRO[-nrow(IND_PRO),]

Oil = as.data.frame(QtrChange(US_Oil_WTI))*100
Oil$Date = rownames(Oil)
Oil = Oil[-nrow(Oil),]

SP500 = as.data.frame(QtrChange(US_SP500))*100
SP500$Date = rownames(SP500)
SP500 = SP500[-nrow(SP500),]

UNEM = as.data.frame(QtrChange(US_UNEM))*100
UNEM$Date = rownames(UNEM)
UNEM = UNEM[-nrow(UNEM),]

Gold = as.data.frame(QtrChange(US_Gold))*100
Gold$Date = rownames(Gold)
Gold = Gold[-nrow(Gold),]

US_YC_PCA = cbind(as.data.frame(QtrChange(US_YC_PCA[,c(1,2)]))*100,as.data.frame(QtrChange(US_YC_PCA[,c(1,3)]))*100,
          as.data.frame(QtrChange(US_YC_PCA[,c(1,4)]))*100)
colnames(US_YC_PCA) = c("PCA1","PCA2","PCA3")
US_YC_PCA$Date = rownames(US_YC_PCA)

#PCA 1 correlations
paste("Correlations b/w PC 1 and GDP (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA1,GDP$quarterly.returns),2))
paste("Correlations b/w PC 1 and Gold (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA1,Gold$quarterly.returns),2))
paste("Correlations b/w PC 1 and CPI (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA1,CPI$quarterly.returns),2))
paste("Correlations b/w PC 1 and WTI Oil (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA1,Oil$quarterly.returns),2))
paste("Correlations b/w PC 1 and S&P 500 (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA1,SP500$quarterly.returns),2))
paste("Correlations b/w PC 1 and Unemployment (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA1,UNEM$quarterly.returns),2))
paste("Correlations b/w PC 1 and Industrial Production (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA1,IND_PRO$quarterly.returns),2))
paste("Correlations b/w PC 1 and Capacity Utilisation (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA1,CAP_UTIL$quarterly.returns),2))

#PCA 2 correlations
paste("Correlations b/w PC 2 and GDP (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA2,GDP$quarterly.returns),2))
paste("Correlations b/w PC 2 and Gold (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA2,Gold$quarterly.returns),2))
paste("Correlations b/w PC 2 and CPI (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA2,CPI$quarterly.returns),2))
paste("Correlations b/w PC 2 and WTI Oil (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA2,Oil$quarterly.returns),2))
paste("Correlations b/w PC 2 and S&P 500 (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA2,SP500$quarterly.returns),2))
paste("Correlations b/w PC 2 and Unemployment (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA2,UNEM$quarterly.returns),2))
paste("Correlations b/w PC 2 and Industrial Production (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA2,IND_PRO$quarterly.returns),2))
paste("Correlations b/w PC 2 and Capacity Utilisation (Quaterly %age change)= ",round(cor(US_YC_PCA$PCA2,CAP_UTIL$quarterly.returns),2))

#Plots PC 1
plot(US_YC_PCA$PCA1,GDP$quarterly.returns,type = 'p',xlab = 'PC 1',ylab = 'GDP', xlim = c(-100,100))
lines(US_YC_PCA$PCA1, predict.lm(lm(GDP$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)), newdata = list(x = US_YC_PCA$PCA1)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(GDP$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

plot(US_YC_PCA$PCA1,Gold$quarterly.returns,type = 'p',xlab = 'PC 1',ylab = 'Gold', xlim = c(-100,100))
lines(US_YC_PCA$PCA1, predict.lm(lm(Gold$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)), newdata = list(x = US_YC_PCA$PCA1)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(Gold$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

plot(US_YC_PCA$PCA1,CPI$quarterly.returns,type = 'p',xlab = 'PC 1',ylab = 'CPI',xlim = c(-100,100))
lines(US_YC_PCA$PCA1, predict.lm(lm(CPI$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)), newdata = list(x = US_YC_PCA$PCA1)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(CPI$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

plot(US_YC_PCA$PCA1,Oil$quarterly.returns,type = 'p',xlab = 'PC 1',ylab = 'WTI Oil',xlim = c(-100,100))
lines(US_YC_PCA$PCA1, predict.lm(lm(Oil$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)), newdata = list(x = US_YC_PCA$PCA1)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(Oil$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

plot(US_YC_PCA$PCA1,SP500$quarterly.returns,type = 'p',xlab = 'PC 1',ylab = 'S&P 500',xlim = c(-100,100))
lines(US_YC_PCA$PCA1, predict.lm(lm(SP500$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)), newdata = list(x = US_YC_PCA$PCA1)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(SP500$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

plot(US_YC_PCA$PCA1,UNEM$quarterly.returns,type = 'p',xlab = 'PC 1',ylab = 'Unemployment Rate',xlim = c(-100,100))
lines(US_YC_PCA$PCA1, predict.lm(lm(UNEM$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)), newdata = list(x = US_YC_PCA$PCA1)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(UNEM$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

plot(US_YC_PCA$PCA1,IND_PRO$quarterly.returns,type = 'p',xlab = 'PC 1',ylab = 'Industrial Production',xlim = c(-100,100))
lines(US_YC_PCA$PCA1, predict.lm(lm(IND_PRO$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)), newdata = list(x = US_YC_PCA$PCA1)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(IND_PRO$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

plot(US_YC_PCA$PCA1,CAP_UTIL$quarterly.returns,type = 'p',xlab = 'PC 1',ylab = 'Capacity Utilisation',xlim = c(-100,100))
lines(US_YC_PCA$PCA1, predict.lm(lm(CAP_UTIL$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)), newdata = list(x = US_YC_PCA$PCA1)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(CAP_UTIL$quarterly.returns ~ poly(US_YC_PCA$PCA1, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

#Plots PC 2
plot(US_YC_PCA$PCA2,GDP$quarterly.returns,type = 'p',xlab = 'PC 2',ylab = 'GDP', xlim = c(-100,100))
lines(US_YC_PCA$PCA2, predict.lm(lm(GDP$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)), newdata = list(x = US_YC_PCA$PCA2)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(GDP$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

plot(US_YC_PCA$PCA2,Gold$quarterly.returns,type = 'p',xlab = 'PC 2',ylab = 'Gold', xlim = c(-100,100))
lines(US_YC_PCA$PCA2, predict.lm(lm(Gold$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)), newdata = list(x = US_YC_PCA$PCA2)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(Gold$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

plot(US_YC_PCA$PCA2,CPI$quarterly.returns,type = 'p',xlab = 'PC 2',ylab = 'CPI',xlim = c(-100,100))
lines(US_YC_PCA$PCA2, predict.lm(lm(CPI$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)), newdata = list(x = US_YC_PCA$PCA2)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(CPI$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

plot(US_YC_PCA$PCA2,Oil$quarterly.returns,type = 'p',xlab = 'PC 2',ylab = 'WTI Oil',xlim = c(-100,100))
lines(US_YC_PCA$PCA2, predict.lm(lm(Oil$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)), newdata = list(x = US_YC_PCA$PCA2)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(Oil$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

plot(US_YC_PCA$PCA2,SP500$quarterly.returns,type = 'p',xlab = 'PC 2',ylab = 'S&P 500',xlim = c(-100,100))
lines(US_YC_PCA$PCA2, predict.lm(lm(SP500$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)), newdata = list(x = US_YC_PCA$PCA2)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(SP500$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

plot(US_YC_PCA$PCA2,UNEM$quarterly.returns,type = 'p',xlab = 'PC 2',ylab = 'Unemployment Rate',xlim = c(-100,100))
lines(US_YC_PCA$PCA2, predict.lm(lm(UNEM$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)), newdata = list(x = US_YC_PCA$PCA2)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(UNEM$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

plot(US_YC_PCA$PCA2,IND_PRO$quarterly.returns,type = 'p',xlab = 'PC 2',ylab = 'Industrial Production',xlim = c(-100,100))
lines(US_YC_PCA$PCA2, predict.lm(lm(IND_PRO$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)), newdata = list(x = US_YC_PCA$PCA2)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(IND_PRO$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

plot(US_YC_PCA$PCA2,CAP_UTIL$quarterly.returns,type = 'p',xlab = 'PC 2',ylab = 'Capacity Utilisation',xlim = c(-100,100))
lines(US_YC_PCA$PCA2, predict.lm(lm(CAP_UTIL$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)), newdata = list(x = US_YC_PCA$PCA2)), col = 2)  ## add regression curve (colour: red)
legend("bottomleft",legend = paste("Adjusted R-squared = ",round(summary(lm(CAP_UTIL$quarterly.returns ~ poly(US_YC_PCA$PCA2, 1)))$adj.r.squared,2),sep = ''), 
       pt.cex = 1, cex = 1,text.col = "black")

#Regression
Reg_Data_PC1 = data.frame(PC = US_YC_PCA$PCA1,
                          GDP = GDP$quarterly.returns,
                          Gold = Gold$quarterly.returns,
                          CPI = CPI$quarterly.returns,
                          Oil = Oil$quarterly.returns,
                          SP500 = SP500$quarterly.returns,
                          Unem = UNEM$quarterly.returns,
                          IndPro = IND_PRO$quarterly.returns,
                          CapUtil = CAP_UTIL$quarterly.returns)
#Econ_Model_1 = lm(PC~. ,Reg_Data_PC1)
Econ_Model_1 = lm(PC~. - SP500 - Gold - IndPro - Oil - Unem,Reg_Data_PC1)
summary(Econ_Model_1)

Reg_Data_PC2 = data.frame(PC = US_YC_PCA$PCA2,
                          GDP = GDP$quarterly.returns,
                          Gold = Gold$quarterly.returns,
                          CPI = CPI$quarterly.returns,
                          Oil = Oil$quarterly.returns,
                          SP500 = SP500$quarterly.returns,
                          Unem = UNEM$quarterly.returns,
                          IndPro = IND_PRO$quarterly.returns,
                          CapUtil = CAP_UTIL$quarterly.returns)
#Econ_Model_2 = lm(PC~.,Reg_Data_PC2)
Econ_Model_2 = lm(PC~Unem ,Reg_Data_PC2)
summary(Econ_Model_2)

################################################################################### 
######## Find PCs of different countries #########################################

#USA, CAN, JPN, SWISS, FRANCE
load('YieldCurve.RData')
#USA
USA_YC = USA_YC[3993:241,c(1,5:12)]
USA_YC[,2:9] = USA_YC[,2:9]/100
rownames(USA_YC) = 1:nrow(USA_YC)
USA_YC[is.na(USA_YC[,9]),9] = USA_YC[is.na(USA_YC[,9]),8]
x = c(1,2,3,5,7,10,20,30)
USA_YC_SPline = matrix(rep(0,30*nrow(USA_YC)),nrow = nrow(USA_YC),ncol = 30)
for (i in 1:nrow(USA_YC)) {
  y = USA_YC[i,-1]
  USA_YC_SPline[i,] = spline(x,y,n=30)$y
}
date_USA = USA_YC$Date
y.pca = prcomp(USA_YC_SPline,scale = T,center = T)
factor_loadings_USA = y.pca$rotation[,1:3]
PC_USA = y.pca$x[,1:3]
scale = y.pca$scale
center = y.pca$center
PCA_YC_USA = PC_USA%*%t(factor_loadings_USA)
PCA_YC_USA = apply(PCA_YC_USA,1,function(x) x*scale+center)
PCA_YC_USA = t(PCA_YC_USA)
plot(c(1:10),(cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)*100)[1:10],ylim = c(90,100),type = 'b',xlab = 'Principal Components',
     ylab = 'Percentage of cumulative varinace explained', main = 'Cumlative variance explained by Pricipal Components',col = 'red',
     xaxt = 'n')
axis(side = 1, at = c(1:10))
abline(h = 100,col = 'gray',lty=2)
Expl_Var = data.frame(USA=(cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)*100)[1:10])
a = data.frame(Date = rep(date_USA,3),Values = c(PC_USA[,1],PC_USA[,2],PC_USA[,3]),PC = c(rep('PC 1',length(date_USA)),rep('PC 2',length(date_USA)),rep('PC 3',length(date_USA))))
ggplot(data = a) +
  geom_line(aes(x = Date, y = Values, colour = PC)) +
  geom_hline(yintercept = 0,linetype = 'dashed', color = 'grey') +
  scale_x_date(date_breaks = '2 years') +
  labs(title = "Evolution of Principal Components over time (USA)", x = "Date", y = "", color = "Principal Components") +
  scale_color_manual(labels = c("PC 1", "PC 2", 'PC 3'), values = c("red", "green",'blue')) +
  theme_bw() +
  theme(legend.position = "top",plot.title = element_text(hjust = 0.5))
# rand_rows = c(1035,2467,3182)
# rand_dates = date_USA[rand_rows]
# b = data.frame(Maturity = c(1:30))
# for (i in 1:3) {
#   b = cbind(b,data.frame(a = USA_YC_SPline[rand_rows[i],]),data.frame(b = PCA_YC_USA[rand_rows[i],]))
#   names(b)[(i*2):(i*2+1)] <- c(paste('Observed-',rand_dates[i],sep = ''),paste(rand_dates[i],'-PCA',sep = ''))
# }
# ggplot(data = b) +
#   geom_line(aes(x=b[,1], y=b[,2]), color = 'red') +
#   geom_point(aes(x=b[,1], y=b[,3]), color = 'red' ) +
#   geom_line(aes(x=b[,1], y=b[,4]), color = 'green') +
#   geom_point(aes(x=b[,1], y=b[,5]), color = 'green') +
#   geom_line(aes(x=b[,1], y=b[,6]), color = 'blue') +
#   geom_point(aes(x=b[,1], y=b[,7]),  color = 'blue') +
#   # geom_line(aes(x=b[,1], y=b[,8]), color = 'orange') +
#   # geom_point(aes(x=b[,1], y=b[,9]),  color = 'orange') +
#   geom_label(label='24-02-2004', x=10.1,y=0.045,label.size = 0.2,color = "black",fill=NA) +
#   geom_label(label='10-11-2009', x=20.1,y=0.045,label.size = 0.2,color = "black",fill=NA) +
#   geom_label(label='17-09-2012', x=8.1,y=0.02,label.size = 0.2,color = "black",fill=NA) +
#   # geom_label(label='07-01-2019', x=15.1,y=0.03,label.size = 0.2,color = "black",fill=NA) +
#   labs(title = "Randomly selected yield curves and PC fit", x = "Maturity", y = "Rate") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5))

#Japan
JPN_YC = JPN_YC[3906:233,c(1:15)]
JPN_YC[,2:15] = JPN_YC[,2:15]/100
rownames(JPN_YC) = 1:nrow(JPN_YC)
JPN_YC[is.na(JPN_YC[,14]),14] = JPN_YC[is.na(JPN_YC[,14]),13]
x = c(1:10,15,20,25,30)
JPN_YC_SPline = matrix(rep(0,30*nrow(JPN_YC)),nrow = nrow(JPN_YC),ncol = 30)
for (i in 1:nrow(JPN_YC)) {
  y = JPN_YC[i,-1]
  JPN_YC_SPline[i,] = spline(x,y,n=30)$y
}
date_JPN = JPN_YC$Date
y.pca = prcomp(JPN_YC_SPline,scale = T,center = T)
factor_loadings_JPN = y.pca$rotation[,1:3]
PC_JPN = y.pca$x[,1:3]
scale = y.pca$scale
center = y.pca$center
PCA_YC_JPN = PC_JPN%*%t(factor_loadings_JPN)
PCA_YC_JPN = apply(PCA_YC_JPN,1,function(x) x*scale+center)
PCA_YC_JPN = t(PCA_YC_JPN)
plot(c(1:10),(cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)*100)[1:10],ylim = c(90,100),type = 'b',xlab = 'Principal Components',
     ylab = 'Percentage of cumulative varinace explained', main = 'Cumlative variance explained by Pricipal Components',col = 'red',
     xaxt = 'n')
axis(side = 1, at = c(1:10))
abline(h = 100,col = 'gray',lty=2)
Expl_Var$JPN = (cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)*100)[1:10]
a = data.frame(Date = rep(date_JPN,3),Values = c(-PC_JPN[,1],PC_JPN[,2],PC_JPN[,3]),PC = c(rep('PC 1',length(date_JPN)),rep('PC 2',length(date_JPN)),rep('PC 3',length(date_JPN))))
ggplot(data = a) +
  geom_line(aes(x = Date, y = Values, colour = PC)) +
  geom_hline(yintercept = 0,linetype = 'dashed', color = 'grey') +
  scale_x_date(date_breaks = '2 years') +
  labs(title = "Evolution of Principal Components over time (Japan)", x = "Date", y = "", color = "Principal Components") +
  scale_color_manual(labels = c("PC 1", "PC 2", 'PC 3'), values = c("red", "green",'blue')) +
  theme_bw() +
  theme(legend.position = "top",plot.title = element_text(hjust = 0.5))


#CANADA
CAN_YC = CAN_YC[3992:241,c(1,5:11)]
CAN_YC[,2:8] = CAN_YC[,2:8]/100
rownames(CAN_YC) = 1:nrow(CAN_YC)
CAN_YC[is.na(CAN_YC[,2]),2] = 0
CAN_YC[is.na(CAN_YC[,3]),3] = CAN_YC[is.na(CAN_YC[,3]),2]
CAN_YC[is.na(CAN_YC[,4]),4] = CAN_YC[is.na(CAN_YC[,4]),3]
CAN_YC[is.na(CAN_YC[,5]),5] = CAN_YC[is.na(CAN_YC[,5]),4]
CAN_YC[is.na(CAN_YC[,6]),6] = CAN_YC[is.na(CAN_YC[,6]),5]
CAN_YC[is.na(CAN_YC[,7]),7] = CAN_YC[is.na(CAN_YC[,7]),6]
CAN_YC[is.na(CAN_YC[,8]),8] = CAN_YC[is.na(CAN_YC[,8]),7]
x = c(1,2,3,5,7,10,30)
CAN_YC_SPline = matrix(rep(0,30*nrow(CAN_YC)),nrow = nrow(CAN_YC),ncol = 30)
for (i in 1:nrow(CAN_YC)) {
  y = CAN_YC[i,-1]
  CAN_YC_SPline[i,] = spline(x,y,n=30)$y
}
date_CAN = CAN_YC$Date
y.pca = prcomp(CAN_YC_SPline,scale = T,center = T)
factor_loadings_CAN = y.pca$rotation[,1:3]
PC_CAN = y.pca$x[,1:3]
scale = y.pca$scale
center = y.pca$center
PCA_YC_CAN = PC_CAN%*%t(factor_loadings_CAN)
PCA_YC_CAN = apply(PCA_YC_CAN,1,function(x) x*scale+center)
PCA_YC_CAN = t(PCA_YC_CAN)
plot(c(1:10),(cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)*100)[1:10],ylim = c(90,100),type = 'b',xlab = 'Principal Components',
     ylab = 'Percentage of cumulative varinace explained', main = 'Cumlative variance explained by Pricipal Components',col = 'red',
     xaxt = 'n')
axis(side = 1, at = c(1:10))
abline(h = 100,col = 'gray',lty=2)
Expl_Var$CAN = (cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)*100)[1:10]
a = data.frame(Date = rep(date_CAN,3),Values = c(PC_CAN[,1],PC_CAN[,2],PC_CAN[,3]),PC = c(rep('PC 1',length(date_CAN)),rep('PC 2',length(date_CAN)),rep('PC 3',length(date_CAN))))
ggplot(data = a) +
  geom_line(aes(x = Date, y = Values, colour = PC)) +
  geom_hline(yintercept = 0,linetype = 'dashed', color = 'grey') +
  scale_x_date(date_breaks = '2 years') +
  labs(title = "Evolution of Principal Components over time (Canada)", x = "Date", y = "", color = "Principal Components") +
  scale_color_manual(labels = c("PC 1", "PC 2", 'PC 3'), values = c("red", "green",'blue')) +
  theme_bw() +
  theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) +
  ylim(-15,15)

#SWISS
SWISS_YC = SWISS_YC[4071:244,c(1,6:14)]
SWISS_YC[,2:10] = SWISS_YC[,2:10]/100
rownames(SWISS_YC) = 1:nrow(SWISS_YC)
SWISS_YC[is.na(SWISS_YC[,2]),2] = SWISS_YC[is.na(SWISS_YC[,2]),3]
SWISS_YC[is.na(SWISS_YC[,2]),2] = 0
SWISS_YC[is.na(SWISS_YC[,3]),3] = SWISS_YC[is.na(SWISS_YC[,3]),2]
SWISS_YC[is.na(SWISS_YC[,4]),4] = SWISS_YC[is.na(SWISS_YC[,4]),3]
SWISS_YC[is.na(SWISS_YC[,5]),5] = SWISS_YC[is.na(SWISS_YC[,5]),4]
SWISS_YC[is.na(SWISS_YC[,6]),6] = SWISS_YC[is.na(SWISS_YC[,6]),5]
SWISS_YC[is.na(SWISS_YC[,7]),7] = SWISS_YC[is.na(SWISS_YC[,7]),6]
SWISS_YC[is.na(SWISS_YC[,8]),8] = SWISS_YC[is.na(SWISS_YC[,8]),7]
SWISS_YC[is.na(SWISS_YC[,9]),9] = SWISS_YC[is.na(SWISS_YC[,9]),8]
SWISS_YC[is.na(SWISS_YC[,10]),10] = SWISS_YC[is.na(SWISS_YC[,10]),9]
x = c(1,2,3,4,5,7,10,20,30)
SWISS_YC_SPline = matrix(rep(0,30*nrow(SWISS_YC)),nrow = nrow(SWISS_YC),ncol = 30)
for (i in 1:nrow(SWISS_YC)) {
  y = SWISS_YC[i,-1]
  SWISS_YC_SPline[i,] = spline(x,y,n=30)$y
}
date_SWISS = SWISS_YC$Date
y.pca = prcomp(SWISS_YC_SPline,scale = T,center = T)
factor_loadings_SWISS = y.pca$rotation[,1:3]
PC_SWISS = y.pca$x[,1:3]
scale = y.pca$scale
center = y.pca$center
PCA_YC_SWISS = PC_SWISS%*%t(factor_loadings_SWISS)
PCA_YC_SWISS = apply(PCA_YC_SWISS,1,function(x) x*scale+center)
PCA_YC_SWISS = t(PCA_YC_SWISS)
plot(c(1:10),(cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)*100)[1:10],ylim = c(90,100),type = 'b',xlab = 'Principal Components',
     ylab = 'Percentage of cumulative varinace explained', main = 'Cumlative variance explained by Pricipal Components',col = 'red',
     xaxt = 'n')
axis(side = 1, at = c(1:10))
abline(h = 100,col = 'gray',lty=2)
Expl_Var$SWISS = (cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)*100)[1:10]
a = data.frame(Date = rep(date_SWISS,3),Values = c(PC_SWISS[,1],PC_SWISS[,2],PC_SWISS[,3]),PC = c(rep('PC 1',length(date_SWISS)),rep('PC 2',length(date_SWISS)),rep('PC 3',length(date_SWISS))))
ggplot(data = a) +
  geom_line(aes(x = Date, y = Values, colour = PC)) +
  geom_hline(yintercept = 0,linetype = 'dashed', color = 'grey') +
  scale_x_date(date_breaks = '2 years') +
  labs(title = "Evolution of Principal Components over time (Switzerland)", x = "Date", y = "", color = "Principal Components") +
  scale_color_manual(labels = c("PC 1", "PC 2", 'PC 3'), values = c("red", "green",'blue')) +
  theme_bw() +
  theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) +
  ylim(-15,15)

#Explained Variance Chart
Expl_Var = gather(Expl_Var, Country, ExplVar, USA:SWISS)
Expl_Var$PC = rep(c(1:10),4)
ggplot(data = Expl_Var) +
  geom_line(aes(x = PC,y=ExplVar, color = Country)) +
  scale_x_continuous(breaks = c(1:10)) +
  theme_bw() +
  theme(legend.position = 'top',plot.title = element_text(hjust = 0.5))+
  labs(title = "Explained Variance of PCs for different countries", x = "PC #", y = "Explained Variance") 

#Taking common dates
date_PC = Reduce(intersect, list(date_USA,date_CAN,date_JPN,date_SWISS))

row_USA = sapply(date_PC, function(x) which(date_USA==x))
PC_USA = PC_USA[row_USA,]
date_USA = date_USA[row_USA]

row_CAN = sapply(date_PC, function(x) which(date_CAN==x))
PC_CAN = PC_CAN[row_CAN,]
date_CAN = date_CAN[row_CAN]

row_JPN = sapply(date_PC, function(x) which(date_JPN==x))
PC_JPN = PC_JPN[row_JPN,]
date_JPN = date_JPN[row_JPN]

row_SWISS = sapply(date_PC, function(x) which(date_SWISS==x))
PC_SWISS = PC_SWISS[row_SWISS,]
date_SWISS = date_SWISS[row_SWISS]

#Checking Correlation
PC1 = data.frame(USA = PC_USA[,1],CAN = PC_CAN[,1], JPN = -PC_JPN[,1], SWISS = PC_SWISS[,1])
corrplot(cor(PC1), type = 'upper', method = 'number')

PC2 = data.frame(USA = PC_USA[,2],CAN = PC_CAN[,2], JPN = PC_JPN[,2], SWISS = PC_SWISS[,2])
corrplot(cor(PC2), type = 'upper', method = 'number')

PC3 = data.frame(USA = PC_USA[,3],CAN = PC_CAN[,3], JPN = PC_JPN[,3], SWISS = PC_SWISS[,3])
corrplot(cor(PC3), type = 'upper', method = 'number')

PCs = data.frame(USA_1 = PC_USA[,1],CAN_1 = PC_CAN[,1], JPN_1 = PC_JPN[,1], SWISS_1 = PC_SWISS[,1],
                 USA_2 = PC_USA[,2],CAN_2 = PC_CAN[,2], JPN_2 = PC_JPN[,2], SWISS_2 = PC_SWISS[,2],
                 USA_3 = PC_USA[,3],CAN_3 = PC_CAN[,3], JPN_3 = PC_JPN[,3], SWISS_3 = PC_SWISS[,3])
corrplot(cor(PCs), type = 'full', method = 'number')

#Creating global PC
y.pca = prcomp(PCs,scale = T,center = T)
factor_loadings_Global = y.pca$rotation[,1:6]
PC_Global = y.pca$x[,1:6]
scale = y.pca$scale
center = y.pca$center
PCA_YC_Global = PC_Global%*%t(factor_loadings_Global)
PCA_YC_Global = apply(PCA_YC_Global,1,function(x) x*scale+center)
PCA_YC_Global = t(PCA_YC_Global)
plot(c(1:10),(cumsum(y.pca$sdev^2)/sum(y.pca$sdev^2)*100)[1:10],ylim = c(0,100),type = 'b',xlab = 'Principal Components',
     ylab = 'Percentage of cumulative varinace explained', main = 'Cumlative variance explained by global Pricipal Components',col = 'red',
     xaxt = 'n')
axis(side = 1, at = c(1:10))
abline(h = 100,col = 'gray',lty=2)
a = data.frame(Date = rep(date_PC,6),
               Values = c(PC_Global[,1],PC_Global[,2],PC_Global[,3],PC_Global[,4],PC_Global[,5],PC_Global[,6]),
               PC = c(rep('PC 1',length(date_PC)),rep('PC 2',length(date_PC)),rep('PC 3',length(date_PC)),rep('PC 4',length(date_PC)),rep('PC 5',length(date_PC)),rep('PC 6',length(date_PC))))
a$Date = as.Date(date_PC)
ggplot(data = a) +
  geom_line(aes(x = Date, y = Values, colour = PC)) +
  geom_hline(yintercept = 0,linetype = 'dashed', color = 'grey') +
  scale_x_date(date_breaks = '2 years') +
  labs(title = "Evolution of Principal Components over time", x = "Date", y = "", color = "Principal Components") +
  scale_color_manual(labels = c("PC 1", "PC 2", 'PC 3', 'PC 4', 'PC 5', 'PC 6'), values = c("red", "green",'blue','violet','orange','grey')) +
  theme_bw() +
  theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) +
  ylim(-5,5)
