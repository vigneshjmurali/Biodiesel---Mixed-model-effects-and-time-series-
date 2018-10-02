#----------------------------
#linear regression
#--------------------------
results1=lm(mono~lipase)
results2=lm(mono~temp)
results3=lm(temp~lipase)
#---------------------------
#multiple regression with all combinations
#--------------------------
results=lm(ph~(temp+mono+lipase+temp*mono+temp*lipase+
                 lipase*mono+temp*mono*lipase),alternative)
#--------------------------
#ph subtraction between high to low
#--------------------------
results=lm(ph~(temp+mono+lipase),data25)
#---------------------------
#regression for average
#---------------------------
results=lm(ph~(temp+mono+lipase),phave)
#------------------
boxplot(ph~lipase,mono)
boxoplot(phbtos)
boxplot(ph~mono,main="phbtosn")
boxplot(ph~lipase,main="phbtosn")
#-----------------------------
#nonlinear

curve.fit=nls(rate~b,data=,
              start=list(b1=b1.0,b2=b2.0))
#----------------------------------
#plots

stripchart(ph, pch=10, cex=3)

#-------------
#ggplot
p1<-ggplot(example,aes(x=observation, y=ph))
+ theme(legend.position = "top",axis.text=element_text(size=6))
p1+geom_bar()

d=ggplot(example, aes(x=observation, y=ph))
d+geom_line()
d+geom_point(aes(color=cut))

p1 + geom_point(aes(color = "green"))
p1 + geom_point(color = "green")
p1 + geom_point(color = color)

#--------------
df<-timeseries[,c("observations")]
df<-df[lubridate::year(df$observations) %in% c(1:24),]

ts.plot(time,gpars=list(lty=c(2:13)))
----------------------------------------------
  #Read data
  Data1=read.csv("timeseries.csv", 1)
Data2=timeseries[,-1] #delect 1st column
#define every column
x=25C Min Lipase:35C Max Lipase #observation
y1=Data2[,1]
y2=Data2[,2]
y3=Data2[,3]
y4=Data2[,4]
y5=Data2[,5]
y6=Data2[,6]
y7=Data2[,7]
y8=Data2[,8]
y9=Data2[,9]
y10=Data2[,10]
y11=Data2[,11]
y12=Data2[,12]
#Main plot function
matplot(x, cbind(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12), pch=6, xlim=c(1,24),ylim=c(7,8.7),xaxt="n",col = "red",xlab = "Observation",ylab = "Ph at different Time")
axis(1,1:24)
#--------------------
install.package(zoo)
zoo.basket1<-as.zoo(basket)
basket1<-cbind(comb13,comb14,comb15,comb16,comb17,comb18,comb19,comb20,comb21,comb22,comb23,comb24)
xrange<-range(0:12)
tsRainbow<-rainbow(ncol(zoo.basket2))
plot(x=zoo.basket1,col=tsRainbow,screens=1,xlim=c(0,12),xaxt="n",xlab="25°C with all combinations",ylab="pH")
legend(x="bottomleft",legend=c("0M:1L","0M:2L","0M:3L","1M:1L","1M:2L","1M3L","2M:1L","2M:2L","2M:3L","3M:1L","3M:2L","3M:3L"),lty=2,col=tsRainbow,cex=0.5)

xtick<-seq(from=0,to=,by=)
axis(1,at=1:60,labels=TRUE)
levels(Time) = c(0,2,5,7,10,15,20,25,35,40,50,60)
#----------------------------

qplot(lipase, ph, data=data, geom=c("boxplot", "jitter"))
eruption.lm = lm(ph40 ~ mono, data=last40)
eruption.stdres = rstandard(eruption.lm)

qqnorm(eruption.stdres,
       +     ylab="Standardized Residuals",
       +     xlab="Normal Scores")
qqline(eruption.stdres)
