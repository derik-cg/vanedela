#script para analizar base vanedela
#cambiarse al directorio
rm(list=ls())
setwd("~/Investigacion/Vanedela")
#leer la base en cvs
vanedela<-read.csv("base edad cd y rd18417.csv")
# revisar que las fechas sean progresivas dentro de cd1.1 ABC
#corresponde a columnas 5:7

######################################
#make a function that takes a vector and compares the numbers
#to see if they are ordered

orve<-function(vec)
{
  cmp<-!is.na(vec)
  if (sum(cmp)==2)#two dates
  {
    if (vec[cmp][1]<vec[cmp][2])
    {
      return("ok") 
    } else return("error")
  }
  if (sum(cmp)==3)#three dates
  {
    if ((vec[cmp][1]<vec[cmp][2])&(vec[cmp][2]<vec[cmp][3]))
    {
      return("ok") 
    } else return("error")
  }
  else return("NA")
}

#####################
apply(vanedela,1,orve)
#there seems to be no errors in the dates

#the number of data in the first, second and third columns

apply(!is.na(vanedela[,5:7]),2,sum)
#21, 51, 65
#there are more numbers in the late stages

#the distribution of the numbers in each stage
hist(vanedela[,5],breaks=6)
hist(vanedela[,6])
hist(vanedela[,7])
#the last two seem a bit normal

#the time lag between first and second
dtCD1AB<-vanedela[,6]-vanedela[,5]
#number of kids with computable lag
sum(!is.na(dtCD1AB)) #19 too little
#mean lag
hist(dtCD1AB)
#this distance is not normal. Then use a median
median(dtCD1AB,na.rm=T)
#15
#the confidence intervals are computed using bootstraps
require(boot)
medboot<-function(me,i){median(me[i],na.rm=T)}
pre.boot<-boot(dtCD1AB,medboot,R=1000)
median.dtCD1AB<-boot.ci(boot.out=pre.boot,conf=0.95,type = "norm")

#the time lag between second and third
#number of kids with computable lag
sum(!is.na(dtCD1BC)) #39
#median lag
hist(dtCD1BC) #a bit skewed
#compute median
median(dtCD1BC,na.rm=T) #40 days
dtCD1BC<-vanedela[,7]-vanedela[,6]
pre.boot<-boot(dtCD1BC,medboot,R=1000)
median.dtCD1AC<-boot.ci(boot.out=pre.boot,conf=0.95,type="norm")

#apparently the lag is larger in the second part 

######### medians for each phase
#Phase A
pre.boot<-boot(vanedela[,5],medboot,R=1000)
medi.CD1A<-boot.ci(boot.out=pre.boot,conf=0.95,type = "norm")
#phase B
pre.boot<-boot(vanedela[,6],medboot,R=1000)
medi.CD1B<-boot.ci(boot.out=pre.boot,conf=0.95,type = "norm")
#Phase C
pre.boot<-boot(vanedela[,7],medboot,R=1000)
medi.CD1C<-boot.ci(boot.out=pre.boot,conf=0.95,type = "norm")

plot(vanedela[,5],vanedela[,6],xlim=c(0,70),ylim=c(0,140),xlab="lag A-B",ylab="lag B-C")
points(vanedela[,6],vanedela[,7],pch=2)
abline(a=0,b=1)
#all lags are above the identity line, that is they take more time
#from B-C than from A-B. The transition from A to B is at most
#30 days, but the transition B to C is about 60.

#The variability in the second lag is much larger.

#make a plot with days on the x axes and phases on the y axes
#lines joins data for kids with 2 or 3 available data
plot(vanedela[,5],rep(1,126),xlim=c(0,160),ylim=c(0,4),xlab="dias",ylab="fases")
points(vanedela[,6],rep(2,126),pch=2)
points(vanedela[,7],rep(3,126),pch=3)
for (r in 1:126)
{
  lines(as.numeric(vanedela[r,5:7]),1:3,lty=2)  
}

### now plot the confidence intervals

plot(1,type="n",xlim=c(0,160),ylim=c(0,4),xlab="dias",ylab="fases")
points(c(medi.CD1A$t0,medi.CD1B$t0,medi.CD1C$t0),c(1,2,3))
lines(medi.CD1A$normal[2:3],c(1,1))
lines(medi.CD1B$normal[2:3],c(2,2))
lines(medi.CD1C$normal[2:3],c(3,3))
#one idea is to compute confidence intervals for all phases
#then all of the first CI has to be completed, at least
#half  of the second CI and a portion of the third CI
#the idea is that they are dependent on each other.

boxplot(vanedela[,5:7])
#make a function to obtain confidence intervals
