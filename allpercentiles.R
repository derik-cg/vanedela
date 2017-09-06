#this script is for calculating all of the quartiles percentiles
#together with the 95% intervals

#change dir
setwd("~/Investigacion/Vanedela")
rm(list=ls())
load("~/Investigacion/Vanedela/.RData")

#load database
vanedela<-read.csv("base edad cd y rd18417.csv")

#create a vector of all the triplets for each part
#all of the CD reacs are three stages long.  
#they start in 5 they stop at 184 there are 180 
#reacs. divided into groups of 3 there are 60
#nom<-names(vanedela[5:184])
#nom<-matrix(nom,ncol=3,byrow=T)
#Now compute the percentiles together with ic 95

# i need a dataframe to store all the results, build in advance
all90<-data.frame(matrix(rep(NA,180*3),nrow=3))
names(all90)<-names(vanedela[5:184])
rownames(all90)<-c("percentil 0.9","inferior IC95%","superior IC95%")
#call the package
require(boot)
#define function for bootstrap to work
#pe is the dataframe name, i is the column
p90boot<-function(pe,i){quantile(pe[i],0.9,na.rm=T)}
#apply to data
for (i in 5:184)
{
  if (sum(vanedela[,i],na.rm=T)!=0)
  {
    pre.boot<-boot(vanedela[,i],p90boot,R=1000)
    tmpci<-boot.ci(boot.out=pre.boot,conf=0.95,type = "norm")
    all90[1,(i-4)]<-tmpci$t0
    all90[2:3,(i-4)]<-tmpci$normal[2:3]
  }
}

#now that I have all the intervals, plot them. Use
#lines joining the extremes of the intervals
#the x values are the data on the intervals
#the y values are the names of the data frame

#plot all medians of the 0.9 percentile
plot(as.numeric(all90[1,]),1:180,type="l",main="todo",xlab="dias",ylab="reactivos")

###########################
# this is for CD1.1-10
############################

#overlap all reacs
plot(as.numeric(all90[1,1:30]),rep(1:3,10),xlim=c(10,150),
     xlab="dias",ylab="fases",main="reactivos CD1.1-10",yaxt="n")
axis(2,at=c(1,2,3),labels=c("A","B","C"))
for (i in seq(from=1,to=30,by=3))
{
  lines(as.numeric(all90[2:3,i]),c(1,1))
  lines(as.numeric(all90[2:3,(i+1)]),c(2,2))
  lines(as.numeric(all90[2:3,(i+2)]),c(3,3))
}

#this plots all A phases
plot(as.numeric(all90[1,seq(from=1,to=30,by=3)]),1:10)
#this plots all B phases
plot(as.numeric(all90[1,seq(from=2,to=30,by=3)]),1:10)
#this plots all C phases
plot(as.numeric(all90[1,seq(from=3,to=30,by=3)]),1:10)

#plot all ABC in a separate line for each reac
#the ases are 0:120 and 0:30 
par(las=2)
plot(1,xlim=c(20,140),ylim=c(0,30),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=seq(from=1,to=30,by=3),labels=names(all90[1,seq(from=1,to=30,by=3)]))
for(i in seq(from=1,to=30,by=3))
  {
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90[1,i]),i,pch=0)
  points(as.numeric(all90[1,(i+1)]),i+1,pch=1)
  points(as.numeric(all90[1,(i+2)]),i+2,pch=2)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90[1,i:(i+2)]),i:(i+2),lty=2)
  #this plots horizontal lines for ic95 for phase A
  lines(as.numeric(all90[,i]),rep(i,3))
  #this plots horizontal lines for ic95 for phase B
  lines(as.numeric(all90[,(i+1)]),rep(i+1,3))
  #this plots horizontal lines for ic95 for phase C
  lines(as.numeric(all90[,(i+2)]),rep(i+2,3))
}
par(las=0)

###########################
### this is for CD4
##########################
#overlap all reacs
plot(as.numeric(all90[1,31:60]),rep(1:3,10),xlim=c(90,250),
     xlab="dias",ylab="fases",main="reactivos CD4.1-10",yaxt="n")
axis(2,at=c(1,2,3),labels=c("A","B","C"))
for (i in seq(from=31,to=60,by=3))
{
  lines(as.numeric(all90[2:3,i]),c(1,1))
  lines(as.numeric(all90[2:3,(i+1)]),c(2,2))
  lines(as.numeric(all90[2:3,(i+2)]),c(3,3))
}

#this plots all A phases
plot(as.numeric(all90[1,seq(from=31,to=60,by=3)]),1:10)
#this plots all B phases
plot(as.numeric(all90[1,seq(from=32,to=60,by=3)]),1:10)
#this plots all C phases
plot(as.numeric(all90[1,seq(from=33,to=60,by=3)]),1:10)

#plot all ABC in a separate line for each reac
#the ases are 0:120 and 0:30 
par(las=2)
plot(1,xlim=c(90,250),ylim=c(0,30),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=seq(from=1,to=30,by=3),
     labels=names(all90[1,seq(from=31,to=60,by=3)]))
for(i in seq(from=31,to=60,by=3))
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90[1,i]),i-30,pch=0)
  points(as.numeric(all90[1,(i+1)]),i-30+1,pch=1)
  points(as.numeric(all90[1,(i+2)]),i-30+2,pch=2)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90[1,i:(i+2)]),(i-30):(i-30+2),lty=2)
  #this plots horizontal lines for ic95 for phase A
  lines(as.numeric(all90[,i]),rep(i-30,3))
  #this plots horizontal lines for ic95 for phase B
  lines(as.numeric(all90[,(i+1)]),rep(i-30+1,3))
  #this plots horizontal lines for ic95 for phase C
  lines(as.numeric(all90[,(i+2)]),rep(i-30+2,3))
}
par(las=0)

######################
## This is for CD8.1-10
######################
#overlap all reacs
plot(as.numeric(all90[1,61:90]),rep(1:3,10),xlim=c(150,400),
     xlab="dias",ylab="fases",main="reactivos CD8.1-10",yaxt="n")
axis(2,at=c(1,2,3),labels=c("A","B","C"))
for (i in seq(from=61,to=90,by=3))
{
  lines(as.numeric(all90[2:3,i]),c(1,1))
  lines(as.numeric(all90[2:3,(i+1)]),c(2,2))
  lines(as.numeric(all90[2:3,(i+2)]),c(3,3))
}

#this plots all A phases
plot(as.numeric(all90[1,seq(from=61,to=90,by=3)]),1:10)
#this plots all B phases
plot(as.numeric(all90[1,seq(from=62,to=90,by=3)]),1:10)
#this plots all C phases
plot(as.numeric(all90[1,seq(from=63,to=90,by=3)]),1:10)

#plot all ABC in a separate line for each reac
#the ases are 0:120 and 0:30 
par(las=2)
plot(1,xlim=c(150,400),ylim=c(0,30),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=seq(from=1,to=30,by=3),
     labels=names(all90[1,seq(from=61,to=90,by=3)]))
for(i in seq(from=61,to=90,by=3))
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90[1,i]),i-60,pch=0)
  points(as.numeric(all90[1,(i+1)]),i-60+1,pch=1)
  points(as.numeric(all90[1,(i+2)]),i-60+2,pch=2)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90[1,i:(i+2)]),(i-60):(i-60+2),lty=2)
  #this plots horizontal lines for ic95 for phase A
  lines(as.numeric(all90[,i]),rep(i-60,3))
  #this plots horizontal lines for ic95 for phase B
  lines(as.numeric(all90[,(i+1)]),rep(i-60+1,3))
  #this plots horizontal lines for ic95 for phase C
  lines(as.numeric(all90[,(i+2)]),rep(i-60+2,3))
}
par(las=0)

######################
## This is for CD12.1-10
######################
#overlap all reacs
plot(as.numeric(all90[1,91:120]),rep(1:3,10),xlim=c(250,600),
     xlab="dias",ylab="fases",main="reactivos CD12.1-10",yaxt="n")
axis(2,at=c(1,2,3),labels=c("A","B","C"))
for (i in seq(from=91,to=120,by=3))
{
  lines(as.numeric(all90[2:3,i]),c(1,1))
  lines(as.numeric(all90[2:3,(i+1)]),c(2,2))
  lines(as.numeric(all90[2:3,(i+2)]),c(3,3))
}

#this plots all A phases
plot(as.numeric(all90[1,seq(from=61,to=90,by=3)]),1:10)
#this plots all B phases
plot(as.numeric(all90[1,seq(from=62,to=90,by=3)]),1:10)
#this plots all C phases
plot(as.numeric(all90[1,seq(from=63,to=90,by=3)]),1:10)

#plot all ABC in a separate line for each reac
#the ases are 0:120 and 0:30 
par(las=2)
plot(1,xlim=c(250,600),ylim=c(0,30),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=seq(from=1,to=30,by=3),
     labels=names(all90[1,seq(from=91,to=120,by=3)]))
for(i in seq(from=91,to=120,by=3))
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90[1,i]),i-90,pch=0)
  points(as.numeric(all90[1,(i+1)]),i-90+1,pch=1)
  points(as.numeric(all90[1,(i+2)]),i-90+2,pch=2)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90[1,i:(i+2)]),(i-90):(i-90+2),lty=2)
  #this plots horizontal lines for ic95 for phase A
  lines(as.numeric(all90[,i]),rep(i-90,3))
  #this plots horizontal lines for ic95 for phase B
  lines(as.numeric(all90[,(i+1)]),rep(i-90+1,3))
  #this plots horizontal lines for ic95 for phase C
  lines(as.numeric(all90[,(i+2)]),rep(i-90+2,3))
}
par(las=0)

######################
## This is for CD18.1-10
######################
#overlap all reacs
plot(as.numeric(all90[1,121:150]),rep(1:3,10),xlim=c(350,800),
     xlab="dias",ylab="fases",main="reactivos CD18.1-10",yaxt="n")
axis(2,at=c(1,2,3),labels=c("A","B","C"))
for (i in seq(from=121,to=150,by=3))
{
  lines(as.numeric(all90[2:3,i]),c(1,1))
  lines(as.numeric(all90[2:3,(i+1)]),c(2,2))
  lines(as.numeric(all90[2:3,(i+2)]),c(3,3))
}

#this plots all A phases
plot(as.numeric(all90[1,seq(from=61,to=90,by=3)]),1:10)
#this plots all B phases
plot(as.numeric(all90[1,seq(from=62,to=90,by=3)]),1:10)
#this plots all C phases
plot(as.numeric(all90[1,seq(from=63,to=90,by=3)]),1:10)

#plot all ABC in a separate line for each reac
#the axes are 0:120 and 0:30 
par(las=2)
plot(1,xlim=c(350,800),ylim=c(0,30),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=seq(from=1,to=30,by=3),
     labels=names(all90[1,seq(from=121,to=150,by=3)]))
for(i in seq(from=121,to=150,by=3))
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90[1,i]),i-120,pch=0)
  points(as.numeric(all90[1,(i+1)]),i-120+1,pch=1)
  points(as.numeric(all90[1,(i+2)]),i-120+2,pch=2)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90[1,i:(i+2)]),(i-120):(i-120+2),lty=2)
  #this plots horizontal lines for ic95 for phase A
  lines(as.numeric(all90[,i]),rep(i-120,3))
  #this plots horizontal lines for ic95 for phase B
  lines(as.numeric(all90[,(i+1)]),rep(i-120+1,3))
  #this plots horizontal lines for ic95 for phase C
  lines(as.numeric(all90[,(i+2)]),rep(i-120+2,3))
}
par(las=0)

######################
## This is for CD24.1-10
######################
#overlap all reacs
plot(as.numeric(all90[1,151:180]),rep(1:3,10),xlim=c(550,900),
     xlab="dias",ylab="fases",main="reactivos CD24.1-10",yaxt="n")
axis(2,at=c(1,2,3),labels=c("A","B","C"))
for (i in seq(from=151,to=180,by=3))
{
  lines(as.numeric(all90[2:3,i]),c(1,1))
  lines(as.numeric(all90[2:3,(i+1)]),c(2,2))
  lines(as.numeric(all90[2:3,(i+2)]),c(3,3))
}

#this plots all A phases
plot(as.numeric(all90[1,seq(from=61,to=90,by=3)]),1:10)
#this plots all B phases
plot(as.numeric(all90[1,seq(from=62,to=90,by=3)]),1:10)
#this plots all C phases
plot(as.numeric(all90[1,seq(from=63,to=90,by=3)]),1:10)

#plot all ABC in a separate line for each reac
#the ases are 0:120 and 0:30 
par(las=2)
plot(1,xlim=c(500,900),ylim=c(0,30),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=seq(from=1,to=30,by=3),
     labels=names(all90[1,seq(from=151,to=180,by=3)]))
for(i in seq(from=151,to=180,by=3))
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90[1,i]),i-150,pch=0)
  points(as.numeric(all90[1,(i+1)]),i-150+1,pch=1)
  points(as.numeric(all90[1,(i+2)]),i-150+2,pch=2)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90[1,i:(i+2)]),(i-150):(i-150+2),lty=2)
  #this plots horizontal lines for ic95 for phase A
  lines(as.numeric(all90[,i]),rep(i-150,3))
  #this plots horizontal lines for ic95 for phase B
  lines(as.numeric(all90[,(i+1)]),rep(i-150+1,3))
  #this plots horizontal lines for ic95 for phase C
  lines(as.numeric(all90[,(i+2)]),rep(i-150+2,3))
}
par(las=0)

######################
### tabla con los percentiles e intervalos
######################
t(all90)
#####################
### this reorders the percentiles by number, three stages per 
### number. Same number for all months
#this produces the proper order

order<-NA
for (i in seq(from=0,to=29,by=3))
{
  for(j in seq(from=i,to=179,by=30))
  order<-c(order,seq(from=j+1,to=j+1+2))
}
order<-order[2:181]
#extract names to be sure
#names(all90)[order]
all90m<-all90[,order]
## plot them by month
plot(as.numeric(all90m[1,1:18]))

###################################

#this plots the first reactive
par(las=2)
plot(1,xlim=c(0,800),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=1:18,
     labels=names(all90m[1,1:18]))
for(i in 1:18)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90m[1,i]),i,pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90m[2:3,i]),c(i,i),lty=1)
}
par(las=0)

#this plots the second reactive
par(las=2)
plot(1,xlim=c(0,750),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=1:18,
     labels=names(all90m[1,19:36]))
for(i in 19:36)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90m[1,i]),i-18,pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90m[2:3,i]),c(i-18,i-18),lty=1)
}
par(las=0)

#this plots the third reactive
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=1:18,
     labels=names(all90m[1,37:54]))
for(i in 37:54)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90m[1,i]),(i-36),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90m[2:3,i]),c(i-36,i-36),lty=1)
}
par(las=0)

#this plots the fourth reactive
par(las=2)
plot(1,xlim=c(0,800),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=1:18,
     labels=names(all90m[1,55:72]))
for(i in 55:72)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90m[1,i]),(i-54),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90m[2:3,i]),c(i-54,i-54),lty=1)
}
par(las=0)

#this plots the fifth reactive
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=1:18,
     labels=names(all90m[1,73:90]))
for(i in 73:90)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90m[1,i]),(i-72),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90m[2:3,i]),c(i-72,i-72),lty=1)
}
par(las=0)

#this plots the sixth reactive
par(las=2)
plot(1,xlim=c(0,800),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=1:18,
     labels=names(all90m[1,91:108]))
for(i in 91:108)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90m[1,i]),(i-90),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90m[2:3,i]),c(i-90,i-90),lty=1)
}
par(las=0)

#this plots the seventh reactive
par(las=2)
plot(1,xlim=c(0,750),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=1:18,
     labels=names(all90m[1,109:126]))
for(i in 109:126)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90m[1,i]),(i-108),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90m[2:3,i]),c(i-108,i-108),lty=1)
}
par(las=0)

#this plots the eight reactive
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=1:18,
     labels=names(all90m[1,127:144]))
for(i in 127:144)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90m[1,i]),(i-126),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90m[2:3,i]),c(i-126,i-126),lty=1)
}
par(las=0)

#this plots the ninth reactive
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=1:18,
     labels=names(all90m[1,145:162]))
for(i in 145:162)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90m[1,i]),(i-144),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90m[2:3,i]),c(i-144,i-144),lty=1)
}
par(las=0)

#this plots the tenth reactive
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=1:18,
     labels=names(all90m[1,163:180]))
for(i in 163:180)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all90m[1,i]),(i-162),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all90m[2:3,i]),c(i-162,i-162),lty=1)
}
par(las=0)

#############################################
##percentiles 10%
#############################################

all10<-data.frame(matrix(rep(NA,180*3),nrow=3))
names(all10)<-names(vanedela[5:184])
rownames(all10)<-c("percentil 0.1","inferior IC95%","superior IC95%")
#call the package
require(boot)
#define function for bootstrap to work
#pe is the dataframe name, i is the column
p10boot<-function(pe,i){quantile(pe[i],0.25,na.rm=T)}
#apply to data
for (i in 5:184)
{
  if (sum(vanedela[,i],na.rm=T)!=0)
  {
    pre.boot<-boot(vanedela[,i],p10boot,R=1000)
    tmpci<-boot.ci(boot.out=pre.boot,conf=0.95,type = "norm")
    all10[1,(i-4)]<-tmpci$t0
    all10[2:3,(i-4)]<-tmpci$normal[2:3]
  }
}

#now that I have all the intervals, plot them. Use
#lines joining the extremes of the intervals
#the x values are the data on the intervals
#the y values are the names of the data frame

#plot all medians of the 0.9 percentile
plot(as.numeric(all10[1,]),1:180,type="l",main="todo",xlab="dias",ylab="reactivos")

###########################
# this is for CD1.1-10
############################

#overlap all reacs
plot(as.numeric(all10[1,1:30]),rep(1:3,10),xlim=c(0,60),
     xlab="dias",ylab="fases",main="reactivos CD1.1-10",yaxt="n")
axis(2,at=c(1,2,3),labels=c("A","B","C"))
for (i in seq(from=1,to=30,by=3))
{
  lines(as.numeric(all10[2:3,i]),c(1,1))
  lines(as.numeric(all10[2:3,(i+1)]),c(2,2))
  lines(as.numeric(all10[2:3,(i+2)]),c(3,3))
}

#this plots all A phases
plot(as.numeric(all10[1,seq(from=1,to=30,by=3)]),1:10)
#this plots all B phases
plot(as.numeric(all10[1,seq(from=2,to=30,by=3)]),1:10)
#this plots all C phases
plot(as.numeric(all10[1,seq(from=3,to=30,by=3)]),1:10)

#plot all ABC in a separate line for each reac
#the ases are 0:120 and 0:30 
par(las=2)
plot(1,xlim=c(0,70),ylim=c(0,30),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=seq(from=1,to=30,by=3),labels=names(all10[1,seq(from=1,to=30,by=3)]))
for(i in seq(from=1,to=30,by=3))
{
  #this plots points for percentiles 10 all phases
  points(as.numeric(all10[1,i]),i,pch=0)
  points(as.numeric(all10[1,(i+1)]),i+1,pch=1)
  points(as.numeric(all10[1,(i+2)]),i+2,pch=2)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10[1,i:(i+2)]),i:(i+2),lty=2)
  #this plots horizontal lines for ic95 for phase A
  lines(as.numeric(all10[,i]),rep(i,3))
  #this plots horizontal lines for ic95 for phase B
  lines(as.numeric(all10[,(i+1)]),rep(i+1,3))
  #this plots horizontal lines for ic95 for phase C
  lines(as.numeric(all10[,(i+2)]),rep(i+2,3))
}
par(las=0)

###########################
### this is for CD4
##########################
#overlap all reacs
plot(as.numeric(all10[1,31:60]),rep(1:3,10),xlim=c(40,160),
     xlab="dias",ylab="fases",main="reactivos CD4.1-10",yaxt="n")
axis(2,at=c(1,2,3),labels=c("A","B","C"))
for (i in seq(from=31,to=60,by=3))
{
  lines(as.numeric(all10[2:3,i]),c(1,1))
  lines(as.numeric(all10[2:3,(i+1)]),c(2,2))
  lines(as.numeric(all10[2:3,(i+2)]),c(3,3))
}

#this plots all A phases
plot(as.numeric(all10[1,seq(from=31,to=60,by=3)]),1:10)
#this plots all B phases
plot(as.numeric(all10[1,seq(from=32,to=60,by=3)]),1:10)
#this plots all C phases
plot(as.numeric(all10[1,seq(from=33,to=60,by=3)]),1:10)

#plot all ABC in a separate line for each reac
#the ases are 0:120 and 0:30 
par(las=2)
plot(1,xlim=c(40,160),ylim=c(0,30),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=seq(from=1,to=30,by=3),
     labels=names(all10[1,seq(from=31,to=60,by=3)]))
for(i in seq(from=31,to=60,by=3))
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10[1,i]),i-30,pch=0)
  points(as.numeric(all10[1,(i+1)]),i-30+1,pch=1)
  points(as.numeric(all10[1,(i+2)]),i-30+2,pch=2)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10[1,i:(i+2)]),(i-30):(i-30+2),lty=2)
  #this plots horizontal lines for ic95 for phase A
  lines(as.numeric(all10[,i]),rep(i-30,3))
  #this plots horizontal lines for ic95 for phase B
  lines(as.numeric(all10[,(i+1)]),rep(i-30+1,3))
  #this plots horizontal lines for ic95 for phase C
  lines(as.numeric(all10[,(i+2)]),rep(i-30+2,3))
}
par(las=0)

######################
## This is for CD8.1-10
######################
#overlap all reacs
plot(as.numeric(all10[1,61:90]),rep(1:3,10),xlim=c(120,280),
     xlab="dias",ylab="fases",main="reactivos CD8.1-10",yaxt="n")
axis(2,at=c(1,2,3),labels=c("A","B","C"))
for (i in seq(from=61,to=90,by=3))
{
  lines(as.numeric(all10[2:3,i]),c(1,1))
  lines(as.numeric(all10[2:3,(i+1)]),c(2,2))
  lines(as.numeric(all10[2:3,(i+2)]),c(3,3))
}

#this plots all A phases
plot(as.numeric(all90[1,seq(from=61,to=90,by=3)]),1:10)
#this plots all B phases
plot(as.numeric(all90[1,seq(from=62,to=90,by=3)]),1:10)
#this plots all C phases
plot(as.numeric(all90[1,seq(from=63,to=90,by=3)]),1:10)

#plot all ABC in a separate line for each reac
#the ases are 0:120 and 0:30 
par(las=2)
plot(1,xlim=c(130,280),ylim=c(0,30),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=seq(from=1,to=30,by=3),
     labels=names(all10[1,seq(from=61,to=90,by=3)]))
for(i in seq(from=61,to=90,by=3))
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10[1,i]),i-60,pch=0)
  points(as.numeric(all10[1,(i+1)]),i-60+1,pch=1)
  points(as.numeric(all10[1,(i+2)]),i-60+2,pch=2)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10[1,i:(i+2)]),(i-60):(i-60+2),lty=2)
  #this plots horizontal lines for ic95 for phase A
  lines(as.numeric(all10[,i]),rep(i-60,3))
  #this plots horizontal lines for ic95 for phase B
  lines(as.numeric(all10[,(i+1)]),rep(i-60+1,3))
  #this plots horizontal lines for ic95 for phase C
  lines(as.numeric(all10[,(i+2)]),rep(i-60+2,3))
}
par(las=0)

######################
## This is for CD12.1-10
######################
#overlap all reacs
plot(as.numeric(all10[1,91:120]),rep(1:3,10),xlim=c(200,450),
     xlab="dias",ylab="fases",main="reactivos CD12.1-10",yaxt="n")
axis(2,at=c(1,2,3),labels=c("A","B","C"))
for (i in seq(from=91,to=120,by=3))
{
  lines(as.numeric(all10[2:3,i]),c(1,1))
  lines(as.numeric(all10[2:3,(i+1)]),c(2,2))
  lines(as.numeric(all10[2:3,(i+2)]),c(3,3))
}

#this plots all A phases
plot(as.numeric(all10[1,seq(from=61,to=90,by=3)]),1:10)
#this plots all B phases
plot(as.numeric(all10[1,seq(from=62,to=90,by=3)]),1:10)
#this plots all C phases
plot(as.numeric(all10[1,seq(from=63,to=90,by=3)]),1:10)

#plot all ABC in a separate line for each reac
#the ases are 0:120 and 0:30 
par(las=2)
plot(1,xlim=c(200,450),ylim=c(0,30),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=seq(from=1,to=30,by=3),
     labels=names(all10[1,seq(from=91,to=120,by=3)]))
for(i in seq(from=91,to=120,by=3))
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10[1,i]),i-90,pch=0)
  points(as.numeric(all10[1,(i+1)]),i-90+1,pch=1)
  points(as.numeric(all10[1,(i+2)]),i-90+2,pch=2)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10[1,i:(i+2)]),(i-90):(i-90+2),lty=2)
  #this plots horizontal lines for ic95 for phase A
  lines(as.numeric(all10[,i]),rep(i-90,3))
  #this plots horizontal lines for ic95 for phase B
  lines(as.numeric(all10[,(i+1)]),rep(i-90+1,3))
  #this plots horizontal lines for ic95 for phase C
  lines(as.numeric(all10[,(i+2)]),rep(i-90+2,3))
}
par(las=0)

######################
## This is for CD18.1-10
######################
#overlap all reacs
plot(as.numeric(all10[1,121:150]),rep(1:3,10),xlim=c(280,620),
     xlab="dias",ylab="fases",main="reactivos CD18.1-10",yaxt="n")
axis(2,at=c(1,2,3),labels=c("A","B","C"))
for (i in seq(from=121,to=150,by=3))
{
  lines(as.numeric(all10[2:3,i]),c(1,1))
  lines(as.numeric(all10[2:3,(i+1)]),c(2,2))
  lines(as.numeric(all10[2:3,(i+2)]),c(3,3))
}

#this plots all A phases
plot(as.numeric(all90[1,seq(from=61,to=90,by=3)]),1:10)
#this plots all B phases
plot(as.numeric(all90[1,seq(from=62,to=90,by=3)]),1:10)
#this plots all C phases
plot(as.numeric(all90[1,seq(from=63,to=90,by=3)]),1:10)

#plot all ABC in a separate line for each reac
#the axes are 0:120 and 0:30 

par(las=2)
plot(1,xlim=c(280,620),ylim=c(0,30),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=seq(from=1,to=30,by=3),
     labels=names(all10[1,seq(from=121,to=150,by=3)]))
for(i in seq(from=121,to=150,by=3))
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10[1,i]),i-120,pch=0)
  points(as.numeric(all10[1,(i+1)]),i-120+1,pch=1)
  points(as.numeric(all10[1,(i+2)]),i-120+2,pch=2)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10[1,i:(i+2)]),(i-120):(i-120+2),lty=2)
  #this plots horizontal lines for ic95 for phase A
  lines(as.numeric(all10[,i]),rep(i-120,3))
  #this plots horizontal lines for ic95 for phase B
  lines(as.numeric(all10[,(i+1)]),rep(i-120+1,3))
  #this plots horizontal lines for ic95 for phase C
  lines(as.numeric(all10[,(i+2)]),rep(i-120+2,3))
}
par(las=0)

######################
## This is for CD24.1-10
######################
#overlap all reacs
plot(as.numeric(all10[1,151:180]),rep(1:3,10),xlim=c(400,800),
     xlab="dias",ylab="fases",main="reactivos CD24.1-10",yaxt="n")
axis(2,at=c(1,2,3),labels=c("A","B","C"))
for (i in seq(from=151,to=180,by=3))
{
  lines(as.numeric(all10[2:3,i]),c(1,1))
  lines(as.numeric(all10[2:3,(i+1)]),c(2,2))
  lines(as.numeric(all10[2:3,(i+2)]),c(3,3))
}

#this plots all A phases
plot(as.numeric(all10[1,seq(from=61,to=90,by=3)]),1:10)
#this plots all B phases
plot(as.numeric(all10[1,seq(from=62,to=90,by=3)]),1:10)
#this plots all C phases
plot(as.numeric(all10[1,seq(from=63,to=90,by=3)]),1:10)

#plot all ABC in a separate line for each reac
#the ases are 0:120 and 0:30 
par(las=2)
plot(1,xlim=c(400,800),ylim=c(0,30),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 e IC 95%")
axis(2,at=seq(from=1,to=30,by=3),
     labels=names(all10[1,seq(from=151,to=180,by=3)]))
for(i in seq(from=151,to=180,by=3))
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10[1,i]),i-150,pch=0)
  points(as.numeric(all10[1,(i+1)]),i-150+1,pch=1)
  points(as.numeric(all10[1,(i+2)]),i-150+2,pch=2)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10[1,i:(i+2)]),(i-150):(i-150+2),lty=2)
  #this plots horizontal lines for ic95 for phase A
  lines(as.numeric(all10[,i]),rep(i-150,3))
  #this plots horizontal lines for ic95 for phase B
  lines(as.numeric(all10[,(i+1)]),rep(i-150+1,3))
  #this plots horizontal lines for ic95 for phase C
  lines(as.numeric(all10[,(i+2)]),rep(i-150+2,3))
}
par(las=0)

######################
### tabla con los percentiles e intervalos
######################
t(all10)
#####################
### this reorders the percentiles by number, three stages per 
### number. Same number for all months
#this produces the proper order

order<-NA
for (i in seq(from=0,to=29,by=3))
{
  for(j in seq(from=i,to=179,by=30))
    order<-c(order,seq(from=j+1,to=j+1+2))
}
order<-order[2:181]
#extract names to be sure
#names(all90)[order]
all10m<-all10[,order]
## plot them by month
plot(as.numeric(all90m[1,1:18]))

###################################

#this plots the first reactive
par(las=2)
plot(1,xlim=c(0,650),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,1:18]))
for(i in 1:18)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),i,pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i,i),lty=1)
}
par(las=0)

#this plots the second reactive
par(las=2)
plot(1,xlim=c(0,600),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,19:36]))
for(i in 19:36)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),i-18,pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-18,i-18),lty=1)
}
par(las=0)

#this plots the third reactive
par(las=2)
plot(1,xlim=c(0,750),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,37:54]))
for(i in 37:54)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-36),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-36,i-36),lty=1)
}
par(las=0)

#this plots the fourth reactive
par(las=2)
plot(1,xlim=c(0,700),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,55:72]))
for(i in 55:72)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-54),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-54,i-54),lty=1)
}
par(las=0)

#this plots the fifth reactive
par(las=2)
plot(1,xlim=c(0,750),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,73:90]))
for(i in 73:90)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-72),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-72,i-72),lty=1)
}
par(las=0)

#this plots the sixth reactive
par(las=2)
plot(1,xlim=c(0,700),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,91:108]))
for(i in 91:108)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-90),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-90,i-90),lty=1)
}
par(las=0)

#this plots the seventh reactive
par(las=2)
plot(1,xlim=c(0,600),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,109:126]))
for(i in 109:126)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-108),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-108,i-108),lty=1)
}
par(las=0)

#this plots the eight reactive
par(las=2)
plot(1,xlim=c(0,650),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,127:144]))
for(i in 127:144)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-126),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-126,i-126),lty=1)
}
par(las=0)

#this plots the ninth reactive
par(las=2)
plot(1,xlim=c(0,750),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,145:162]))
for(i in 145:162)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-144),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-144,i-144),lty=1)
}
par(las=0)

#this plots the tenth reactive
par(las=2)
plot(1,xlim=c(0,800),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,163:180]))
for(i in 163:180)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-162),pch=0)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-162,i-162),lty=1)
}
par(las=0)

#####################################
### put both percentiles together ###
#####################################
#this plots both percentiles of the first reactive
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.9 & 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all90m[1,1:18]))
for(i in 1:18)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),i,pch=0)
  points(as.numeric(all90m[1,i]),i,pch=1)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i,i),lty=1)
  lines(as.numeric(all90m[2:3,i]),c(i,i),lty=1)
}
par(las=0)

#this plots both percentiles of the second reactive
par(las=2)
plot(1,xlim=c(0,700),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 & 0.9 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,19:36]))
for(i in 19:36)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),i-18,pch=0)
  points(as.numeric(all90m[1,i]),i-18,pch=1)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-18,i-18),lty=1)
  lines(as.numeric(all90m[2:3,i]),c(i-18,i-18),lty=1)
}
par(las=0)

#this plots both percentiles of the third reactive
par(las=2)
plot(1,xlim=c(0,850),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 & 0.9 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,37:54]))
for(i in 37:54)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-36),pch=0)
  points(as.numeric(all90m[1,i]),(i-36),pch=1)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-36,i-36),lty=1)
  lines(as.numeric(all90m[2:3,i]),c(i-36,i-36),lty=1)
}
par(las=0)

#this plots both percentiles of the fourth reactive
par(las=2)
plot(1,xlim=c(0,800),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 & 0.9 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,55:72]))
for(i in 55:72)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-54),pch=0)
  points(as.numeric(all90m[1,i]),(i-54),pch=1)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-54,i-54),lty=1)
  lines(as.numeric(all90m[2:3,i]),c(i-54,i-54),lty=1)
}
par(las=0)

#this plots both percentiles the fifth reactive
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,73:90]))
for(i in 73:90)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-72),pch=0)
  points(as.numeric(all90m[1,i]),(i-72),pch=1)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-72,i-72),lty=1)
  lines(as.numeric(all90m[2:3,i]),c(i-72,i-72),lty=1)
}
par(las=0)

#this plots both percentiles the sixth reactive
par(las=2)
plot(1,xlim=c(0,800),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,91:108]))
for(i in 91:108)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-90),pch=0)
  points(as.numeric(all90m[1,i]),(i-90),pch=1)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-90,i-90),lty=1)
  lines(as.numeric(all90m[2:3,i]),c(i-90,i-90),lty=1)
}
par(las=0)

#this plots both percentiles the seventh reactive
par(las=2)
plot(1,xlim=c(0,700),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,109:126]))
for(i in 109:126)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-108),pch=0)
  points(as.numeric(all90m[1,i]),(i-108),pch=1)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-108,i-108),lty=1)
  lines(as.numeric(all90m[2:3,i]),c(i-108,i-108),lty=1)
}
par(las=0)

#this plots both percentiles the eight reactive
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,127:144]))
for(i in 127:144)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-126),pch=0)
  points(as.numeric(all90m[1,i]),(i-126),pch=1)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-126,i-126),lty=1)
  lines(as.numeric(all90m[2:3,i]),c(i-126,i-126),lty=1)
}
par(las=0)

#this plots both percentiles the ninth reactive
par(las=2)
plot(1,xlim=c(0,850),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,145:162]))
for(i in 145:162)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-144),pch=0)
  points(as.numeric(all90m[1,i]),(i-144),pch=1)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-144,i-144),lty=1)
  lines(as.numeric(all90m[2:3,i]),c(i-144,i-144),lty=1)
}
par(las=0)

#this plots the tenth reactive
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,18),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:18,
     labels=names(all10m[1,163:180]))
for(i in 163:180)
{
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10m[1,i]),(i-162),pch=0)
  points(as.numeric(all90m[1,i]),(i-162),pch=1)
  #this plots the lines joining the percentiles
  lines(as.numeric(all10m[2:3,i]),c(i-162,i-162),lty=1)
  lines(as.numeric(all90m[2:3,i]),c(i-162,i-162),lty=1)
}
par(las=0)

#########################
### final order of cd
#load the order
conteo2<-c(0,17,25,18,27,26,20,6,9,12,12)
order1<-read.table("sequence1.csv")
conteo1<-c(0,27,26,20,6,9,12,12)
order2<-read.table("sequence2.csv")

#this produces the plots for the first order
cum1<-cumsum(conteo1)
cum2<-cumsum(conteo2)

for (i in 2:length(cum1))
{
  assign(paste0("seqor",as.character(i-1)),
         order1[seq(from=cum1[i-1]+1,to=cum1[i]),1])
}

for (i in 2:length(cum2))
{
  assign(paste0("seqorl",as.character(i-1)),
         order2[seq(from=cum2[i-1]+1,to=cum2[i]),1])
}

####################
####### this plots the first set
#################################
png(filename="set1.png",width=500,height=800)
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,length(seqor1)),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqor1),
     labels=names(all10[1,seqor1]))
  #this plots points for percentiles 90 all phases
  points(as.numeric(all10[1,seqor1]),1:length(seqor1),pch=0)
  points(as.numeric(all90[1,seqor1]),1:length(seqor1),pch=1)
  #this plots the lines joining the percentiles
for (i in 1:length(seqor1))
{
  lines(as.numeric(all10[2:3,seqor1[i]]),c(i,i),lty=1)
  lines(as.numeric(all90[2:3,seqor1[i]]),c(i,i),lty=1)
}
par(las=0)
dev.off()


#################
######### this plots the second set
#####################################
png(filename="set2.png",width=300,height=500)
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,length(seqor2)),
  type="n",xlab="dias",ylab = "reactivos",
  yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqor2),
labels=names(all10[1,seqor2]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqor2]),1:length(seqor2),pch=0)
  points(as.numeric(all90[1,seqor2]),1:length(seqor2),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqor2))
{
  lines(as.numeric(all10[2:3, seqor2[i]]), c(i, i), lty = 1)
  lines(as.numeric(all90[2:3, seqor2[i]]), c(i, i), lty = 1)
}
par(las=0)
dev.off()
##########################
########### this plots the third set
#######################################
png(filename="set3.png",width=300,height=500)
par(las=2)
plot(1,xlim=c(0,600),ylim=c(0,length(seqor3)),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqor3),
     labels=names(all10[1,seqor3]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqor3]),1:length(seqor3),pch=0)
points(as.numeric(all90[1,seqor3]),1:length(seqor3),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqor3))
{
  lines(as.numeric(all10[2:3,seqor3[i]]),c(i,i),lty=1)
  lines(as.numeric(all90[2:3,seqor3[i]]),c(i,i),lty=1)
}
par(las=0)
dev.off()

#####################
########## this plots the fourth set
####################################
png(filename="set4.png",width=300,height=300)
par(las=2)
plot(1,xlim=c(0,400),ylim=c(0,length(seqor4)),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqor4),
     labels=names(all10[1,seqor4]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqor4]),1:length(seqor4),pch=0)
points(as.numeric(all90[1,seqor4]),1:length(seqor4),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqor4))
{
  lines(as.numeric(all10[2:3,seqor4[i]]),c(i,i),lty=1)
  lines(as.numeric(all90[2:3,seqor4[i]]),c(i,i),lty=1)
}
par(las=0)
dev.off()
######################
############ this plots the fifth set
#######################################
png(filename="set5.png",width=300,height=400)
par(las=2)
plot(1,xlim=c(0,600),ylim=c(0,length(seqor5)),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqor5),
     labels=names(all10[1,seqor5]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqor5]),1:length(seqor5),pch=0)
points(as.numeric(all90[1,seqor5]),1:length(seqor5),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqor5))
{
  lines(as.numeric(all10[2:3,seqor5[i]]),c(i,i),lty=1)
  lines(as.numeric(all90[2:3,seqor5[i]]),c(i,i),lty=1)
}
par(las=0)
dev.off()
#######################
########## this plots the sixth set
#####################################
png(filename="set6.png",width=300,height=500)
par(las=2)
plot(1,xlim=c(200,800),ylim=c(0,length(seqor6)),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqor6),
     labels=names(all10[1,seqor6]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqor6]),1:length(seqor6),pch=0)
points(as.numeric(all90[1,seqor6]),1:length(seqor6),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqor6))
{
  lines(as.numeric(all10[2:3,seqor6[i]]),c(i,i),lty=1)
  lines(as.numeric(all90[2:3,seqor6[i]]),c(i,i),lty=1)
}
par(las=0)
dev.off()
#################
############# this plots the seventh set
#######################################
png(filename="set7.png",width=300,height=500)
par(las=2)
plot(1,xlim=c(200,900),ylim=c(0,length(seqor7)),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqor7),
     labels=names(all10[1,seqor7]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqor7]),1:length(seqor7),pch=0)
points(as.numeric(all90[1,seqor7]),1:length(seqor7),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqor7))
{
  lines(as.numeric(all10[2:3,seqor7[i]]),c(i,i),lty=1)
  lines(as.numeric(all90[2:3,seqor7[i]]),c(i,i),lty=1)
}
par(las=0)
dev.off()
######################################
#####################################
#####el segundo orden de los datos
#################################
###############################

###########################################
####### this plots the first set ####
###################

png(filename="setl1.png",width=300,height=500)
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,length(seqorl1)),type="n",xlab="dias",
     ylab="reactivos",yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqorl1),
     labels=names(all10[1,seqorl1]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqorl1]),1:length(seqorl1),pch=0)
points(as.numeric(all90[1,seqorl1]),1:length(seqorl1),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqorl1))
{
  lines(as.numeric(all10[2:3,seqorl1[i]]),c(i,i),lty=1)
  lines(as.numeric(all90[2:3,seqorl1[i]]),c(i,i),lty=1)
}
par(las=0)
dev.off()
#############################################
######### this plots the second set ###
##################
png(filename="setl2.png",width=300,height=500)
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,length(seqorl2)),
     type="n",xlab="dias",ylab = "reactivos",
     yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqorl2),
     labels=names(all10[1,seqorl2]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqorl2]),1:length(seqorl2),pch=0)
points(as.numeric(all90[1,seqorl2]),1:length(seqorl2),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqorl2))
{
  lines(as.numeric(all10[2:3, seqorl2[i]]), c(i, i), lty = 1)
  lines(as.numeric(all90[2:3, seqorl2[i]]), c(i, i), lty = 1)
}
par(las=0)
dev.off()
##########################################
######### this plots the third set ###
###########################
png(filename="setl3.png",width=300,height=500)
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,length(seqorl3)),
     type="n",xlab="dias",ylab = "reactivos",
     yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqorl3),
     labels=names(all10[1,seqorl3]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqorl3]),1:length(seqorl3),pch=0)
points(as.numeric(all90[1,seqorl3]),1:length(seqorl3),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqorl3))
{
  lines(as.numeric(all10[2:3, seqorl3[i]]), c(i, i), lty = 1)
  lines(as.numeric(all90[2:3, seqorl3[i]]), c(i, i), lty = 1)
}
par(las=0)
dev.off()
##########################################
######### this plots the fourth set ###
###########################
png(filename="setl4.png",width=300,height=500)
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,length(seqorl4)),
     type="n",xlab="dias",ylab = "reactivos",
     yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqorl4),
     labels=names(all10[1,seqorl4]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqorl4]),1:length(seqorl4),pch=0)
points(as.numeric(all90[1,seqorl4]),1:length(seqorl4),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqorl4))
{
  lines(as.numeric(all10[2:3, seqorl4[i]]), c(i, i), lty = 1)
  lines(as.numeric(all90[2:3, seqorl4[i]]), c(i, i), lty = 1)
}
par(las=0)
dev.off()
#############################################
######### this plots the fifth set ###
###########################
png(filename="setl5.png",width=300,height=500)
par(las=2)
plot(1,xlim=c(0,900),ylim=c(0,length(seqorl5)),
     type="n",xlab="dias",ylab = "reactivos",
     yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqorl5),
     labels=names(all10[1,seqorl5]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqorl5]),1:length(seqorl5),pch=0)
points(as.numeric(all90[1,seqorl5]),1:length(seqorl5),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqorl5))
{
  lines(as.numeric(all10[2:3,seqorl5[i]]),c(i,i),lty = 1)
  lines(as.numeric(all90[2:3,seqorl5[i]]),c(i,i),lty = 1)
}
par(las=0)
dev.off()
#############################################
######### this plots the sixth set ###
###########################
png(filename="setl6.png",width=300,height=500)
par(las=2)
plot(1,xlim=c(0,600),ylim=c(0,length(seqorl6)),
     type="n",xlab="dias",ylab = "reactivos",
     yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqorl6),
     labels=names(all10[1,seqorl6]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqorl6]),1:length(seqorl6),pch=0)
points(as.numeric(all90[1,seqorl6]),1:length(seqorl6),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqorl6))
{
  lines(as.numeric(all10[2:3,seqorl6[i]]),c(i,i),lty = 1)
  lines(as.numeric(all90[2:3,seqorl6[i]]),c(i,i),lty = 1)
}
par(las=0)
dev.off()
#############################################
######### this plots the seventh set ###
###########################
png(filename="setl7.png",width=300,height=300)
par(las=2)
plot(1,xlim=c(0,400),ylim=c(0,length(seqorl7)),
     type="n",xlab="dias",ylab = "reactivos",
     yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqorl7),
     labels=names(all10[1,seqorl7]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqorl7]),1:length(seqorl7),pch=0)
points(as.numeric(all90[1,seqorl7]),1:length(seqorl7),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqorl7))
{
  lines(as.numeric(all10[2:3,seqorl7[i]]),c(i,i),lty = 1)
  lines(as.numeric(all90[2:3,seqorl7[i]]),c(i,i),lty = 1)
}
par(las=0)
dev.off()
#############################################
######### this plots the eight set ###
###########################
png(filename="setl8.png",width=300,height=300)
par(las=2)
plot(1,xlim=c(0,600),ylim=c(0,length(seqorl8)),
     type="n",xlab="dias",ylab = "reactivos",
     yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqorl8),
     labels=names(all10[1,seqorl8]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqorl8]),1:length(seqorl8),pch=0)
points(as.numeric(all90[1,seqorl8]),1:length(seqorl8),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqorl8))
{
  lines(as.numeric(all10[2:3,seqorl8[i]]),c(i,i),lty = 1)
  lines(as.numeric(all90[2:3,seqorl8[i]]),c(i,i),lty = 1)
}
par(las=0)
dev.off()
#############################################
######### this plots the ninth set ###
###########################
png(filename="setl9.png",width=300,height=300)
par(las=2)
plot(1,xlim=c(200,800),ylim=c(0,length(seqorl9)),
     type="n",xlab="dias",ylab = "reactivos",
     yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqorl9),
     labels=names(all10[1,seqorl9]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqorl9]),1:length(seqorl9),pch=0)
points(as.numeric(all90[1,seqorl9]),1:length(seqorl9),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqorl9))
{
  lines(as.numeric(all10[2:3,seqorl9[i]]),c(i,i),lty = 1)
  lines(as.numeric(all90[2:3,seqorl9[i]]),c(i,i),lty = 1)
}
par(las=0)
dev.off()
#############################################
######### this plots the tenth set ###
###########################
png(filename="setl10.png",width=300,height=300)
par(las=2)
plot(1,xlim=c(200,900),ylim=c(0,length(seqorl10)),
     type="n",xlab="dias",ylab = "reactivos",
     yaxt="n",main="percentil 0.1 e IC 95%")
axis(2,at=1:length(seqorl10),
     labels=names(all10[1,seqorl10]))
#this plots points for percentiles 90 all phases
points(as.numeric(all10[1,seqorl10]),1:length(seqorl10),pch=0)
points(as.numeric(all90[1,seqorl10]),1:length(seqorl10),pch=1)
#this plots the lines joining the percentiles
for (i in 1:length(seqorl10))
{
  lines(as.numeric(all10[2:3,seqorl10[i]]),c(i,i),lty = 1)
  lines(as.numeric(all90[2:3,seqorl10[i]]),c(i,i),lty = 1)
}
par(las=0)
dev.off()
