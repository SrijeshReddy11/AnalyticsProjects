library(faraway)
library(tidyverse)
data(debt)
project8 = debt
str(project8)
view(project8)
#Cleaning the dataset
project8.cleaned= na.omit(project8)
str(project8.cleaned)
summary(project8.cleaned)
view(project8.cleaned)
#converting numerics into factors- ordinal variables
project8.cleaned$incomegp<-as.factor(project8.cleaned$incomegp)#(ordinal)
project8.cleaned$agegp<-as.factor(project8.cleaned$agegp)#(ordinal)
project8.cleaned$manage<-as.factor(project8.cleaned$manage)#(ordinal)
project8.cleaned$ccarduse<-as.factor(project8.cleaned$ccarduse)#(ordinal)
#converting numerics into factors-nominal variables 
project8.cleaned$house<-as.factor(project8.cleaned$house)#(nominal)
project8.cleaned$singpar<-as.factor(project8.cleaned$singpar)#(nominal)
project8.cleaned$bankacc<-as.factor(project8.cleaned$bankacc)#(nominal)
project8.cleaned$bsocacc<-as.factor(project8.cleaned$bsocacc)#(nominal)
project8.cleaned$cigbuy<-as.factor(project8.cleaned$cigbuy)#(nominal)
project8.cleaned$xmasbuy<-as.factor(project8.cleaned$xmasbuy)#(nominal)
str(project8.cleaned)

library(GGally)
ggpairs(data=project8.cleaned, columns= c("incomegp","children", "agegp","locintrn","prodebt","singpar","manage","ccarduse"))
ggpairs(data=project8.cleaned, columns= c("children", "locintrn","singpar","manage","incomegp","prodebt"),lower = list(continuous="smooth"))

#Analysis of the data
lmobj.1 <- lm(prodebt~ locintrn ,data= project8.cleaned)
print(summary(lmobj.1)) 

lmobj.2 <- lm(prodebt~ locintrn+manage ,data= project8.cleaned)
print(summary(lmobj.2)) 

lmobj.3 <- lm(prodebt~ locintrn+manage+children ,data= project8.cleaned)
print(summary(lmobj.3)) 

lmobj.4 <- lm(prodebt~ locintrn+manage+children+singpar ,data= project8.cleaned)
print(summary(lmobj.4)) 

lmobj.5 <- lm(prodebt~ locintrn+manage+children+singpar+incomegp ,data= project8.cleaned)
print(summary(lmobj.5)) 

lmobj.6 <- lm(prodebt~ locintrn+manage+children+singpar+incomegp+agegp ,data= project8.cleaned)
print(summary(lmobj.6))

lmobj.7 <- lm(prodebt~ locintrn+manage+children+singpar+incomegp+ccarduse ,data= project8.cleaned)
print(summary(lmobj.7))

#using anova function to compare all the models
print(anova(lmobj.1,lmobj.2,lmobj.3,lmobj.4))
print(anova(lmobj.1,lmobj.2,lmobj.3,lmobj.4,lmobj.5))


