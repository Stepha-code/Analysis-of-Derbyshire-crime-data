crime<-read.csv("AssessmentCrimeData.csv")
#Create new variable name called Districts
crime$District = sub("(\\D+\\s).*","\\1",crime$Name)
#view,discribd and summarize data
View(crime)
str(crime)
describe(crime)
summary(crime)

r<-sumtable(crime, out = "return" )
View(r)
#create a table of summary statistics
library (vtable)
tsumm<- sumtable(crime[6:19], out = "return")
View(tsumm)
#rename "variable" column to "crimes"
table_sum<-rename(tsumm, c("Variable" = "Crimes"))
View(table_sum)

#table for population of different districts
#import libraries
library(dplyr)
library(tidyverse)
crime %>%
  drop_na(District)%>%
  group_by(District)%>%
  summarise(Min=min(Population),
            average=mean(Population),
            Max=max(Population),
            Difference=
              max(Population)-min(Population))%>%
  arrange(average)%>%
  View()
#VISUALIZE THE DATA

###BOXPLOT for each crimes
par(mfrow=c(2,4))
boxplot(log10(crime$Anti.Social.Behaviour/crime$Land.Area.in.Hectares),col='blue',pch=19,main="Anti.Social.Behaviour")
boxplot(log10(crime$Burglary/crime$Land.Area.in.Hectares),col='red',pch=19,main="Burglary")
boxplot(log10(crime$Robbery/crime$Land.Area.in.Hectares),col='brown',pch=19,main="Robbery")
boxplot(log10(crime$Vehicle.Crimes/crime$Land.Area.in.Hectares),col='pink',pch=19,main="Vehicle.crimes")
boxplot(log10(crime$Violent.Crimes/crime$Land.Area.in.Hectares),col='light blue',pch=19,main="Violent.Crimes")
boxplot(log10(crime$Shoplifting/crime$Land.Area.in.Hectares),col='yellow',pch=19,main="ShopLifting")
boxplot(log10(crime$Criminal.Damage...Arson/crime$Land.Area.in.Hectares),col='brown',pch=19,main="Criminal.Damage...Arson")
boxplot(log10(crime$Other.Theft/crime$Land.Area.in.Hectares),col='white',pch=19,main="other.Theft")
boxplot(log10(crime$Drugs/crime$Land.Area.in.Hectares),col='purple',pch=19,main="Drugs")
boxplot(log10(crime$Other.Crimes/crime$Land.Area.in.Hectares),col='yellow',pch=19,main="Other.crimes")
boxplot(log10(crime$Bike.Theft/crime$Land.Area.in.Hectares),col='green',pch=19,main="Bike.Theft")
boxplot(log10(crime$Possession.of.Weapons/crime$Land.Area.in.Hectares),col='dark orange',pch=19,main="Possession.of.Weapons")
boxplot(log10(crime$Public.Order/crime$Land.Area.in.Hectares),col='pink',pch=19,main="Public.Order")
boxplot(log10(crime$Theft.From.the.Person/crime$Land.Area.in.Hectares),col='dark green',pch=19,main="Theft.From.the.Person")


#HISTOGRAM for each crimes 
par(mfrow=c(2,4))
hist(log10(crime$"Anti.Social.Behaviour"/crime$"Land.Area.in.Hectares"),main = "Anti.Social.Behaviour",xlab = "log10(Anti.Social.Behaviour Density)",col='blue',cex.lab=0.85)
hist(log10(crime$"Burglary"/crime$"Land.Area.in.Hectares"),main = "Burglary",xlab = " Burglary Density
",col='blue')
hist(log10(crime$"Robbery"/crime$"Land.Area.in.Hectares"),main = "Robbery",xlab = "log10(Robbery Density)
",col='blue')
hist(log10(crime$"Vehicle.Crimes"/crime$"Land.Area.in.Hectares"),main = "Vehicle crimes",xlab = "log10(Vehicle crimes Density)
",col='blue')
hist(log10(crime$"Violent.Crimes"/crime$"Land.Area.in.Hectares"),main = "Violent crimes",xlab =  "log10(Violent crimes Density)
",col='blue')
hist(log10(crime$"Shoplifting"/crime$"Land.Area.in.Hectares"),main = "Shoplifting",xlab = "log10(ShopLifting Density)
",col='blue')
hist(log10(crime$"Criminal.Damage...Arson"/crime$"Land.Area.in.Hectares"),main = "Criminal.Damage...Arson",xlab = "log10(Criminal.Damage...Arson Density)
",col='blue')
hist(log10(crime$"Other.Theft"/crime$"Land.Area.in.Hectares"),main = "Other.Theft",xlab = "log10(Other.Theft Density)
",col='blue')
hist(log10(crime$"Drugs"/crime$"Land.Area.in.Hectares"),main ="Drugs",xlab="log10(Drugs Density)
",col='blue')
hist(log10(crime$"Other.Crimes"/crime$"Land.Area.in.Hectares"),main = "Criminal.Damage...Arson",xlab = "log10(Other.Crimes Density)
",col='blue')
hist(log10(crime$"Bike.Theft"/crime$"Land.Area.in.Hectares"),main = "Bike.Theft",xlab = "log10(Bike.Theft Density) 
",col='blue')
hist(log10(crime$"Possession.of.Weapons"/crime$"Land.Area.in.Hectares"),main = "Criminal.Damage...Arson",xlab = "log10(Possession.of.Weapons Density)
",col='blue')
hist(log10(crime$"Public.Order"/crime$"Land.Area.in.Hectares"),main = "Criminal.Damage...Arson",xlab = "log10(Public.Order Density)
",col='blue')
hist(log10(crime$'Theft.From.the.Person'/crime$"Land.Area.in.Hectares"),main = "Criminal.Damage...Arson",xlab = "log10 (Theft.From.the.Person Density)
",col='blue')



##BARPLOT for population of districts
library(plotly)
bar_fig<-plot_ly(data = crime,x=~Population,y=~District,type="bar")
bar_fig


#Simple linear regression
model1<-lm(log10(crime$'Anti.Social.Behaviour'/crime$'Land.Area.in.Hectares')~log10(crime$'Population'/crime$'Land.Area.in.Hectares'))
summary(model1)
model2<-lm(log10(crime$'Burglary'/crime$'Land.Area.in.Hectares')~log10(crime$'Population'/crime$'Land.Area.in.Hectares'))
summary(model2)
model3<-lm(log10(crime$'Robbery'/crime$'Land.Area.in.Hectares')~log10(crime$'Population'/crime$'Land.Area.in.Hectares'))
summary(model3)
model4<-lm(log10(crime$'Vehicle.Crimes'/crime$'Land.Area.in.Hectares')~log10(crime$'Population'/crime$'Land.Area.in.Hectares'))
summary(model4)
model5<-lm(log10(crime$'Violent.Crimes'/crime$'Land.Area.in.Hectares')~log10(crime$'Population'/crime$'Land.Area.in.Hectares'))
summary(model5)
model6<-lm(log10(crime$'Shoplifting'/crime$'Land.Area.in.Hectares')~log10(crime$'Population'/crime$'Land.Area.in.Hectares'))
summary(model6)
model7<-lm(log10(crime$'Criminal.Damage...Arson'/crime$'Land.Area.in.Hectares')~log10(crime$'Population'/crime$'Land.Area.in.Hectares'))
summary(model7)
model8<-lm(log10(crime$'Other.Theft'/crime$'Land.Area.in.Hectares')~log10(crime$'Population'/crime$'Land.Area.in.Hectares'))
summary(model8)
model9<-lm(log10(crime$'Drugs'/crime$'Land.Area.in.Hectares')~log10(crime$'Population'/crime$'Land.Area.in.Hectares'))
summary(model9)
model10<-lm(log10(crime$'Other.Crimes'/crime$'Land.Area.in.Hectares')~log10(crime$'Population'/crime$'Land.Area.in.Hectares'))
summary(model10)
model11<-lm(log10(crime$'Bike.Theft'/crime$'Land.Area.in.Hectares')~log10(crime$'Population'/crime$'Land.Area.in.Hectares'))
summary(model11)
model12<-lm(log10(crime$'Possession.of.Weapons'/crime$'Land.Area.in.Hectares')~log10(crime$'Population'/crime$'Land.Area.in.Hectares'))
summary(model12)
model13<-lm(log10(crime$'Public.Order'/crime$'Land.Area.in.Hectares')~log10(crime$'Population'/crime$'Land.Area.in.Hectares'))
summary(model13)
model14<-lm(log10(crime$'Theft.From.the.Person'/crime$'Land.Area.in.Hectares')~log10(crime$'Population'/crime$'Land.Area.in.Hectares'))
summary(model14)


#visualization for each model 
#Linearity Plot
par(mfrow=c(2,4))
plot(log10(crime$Population/crime$Land.Area.in.Hectares),log10(crime$Anti.Social.Behaviour/crime$Land.Area.in.Hectares),xlab ="log10(population Density)",ylab="log10(ASB Density)",main ="Anti.Social.Behaviour",col='blue',pch=19,cex.lab=1, cex =0.2)
abline(model1,col="red",lwd=3)
plot(log10(crime$Population/crime$Land.Area.in.Hectares),log10(crime$Burglary/crime$Land.Area.in.Hectares),xlab ="log10(population Density)",ylab="log10(Burglary Density)",main ="Burglary",col='dark red',pch=19,cex.lab=1, cex =0.2)
abline(model2,col="red",lwd=3)
plot(log10(crime$Population/crime$Land.Area.in.Hectares),log10(crime$Robbery/crime$Land.Area.in.Hectares),xlab ="log10(population Density)",ylab="log10(Robbery Density)",main ="Robbery",col='brown',pch=19,cex.lab=1, cex =0.2)
abline(model3,col="red",lwd=3)
plot(log10(crime$Population/crime$Land.Area.in.Hectares),log10(crime$Vehicle.Crimes/crime$Land.Area.in.Hectares),xlab ="log10(population Density)",ylab="log10(vehicle crime Density)",main ="vehicle crime",col='pink',pch=19,cex.lab=1, cex =0.2)
abline(model4,col="red",lwd=3)
plot(log10(crime$Population/crime$Land.Area.in.Hectares),log10(crime$Violent.Crimes/crime$Land.Area.in.Hectares),xlab ="log10(population Density)",ylab="log10(violent crime Density)",main ="violent crime",col='light blue',pch=19,cex.lab=1, cex =0.2)
abline(model5,col="red",lwd=3)
plot(log10(crime$Population/crime$Land.Area.in.Hectares),log10(crime$Shoplifting/crime$Land.Area.in.Hectares),xlab ="log10(population Density)",ylab="log10(shoplifting Desity)",main ="shoplifting",col='black',pch=19,cex.lab=1, cex =0.2)
abline(model6,col="red",lwd=3)
plot(log10(crime$Population/crime$Land.Area.in.Hectares),log10(crime$Criminal.Damage...Arson/crime$Land.Area.in.Hectares),xlab ="log10(population Density)",ylab="log10(criminal damage Density)",main ="criminal damage",col='dark green',pch=19,cex.lab=1, cex =0.2)
abline(model7,col="red",lwd=3)
plot(log10(crime$Population/crime$Land.Area.in.Hectares),log10(crime$Other.Theft/crime$Land.Area.in.Hectares),xlab ="log10(population Density)",ylab="log10(other theft)",main ="other theft",col='black',pch=19,cex.lab=1, cex =0.2)
abline(model8,col="red",lwd=3)
plot(log10(crime$Population/crime$Land.Area.in.Hectares),log10(crime$Drugs/crime$Land.Area.in.Hectares),xlab ="log10(population Density)",ylab="log10(Drugs Density)",main ="Drugs",col='purple',pch=19,cex.lab=1, cex =0.2)
abline(model9,col="red",lwd=3)
plot(log10(crime$Population/crime$Land.Area.in.Hectares),log10(crime$Other.Crimes/crime$Land.Area.in.Hectares),xlab ="log10(population Density)",ylab="log10(other crimes Density)",main ="other crimes",col='dark red',pch=19,cex.lab=1, cex =0.2)
abline(model10,col="black",lwd=3)
plot(log10(crime$Population/crime$Land.Area.in.Hectares),log10(crime$Bike.Theft/crime$Land.Area.in.Hectares),xlab ="log10(population Density)",ylab="log10(Bike theft density)",main ="Bike theft",col='green',pch=19,cex.lab=1, cex =0.2)
abline(model11,col="red",lwd=3)
plot(log10(crime$Population/crime$Land.Area.in.Hectares),log10(crime$Possession.of.Weapons/crime$Land.Area.in.Hectares),xlab ="log10(population Density)",ylab="log10(Posession of weapons)",main ="Possession.of.Weapons",col='dark blue',pch=19,cex.lab=1, cex =0.2)
abline(model12,col="red",lwd=3)
plot(log10(crime$Population/crime$Land.Area.in.Hectares),log10(crime$Public.Order/crime$Land.Area.in.Hectares),xlab ="log10(population Density)",ylab="log10(Public order Density)",main ="Public orde",col='orange',pch=19,cex.lab=1, cex =0.2)
abline(model13,col="red",lwd=3)
plot(log10(crime$Population/crime$Land.Area.in.Hectares),log10(crime$Theft.From.the.Person/crime$Land.Area.in.Hectares),xlab ="log10(population Density)",ylab="log10(theft from a person Density)",main ="theft from a person",col='purple',pch=19,cex.lab=1, cex =0.2)
abline(model14,col="red",lwd=3)


#Normality plot 
par(mfrow=c(2,4))
hist(model1$residuals,main=' Antisocial Behaviour',xlab='residual',col='red', freq=F)
lines(density(model1$residuals),lwd=1,col="blue")
hist(model2$residuals,main='Burglary',xlab='residual',col='red', freq=F)
lines(density(model2$residuals),lwd=1,col="blue")
hist(model3$residuals,main= 'Robbery',xlab='residual',col='red', freq=F)
lines(density(model3$residuals),lwd=1,col="blue")
hist(model4$residuals,main='Vehicle crime',xlab='residual',col='red', freq=F)
lines(density(model4$residuals),lwd=1,col="blue")
hist(model5$residuals,main='Violent crime',xlab='residual',col='red', freq=F)
lines(density(model5$residuals),lwd=1,col="blue")
hist(model6$residuals,main='Shoplifting',xlab='residual',col='red', freq=F)
lines(density(model6$residuals),lwd=1,col="blue")
hist(model7$residuals,main='Criminal Damage',xlab='residual',col='red')
lines(density(model7$residuals),lwd=1,col="blue")
hist(model8$residuals,main=' Other theft',xlab='residual',col='red', freq=F)
lines(density(model8$residuals),lwd=1,col="blue")
hist(model9$residuals,main='Drugs',xlab='residual',col='red', freq=F)
lines(density(model9$residuals),lwd=1,col="blue")
hist(model10$residuals,main=' Other crimes',xlab='residual',col='red', freq=F)
lines(density(model10$residuals),lwd=1,col="blue")
hist(model11$residuals,main='Bike theft',xlab='residual',col='red', freq=F)
lines(density(model11$residuals),lwd=1,col="blue")
hist(model12$residuals,main='Possession of Weapons',xlab='residual',col='red', freq=F)
lines(density(model12$residuals),lwd=1,col="blue")
hist(model13$residuals,main=' Public Order',xlab='residual',col='red', freq=F)
lines(density(model13$residuals),lwd=1,col="blue")
hist(model14$residuals,main='Theft from a person',xlab='residual',col='red', freq=F)
lines(density(model14$residuals),lwd=1,col="blue")

# visualisation for Independence
#create residuals for each plot
residual1<-residuals(model1)
residual2<-residuals(model2)
residual3<-residuals(model3)
residual4<-residuals(model4)
residual5<-residuals(model5)
residual6<-residuals(model6)
residual7<-residuals(model7)
residual8<-residuals(model8)
residual9<-residuals(model9)
residual10<-residuals(model10)
residual11<-residuals(model11)
residual12<-residuals(model12)
residual13<-residuals(model13)
residual14<-residuals(model14)
#INDEPENDENCE 
par(mfrow=c(2,4))
plot(residual1[-length(residual1)],residual1[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]),pch=19,cex=0.2,main="Anti-social behaviour",cex.lab=1.5)
plot(residual2[-length(residual2)],residual2[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]),pch=19,cex=0.2,main="burglary",cex.lab=1.5)
plot(residual3[-length(residual3)],residual3[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]),pch=19,cex=0.2,main="Robbery",cex.lab=1.5)
plot(residual4[-length(residual4)],residual4[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]),pch=19,cex=0.2,main="vehicle crime",cex.lab=1.5)
plot(residual5[-length(residual5)],residual5[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]),pch=19,cex=0.2,main="violent crime",cex.lab=1.5)
plot(residual6[-length(residual6)],residual6[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]),pch=19,cex=0.2,main="shoplifting",cex.lab=1.5)
plot(residual7[-length(residual10)],residual10[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]),pch=19,cex=0.2,main="criminal damage",cex.lab=1.5)
plot(residual8[-length(residual1)],residual9[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]),pch=19,cex=0.2,main="other theft",cex.lab=1.5)
plot(residual9[-length(residual4)],residual4[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]),pch=19,cex=0.2,main="drugs",cex.lab=1.5)
plot(residual10[-length(residual11)],residual11[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]),pch=19,cex=0.2,main="other crime",cex.lab=1.5)
plot(residual11[-length(residual6)],residual6[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]),pch=19,cex=0.2,main="Bike theft",cex.lab=1.5)
plot(residual12[-length(residual12)],residual12[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]),pch=19,cex=0.2,main="possession of weapon",cex.lab=1.5)
plot(residual13[-length(residual14)],residual14[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]),pch=19,cex=0.2,main="public order",cex.lab=1.5)
plot(residual1[-length(residual13)],residual13[-1],xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]),pch=19,cex=0.2,main="theft from a person",cex.lab=1.5)

#Constant variance
par(mfrow=c(4,4))
plot(fitted(model1),residuals(model1),xlab="fitted",ylab="residuals",main="Aniti social behavour",cex.lab=1)
abline(h=0,lwd=2,col=2)
plot(fitted(model2),residuals(model2),xlab="fitted",ylab="residuals",main="Burglary",cex.lab=1)
abline(h=0,lwd=2,col=2)
plot(fitted(model3),residuals(model3),xlab="fitted",ylab="residuals",main="Robbery",cex.lab=1)
abline(h=0,lwd=2,col=2)
plot(fitted(model4),residuals(model4),xlab="fitted",ylab="residuals",main="Vehicle crime",cex.lab=1)
abline(h=0,lwd=2,col=2)
plot(fitted(model5),residuals(model5),xlab="fitted",ylab="residuals",main="Violent crime",cex.lab=1)
abline(h=0,lwd=2,col=2)
plot(fitted(model6),residuals(model6),xlab="fitted",ylab="residuals",main="Shoplifting",cex.lab=1)
abline(h=0,lwd=2,col=2)
plot(fitted(model7),residuals(model7),xlab="fitted",ylab="residuals",main="Criminal Damage",cex.lab=1)
abline(h=0,lwd=2,col=2)
plot(fitted(model8),residuals(model8),xlab="fitted",ylab="residuals",main="Other theft",cex.lab=1)
abline(h=0,lwd=2,col=2)
plot(fitted(model9),residuals(model9),xlab="fitted",ylab="residuals",main="Drugs",cex.lab=1)
abline(h=0,lwd=2,col=2)
plot(fitted(model10),residuals(model10),xlab="fitted",ylab="residuals",main="other crimes",cex.lab=1)
abline(h=0,lwd=2,col=2)
plot(fitted(model11),residuals(model11),xlab="fitted",ylab="residuals",main="Bike theft",cex.lab=1)
abline(h=0,lwd=2,col=2)
plot(fitted(model12),residuals(model12),xlab="fitted",ylab="residuals",main="POW",cex.lab=1)
abline(h=0,lwd=2,col=2)
plot(fitted(model13),residuals(model13),xlab="fitted",ylab="residuals",main="Public Order",cex.lab=1)
abline(h=0,lwd=2,col=2)
plot(fitted(model14),residuals(model14),xlab="fitted",ylab="residuals",main="Theft from a person",cex.lab=1)
abline(h=0,lwd=2,col=2)


##RESIDUALS FOR EACH MODEL 
#use save all the residuals in a data frame
residuals<-data.frame(model1$residuals,model2$residuals,model3$residuals,model4$residuals,model5$residuals,model6$residuals,model7$residuals,model8$residuals,model9$residuals,model10$residuals,model11$residuals,model12$residuals,model13$residuals,model14$residuals)
colnames(residuals)<-c("ASB","Burglary","Robbery","vehicle.crimes","violent.crimes","Shoplifting","Criminal.damage","other.theft","Drugs","other.crimes","Bike.theft","POW.","public.order","theft.from.a.person")
View(residuals)

#scatter plot
plot(residuals,cex=0.2)
#HEATMAP
#import and install libraries
library('graphics')
library('gplots')
library('RColorBrewer')
#correlation for residuals
cr<-cor(residuals,use = "pairwise",method = "pearson")
cr
#create color palette for the heatmap

my_palette<-colorRampPalette(c("lightcyan2","lightblue2","lightblue3","lightcyan4","lightblue4","deepskyblue3","deepskyblue4","dodgerblue3","dodgerblue4","royalblue4","royalblue"))(n=100)

heatmap.2(cr,
          trace = "column",
          main = "My Heatmap",
          col = my_palette,cexCol = 0.53,cexRow = 0.53)


#DENDOGRAM
library(networkD3)
df.scaled<-scale(cr)
head(df.scaled)
require(stats)
res.dist<-dist(x=df.scaled,method ="euclidean")
####distance matrix
res.hc<-hclust(d=res.dist,method = "complete")
plot(x=res.hc)



