x<- read.csv("/home/hirschc1/lixx5447/projects/nil/github/ammi/ammi.csv",sep=",", na.string=".")
x1<- x[,4:34]#one trait as example
# Full replications
library(agricolae)
# Example 1
model1<- with(x,AMMI(envi1, Genotype, rep, SL, console=FALSE))
model1$ANOVA
# see help(plot.AMMI)
# biplot
#filename1 <- CL
ppi <- 300
png('filename1',width=7*ppi, height=6.5*ppi, res=ppi)
plot(model1)
dev.off()

# triplot PC 1,2,3 
plot(model1, type=2, number=TRUE)
# biplot PC1 vs Yield 
plot(model1, first=0,second=1, number=TRUE)
# Example 2
data(CIC)
data1<-CIC$comas[,c(1,6,7,17,18)]
data2<-CIC$oxapampa[,c(1,6,7,19,20)]
cic <- rbind(data1,data2)
model<-with(cic,AMMI(Locality, Genotype, Rep, relative))
model$ANOVA
plot(model1,0,1,angle=20,ecol="brown")
# Example 3
# Only means. Mean square error is well-known.
data(sinRepAmmi)
REP <- 3
MSerror <- 93.24224
#startgraph
model<-with(sinRepAmmi,AMMI(ENV, GEN, REP, YLD, MSerror,PC=TRUE))
# print anova
print(model1$ANOVA,na.print = "")
# Biplot with the one restored observed.
plot(model1,0,1,type=1)
# with principal components model$PC is class "princomp" 
pc<- model1$PC
pc$loadings
summary(pc)
biplot(pc)
# Principal components by means of the covariance similar AMMI
# It is to compare results with AMMI
cova<-cov(model1$genXenv)
values<-eigen(cova)
total<-sum(values$values)
round(values$values*100/total,2)
# AMMI: 64.81 18.58 13.50  3.11  0.00
