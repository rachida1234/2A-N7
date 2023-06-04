# TP2 HPC-BigData : script d'evaluation des modeles

library(MASS)

data=read.table(file="Data.txt",header=TRUE)

RMSE=function(obs,pr){
return(sqrt(mean((pr-obs)^2)))}

# Choix automatique de predicteurs a partir du modele complet sans puis avec interactions

lm.outBIC=stepAIC(lm(O3~.,data),k=log(nrow(data)),trace=0)
lm.outBICint=stepAIC(lm(O3~.*.,data),k=log(nrow(data)),trace=0)
lm.out1=lm(O3~O3v,data=data)

k=100

tab=matrix(nrow=k,ncol=6)

for (i in 1:k) {

nappr=ceiling(0.8*nrow(data))
ii=sample(1:nrow(data),nappr)
jj=setdiff(1:nrow(data),ii)
datatest=data[jj,]
datapp=data[ii,]

# Estimation des modeles
lm.out1=lm(O3~O3v,datapp)
lm.outBIC=lm(formula(lm.outBIC),datapp)
lm.outBICint=lm(formula(lm.outBICint),datapp)

# Scores sur apprentissage
tab[i,1]=RMSE(datapp$O3,predict(lm.out1))
tab[i,2]=RMSE(datapp$O3,predict(lm.outBIC))
tab[i,3]=RMSE(datapp$O3,predict(lm.outBICint))

# Scores sur test
tab[i,4]=RMSE(datatest$O3,predict(lm.out1,datatest))
tab[i,5]=RMSE(datatest$O3,predict(lm.outBIC,datatest))
tab[i,6]=RMSE(datatest$O3,predict(lm.outBICint,datatest))

}

x11()
boxplot(tab,col=c(rep("blue",3),rep("red",3)),xlab="bleu=apprentissage - rouge=test",
names=c("lm.out1","lm.outBIC","lm.outBICint","lm.out1","lm.outBIC","lm.outBICint"),main="Modele lineaire gaussien - Score RMSE")
