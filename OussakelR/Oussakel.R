# 1 - Chargement librairies et donnees

data=read.table(file="Data.txt",header=TRUE)

data$RR=as.factor(data$RR)
data$DD=as.factor(data$DD)

names(data)
summary(data)
dim(data)

# 2 Regression Simple

lm.out1=lm(O3~O3v,data=data)
summary(lm.out1)


x11()
plot(data$O3v,data$O3,xlab="O3 Concentration d'ozone J ",ylab="O3 Concentration ozone J-1",
     main="PREVISIONS",pch='+')
abline(lm.out1,col="red")

#Analyse des performances : 
# On voit que O3v est jugé significatif comme prédicteur car son p_value associée est > 0.05
#Mais d'après le coefficient R-squared, on voit que le modèle n'explique que 46,86% de la variabilité de notre prédictant qu est O3
# Ainsi le modèle n'est pas trop performant.

# 3 Modèle sans interaction :

lm.RR=lm(O3~RR,data=data)
summary(lm.RR)
model.matrix(lm.RR)[1:10,]

rrPluie=subset(data,RR=="Pluie")
summary(rrPluie)

rrSec=subset(data,RR=="Sec")
summary(rrSec)

# -> on remarque que la contrainte d'identification imposée par R sur le facteur RR est alpha(pluie) = 0, et que p_value associée au test de student< 0.05, cad que les modalités pluie et sec ont des comportement différents.

lm.out2=lm(O3~T+N+FF+DD+RR+O3v,data=data)
summary(lm.out2)

# La variable N a une p_value associée au test de student << 0.05 et donc elle est jugée significative dans notre modèle, on voit aussi qu'elle a une valeur négative, et donc en prenant les autres variables fixes, N aide à décroitre la concentration en Ozone.
# Le facteur DD : On voit que R a imposé comme contrainte d'identification Alpha(Est) = 0
# Ainsi, en analysant les p_values associées au test de student, on voit que la modalité DDNord et DDSud ont des comportements différents de la modalité DDEst qui est prise comme réference, tandis que la modalité DDOuest est jugée avoir un comportement similaire à celui de DDEst(la réference)

#Les hypotheses du modèle lineaire gaussion 
# 1- heteroscedasticite :
plot(fitted(lm.out2),residuals(lm.out2),main="Hypothèse d'homoscédasticité",xlab="Valeurs ajustées (Y*)",ylab="Résidus")
# -> heteroscedasticite : les résidus semblent être répartis de manière uniforme et symétrique autour de zéro, sans tendance apparente (ni éventail, ni cône), cela soutient l'hypothèse d'homoscédasticité.

# 2 - normalité
qqnorm(residuals(lm.out2))
hist(residuals(lm.out2))
# residus centres par construction sur apprentissage,
# les queues de distribution s'eloignent de la normalite.
#-> les résidus suivent approximativement une ligne droite diagonale dans le QQ-plot, cela indique que les résidus sont proches d'une distribution normale. Les écarts mineurs par rapport à la ligne droite sont généralement tolérés.

#3- Indépendance:
acf(residuals(lm.out2))

# -> Les barres ne sont situées en dehors des intervalles de confiance que pendant le 1 er lag, ainsi elles sont considérées comme statistiquement non significatives. Cela indique une absence de corrélation significative entre les résidus.
#ainsi pendant le 1er lag on remarque une corrélation positive : La barre au-dessus de zéro indique une corrélation positive, ce qui signifie que les résidus à ce retard ont tendance à être similaires.
# -> pas d'autocorrélation et donc l'hypothèse d'independance est bien respectée

# 4-Linéarité :

plot(fitted(lm.out2),data$O3,xlab="[03] en J ",ylab="[03] en J-1",pch="+",main="Hypothèse de linéarité")
#-> d'après le tracé l'hypothèse de linéarité est bien verifiée

# -> Conclusion : le modèle respecte bien les hypothèse du modèle lineaire gaussien et on remarque aussi qu'il explique 83.58% de la variabilité de la concentration en O3

# Prédicteurs à conserver :
# -> il parait judicieux d'éliminer la variable FF car son p_value associée au test de student >> 0.05 et donc il est jugé non significatif sur notre modèle, ainsi garder les autres prédicteurs car ils ont un mpact significatif sur l'augmentation de la performance de notre modèle

# Selection automatique des predicteurs

library(MASS)

lm.outBIC=stepAIC(lm.out2,k=log(nrow(data)))
summary(lm.outBIC)
formula(lm.outBIC)
#Le BIC est un critère qui prend en compte à la fois l'ajustement aux données et la complexité du modèle. ainsi,la selection automatique descendante utilisant le critere BIC pénalise plus le modèle et retire la variable FF et le facteur RR.
# -> on remarque ainsi que le pourcentage de la variance passe à 83.22%


# 4 Modèle avec interactions :

# l'algorithme permet d'effectuer un premier tri, essayez :
lm.outint=lm(O3~.*.,data)
summary(lm.outint)
lm.outBICint=stepAIC(lm.outint,k=log(nrow(data)))
summary(lm.outBICint)
formula(lm.outBICint)
# -> notre nouveau modèle est de dimension 7
# -> on remarque qu'en prenant en compte les interactions d'ordre 2, le modèle explique 84.65% la variablité de la concentration en O3 le jour J

# 5 Visualisation des prévisions :

plot(data$O3,type ="l",main="Concentration d'ozone le jour J",xlab="Indice",ylab="[O3]")
points(fitted(lm.out1),col="red",pch="+",cex=1.2)
points(fitted(lm.outBICint),col="blue",pch="+",cex=1.2)
legend(0,330,lty=1,col=c("black"),legend=c("o3"),bty="n")
legend(0,320,pch="+",col=c("blue","red"),legend=c("lm.outBICint","lm.out1"),bty="n") 
# -> Analyse : On voit que le modele lm.outBICint en bleu ameliore la regression simple en rouge en permettant une plus grande variabilite aux previsions statistiques (modele plus flexible)


# 6 Evaluationn des modèles :


# fonction calculant le biais et le RMSE
score=function(obs,prev) {
rmse=sqrt(mean((prev-obs)**2))
biais=mean(prev-obs)
print("Biais  RMSE") 
return(round(c(biais,rmse),3))
}

# Creation des fichiers d'apprentissage et de test
nappr=ceiling(0.8*nrow(data))
ii=sample(1:nrow(data),nappr)
jj=setdiff(1:nrow(data),ii)
datatest=data[jj,]
datapp=data[ii,]

# Entrainement des modeles sur le fichier datapp
lm.out1=lm(O3~O3v,datapp)
lm.outBIC=lm(formula(lm.outBIC),datapp)
lm.outBICint=lm(formula(lm.outBICint),datapp)

score(datapp$O3,datapp$O3v) # scores bruts 
#[1] "Biais  RMSE"
#[1] -0.122 23.826

score(datapp$O3,fitted(lm.out1))
#[1] "Biais  RMSE"
#[1]  0.000 21.665

score(datapp$O3,fitted(lm.outBIC))
#[1] "Biais  RMSE"
#[1]  0.000 11.993
score(datapp$O3,fitted(lm.outBICint))
#[1] "Biais  RMSE"
#[1]  0.000 11.655

# Plus le modele est complexe, plus il est performant sur les donnees d'apprentissage,
# le biais est nul par construction. Et le modèle lm.outBICint a le plus faible valeur de RMSE.


score(datatest$O3,datatest$O3v)
#[1] "Biais  RMSE"
#[1]  1.864 14.680
score(datatest$O3,predict(lm.out1,datatest))
#[1] "Biais  RMSE"
#[1]  3.449 14.663
score(datatest$O3,predict(lm.outBIC,datatest))
#[1] "Biais  RMSE"
#[1] -2.378  9.584
score(datatest$O3,predict(lm.outBICint,datatest))
#[1] "Biais  RMSE"
#[1] -3.066  8.167

# Les scores se degradent sur les données de test, ainsi, des biais apparaissent dans tous les modèles

#Comparaison avec la stratégie triviale : 
cor.test(data$O3,data$O3v)
#-> on voit que la variable O3v est bien corrélée à O3 mais cela ne donnera pas de bonnes prévisions si on compte associer directement la mesure d'ozone de la veille au pic du jour, vu que les deux variables ne sont pas complétement corrélées, et on a vu précedement que beaucoup de variables autre que O3v ont une influence significative sur les prédictions à faire. 
#Adaptation du fichier CV.R :

source("CV.R")
#-> Variabilité : La hauteur des boîtes et la longueur des moustaches (lignes qui s'étendent au-delà des boîtes) donnent une idée de la variabilité des données test.Ainsi, les tests ont une plus grande dispersion des valeurs.
#-> On remarque une lègere difference entre les medianes des boites test et boites d'apprentissage mais qui reste tolérée.

#-> #final : les modèles lm.out1, lm.outBICet lm.outBICint sont robustes, le lm.outBICintetant significativement le meilleur sur test c'est le modele à proposer au final car son RMSE est le plus faible en comparaisant avec le reste..