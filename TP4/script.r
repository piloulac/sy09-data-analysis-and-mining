#TP4
library(MASS)
library(tree)
#first set your setwd() to the main directory
#path to the working directory on the SY09 computer : setwd("Z:/Documents/SY09/TP4")
setwd("/Users/pierrelouislacorte/SY09/TP4")
#import function from fonction directory
setwd("fonctions")
# A COMPLETER
source("anadisc.R")
source("logistic.R")
source("mvdnorm.R")
source("prob.ad.R")
source("prob.log.R")
source("prob.log2.R")
source("separ1.R")
source("tx_erreurs_discriminante.R")
source("tx_erreurs_logistique.R")
source("tx_erreurs_arbre.R")
source("analyse.R")
#import data from donnees directory
setwd("..")
setwd("donnees")
donn_bcw <- read.csv("bcw.csv")
donn_Pima <- read.csv("Pima.csv")
donn_spam <- read.csv("spam.csv")
donn_Synth1_40 <- read.csv("Synth1-40.csv")
donn_Synth1_1000 <- read.csv("Synth1-1000.csv")
donn_Synth2_1000 <- read.csv("Synth2-1000.csv")
donn_Synth3_1000 <- read.csv("Synth3-1000.csv")
setwd("..")
Xapp <- NA
Xtst <- NA
classe <- NA
# declaration des variables
X40 = donn_Synth1_40[,1:2]
z40 = donn_Synth1_40[,3]

X1_1000 = donn_Synth1_1000[,1:2]
z1_1000 = donn_Synth1_1000[,3]

X2_1000 = donn_Synth2_1000[,1:2]
z2_1000 = donn_Synth2_1000[,3]

X3_1000 = donn_Synth3_1000[,1:2]
z3_1000 = donn_Synth3_1000[,3]

XPima = donn_Pima[,1:7]
zPima = donn_Pima[,8]

Xbcw = donn_bcw[,1:9]
zbcw =donn_bcw[,10]



getAllGraph(X40,z40,"Synth1_40",20,F)
getAllGraph(X1_1000,z1_1000,"Synth1_1000",20,F)
getAllGraph(X2_1000,z2_1000 ,"Synth2_1000",20,F)
getAllGraph(X3_1000,z3_1000 ,"Synth3_1000",20,F)
getAllGraph(XPima,zPima,"Pima",100,F)

png(file = "res/global_pima.png",width=400,height=400)
plot(XPima,col=c("blue","orange")[zPima])
dev.off()
cat("res/global_pima.png\n")

getAllGraph(Xbcw,zbcw,"Breastcancer",100,F,F)
#getAllGraph(Xspam,zspam,"spam",20,F,F)

# Random forest on Xspam
Xspam = donn_spam[,2:59]
Xspam$z = as.factor(Xspam$z)

#permet d'obtenir les même résultats à chaque fois (on "fixe" le random)
set.seed(123)
fit = randomForest(formula = z ~ ., data = Xspam, ntree= 2000)

png(file = "res/spam_fit_random_forest.png",width=400,height=400)
varImpPlot(fit)
dev.off()
cat("res/spam_fit_random_forest.png sauvegardee\n")


fit$importance[order(fit$importance[, 1], decreasing = TRUE), ]

# plot de v52 et v53 les variables les plus explicatives
png(file = "res/spam_v52.png",width=400,height=400)
plot(z~V52, data=Xspam)
dev.off()
cat("res/spam_v52.png sauvegardee\n")

png(file = "res/spam_v53.png",width=400,height=400)
plot(z~V53, data=Xspam)
dev.off()
cat("res/spam_v53.png sauvegardee\n")

# plot d'erreur OOB
png(file = "res/spam_erreur_oob.png",width=400,height=400)
plot(fit$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB")
dev.off()
cat("res/spam_erreur_oob.png sauvegardee\n")

par(mfrow = c(1, 2))
