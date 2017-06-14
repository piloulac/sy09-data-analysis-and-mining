notes <- read.csv("sy02-p2016.csv", na.strings="", header=T)
notes$nom <- factor(notes$nom, levels=notes$nom)
notes$correcteur.median <- factor(notes$correcteur.median, levels=c("Cor1","Cor2","Cor3","Cor4","Cor5","Cor6","Cor7","Cor8"))
notes$correcteur.final <- factor(notes$correcteur.final, levels=c("Cor1","Cor2","Cor3","Cor4","Cor5","Cor6","Cor7","Cor8"))
notes$niveau <- factor(notes$niveau, ordered=T)
notes$resultat <- factor(notes$resultat, levels=c("F","Fx","E","D","C","B","A"), ordered=T)

#summary
summary(notes)
#clean NA value
notes = na.omit(notes)

#general repartition
png("TP1/ex1/plotSpecialite.png")
plot(notes$specialite, 
	main="repartition par specialite", 
	col = c("purple"),
	xlab="specialite",
	ylab="frequence")
dev.off()
cat("plotSpecialite.png sauvegardee\n")

png("TP1/ex1/plotResultat.png")
plot(notes$resultat, 
	main="resultats",
	col = c("red","red","dark green","dark green","dark green","dark green","green"),
	xlab="resultat",
	ylab="frequence")
dev.off()
cat("plotResultat.png sauvegardee\n")

# SELECT resultat FROM notes WHERE notes == 'specialite XX';
XGSM = notes$resultat[notes$specialite == 'GSM']
XGM = notes$resultat[notes$specialite == 'GM']
XISS = notes$resultat[notes$specialite == 'ISS']
XTC = notes$resultat[notes$specialite == 'TC']
XHuTech = notes$resultat[notes$specialite == 'HuTech']
XGI = notes$resultat[notes$specialite == 'GI']
XGP = notes$resultat[notes$specialite == 'GP']
XGSU = notes$resultat[notes$specialite == 'GSU']
XGB = notes$resultat[notes$specialite == 'GB']

png(file = "TP1/ex1/boxplotResultatSpecialite.png",width=600)
boxplot(XGI, XGB, XGSU,XGP,XGSM,XGM,XISS,XTC,
	main = "Comparaison des resultats des branches",  
	col = c("blue", "red","green","orange","purple","dark blue","brown","grey","yellow"), 
	names = c("GI", "GB","GSU","GP","GSM","GM","ISS","TC"), 
	ylab = "resultats", 
	xlab = "specialite",
	notch = FALSE,
	las=1)
dev.off()

# SELECT note median FROM notes WHERE correcteur == 'correcteur XX';
XCor1 = notes$note.median[notes$correcteur.median == 'Cor1']
XCor2 = notes$note.median[notes$correcteur.median == 'Cor2']
XCor3 = notes$note.median[notes$correcteur.median == 'Cor3']
XCor4 = notes$note.median[notes$correcteur.median == 'Cor4']
XCor5 = notes$note.median[notes$correcteur.median == 'Cor5']
XCor6 = notes$note.median[notes$correcteur.median == 'Cor6']
XCor7 = notes$note.median[notes$correcteur.median == 'Cor7']
XCor8 = notes$note.median[notes$correcteur.median == 'Cor8']

png(file = "TP1/ex1/boxplotMedianCorrecteur.png")
boxplot(XCor1, XCor2, XCor3, XCor4, XCor5, XCor6, XCor7, XCor8,
	main = "Comparaison resultats du médian selon correcteur",  
	col = c("blue", "red","green","orange","purple","dark blue","brown","grey","yellow"), 
	names = c("1", "2","3","4","5","6","7","8"), 
	xlab = "correcteur", 
	ylab = "resultats", 
	notch = FALSE,
	las=1)
dev.off()

# SELECT note final FROM notes WHERE correcteur == 'correcteur XX';
YCor1 = notes$note.final[notes$correcteur.final == 'Cor1']
YCor2 = notes$note.final[notes$correcteur.final == 'Cor2']
YCor3 = notes$note.final[notes$correcteur.final == 'Cor3']
YCor4 = notes$note.final[notes$correcteur.final == 'Cor4']
YCor5 = notes$note.final[notes$correcteur.final == 'Cor5']
YCor6 = notes$note.final[notes$correcteur.final == 'Cor6']
YCor7 = notes$note.final[notes$correcteur.final == 'Cor7']
YCor8 = notes$note.final[notes$correcteur.final == 'Cor8']

png(file = "TP1/ex1/boxplotFinalCorrecteur.png")
boxplot(YCor1, YCor2, YCor3, YCor4, YCor5, YCor6, YCor7, YCor8,
	main = "Comparaison resultats du final selon correcteur",  
	col = c("blue", "red","green","orange","purple","dark blue","brown","grey","yellow"), 
	names = c("1", "2","3","4","5","6","7","8"), 
	xlab = "correcteur", 
	ylab = "resultats", 
	notch = FALSE,
	las=1)
dev.off()

#correlation 
cor(cbind(notes$note.median,notes$note.final,notes$note.totale))

#etude d'une variable qualitative et quantitative
#variance de la variable quantivative
vartot <- function(x) {res <- sum((x-mean(x))^2) ; return(res)}

#variance de la variable qualitative, etude intergroupe
varinter <- function(x,gpe) {
     moyennes <- tapply(x,gpe,mean)
     effectifs <- tapply(x,gpe,length)
     res <- (sum(effectifs*(moyennes-mean(x))^2))
     return(res)
 }

#rapport de corrélation des deux variables
rapport <- function(x,gpe) {res <- varinter(x,gpe)/vartot(x) ; return(res)}

#calcul du rapport de la note finale et de résultat
rapport(notes$note.totale,notes$resultat)

#calcul du rapport de la note finale et de résultat
rapport(notes$note.totale,notes$specialite)

#représentater ces deux groupes!
graphnf <- function(x,gpe) {
     stripchart(x~gpe,las=1)
     points(tapply(x,gpe,mean),1:length(levels(gpe)),col="red",pch=19,cex=1.5)
     abline(v=mean(x),lty=2)
     moyennes <- tapply(x,gpe,mean)
     traitnf <- function(n) segments(moyennes[n],n,mean(x),n,col="blue",lwd=2)
     sapply(1:length(levels(gpe)),traitnf)
 }

png(file = "TP1/ex1/rapportNoteResultat.png",width=800,height=350)
graphnf(notes$note.totale,notes$resultat)
dev.off()
png(file = "TP1/ex1/rapportNoteSpecialite.png",width=800,height=350)
graphnf(notes$note.totale,notes$specialite)
dev.off()

#tableau de contingence du dernier diplome et des resultats
contingence = table(notes$dernier.diplome.obtenu,notes$resultat)
png(file = "TP1/ex1/contingenceDiplomeResultat.png",width=600,height=400)
library(ade4)
table.cont(contingence, csize = 2)
dev.off()
res <- chisq.test(touristes)
res$expected
#conversion de la table de contingence en %
contingenceP = addmargins(contingence,FUN=list(Pourcentage=function(x) round(sum(x)/sum(contingence)*100,2)))
#conversion de la table de contingence en ligne(%)
tabligne=cbind(addmargins(prop.table(addmargins(contingence,1),1),2), c(margin.table(contingence,1),sum(contingence)))
rownames(tabligne)<-c(rownames(contingence),"TOTAL","EFFECTIF") 
#conversion de la table de contingence en colonne(%)
tabcol=addmargins(prop.table(addmargins(contingence,2),2),1)
rownames(tabcol)<-c(rownames(contingence),"TOTAL") 
tabcol=round(tabcol*100,digit=2)

F <- contingence/sum(contingence)
round(contingence*100,digit=2)

