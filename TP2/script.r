 plot(C[,1],C[,2],
 type="n",
 main="Affichage dans le plan factoriel",
	xlab="Premiere Composante", 
	ylab="deuxieme composante");
 text(C[,1],C[,2],
	rownames(data),
	col=c("blue","orange")[crabs2$sp]);
	
	
plot(C[,1],C[,2],
 main="Affichage dans le plan factoriel",
	xlab="Premiere Composante", 
	ylab="deuxieme composante",
	col=c("blue","orange")[crabs2$sp],
	pch=c(19,21)[crabs2$sex]);
	
legend("bottomleft",inset=.02,c("Blue male","Blue female","Orange male", "Orange female"), 
	col=c("blue","blue","orange","orange"), 
	pch=c(19,25,19,25))

png(file = "plot_shepard_K2.png")	
s1 = Shepard(as.dist(mutations), cmdscale(mutations, k = 2))	
plot(s1, main = "diagramme de Shepard de Mutations (d = 2)", 
	pch = 22,
	xlab="(Shepard(as.dist(mutations), cmdscale(mutations, d = 2)$x",
	ylab="(Shepard(as.dist(mutations), cmdscale(mutations, d = 2)$y")
abline(0,1)
dev.off()

png(file = "plot_shepard_K3.png")	
s2 = Shepard(as.dist(mutations), cmdscale(mutations, k = 3))	
plot(s2, main = "diagramme de Shepard de Mutations (d = 3)", 
	pch = 22,
	xlab="(Shepard(as.dist(mutations), cmdscale(mutations, d = 3)$x",
	ylab="(Shepard(as.dist(mutations), cmdscale(mutations, d = 3)$y")
abline(0,1)
dev.off()

png(file = "plot_shepard_K4.png")	
s3 = Shepard(as.dist(mutations), cmdscale(mutations, k = 4))	
plot(s3, main = "diagramme de Shepard de Mutations (d = 4)", 
	pch = 22,
	xlab="(Shepard(as.dist(mutations), cmdscale(mutations, d = 4)$x",
	ylab="(Shepard(as.dist(mutations), cmdscale(mutations, d = 4)$y")
abline(0,1)
dev.off()

png(file = "plot_shepard_K5.png")	
s4 = Shepard(as.dist(mutations), cmdscale(mutations, k = 5))	
plot(s4, main = "diagramme de Shepard de Mutations (d = 5)", 
	pch = 22,
	xlab="(Shepard(as.dist(mutations), cmdscale(mutations, d = 5)$x",
	ylab="(Shepard(as.dist(mutations), cmdscale(mutations, d = 5)$y")
abline(0,1)
dev.off()


#methode des centres mobiles

data(iris)
cmob2 = kmeans(iris[,1:4],2, algorithm = "MacQueen")
cmob3 = kmeans(iris[,1:4],3, algorithm = "MacQueen")
cmob4 = kmeans(iris[,1:4],4, algorithm = "MacQueen")

png("kmeansIris2.png", width = 500, height = 400)
#decaler l'écran pour visualiser la légende sur la droite
par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
clusplot(iris[,1:4], cmob2$cluster, 
         color = T, 
         shade = T, 
         labels = 0, 
         lines = 0,
         col.p = "black",
         main = "Representation des classes predites par la methode des
         centres mobiles en partitionnant en 2 classes",
         xlab = "Composante 1",
         ylab = "Composante 2",
         sub = "")
legend(4.2, 2,
       c("Classe 1", "Classe 2"),
       pch = c(1,2))
#ramener l'écran
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()
cat("kmeansIris2.png sauvegardee\n")

png("kmeansIris3.png", width = 500, height = 400)
par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
clusplot(iris[,1:4], cmob3$cluster, 
         color = T, 
         shade = T, 
         labels = 0, 
         lines = 0,
         col.p = "black",
         main = "Representation des classes predites par la methode des
         centres mobiles en partitionnant en 3 classes",
         xlab = "Composante 1",
         ylab = "Composante 2",
         sub = "")
legend(4.2, 2,
       c("Classe 1", "Classe 2","Classe 3"),
       pch = c(1,2,3))
#ramener l'écran
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()
cat("kmeansIris3.png sauvegardee\n")

png("kmeansIris4.png", width = 500, height = 400)
par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
clusplot(iris[,1:4], cmob4$cluster, 
         color = T, 
         shade = T, 
         labels = 0, 
         lines = 0,
         col.p = "black",
         main = "Representation des classes predites par la methode des
         centres mobiles en partitionnant en 4 classes",
         xlab = "Composante 1",
         ylab = "Composante 2",
         sub = "")
legend(4.2, 2,
       c("Classe 1", "Classe 2","Classe 3","Classe 4"),
       pch = c(1,2,3,4))
#ramener l'écran
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()
cat("kmeansIris4.png sauvegardee\n")

#etude d'une classification sur 3 classes
cmob3SL = kmeans(iris[,1],3, algorithm = "MacQueen")
cmob3SW = kmeans(iris[,2],3, algorithm = "MacQueen")
cmob3PL = kmeans(iris[,3],3, algorithm = "MacQueen")
cmob3PW = kmeans(iris[,4],3, algorithm = "MacQueen")

png("kmeansIris3SepalLenght.png", width = 500, height = 400)
par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
clusplot(iris[,1:4], cmob3SL$cluster, 
         color = T, 
         shade = T, 
         labels = 0, 
         lines = 0,
         col.p = "black",
         main = "Representation des classes predites par la methode des
         centres mobiles en partitionnant en 3 classes sur Sepal Lenght",
         xlab = "Composante 1",
         ylab = "Composante 2",
         sub = "")
legend(4.2, 2,
       c("Classe 1", "Classe 2","Classe 3"),
       pch = c(1,2,3))
#ramener l'écran
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()
cat("kmeansIris3SepalLenght.png sauvegardee\n")

png("kmeansIris3SepalWidth.png", width = 500, height = 400)
par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
clusplot(iris[,1:4], cmob3SW$cluster, 
         color = T, 
         shade = T, 
         labels = 0, 
         lines = 0,
         col.p = "black",
         main = "Representation des classes predites par la methode des
         centres mobiles en partitionnant en 3 classes sur Sepal Width",
         xlab = "Composante 1",
         ylab = "Composante 2",
         sub = "")
legend(4.2, 2,
       c("Classe 1", "Classe 2","Classe 3"),
       pch = c(1,2,3))
#ramener l'écran
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()
cat("kmeansIris3SepalWidth.png sauvegardee\n")

png("kmeansIris3PetalLenght.png", width = 500, height = 400)
par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
clusplot(iris[,1:4], cmob3PL$cluster, 
         color = T, 
         shade = T, 
         labels = 0, 
         lines = 0,
         col.p = "black",
         main = "Representation des classes predites par la methode des
         centres mobiles en partitionnant en 3 classes sur Petal Lenght",
         xlab = "Composante 1",
         ylab = "Composante 2",
         sub = "")
legend(4.2, 2,
       c("Classe 1", "Classe 2","Classe 3"),
       pch = c(1,2,3))
#ramener l'écran
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()
cat("kmeansIris3PetalLenght.png sauvegardee\n")

png("kmeansIris3PetalWidth.png", width = 500, height = 400)
par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
clusplot(iris[,1:4], cmob3PW$cluster, 
         color = T, 
         shade = T, 
         labels = 0, 
         lines = 0,
         col.p = "black",
         main = "Representation des classes predites par la methode des
         centres mobiles en partitionnant en 3 classes sur Petal Width",
         xlab = "Composante 1",
         ylab = "Composante 2",
         sub = "")
legend(4.2, 2,
       c("Classe 1", "Classe 2","Classe 3"),
       pch = c(1,2,3))
#ramener l'écran
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()
cat("kmeansIris3PetalWidth.png sauvegardee\n")
	   
inertie3k.expl<-rep(0,times=4)
inertie3k.expl[2] <-cmob3PL$betweenss/cmob3PL$totss
inertie3k.expl[3] <-cmob3PW$betweenss/cmob3PW$totss
inertie3k.expl[4] <-cmob3SL$betweenss/cmob3SL$totss
inertie3k.expl[1] <-cmob3SW$betweenss/cmob3SW$totss
#magouille
inertie3k.expl=as.matrix(inertie3k.expl)
rownames(inertie3k.expl)=c("SW","PL","PW","SL")
png("kmeans3Inertie.png", width = 500, height = 400)
plot(inertie3k.expl,xlab="Classification par caractéristique des individus",ylab="% inertie expliquée",pch='')
text(inertie3k.expl, row.names(inertie3k.expl))
dev.off()
cat("kmeans3Inertie.png sauvegardee\n")

#inertie3k.expl<-rep(0,times=50)
#for (k in 1:50){
#cmob3k<-kmeans(iris[,1:4],centers=2,nstart=5)  
#	inertie.expl[k] <-clus$betweenss/clus$totss
#}
#plot(inertie.expl,type="b",xlab="Nb. d'iteration",ylab="% inertie expliquée")

inertie3knew<-rep(0,times=10)
for(i in 1:10) {
    kmeans3 <- kmeans(iris[,1:4], 3, algorithm = "MacQueen")
    filename <- paste("kmeansIris3_", i, ".png", sep = "")
    png(filename, width = 500, height = 400)
    par(xpd = T, mar = par()$mar + c(0, 0, 0, 5))
    clusplot(donnees$num, kmeans3$cluster,
             color = T, 
             shade = T, 
             labels = 0, 
             lines = 0,
             col.p = "black",
             main = "Representation des classes predites par la methode des
             centres mobiles en partitionnant en 3 classes",
             xlab = "Composante 1",
             ylab = "Composante 2",
             sub = "")
    legend(4.2, 2,
           c("Classe 1", "Classe 2", "Classe 3"),
           pch = c(1:3))
    par(mar = c(5, 4, 4, 2) + 0.1)
    dev.off()
    cat(filename, "sauvegardee\n")
	inertie3knew[i] <-kmeans3$betweenss/kmeans3$totss

    if(i == 1) {
        cat("Tableau de contingence des classes predites par la methode
            des centres mobiles par rapport aux classes reelles\n")
        print(table(iris[,5], kmeans3$cluster))
        cat("\n")
    }
}
plot(inertie3knew,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")


#methode du coude
for (k in 2:10){
	clus<-kmeans(iris[,1:4],centers=k,nstart=5)  
	inertie.expl[k] <-clus$betweenss/clus$totss
}
#representation graphique par la methode du coude
plot(1:10,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")

#Calinski Harabasz
library(fpc)
#évaluation des solutions
sol.kmeans<-kmeansruns(iris[,1:4],krange=2:10,criterion="ch")
#representation graphique
plot(1:10,sol.kmeans$crit,type="b",xlab="Nb. de groupes",ylab="Silhouette")

