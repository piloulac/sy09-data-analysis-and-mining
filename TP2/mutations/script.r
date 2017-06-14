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
