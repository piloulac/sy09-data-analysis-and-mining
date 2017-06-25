calcul_tx_erreurs_arbre <- function(X, N, nbcol){
    
    # initialisation d'une matrice pour récupérer les tx_erreurs_calculés
    m = matrix(data = NA, nrow = N, ncol = 1)
    for ( i in 1:N) {
        # definition d'un nouveau jeu de données à partir du jeu fournis
        # utilisation plusieurs fois de separ1 pour ne pas que Xapp, Xval et Xtst dépendent les uns des autres
        donn.sep <- separ1(X[,1:nbcol], X[,nbcol+1])
        Xapp <<- donn.sep$Xapp
        zapp <- donn.sep$zapp
        donn.sep <- separ1(X[,1:nbcol], X[,nbcol+1])
        Xtst <<- donn.sep$Xtst
        ztst <- donn.sep$ztst
        
        # creation du facteur
        classe <<- factor(zapp)

        # construction de l'arbre
        tr = tree(classe ~., data=Xapp,control=tree.control(nobs=dim(Xapp)[1],mindev = 0.0001))
        vtst = max.col(predict(tr,newdata = Xtst))

        simTst_log2 = length(which(vtst==ztst))
        tx_erreur_app_arbre = 1 - (1/nrow(Xtst)) * simTst_log2

        
        # sauvegarde des taux d'erreurs
        m[i,] <- c(tx_erreur_app_arbre)
        colnames(m) <- c("tx_erreur_tree")
    }
    
    # estimation ponctuelle
    estimation_ponctuelle = (colSums(m)/N)
    Xapp <<- NA
    Xtst <<- NA
    classe <<- NA
    
    return(estimation_ponctuelle* 100)
}
