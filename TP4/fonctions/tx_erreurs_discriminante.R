calcul_tx_erreurs_discriminante <- function(X, N, nbcol){
    
    # initialisation d'une matrice pour récupérer les tx_erreurs_calculés
    m = matrix(data = NA, nrow = N, ncol = 3)
    for ( i in 1:N) {
        # definition d'un nouveau jeu de données à partir du jeu fournis
        # utilisation plusieurs fois de separ1 pour ne pas que Xapp, Xval et Xtst dépendent les uns des autres
        donn.sep <- separ1(X[,1:nbcol], X[,nbcol+1])
        Xapp <- donn.sep$Xapp
        zapp <- donn.sep$zapp
        donn.sep <- separ1(X[,1:nbcol], X[,nbcol+1])
        Xtst <- donn.sep$Xtst
        ztst <- donn.sep$ztst
        
        # methode quadratique 
        adq_app = adq.app(Xapp,zapp)
        adval_adq = ad.val(adq_app,Xtst)
        vTst_adq = adval_adq$pred
        simTst_adq = length(which(vTst_adq==ztst))
        tx_erreur_tst_adq = 1 - (1/nrow(Xtst)) * simTst_adq

        # methode lineaire
        adl_app = adl.app(Xapp,zapp)
        adval_adl = ad.val(adl_app,Xtst)
        vTst_adl = adval_adl$pred
        simTst_adl = length(which(vTst_adl==ztst))
        tx_erreur_tst_adl = 1 - (1/nrow(Xtst)) * simTst_adl

        # methode baysien naif
        nba_app = nba.app(Xapp,zapp)
        adval_nba = ad.val(nba_app,Xtst)
        vTst_nba = adval_nba$pred
        simTst_nba = length(which(vTst_nba==ztst))
        tx_erreur_tst_nba = 1 - (1/nrow(Xtst)) * simTst_nba

        
        # sauvegarde des taux d'erreurs
        m[i,] <- c(tx_erreur_tst_adq,tx_erreur_tst_adl,tx_erreur_tst_nba)
        colnames(m) <- c("tx_erreur_adq","tx_erreur_adl","tx_erreur_nba")
    }
    
    # estimation ponctuelle
    estimation_ponctuelle = (colSums(m)/N)
    
    return(estimation_ponctuelle* 100)
}
