calcul_tx_erreurs_logistique <- function(X, N, nbcol, intr, epsi, doLogQuad = T){
    
    # initialisation d'une matrice pour récupérer les tx_erreurs_calculés
    if (doLogQuad==T) {
        m = matrix(data = NA, nrow = N, ncol = 2)
    }
    else {
        m = matrix(data = NA, nrow = N, ncol = 1)
    }
    for ( i in 1:N) {
        # definition d'un nouveau jeu de données à partir du jeu fournis
        # utilisation plusieurs fois de separ1 pour ne pas que Xapp, Xval et Xtst dépendent les uns des autres
        donn.sep <- separ1(X[,1:nbcol], X[,nbcol+1])
        Xapp <- donn.sep$Xapp
        zapp <- donn.sep$zapp
        donn.sep <- separ1(X[,1:nbcol], X[,nbcol+1])
        Xtst <- donn.sep$Xtst
        ztst <- donn.sep$ztst
        
        # regression logistique
        log_app = log.app(Xapp,zapp,intr,epsi)
        log_val = log.val(log_app$beta,Xtst)
        vTst_log = log_val$pred
        simTst_log = length(which(vTst_log==ztst))
        tx_erreur_tst_log = 1 - (1/nrow(Xtst)) * simTst_log

        if (doLogQuad==T) {
            # regression logistique quadratique
            # etablissement d'un nouveau jeu de données d'apprentissage
            Xapp_2 = getSqEnsemble(Xapp)
            # etablissement d'un nouveau jeu de données de test
            Xtst_2 = getSqEnsemble(Xtst)
            log2_app = log.app(Xapp_2,zapp,intr,epsi)
            log2_val = log.val(log2_app$beta,Xtst_2)
            vTst_log2 = log2_val$pred
            simTst_log2 = length(which(vTst_log2==ztst))
            tx_erreur_tst_log2 = 1 - (1/nrow(Xtst)) * simTst_log2
            
            # sauvegarde des taux d'erreurs
            m[i,] <- c(tx_erreur_tst_log, tx_erreur_tst_log2)
            colnames(m) <- c("tx_erreur_log","tx_erreur_log2")
        }
        else {
            m[i,] <- c(tx_erreur_tst_log)
            colnames(m) <- c("tx_erreur_log")
        }

    }
    
    # estimation ponctuelle
    estimation_ponctuelle = (colSums(m)/N)
    
    return(estimation_ponctuelle* 100)
}
