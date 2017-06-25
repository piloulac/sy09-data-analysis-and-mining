getSqEnsemble <- function (X) {
    X = as.matrix(X)
    Y = X
    colY=colnames(X)
    for (i in 1:(dim(X)[2]-1)){
        for (j in (i+1):(dim(X)[2])){
            Y = as.matrix(cbind(Y,X[,i]*X[,j]))
            colY = c(colY,paste(colnames(X)[i],"*",colnames(X)[j]))
        }
    }
    for (i in 1:dim(X)[2]){
        Y = as.matrix(cbind(Y,X[,i]^2))
        colY = c(colY,paste(colnames(X)[i],"^2"))
    }
    colnames(Y)=colY
    as.matrix(Y)
}

getAllGraph <- function (X ,z , name, N, makeGraphs = T, doLogQuad = T){
    
    # analyse discriminante
    ADQ = adq.app(X,z)
    ADL = adl.app(X,z)
    NBA = nba.app(X,z)
    
    # régression logistique
    
    LOG_F = log.app(X,z,F,1e-5)
    LOG_T = log.app(X,z,T,1e-5)

    # régression logistique quadratique
    
    if (doLogQuad == T) {
        X_2 = getSqEnsemble(X)
        LOG2_F = log.app(X_2,z,F,1e-5)
        LOG2_T = log.app(X_2,z,T,1e-5)
    }


    if (makeGraphs == T){
        # analyse discriminante quadratique
        png(file = paste("res/adq_",name,".png", sep=""),width=400,height=400)
        prob.ad(ADQ,X,z,c(0.2,0.4,0.6,0.8))
        dev.off()
        cat(paste("res/adq_",name,".png"," sauvegardee\n", sep=""))
        # analyse discriminante lineaire
        png(file = paste("res/adl_",name,".png", sep=""),width=400,height=400)
        prob.ad(ADL,X,z,c(0.2,0.4,0.6,0.8))
        dev.off()
        cat(paste("res/adl_",name,".png"," sauvegardee\n", sep=""))
        # analyse avec classifieur baysien
        png(file = paste("res/nba_",name,".png", sep=""),width=400,height=400)
        prob.ad(NBA,X,z,c(0.2,0.4,0.6,0.8))
        dev.off()
        cat(paste("res/nba_",name,".png"," sauvegardee\n", sep=""))
        
        # regression logistique False
        png(file = paste("res/log_",name,"_F.png", sep=""),width=400,height=400)
        prob.log(LOG_F$beta,X,z,c(0.2,0.4,0.6,0.8))
        dev.off()
        cat(paste("res/log_",name,"_F.png"," sauvegardee\n", sep=""))
        # regression logistique True
        png(file = paste("res/log_",name,"_T.png", sep=""),width=400,height=400)
        prob.log(LOG_T$beta,X,z,c(0.2,0.4,0.6,0.8))
        dev.off()
        cat(paste("res/log_",name,"_T.png"," sauvegardee\n", sep=""))
        
        if (doLogQuad == T) {
            # regression logistique quadratique False
            png(file = paste("res/log2_",name,"_F.png", sep=""),width=400,height=400)
            prob.log2(LOG2_F$beta,X,z,c(0.2,0.4,0.6,0.8))
            dev.off()
            cat(paste("res/log2_",name,"_F.png"," sauvegardee\n", sep=""))
            # regression logistique quadratique True
            png(file = paste("res/log2_",name,"_T.png", sep=""),width=400,height=400)
            prob.log2(LOG2_T$beta,X,z,c(0.2,0.4,0.6,0.8))
            dev.off()
            cat(paste("res/log2_",name,"_T.png"," sauvegardee\n", sep=""))
        }
    }
    
    #calcul d'erreurs
    
    erreur_disc=calcul_tx_erreurs_discriminante(as.matrix(cbind(X,z)), N, dim(X)[2])
    erreur_log=calcul_tx_erreurs_logistique(as.matrix(cbind(X,z)), N, dim(X)[2], F, 1e-5,doLogQuad)
    erreur_tree=calcul_tx_erreurs_arbre(cbind(X,z), N, dim(X)[2])
    
    c(erreur_disc,erreur_log,erreur_tree)
}
