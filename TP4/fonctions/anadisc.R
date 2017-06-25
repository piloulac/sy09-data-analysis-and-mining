adq.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		# indices des individus de la classe
		indk <- which(zapp==k)
		# on extrait les individus de la classe
		individus_de_la_classe = Xapp[indk,]
		# on store les paramètres de la classe k
		# esperance de la classe k
		param$mean[k,] <- apply(individus_de_la_classe, 2, mean)
		# proportion de la classe k
		param$prop[k] <- length(indk)/length(zapp)
		# matrice de covariance de la classe k
		param$MCov[,,k] <- cov(individus_de_la_classe)
	}

	param
}

adl.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	MCov <- array(0, c(p,p))
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)
		# on extrait les individus de la classe
		individus_de_la_classe = Xapp[indk,]
		# on store les paramètres de la classe k
		# esperance de la classe k
		param$mean[k,] <- apply(individus_de_la_classe, 2, mean)
		# proportion de la classe k
		param$prop[k] <- length(indk)/length(zapp)
		# covariance de la classe k
		MCov <- MCov + length(indk)*cov(individus_de_la_classe)
	}
	MCov <- MCov / n # == length(zapp)
	for (k in 1:g)
	{
		param$MCov[,,k] <- MCov
	}

	param
}

nba.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)
		# on extrait les individus de la classe
		individus_de_la_classe = Xapp[indk,]
		# on store les paramètres de la classe k
		# esperance de la classe k
		param$mean[k,] <- apply(individus_de_la_classe, 2, mean)
		# proportion de la classe k
		param$prop[k] <- length(indk)/length(zapp)
		# covariance de la classe k
		param$MCov[,,k] <- diag(diag(cov(individus_de_la_classe)))
	}

	param
}

ad.val <- function(param, Xtst)
{
	n <- dim(Xtst)[1]
	p <- dim(Xtst)[2]
	g <- length(param$prop)

	out <- NULL

	prob <- matrix(0, nrow=n, ncol=g)

	for (k in 1:g)
	{
		# calcul de la loi de densité pour la classe k
		f <- mvdnorm(Xtst, param$mean[k,], param$MCov[,,k])
		# calcul de la probabilité a posteriori pour la classe k
		prob[,k] <- param$prop[k] * f
	}

	# on calcul la probabilité a posteriori
	prob <- prob / apply(prob,1,sum) 
	pred <- max.col(prob)

	out$prob <- prob
	out$pred <- pred

	out
}
