fitLine3D <- function(X){

	# Source: http://r.789695.n4.nabble.com/Fit-a-3-Dimensional-Line-to-Data-Points-td863596.html

	if(ncol(X) == 2) X <- cbind(X, rep(0, nrow(X)))

	N <- nrow(X) 

	meanX <- apply(X, 2, mean) 
	Xpca   <- princomp(X) 
	dirVector <- Xpca$loadings[, 1] 

	Xfit1 <- matrix(rep(meanX, each=N), ncol=3) + Xpca$score[, 1] %*% t(dirVector) 
	t <- c(min(Xpca$score[, 1])-.2, max(Xpca$score[, 1])+.2) 
	endpts <- rbind(meanX + t[1]*dirVector, meanX + t[2]*dirVector)
	
	list(
		'v'=uvector(endpts[2,]-endpts[1,]),
		'p1'=endpts[1,],
		'p2'=endpts[2,]
	)
}