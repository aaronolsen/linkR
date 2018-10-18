fitBox <- function(X){

	# Source: http://r.789695.n4.nabble.com/Fit-a-3-Dimensional-Line-to-Data-Points-td863596.html

	#if(ncol(X) == 2) X <- cbind(X, rep(0, nrow(X)))

	meanX <- apply(X, 2, mean) 
	pca <- prcomp(X)

	v <- matrix(NA, nrow=3, ncol=3)
	endpts <- array(NA, dim=c(2,3,3))

	for(i in 1:3){
		t <- range(pca$x[, i])
		endpts[1,,i] <- meanX + t[1]*pca$rotation[, i]
		endpts[2,,i] <- meanX + t[2]*pca$rotation[, i]
		v[i, ] <- uvector(endpts[2,,i]-endpts[1,,i])
	}
	
	list(
		'v'=v,
		'endpts'=endpts
	)
}