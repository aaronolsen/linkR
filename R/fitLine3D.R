fitLine3D <- function(X, margin=0.2){

	# Source: http://r.789695.n4.nabble.com/Fit-a-3-Dimensional-Line-to-Data-Points-td863596.html

	#if(ncol(X) == 2) X <- cbind(X, rep(0, nrow(X)))

	meanX <- apply(X, 2, mean) 
	pca <- prcomp(X)
	t <- c(min(pca$x[, 1])-margin, max(pca$x[, 1])+margin) 
	endpts <- rbind(meanX + t[1]*pca$rotation[, 1], meanX + t[2]*pca$rotation[, 1])
	
	list(
		'v'=uvector(endpts[2,]-endpts[1,]),
		'p1'=endpts[1,],
		'p2'=endpts[2,]
	)
}