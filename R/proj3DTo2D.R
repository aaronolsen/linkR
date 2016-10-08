proj3DTo2D <- function(m, eyez = 300, zoom = 280, maxzoom = -200){

	if(is.vector(m)) m <- matrix(m, nrow=1, ncol=length(m))
	
	if(length(dim(m)) == 3){

		rm <- array(NA, dim=c(dim(m)[1], 2, dim(m)[3]), dimnames=list(dimnames(m)[[1]], NULL, dimnames(m)[[3]]))
		for(i in 1:dim(m)[3]) rm[, , i] <- proj3DTo2D(m[, , i])
		return(rm)
	}

	rm <- matrix(NA, nrow(m), 2, dimnames=list(dimnames(m)[[1]], NULL))

	depth = zoom * (eyez - maxzoom) / 100 + eyez

	for(i in 1:nrow(m)){
		u <- -(depth - eyez) / (m[i, 3] - eyez)
		rm[i, 1] <- u * m[i, 1]
		rm[i, 2] <- u * m[i, 2]
	}

	return(rm)
}