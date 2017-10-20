rotateBody <- function(m, v, a, p = NULL){

	if(is.vector(m)) m <- matrix(m, nrow=1, ncol=length(m))
	
	if(length(a) > 1){

		# Convert matrix to array with same number of iterations as angles
		m_arr <- array(NA, dim=c(dim(m), length(a)), dimnames=list(dimnames(m)[[1]], dimnames(m)[[2]], NULL))

		# Rotate points at each iteration
		for(iter in 1:length(a)) m_arr[,,iter] <- rotateBody(m=m, v=v, a=a[iter], p=p)

		return(m_arr)
	}

	# IF INPUT MATRIX HAS NO ROWS, RETURN EMPTY MATRIX
	if(dim(m)[1] == 0) return(m)

	if(!is.null(p)){
		# GET TRANSLATION MATRIX
		tm <- matrix(p, nrow=nrow(m), ncol=ncol(m), byrow=TRUE)

		# MAKE ROTATION POINT ORIGIN
		m <- m - tm
	}

	# FIND ROTATION MATRIX
	RM <- tMatrixEP(v=v, a=a)

	# ROTATE POINTS
	m <- m %*% RM

	# MOVE POINTS BACK TO ORIGINAL POSITION
	if(!is.null(p)){
		m <- m + tm
	}
	
	m
}