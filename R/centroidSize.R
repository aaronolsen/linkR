centroidSize <- function(m, na.rm = TRUE, transpose = FALSE){

	if(is.array(m) && length(dim(m)) > 2){
		r <- rep(NA, dim(m)[3])
		for(i in 1:dim(m)[3]){

			n <- na.omit(m[,, i])
		
			# CENTER
			n <- n - matrix(colMeans(n, na.rm=na.rm), nrow=nrow(n), ncol=ncol(n), byrow=TRUE)
		
			# FIND CENTROID SIZE
			r[i] <- sqrt(sum(n^2))
		}
		return(r)
	}else{
		
		if(transpose) m <- t(m)
	
		m <- na.omit(m)
	
		# CENTER
		m <- m - matrix(colMeans(m, na.rm=na.rm), nrow=nrow(m), ncol=ncol(m), byrow=TRUE)
	
		# FIND CENTROID SIZE
		r <- sqrt(sum(m^2))
	}
	r
}