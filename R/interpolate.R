interpolate <- function(x, t1, t2, type = 'linear'){

	## Interpolates values in x(t1) to x(t2)
	if(length(t1) == length(t2) && sum(abs(t1 - t2)) == 0) return(x)

	if(is.array(x)){
		if(length(dim(x)) == 2){
			x2 <- apply(x, 2, 'interpolate', t1=t1, t2=t2, type=type)
			colnames(x2) <- colnames(x)
			return(x2)
		}
		if(length(dim(x)) == 3){
			x2 <- array(NA, dim=c(dim(x)[1], dim(x)[2], length(t2)))
			for(ii in 1:dim(x)[1]) x2[ii, , ] <- t(apply(t(x[ii, , ]), 2, 'interpolate', t1=t1, t2=t2, type=type))
			dimnames(x2)[[1]] <- dimnames(x)[[1]]
			return(x2)
		}
	}

	if(is.na(x[1])) return(rep(NA, length(t2)))

	x2 <- rep(NA, length(t2))

	#
	for(ii in 1:length(t2)){
	
		t12 <- t1 - t2[ii]
		
		# Exact same time point
		if(sum(t12 == 0) > 0){
			x2[ii] <- x[which(t12 == 0)[1]]
			next
		}

		t12_p <- t12_n <- t12

		# Find nearest time points before and after new time point
		t12_p[t12 < 0] <- NA
		t12_n[t12 > 0] <- NA
		
		if(length(t12_p) == 0 || length(t12_n) == 0) next

		idx <- c(which.min(abs(t12_n)), which.min(abs(t12_p)))
		
		x_range <- x[idx]

		if(type == 'linear'){
			x2[ii] <- (x_range[2] - x_range[1])*(abs(t2[ii] - t1[idx[1]]) / abs(diff(t1[idx]))) + x_range[1]
		}
	}
	
	x2
}