applyTransform <- function(pmat, tarr, assoc = NULL){

	# If single point convert to matrix
	if(is.vector(pmat)) pmat <- matrix(pmat, nrow=1, ncol=length(pmat))

	# Transformation matrix
	if(length(dim(tarr)) == 2){
	
		# Get point coordinates as matrix for transformation - coerce to matrix if single point
		pcoor <- matrix(1, nrow(pmat), 4, dimnames=list(rownames(pmat), NULL))
		pcoor[, 1:3] <- pmat

		# Apply transformation
		parr <- pcoor %*% t(tarr)

		# Remove 1s row and transpose
		return(parr[, 1:3])

	# Transformation 3-d array
	}else if(length(dim(tarr)) == 3){
	
		if(length(dim(pmat)) == 2){

			## Single body
			# Create array for transformed points
			parr <- array(NA, dim=c(nrow(pmat), 3, dim(tarr)[3]), dimnames=list(rownames(pmat), NULL, NULL))

			# Get point coordinates as matrix for transformation - coerce to matrix if single point
			pcoor <- matrix(1, 4, nrow(pmat), dimnames=list(NULL, rownames(pmat)))
			pcoor[1:3, ] <- t(pmat)

			# Apply transformation
			tcoor <- apply(tarr, 3, '%*%', pcoor)

			# Convert to array
			tcoor_arr <- array(tcoor, dim=c(4, nrow(pmat), dim(tarr)[3]))

			# Swap first two dimensions (transpose each "matrix" within array) and remove 1s
			return(aperm(tcoor_arr[1:3, , ], perm=c(2,1,3)))

		}else if(length(dim(pmat)) == 3){
		
			# 
			n_iter <- dim(tarr)[3]
			for(iter in 1:n_iter) pmat[, , iter] <- applyTransform(pmat[, , iter], tarr[, , iter])
			
			return(pmat)

			#tmat <- matrix(tarr, nrow=4, ncol=4*dim(tarr)[3])
			#rmat1 <- rbind(matrix(pmat, nrow=3, ncol=dim(pmat)[1]*dim(pmat)[3]), rep(1, dim(pmat)[1]*dim(pmat)[3]))
			#rmat2 <- matrix(rmat1, nrow=4*dim(pmat)[3], ncol=dim(pmat)[1])
		}

	# Transformation 4-d array
	}else{

		# Create array for transformed points
		parr <- array(NA, dim=c(nrow(pmat), 3, dim(tarr)[4]), dimnames=list(rownames(pmat), NULL, NULL))

		# Multiple bodies - apply transformations for each body
		for(body in 1:dim(tarr)[3]){

			# Get body name
			body_name <- dimnames(tarr)[[3]][body]

			# Find points associated with body
			body_assoc <- which(assoc == body_name)
	
			# Skip if no points associated with body
			if(length(body_assoc) == 0) next

			# Get point coordinates as matrix for transformation - coerce to matrix if single point
			pcoor <- rbind(matrix(t(pmat[body_assoc, ]), ncol=length(body_assoc)), rep(1, length(body_assoc)))
			colnames(pcoor) <- rownames(pmat)[body_assoc]
	
			# Apply transformation
			tcoor <- apply(tarr[, , body, ], 3, '%*%', pcoor)

			# Convert to array
			tcoor_arr <- array(tcoor, dim=c(4, length(body_assoc), dim(tarr)[4]))

			# Swap first two dimensions (transpose each "matrix" within array) and remove 1s
			parr[colnames(pcoor), , ] <- aperm(tcoor_arr[1:3, , ], perm=c(2,1,3))
		}
	}

	parr
}
