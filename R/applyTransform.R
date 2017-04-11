applyTransform <- function(pmat, tarr, assoc = NULL){

	if(length(dim(tarr)) == 3){
		
		## Single body
		# Create array for transformed points
		parr <- array(NA, dim=c(nrow(pmat), 3, dim(tarr)[3]), dimnames=list(rownames(pmat), NULL, NULL))

		# Get point coordinates as matrix for transformation - coerce to matrix if single point
		pcoor <- rbind(matrix(t(pmat), ncol=nrow(pmat)), rep(1, nrow(pmat)))
		colnames(pcoor) <- rownames(pmat)

		# Apply transformation
		tcoor <- apply(tarr, 3, '%*%', pcoor)

		# Convert to array
		tcoor_arr <- array(tcoor, dim=c(4, nrow(pmat), dim(tarr)[3]))

		# Swap first two dimensions (transpose each "matrix" within array) and remove 1s
		parr <- aperm(tcoor_arr[1:3, , ], perm=c(2,1,3))

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
