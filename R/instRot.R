instRot <- function(m1, m2, na.rm = FALSE){

	## Finds instantaneous center of rotation (CoR) and axis of rotation (AoR) between 
	## coordinates in two different orientations. Four possible solutions can be obtained 
	## by changing the orientation of the AoR and sign of the angle. The following 
	## constraints are imposed to obtain a single solution:
	##    1) Only returns positive angles
	##    2) Returns the minimum of possible positive angles

	# Remove rows with NA values
	if(na.rm){
		m1 <- m1[rowSums(is.na(m1)) == 0, ]
		m2 <- m2[rowSums(is.na(m2)) == 0, ]
	}
	
	# Find centroids
	m1_centroid <- colMeans(m1)
	m2_centroid <- colMeans(m2)

	# Place centroid at origin
	m1c <- m1 - matrix(m1_centroid, nrow(m1), ncol(m1), byrow=TRUE)
	m2c <- m2 - matrix(m2_centroid, nrow(m2), ncol(m2), byrow=TRUE)
	
	# SVD of cross-covariance matrix
	svd <- svd(t(m1c) %*% m2c)

	# Correction to ensure a right-handed coordinate system
	S <- diag(3)
	S[3,3] <- sign(det(svd$v %*% t(svd$u)))

	# Find rotation matrix
	rmat <- svd$v %*% S %*% t(svd$u)

	# Compute eigen vectors and values
	rmat_eigen <- eigen(rmat)

	# Find AoR as the eigenvector corresponding to eigenvalue closest to 1 (truncation errors)
	aor <- Re(rmat_eigen$vectors[, which.min(abs(Re(rmat_eigen$values) - 1))])

	# Get rotation magnitude/angle
	angle <- rep(acos((sum(rmat*diag(3)) - 1) / 2), 4)*c(1,1,-1,-1)

	# Find distance from mid-centroid point to CoR
	dcen <- -(distPointToPoint(m1_centroid, m2_centroid) / 2) / sin(angle[1]/2)

	# Find vector to the CoR
	v2rcen <- cprod(m2_centroid - m1_centroid, aor)

	# Find possible centers of rotation
	rcen <- matrix(colMeans(rbind(m1_centroid, m2_centroid)), 4, 3, byrow=TRUE) + 
		matrix(dcen*uvector(v2rcen), 4, 3, byrow=TRUE)*matrix(rep(c(-1,1), 6), 4, 3)

	# Try each CoR and angle on original m1
	m1r <- array(NA, dim=c(dim(m1)[1:2], 4))
	
	# Try all combinations with potential CoR
	for(i in 1:dim(m1r)[3]) m1r[, , i] <- rotateBody(m1, rcen[i, ], aor, angle[i])

	# Find combination with lowest difference from actual body position
	min_idx <- which.min(apply(abs(m1r - array(m2, dim=c(dim(m1)[1:2], 4))), 3, 'sum'))
	
	# Save only that combination
	rcen <- rcen[min_idx, ]
	angle <- angle[min_idx]

	# If angle is negative, change angle and AoR sign
	if(angle < 0){angle <- -angle; aor <- -aor}

	# Find error as mean distance between points
	mean_error <- mean(sqrt(rowSums((rotateBody(m1, p=rcen, v=aor, a=angle) - m2)^2))); print(mean_error)

	list(
		aor=aor,
		cor=rcen,
		angle=angle
	)
}