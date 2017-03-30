instRotate <- function(m1, m2, na.rm = FALSE, positive.only = TRUE, CoR = NULL){

	## Finds instantaneous center of rotation (CoR) and axis of rotation (AoR) between 
	## coordinates in two different orientations. Four possible solutions can be obtained 
	## by changing the orientation of the AoR and sign of the angle. The following 
	## constraints are imposed to obtain a single solution:
	##    1) Only returns positive angles
	##    2) Returns the minimum of possible positive angles
	## A single axis of rotation can describe any number and combination of rotations about
	## the same center of rotation but not more than one rotation about different centers
	## of rotation.

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
	
	# Check that matrices are not identical
	if(sum(abs(m1-m2)) < 1e-10) return(list('AoR'=c(1,0,0), 'CoR'=c(0,0,0), 'angle'=0))
	
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
	AoR <- Re(rmat_eigen$vectors[, which.min(abs(Re(rmat_eigen$values) - 1))])

	# Get rotation magnitude/angle
	acos_val <- (sum(rmat*diag(3)) - 1) / 2
	
	# Check that value in acos function does not exceed 1 (by trace amount causes error)
	if(abs(acos_val) > 1) acos_val <- sign(acos_val)

	# Find angle from acos value
	angle <- rep(acos(acos_val), 4)*c(1,1,-1,-1)

	if(is.null(CoR)){

		# Find distance from mid-centroid point to CoR
		dcen <- -(distPointToPoint(m1_centroid, m2_centroid) / 2) / tan(angle[1]/2)

		# Find vector to the CoR
		v2rcen <- cprod(m2_centroid - m1_centroid, AoR)

		# Find possible centers of rotation
		rcen <- matrix(colMeans(rbind(m1_centroid, m2_centroid)), 4, 3, byrow=TRUE) + 
			matrix(dcen*uvector(v2rcen), 4, 3, byrow=TRUE)*matrix(rep(c(-1,1), 6), 4, 3)

	}else{
		
		# Set center
		rcen <- matrix(CoR, nrow=2, ncol=3, byrow=TRUE)
		
		# Only test positive and negative angle (because center is already determined)
		angle <- angle[c(1,3)]
	}

	# Try each CoR and angle on original m1
	nrow_rcen <- nrow(rcen)
	m1r <- array(NA, dim=c(dim(m1)[1:2], nrow_rcen))

	# Try all combinations with potential CoR
	for(i in 1:nrow_rcen) m1r[, , i] <- rotateBody(m1, rcen[i, ], AoR, angle[i])

	# Find combination with lowest difference from actual body position
	#print(apply(abs(m1r - array(m2, dim=c(dim(m1)[1:2], nrow_rcen))), 3, 'sum'))
	min_idx <- which.min(apply(abs(m1r - array(m2, dim=c(dim(m1)[1:2], nrow_rcen))), 3, 'sum'))

	# Save only that combination
	rcen <- rcen[min_idx, ]
	angle <- angle[min_idx]

	# If angle is negative, change angle and AoR sign
	if(positive.only && angle < 0){angle <- -angle; AoR <- -AoR}

	# Find error as mean distance between points
	#mean_error <- mean(sqrt(rowSums((rotateBody(m1, p=rcen, v=AoR, a=angle) - m2)^2))); print(mean_error)

	list(
		'AoR'=AoR,
		'CoR'=rcen,
		'angle'=angle
	)
}