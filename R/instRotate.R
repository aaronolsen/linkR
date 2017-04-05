instRotate <- function(m1, m2, na.rm = FALSE, positive.only = TRUE, aor.only = FALSE, AoR = NULL, CoR = NULL){

	## Finds instantaneous center of rotation (CoR) and axis of rotation (AoR) between 
	## coordinates in two different orientations. Four possible solutions can be obtained 
	## by changing the orientation of the AoR and sign of the angle. The following 
	## constraints are imposed to obtain a single solution:
	##    1) Only returns positive angles
	##    2) Returns the minimum of possible positive angles
	## A single axis of rotation can describe any number and combination of rotations about
	## the same center of rotation but not more than one rotation about different centers
	## of rotation. The CoR is necessary to identify the correct sign of the rotation 
	## angle. If is known (even approximately) it can be provided to reduce run time.

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

	if(is.null(AoR)){

		# Compute eigen vectors and values
		rmat_eigen <- eigen(rmat)

		# Find AoR as the eigenvector corresponding to eigenvalue closest to 1 (truncation errors)
		AoR <- Re(rmat_eigen$vectors[, which.min(abs(Re(rmat_eigen$values) - 1))])
	}

	if(aor.only) return(list('AoR'=AoR))

	# Get rotation magnitude/angle
	acos_val <- (sum(rmat*diag(3)) - 1) / 2
	
	# Check that value in acos function does not exceed 1 (by trace amount causes error)
	if(abs(acos_val) > 1) acos_val <- sign(acos_val)

	# Find angle from acos value
	angle <- acos(acos_val)
	
	# Try both angles
	rmatch <- c(sum(abs(m1c %*% tMatrixEP(AoR, angle) - m2c)), sum(abs(m1c %*% tMatrixEP(AoR, -angle) - m2c)))

	# Keep angle with lowest/zero error
	angle <- (c(1,-1)*angle)[which.min(rmatch)]

	# Find or set center of rotation
	if(is.null(CoR)){
	
		# Find distance from mid-centroid point to CoR
		dcen <- -(distPointToPoint(m1_centroid, m2_centroid) / 2) / tan(abs(angle)/2)

		# Find vector to the CoR
		v2rcen <- cprod(m2_centroid - m1_centroid, AoR)

		# Find possible centers of rotation
		rcen <- matrix(colMeans(rbind(m1_centroid, m2_centroid)), 2, 3, byrow=TRUE) + 
			matrix(dcen*uvector(v2rcen), 2, 3, byrow=TRUE)*matrix(rep(c(-1,1), 6), 2, 3)

		# Try both potential CoRs
		rmatch <- c(sum(abs(rotateBody(m1, rcen[1, ], AoR, angle) - m2)), sum(abs(rotateBody(m1, rcen[2, ], AoR, angle) - m2)))

		# Find center with lowest difference from actual body position
		rcen <- rcen[which.min(rmatch), ]

	}else{
		
		# Set center
		rcen <- CoR
	}

	# If angle is negative, change angle and AoR sign
	if(positive.only && angle < 0){angle <- -angle; AoR <- -AoR}

	# Slide CoR along AoR to closest point on AoR from m1 centroid
	#rcen <- pointNormalOnLine(pt=m1_centroid, l1=rcen, l2=rcen + AoR)

	# Find error as mean distance between points
	#mean_error <- mean(sqrt(rowSums((rotateBody(m1, p=rcen, v=AoR, a=angle) - m2)^2))); print(mean_error)

	list(
		'AoR'=AoR,
		'CoR'=rcen,
		'angle'=angle
	)
}