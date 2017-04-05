whichMaxDisperse <- function(mat, n=nrow(mat)){

	## Function finds points that are maximally dispersed among point cloud
	
	# Number of points
	nrow_mat <- nrow(mat)

	if(nrow_mat <= 2) return(1:nrow_mat)
	if(nrow_mat < n) n <- nrow_mat

	# Get ranges
	mat_range <- apply(mat, 2, 'range')
	
	# Find center of point cloud
	center <- colMeans(mat_range)
	
	# Center about 0,0,0 (for vector rotation)
	mat <- mat - matrix(center, nrow(mat), ncol(mat), byrow=TRUE)

	# Get maximum dimension
	max_dist <- distPointToPoint(mat_range) / 2
	
	# PCA
	pca <- prcomp(mat)

	# Set vectors in each orthonormal direction
	vecs <- matrix(c(0,0,0), 6, 3, byrow=TRUE) + rbind(
		-max_dist*pca$rotation[, 1],
		max_dist*pca$rotation[, 1],
		-max_dist*pca$rotation[, 2], 
		max_dist*pca$rotation[, 2],
		-max_dist*pca$rotation[, 3],
		max_dist*pca$rotation[, 3]
	)

	# Add center
	vecs <- rbind(vecs, c(0,0,0))

	# New matrix whose values will turn NA when added to idx
	mat_NA <- mat

	# Find closest point to each extreme point, without resampling
	idx <- c()
	i <- 1
	while(i <= 7 && i <= nrow_mat){

		# Find distance from extreme to every point
		dppt <- distPointToPoint(mat_NA, vecs[i, ])
		
		# Find closest point
		which_min <- which.min(dppt)
		
		idx[i] <- which_min
		mat_NA[which_min, ] <- NA
		i <- i + 1
	}
	
	# Return 7 points if specified
	if(n == 7) return(idx)

	# Rotate vectors to sample other parts of space
	vecsr <- rbind(
		vecs, 
		vecs %*% tMatrixEP(rowMeans(pca$rotation[, 1:2]), 0.8), 
		vecs %*% tMatrixEP(rowMeans(pca$rotation[, 2:3]), 0.8), 
		vecs %*% tMatrixEP(rowMeans(pca$rotation[, 2:3]), 1.3),
		vecs %*% tMatrixEP(rowMeans(pca$rotation[, 1:2]), 2.3)
	)
	
	nrow_vecs <- nrow(vecsr)
	
	# 
	i <- 8
	while(i <= n && i <= nrow_mat){
		
		# Find distance from extreme to every point
		dppt <- distPointToPoint(mat_NA, vecsr[((i-2) %% nrow_vecs) + 1, ])
		
		# Find closest point
		which_min <- which.min(dppt)
		
		idx[i] <- which_min
		mat_NA[which_min, ] <- NA
		i <- i + 1
	}

	idx
}
