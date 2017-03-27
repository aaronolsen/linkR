procAlign <- function(coor){

	## Procrustes alignment of coordinate array across 3rd dimension of array

	# Save coordinates
	x <- coor
	
	# Get dimensions
	k <- dim(x)[1]
	m <- dim(x)[2]
	n <- dim(x)[3]

	all_common <- rowSums(!apply(x[, 1, ], 2, is.na)) == n
	#print(names(all_common)[all_common])

	# Center all shapes by centroid
	for(i in 1:n) x[,, i] <- x[,, i] - matrix(apply(x[all_common,, i], 2, mean, na.rm=TRUE), k, m, byrow=TRUE)
	
	# Get centroid size of each shape
	Csize <- setNames(centroidSize(x[all_common, , ]), dimnames(x)[[3]])

	# Scale to unit centroid size
	for(i in 1:n) x[,, i] <- x[,, i] / matrix(sqrt(sum(x[all_common,, i]^2, na.rm=TRUE)), k, m)

	# Align all configurations to the first configuration using svd
	for(i in 2:n){

		# All common non-NA points
		common <- is.na(x[, 1, 1])+is.na(x[, 1, i]) == 0

		# SVD of cross-covariance matrix
		svd <- svd(t(x[common,, 1]) %*% x[common,, i])

		# Correction to ensure a right-handed coordinate system
		S <- diag(3)
		S[3,3] <- sign(det(svd$v %*% t(svd$u)))

		# Rotate configuration
		x[,, i] <- x[,, i] %*% (svd$v %*% S %*% t(svd$u))
	}

	# Find mean configuration
	mean_config <- t(apply(x, 1, function(x) apply(x, 1, mean)))

	# Scale to unit centroid size
	mean_config <- mean_config / matrix(sqrt(sum(mean_config^2, na.rm=TRUE)), k, m)

	# Find sum of squared differences between target and reference configuration
	#SSEs <- rep(NA, length(n))
	#for(i in 1:n) SSEs[i] <- sum((x[,, i] - mean_config)^2, na.rm=TRUE)
	#print(round(mean(SSEs), 5))

	# Find error
	means_SSE <- sum((mean_config - x[,, 1])^2, na.rm=TRUE)

	# Align all configurations to mean configuration until difference from previous configuration
	# changes less than 1e-7 or after 5 iterations
	j <- 0
	while(means_SSE > 1e-7 && j < 5) {

		# Find mean configuration
		mean_config1 <- t(apply(x, 1, function(x) apply(x, 1, mean)))

		# Scale to unit centroid size
		mean_config1 <- mean_config1 / matrix(sqrt(sum(mean_config1^2, na.rm=TRUE)), k, m)

		# Align each time point to mean configuration
		for(i in 1:n){
			common <- is.na(mean_config1[, 1])+is.na(x[,1, i]) == 0
			svd <- svd(t(mean_config1[common, ]) %*% x[common,, i])
			S <- diag(3)
			S[3,3] <- sign(det(svd$v %*% t(svd$u)))
			x[,, i] <- x[,, i] %*% (svd$v %*% S %*% t(svd$u))
			SSE <- sum((x[,, i] - x[,, 1])^2, na.rm=TRUE)
		}

		# Find new mean configuration
		mean_config2 <- t(apply(x, 1, function(x) apply(x, 1, mean)))
		
		# Scale to unit centroid size
		mean_config2 <- mean_config2 / matrix(sqrt(sum(mean_config2^2, na.rm=TRUE)), k, m)

		# Find difference between previous and new configuration
		means_SSE <- sum((mean_config2 - mean_config1)^2, na.rm=TRUE)
		j <- j + 1
	}
	
	# Perfect overlap
	if(j == 0) mean_config2 <- mean_config

	# Find sum of squared differences between target and reference configuration
	# This will be lower than previous mean SSEs
	#SSEs <- rep(NA, length(n))
	#for(i in 1:n) SSEs[i] <- sum((x[,, i] - mean_config2)^2, na.rm=TRUE)
	#print(round(mean(SSEs), 5))

	return(list(
		'coor'=x, 
		'mean'=mean_config2, 
		'mean.scaled'=mean_config2*mean(Csize), 
		'Csize'=Csize, 
		'common'=all_common)
	)
}