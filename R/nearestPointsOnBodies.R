nearestPointsOnBodies <- function(m1, m2){

	## Finds nearest point between two bodies

	# Find pairwise distances
	pwd <- matrix(NA, nrow=nrow(m1), ncol=nrow(m2), dimnames=list(rownames(m1), rownames(m2)))
	
	# Fill matrix
	for(i in 1:nrow(m1)) pwd[i, ] <- distPointToPoint(m1[i, ], m2)
	
	# Find minimum index (as vector)
	which_min <- which.min(pwd)

	# Find corresponding matrix indices
	idx_vec1 <- rep(1:nrow(m1), nrow(m2))
	idx_vec2 <- c(matrix(rep(1:nrow(m2), nrow(m1)), nrow(m1), nrow(m2),byrow=TRUE))
	min_idx <- c(idx_vec1[which_min], idx_vec2[which_min])

	# Return points
	rbind(m1[min_idx[1], ], m2[min_idx[2], ])
}