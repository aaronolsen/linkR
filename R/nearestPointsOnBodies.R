nearestPointsOnBodies <- function(m1, m2){

	## Finds nearest point between two bodies
	
	# Find pairwise distances
	pwd <- matrix(NA, nrow=nrow(m1), ncol=nrow(m2), dimnames=list(rownames(m1), rownames(m2)))
	
	# Fill matrix
	for(i in 1:nrow(m1)) pwd[i, ] <- distPointToPoint(m1[i, ], m2)

	# Find minimum index (as vector)
	which_min <- which.min(pwd)

	# Find corresponding matrix indices
	min_idx <- c(which_min %% nrow(m1), floor(which_min / nrow(m1))+1)

	# Return points
	rbind(m1[min_idx[1], ], m2[min_idx[2], ])
}