immobilize <- function(coor, ref){

	## Fixes motion of coor relative to pts
	
	# Get number of iterations
	n_iter <- dim(coor)[3]
	
	# Each iteration
	for(iter in 1:n_iter){

		# Find translation and rotation to align coor to pts at point 1
		coor[, , iter] <- findBestAlignment(ref, coor[rownames(ref), , iter], coor[, , iter])$mc
	}

	coor
}