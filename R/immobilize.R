immobilize <- function(coor, pts){

	## Fixes motion of coor relative to pts
	
	# Get number of iterations
	n_iter <- dim(coor)[3]
	
	# Each iteration
	for(iter in 2:n_iter){

		# Find translation and rotation to align coor to pts at point 1
		coor[, , iter] <- findBestAlignment(coor[pts, , 1], coor[pts, , iter], coor[, , iter])$mc
	}

	coor
}