bodyDistMat <- function(coor){

	# Get number of iterations/positions
	n_iter <- dim(coor)[3]

	# Create matrix of pairs and associated distance
	mat <- matrix(NA, (n_iter^2-n_iter)/2, 3, dimnames=list(NULL, c('p1', 'p2', 'dist')))

	# Fill matrix with distance among points between all pairs of positions
	n <- 1
	for(i in 1:n_iter){
		for(j in 1:n_iter){

			if(i >= j) next
			
			# Find mean distance among points
			mat[n, ] <- c(i, j, mean(rowSums((coor[, , i] - coor[, , j])^2)))
			n <- n + 1
		}
	}
	
	mat
}