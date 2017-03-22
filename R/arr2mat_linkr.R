arr2mat_linkr <- function(arr){

	# Get dimensions
	dim_arr <- dim(arr)

	# Dimnames
	dimnames_arr <- dimnames(arr)

	# Repeate landmark names
	lm_repeat <- c(t(matrix(dimnames_arr[[1]], nrow=length(dimnames_arr[[1]]), ncol=3)))

	# Set xyz
	xyz <- rep(letters[24:26], length(dimnames_arr[[1]]))

	#print(paste0(lm_repeat, '.', xyz))
	#print(list(dimnames_arr[[3]], paste0(dimnames_arr[[1]], '.', letters[24:26])))
	
	# Convert to matrix
	mat <- matrix(NA, nrow=dim_arr[3], ncol=dim_arr[1]*dim_arr[2], 
		dimnames=list(NULL, paste0(lm_repeat, '.', xyz)))
	
	# Fill matrix
	for(i in 1:nrow(mat)) mat[i, ] <- t(arr[, , i])

	mat
}
