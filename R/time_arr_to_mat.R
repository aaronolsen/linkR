time_arr_to_mat <- function(arr, times){

	# Convert coordinate array to matrix
	mat <- arr2mat_linkr(arr)
	
	# At time column
	mat <- cbind(mat, times)

	# Add time column label
	colnames(mat) <- c(colnames(mat)[1:(ncol(mat)-1)], 'time')

	mat
}