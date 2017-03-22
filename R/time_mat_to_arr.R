time_mat_to_arr <- function(mat){

	# Get times
	times <- mat[, 'time']

	# Convert mat to array
	arr <- mat2arr_linkr(mat[, !colnames(mat) %in% c('time')])
	
	# Remove gape landmarks
	arr <- arr[!dimnames(arr)[[1]] %in% c('gape.1', 'gape.2'), , ]
	
	dimnames(arr)[[2]] <- c('X', 'Y', 'Z')
	
	list(
		'arr'=arr,
		'times'=times
	)
}