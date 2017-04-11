tmarr2mat <- function(arr){

	## Converts array of transformation matrices to single matrix

	# Set add-on tags
	add_end <- c('R11','R12','R13','01','R21','R22','R23','02','R31','R32','R33','03','TX','TY','TY','1')

	# Repeat body names
	body_names_rep <- c(t(matrix(dimnames(arr)[[3]], nrow=dim(arr)[3], ncol=length(add_end))))
	
	# Repeat add-on tag to body names
	add_end_rep <- c(t(matrix(paste0('_', add_end), nrow=dim(arr)[3], ncol=length(add_end), byrow=TRUE)))

	# Combine to create column names
	col_names <- paste0(body_names_rep, add_end_rep)
	
	# Create empty matrix
	tmat <- matrix(NA, nrow=dim(arr)[4], ncol=dim(arr)[1]*dim(arr)[2]*dim(arr)[3], dimnames=list(NULL, col_names))

	# Fill matrix
	for(iter in 1:dim(arr)[4]) tmat[iter, ] <- c(arr[, , , iter])

	tmat 
}
