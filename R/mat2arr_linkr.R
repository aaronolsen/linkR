mat2arr_linkr <- function(mat, pattern = '[.](x|y|z)$', ignore.case = TRUE){

	# Create array
	arr <- array(NA, dim=c(ncol(mat)/3,3,nrow(mat)), 
		dimnames=list(unique(gsub(pattern, '', colnames(mat), ignore.case=ignore.case)), NULL, NULL))

	# Question marks at end of marker names become '.'
	dimnames(arr)[[1]] <- gsub('[.]$', '', dimnames(arr)[[1]])

	# Fill array
	for(i in 1:dim(arr)[3]) arr[, , i] <- matrix(mat[i, ], nrow=ncol(mat)/3, ncol=3, byrow=TRUE)
	
	arr
}
