readCoordinates <- function(file, period.to.space = TRUE, row.names = 1){

	# Read file
	mat <- as.matrix(read.csv(file=file, row.names=row.names))

	# If name in columns, convert to array
	if(nchar(colnames(mat))[1] > 2){
		
		# Get names
		sp_names <- gsub('[.](x|y|z)', '', colnames(mat))
		
		# Replace period with space
		if(period.to.space) sp_names <- gsub('[.]', ' ', sp_names)
		
		# Get unique names
		unique_sp_names <- unique(sp_names)

		# Create array
		arr <- array(NA, dim=c(nrow(mat), 3, length(unique_sp_names)), dimnames=list(rownames(mat), c('x','y','z'), unique_sp_names))
		
		# Fill array
		for(unique_sp_name in unique_sp_names) arr[, , unique_sp_name] <- mat[, unique_sp_name == sp_names]

		# Return array		
		return(arr)
	}

	mat
}