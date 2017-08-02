readMotion <- function(file, landmark.names=NULL, multiple.as='array', nrows = -1){

	## Reads in matrix of coordinates over time, with or without time column, or 
	## transformation matrices. File type is detected based on whether first column name 
	## ends in R11

	# Detect format
	if(grepl('[.]csv$', file[1])){
		file_format <- 'csv'
	}else if(grepl('[.]txt$', file[1])){
		file_format <- 'txt'
	}

	# Get number of frames from first file
	if(file_format == 'txt'){
		tmta <- time_mat_to_arr(as.matrix(read.table(file[1])))
	}else{

		# Read csv
		read_matrix <- as.matrix(read.csv(file[1], nrows=nrows))

		# If transformations
		if(grepl('_R11$', colnames(read_matrix)[1])){
		
			# Check for non-transformation columns
			tm_grepl <- grepl('_(R[1-3]{2}|[0-3]{2}|1|TX|TY|TZ)', colnames(read_matrix), ignore.case=TRUE)
		
			# Get tm matrix
			tm_matrix <- read_matrix[, tm_grepl]

			# Transformation matrix
			tmat <- matrix(suppressWarnings(as.numeric(tm_matrix)), nrow(tm_matrix), ncol(tm_matrix), 
				dimnames=dimnames(tm_matrix))
			
			# Convert transformation matrix into array
			tm_arr <- tmmat2arr(tmat)

			# Set return list
			rlist <- list('tm.arr'=tm_arr)
			
			# Get non-transformation column names
			if(sum(!tm_grepl) > 0){

				# Add column as list element
				for(non_tm_colname in colnames(read_matrix)[!tm_grepl]) rlist[[non_tm_colname]] <- gsub('^[ ]*|[ ]*$', '', read_matrix[, non_tm_colname])
			}

			return(rlist)
			
		}else{
			if(colnames(read_matrix)[1] == 'X') read_matrix <- read_matrix[, 2:ncol(read_matrix)]
			
			if('time' %in% colnames(read_matrix)){
				tmta <- time_mat_to_arr(read_matrix)
			}else{
				return(mat2arr(read_matrix))
			}
		}
	}
	
	lm_array_ex <- tmta$arr

	# Set number of iterations
	num_iter <- dim(lm_array_ex)[3]
	
	# Set landmark names
	if(is.null(landmark.names)) landmark.names <- dimnames(lm_array_ex)[[1]]

	# Create array for coordinates from all strikes
	if(multiple.as == 'list' && length(file) > 1){
		marker_array <- list()
		for(ii in 1:length(file)){
			marker_array[[ii]] <- array(NA, dim=c(dim(lm_array_ex)[1:2], num_iter), dimnames=list(landmark.names, letters[24:26], NULL))
		}
	}else if(multiple.as == 'array' && length(file) > 1){

		# Get file names from paths
		file_names <- rep(NA, length(file))
		for(i in 1:length(file)){
			str_split <- strsplit(file[i], split='/')[[1]]
			file_names[i] <- gsub('[.](txt|csv)$', '', str_split[length(str_split)], ignore.case=TRUE)
		}

		# Create array
		marker_array <- array(NA, dim=c(dim(lm_array_ex)[1:2], num_iter, length(file)), dimnames=list(landmark.names, letters[24:26], NULL, file_names))

	}else{
		marker_array <- array(NA, dim=c(dim(lm_array_ex)[1:2], num_iter*length(file)), dimnames=list(landmark.names, letters[24:26], NULL))
	}

	# Read coordinates into list/array
	for(i in 1:length(file)){

		# Read coordinates
		if(file_format == 'txt'){
			tmta <- time_mat_to_arr(as.matrix(read.table(file[i])))
		}else{
			read_matrix <- as.matrix(read.csv(file[i]))
			if(colnames(read_matrix)[1] == 'X') read_matrix <- read_matrix[, 2:ncol(read_matrix)]
			tmta <- time_mat_to_arr(read_matrix)
		}

		lm_array <- tmta$arr

		# Add to array/list
		if(multiple.as == 'list' && length(file) > 1){
			marker_array[[i]][landmark.names, , ] <- lm_array[landmark.names, , ]
		}else if(multiple.as == 'array' && length(file) > 1){
			marker_array[landmark.names, , , i] <- lm_array[landmark.names, , ]
		}else{
			# Set frame indices
			frames_idx <- ((i-1)*num_iter+1):(i*num_iter)

			# Add to array
			marker_array[landmark.names, , frames_idx] <- lm_array[landmark.names, , ]
		}
	}
	
	list(
		'xyz'=marker_array, 
		'time'=tmta$times
	)
}