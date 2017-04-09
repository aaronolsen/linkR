readXYZ <- function(file, source='xmalab'){

	if(grepl('[.]csv$', file, ignore.case=TRUE)){

		#	     Frame ljaw_ant_X ljaw_ant_Y ljaw_ant_Z marker002_X
		#	[1,]     1   14.92356   6.905158   1.504102    14.95726
		#	[2,]     2   14.93759   6.914268   1.507317    14.96086
		#	[3,]     3   14.95628   6.924024   1.516012    14.97015

		# Read in 3D marker coordinates
		read_csv <- as.matrix(read.csv(file=file))

		# Remove columns with all NA values
		read_csv <- read_csv[, colSums(!is.na(read_csv)) > 0]

		# Remove first column if frame number
		if(colnames(read_csv)[1] == 'Frame') read_csv <- read_csv[, 2:ncol(read_csv)]

		# Convert XYZ matrix to array
		arr <- mat2arr(read_csv, pattern='(_x|_y|_z)$')
	}

	# Remove filter frequency from rownames, if present
	dimnames(arr)[[1]] <- gsub('_[0-9]+Hz', '', dimnames(arr)[[1]])
	
	# Sort markers alphabetically by name
	arr <- arr[sort(dimnames(arr)[[1]]), , ]

	arr
}
