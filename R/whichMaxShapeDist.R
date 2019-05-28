whichMaxShapeDist <- function(shapes, max.index = dim(shapes)[3], method = c('disp.dist', 'prod.dist'), 
	min.per.var = 1e-3, plot.diag = NULL){

	# Make sure max.index does not exceed number of shapes
	if(max.index > dim(shapes)[3]) max.index <- dim(shapes)[3]

	# Convert array to matrix for PCA
	shapes_mat <- t(apply(shapes, 3, function(y) matrix(t(y), 1)))

	# Perform non-phylogenetic PCA
	resEig <- eigen(cov(shapes_mat))

	# Get PC scores (scores in each iteration)
	scores <- shapes_mat %*% resEig$vectors

	# Get percent variance explained along each axis
	percent.var <- (resEig$values / sum(resEig$values))*100
	
	# Check for set of identical shapes
	if(abs(resEig$values[1]) < 1e-10) return(seq(1,dim(shapes)[3],length=max.index))
	
	# Determine which columns to use
	cols_use <- (1:ncol(scores))[percent.var > min.per.var]
	
	# If just one column, add second highest axis so there are at least 2 columns
	if(length(cols_use) == 1) cols_use <- order(percent.var, decreasing=TRUE)[1:2]
	
	# Set number of columns used
	n_cols_use <- length(cols_use)
	
	# Create vector for distance index
	dist_index <- rep(NA, max.index)

	# Create modifiable scores matrix
	scores_mat <- scores[, cols_use]

	if(method[1] == 'prod.dist'){

		# Find center
		center <- colMeans(scores_mat)

		# Start with point furthest from center
		dist_index[1] <- which.max(distPointToPoint(center, scores_mat))

		# Re-set center
		center <- scores_mat[dist_index[1], ]

		# Create new vector for distances
		dists <- matrix(NA, nrow=nrow(scores_mat), ncol=max.index-1)

		n <- 2
		while(n <= max.index){

			# Find distance to previous points
			for(i in 1:(n-1)) dists[, i] <- distPointToPoint(scores_mat, scores[dist_index[i], cols_use])

			# Get row products
			if(n == 2){
				row_prods <- dists[, 1]
			}else{
				row_prods <- apply(dists[, 1:(n-1)], 1, prod)
			}

			# Get max index of row products
			dist_index[n] <- which.max(row_prods)
		
			# Set to NA so not chosen again
			scores_mat[dist_index[n], ] <- NA
		
			# Update center
			if(n > 1) center <- colMeans(scores[dist_index, cols_use], na.rm=TRUE)

			n <- n + 1
		}
		
		max_dist_pts <- NULL

	}else if(method[1] == 'disp.dist'){
	
		# Create point set matrix for maximally distributed across all axes
		max_dist_pts <- matrix(NA, nrow=max.index, ncol=n_cols_use)

		# Get number of rows
		nrows <- sqrt(max.index)
		
		# Check that max index is a 2-power number
		if(nrows - floor(nrows) != 0) stop('If method is "disp.dist" then "max.index" must be a number to the power of 2 (e.g. 4, 9, 16, 25, ...).')

		# Fill matrix
		for(i in 1:n_cols_use){

			# Get total range
			score_range <- range(scores[, i])

			# Get values in "grid" pattern
			if(i %% 2 == 0){
				max_dist_pts[,i] <- seq(score_range[1], score_range[2], length=nrows)
			}else{
				max_dist_pts[,i] <- sort(rep(seq(score_range[1], score_range[2], length=nrows), nrows))
			}
		}
		
		# Find scores closest to grid points
		for(i in 1:max.index){

			# Find distance from grid point to all points
			dists <- distPointToPoint(max_dist_pts[i,], scores_mat[, cols_use])
			
			# Take point that is closest
			dist_index[i] <- which.min(dists)

			# Set to NA so not chosen again
			scores_mat[dist_index[i], ] <- NA
		}
	}

	# Plot
	if(!is.null(plot.diag)){

		# Set number of plot rows
		n_plot_cols <- 3
		n_plot_rows <- ceiling((n_cols_use-1) / n_plot_cols)
		
		if(n_cols_use < 3) n_plot_cols <- n_cols_use
		
		pdf(plot.diag, width=n_plot_cols*3, height=n_plot_rows*2.8)

		layout(mat=matrix(1:(n_plot_cols*n_plot_rows), n_plot_rows, n_plot_cols, byrow=TRUE))
		par(mar=c(5,5,2,2))
		
		for(i in 1:(n_cols_use-1)){
			plot(scores[, i:(i+1)], xlab=paste0('PC', i, ' (', round(percent.var[i], 2), '%)'), ylab=paste0('PC', i+1, ' (', round(percent.var[i+1], 2), '%)'), col=rgb(0,0,0,0.1))
			points(scores[dist_index, i:(i+1)], col='red', cex=0.8)
			points(scores[dist_index, i:(i+1)], type='l', col=rgb(1,0,0,0.3))
			if(!is.null(max_dist_pts)) points(max_dist_pts[, i:(i+1)], col=rgb(0,1,0,0.2), cex=0.8)
		}

		dev.off()
	}
	
	dist_index
}
