whichMaxShapeDist <- function(shapes, max.index = dim(shapes)[3], method = c('prod.dist'), min.per.var = 1e-3){

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

	# Create vector for distance index
	dist_index <- rep(NA, max.index)

	# Create modifiable scores matrix
	scores_mat <- scores[, cols_use]

	if(method[1] == 'prod.dist'){

		# Find center
		center <- colMeans(scores_mat)

		# Start with point furthest from center
		dist_index[1] <- which.max(distPointToPoint(colMeans(scores_mat), scores_mat))

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

		if(FALSE){
			print(dist_index)
			print(scores[dist_index, 1:3])
			cols <- rainbow(n=length(dist_index), start=0, end=5/6)

			# Find center of score space
			center <- colMeans(scores[1:2, cols_use])
	
			svg.new(file='scores.html')
			svg.points(scores[, 1:3], opacity.stroke=0.05, opacity.fill=0.05)
			svg.points(scores[dist_index, 1:3], cex=c(1:max.index)+1, col.stroke=cols, col.fill='none')
			svg.frame(scores[, 1:3])
			svg.close()
		}
	}
	
	dist_index
}
