estimRotate <- function(coor, coor.align = NULL, max.iter.use = 20, max.pair.use = 20, unique.pairs = FALSE, wt = 'angle'){

	## Estimates center of rotation (CoR) and axis of rotation (AoR) for coordinates over 
	## a series iterations

	# Align coor by centroid, if not input
	if(is.null(coor.align)) coor.align <- alignByCentroid(coor)

	# Set coordinates to use for initial CoR and AoR estimate, if number of iterations exceeds max.iter, choose evenly spaced subset
	if(dim(coor)[3] > max.iter.use){
		coor_init <- coor[, , seq(1, dim(coor)[3], length=max.iter.use)]
		coor_align_init <- coor.align[, , seq(1, dim(coor)[3], length=max.iter.use)]
	}else{
		coor_init <- coor
		coor_align_init <- coor.align
	}

	# Find mean differences in position of centroid aligned coordinates among iterations
	dist_mat <- bodyDistMat(coor_align_init)

	# Rank pairs by distance
	dist_mat <- dist_mat[order(dist_mat[, 'dist'], decreasing=TRUE), ]

	# Add column to track use
	dist_mat <- cbind(dist_mat, rep(0, nrow(dist_mat)))

	# Create matrices for CoR and AoR
	n_iter <- nrow(dist_mat)
	cor_mat <- matrix(NA, n_iter, 3)
	aor_mat <- matrix(NA, n_iter, 3)
	angles <- rep(NA, n_iter)
	avectors <- rep(NA, n_iter)

	# Find instantaneous CoR and AoR, starting with most different and not repeating a position
	for(i in 1:nrow(dist_mat)){

		# If only using unique pair sets, check whether one of pair has been used previously
		if(unique.pairs && dist_mat[i, 4] == 1) next
		if(i > max.pair.use) break

		# Find instantaneous center and axis of rotation
		inst_rot <- instRotate(coor_init[, , dist_mat[i, 'p1']], coor_init[, , dist_mat[i, 'p2']])

		# Set any rows containing first of two pairs to 1 to not repeat
		dist_mat[rowSums(dist_mat[i, 'p1'] == dist_mat[, c('p1','p2')]) > 0, 4] <- 1
		dist_mat[rowSums(dist_mat[i, 'p2'] == dist_mat[, c('p1','p2')]) > 0, 4] <- 1

		# Save CoR and AoR
		cor_mat[i, ] <- inst_rot$CoR
		aor_mat[i, ] <- inst_rot$AoR
		angles[i] <- inst_rot$angle
		avectors[i] <- avec(inst_rot$AoR, c(1,0.3,0))
	}

	# Calculate weights
	if(is.null(wt)){wts <- rep(1, n_iter)}else if(wt == 'angle'){wts <- abs(angles)}

	# Find average CoR and AoR, weighting each iteration by angle magnitude, if specified
	cor_avg <- colSums(cor_mat*wts, na.rm=TRUE) / sum(wts, na.rm=TRUE)
	aor_avg <- uvector(colSums(aor_mat*wts, na.rm=TRUE) / sum(wts, na.rm=TRUE))

	# Add final non-linear optimization


	return(list(
		CoR=cor_avg,
		AoR=aor_avg
	))

	if(FALSE){
		svg.new(file='Test.html')
		for(i in 1:dim(coor)[3]) svg.pointsC(coor[, , i], close=TRUE, cex=3)

		svg.points(cor_avg, col='purple', cex=3)
		svg.arrows(rbind(cor_avg, cor_avg + aor_avg), col='purple')

		svg.points(cor_mat, col='red', cex=2)
		for(i in 1:nrow(cor_mat)) svg.arrows(rbind(cor_mat[i,], cor_mat[i,] + 0.5*aor_mat[i,]), col='red')

		svg.points(c(0.2,0.5,0.8), col='orange', cex=3)
		svg.arrows(rbind(c(0.2,0.5,0.8), c(0.2,0.5,0.8)+2*uvector(c(1,0.3,0))), col='orange')

		svg.close()
	}
}