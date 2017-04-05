linear_cor_trajectory_error <- function(p, coor, max.dist){

	# Returns error in CoR relative to a best line fit and error in angle of line relative 
	# to translation vector
	
	# Create matrix for CoRs
	n_iter <- dim(coor)[3]
	inst_cor <- matrix(NA, nrow=n_iter-1, ncol=3)

	# Find CoRs given translation vector
	for(iter in 2:n_iter){
		
		# Decompose rotation and translation given translation vector
		rtdr <- rtd(coor[, , iter-1], coor[, , iter], tvec=p)
		inst_cor[iter-1, ] <- rtdr$cor
	}

	# Run PCA on CoRs
	N <- nrow(inst_cor)
	meanX <- apply(inst_cor, 2, mean) 
	pca <- prcomp(inst_cor)
	
	# Find axis within bounds of maximum distance
	for(i in 1:2){
		Xfit1 <- matrix(rep(meanX, each=N), ncol=3) + pca$x[, i] %*% t(pca$rotation[, i])
		t <- c(min(pca$x[, i]), max(pca$x[, i]))
		endpts <- rbind(meanX + t[1]*pca$rotation[, i], meanX + t[2]*pca$rotation[, i])
		line_len <- sqrt(sum((endpts[1,]-endpts[2,])^2))
		if(i == 1 && line_len < max.dist) break
	}
	
	#cat(i, ' ')

	# Get standard deviation from first major axis
	line_errors <- distPointToLine(inst_cor, endpts[1, ], endpts[2, ])

	# Find angle between fit line and input translation vector
	angle_error <- abs(avec(p, endpts[2, ]-endpts[1, ]))
	
	if(angle_error > pi/2) angle_error <- pi - angle_error

	# Take equal weighted average
	angle_error + mean(line_errors)
}