fitBox <- function(X){

	# Source: http://r.789695.n4.nabble.com/Fit-a-3-Dimensional-Line-to-Data-Points-td863596.html

	#if(ncol(X) == 2) X <- cbind(X, rep(0, nrow(X)))

	meanX <- apply(X, 2, mean) 
	pca <- prcomp(X)

	v <- matrix(NA, nrow=3, ncol=3)
	endpts <- array(NA, dim=c(2,3,3))
	side_len <- rep(NA, 3)

	for(i in 1:3){

		t <- range(pca$x[, i])

		# Get end points along axis
		endpts[1,,i] <- meanX + t[1]*pca$rotation[, i]
		endpts[2,,i] <- meanX + t[2]*pca$rotation[, i]

		# Get vector corresponding to axis
		v[i, ] <- uvector(endpts[2,,i]-endpts[1,,i])
		
		# Get half of length
		side_len[i] <- distPointToPoint(endpts[1,,i], endpts[2,,i])
	}

	half_side_len <- side_len / 2
	corners_center <- colMeans(rbind(endpts[,,1], endpts[,,2], endpts[,,3]))
	corners <- matrix(NA, 8, 3, byrow=TRUE)
	corners[1,] <- corners_center + half_side_len[1]*v[1,] + half_side_len[2]*v[2,] + half_side_len[3]*v[3,]
	corners[2,] <- corners_center - half_side_len[1]*v[1,] + half_side_len[2]*v[2,] + half_side_len[3]*v[3,]
	corners[3,] <- corners_center - half_side_len[1]*v[1,] - half_side_len[2]*v[2,] + half_side_len[3]*v[3,]
	corners[4,] <- corners_center + half_side_len[1]*v[1,] - half_side_len[2]*v[2,] + half_side_len[3]*v[3,]
	corners[5,] <- corners_center + half_side_len[1]*v[1,] + half_side_len[2]*v[2,] - half_side_len[3]*v[3,]
	corners[6,] <- corners_center - half_side_len[1]*v[1,] + half_side_len[2]*v[2,] - half_side_len[3]*v[3,]
	corners[7,] <- corners_center - half_side_len[1]*v[1,] - half_side_len[2]*v[2,] - half_side_len[3]*v[3,]
	corners[8,] <- corners_center + half_side_len[1]*v[1,] - half_side_len[2]*v[2,] - half_side_len[3]*v[3,]
	
	list(
		'v'=v,
		'center'=corners_center,
		'halflen'=side_len/2,
		'len'=side_len,
		'endpts'=endpts,
		'corners'=corners
	)
}