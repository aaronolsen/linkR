path_min_j2ptp_u_j45ptp <- function(p, params){

	# Create minimization function to find transformation at J3 (U joint) that minimizes 
	#	J2 distance from plane and L45 distance from initial

	# Transform points
	tmat1 <- tmat2 <- tmat3 <- diag(4)
	tmat1[1:3, 4] <- params[['CoR']]
	for(i in 1:2){
		RM <- tMatrixEP(params[['U_axes']][i, ], -p[i])
		tmat2[1:3, 1:3] <- RM %*% tmat2[1:3, 1:3]
		if(i == 1) params[['U_axes']][2, ] <- RM %*% params[['U_axes']][2, ]
	}
	tmat3[1:3, 4] <- -params[['CoR']]
	rtmat <- tmat1 %*% tmat2 %*% tmat3
	
	joints_new <- applyTransform(params[['joints']], tmat=rtmat)
	
	if(params[['return.tmat']]) return(rtmat)

	# Find error
	abs(distPointToPlane(p=joints_new[1,], n=params[['J2_plane_n']], q=params[['J2_plane_p']])) + 
		abs(distPointToPoint(joints_new[2,], params[['J4']]) - params[['J45_length']])
}
