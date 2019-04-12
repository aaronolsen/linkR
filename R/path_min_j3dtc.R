path_min_j3dtc <- function(p, params){

	# Create minimization function to find transformation at J1 (U joint) that minimizes 
	#	J3 (S joint) distance from circle at J2 (R joint)

	# Transform points
	tmat1 <- tmat2 <- tmat3 <- diag(4)
	tmat1[1:3, 4] <- params[['U_cor']]
	for(i in 1:2){
		RM <- tMatrixEP(params[['U_axes']][i, ], -p[i])
		tmat2[1:3, 1:3] <- RM %*% tmat2[1:3, 1:3]
		if(i == 1) params[['U_axes']][2, ] <- RM %*% params[['U_axes']][2, ]
	}
	tmat3[1:3, 4] <- -params[['U_cor']]
	rtmat <- tmat1 %*% tmat2 %*% tmat3

	# Return transformation if optimal parameters are already known
	if(params[['return.tmat']]) return(rtmat)

	# Get new R CoR and axis
	R_mat_new <- applyTransform(params[['R_mat']], tmat=rtmat)

	# Return J2 to resolve toggle
	if(params[['return.J2']]) return(R_mat_new['R_cor',])

	# Define circle
	circle <- defineCircle(center=R_mat_new['R_cor',], nvector=R_mat_new['R_axis_pt',]-R_mat_new['R_cor',], 
		point_on_radius=R_mat_new['R_pt',], redefine_center=TRUE)

	# Find distance from point to circle defined by R joint center, axis, and radius
	abs(distPointToCircle(circle=circle, p=params[['S_cor_S']]))
}
