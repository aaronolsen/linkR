fit_elliptic_cylinder_error <- function(p, mat){

	# Define elliptic cylinder
	ecylinder <- defineEllipticCylinder(p[1:3], p[4:6], p[7], p[8], p[9])

	# Points on central axis closest to point
	pt_ca <- pointNormalOnLine(mat, p[1:3], p[1:3] + p[4:6], checks=FALSE)

	# Distance from point to central axis
	dist_ca <- sqrt(rowSums((mat-pt_ca)^2))

	# Find distances from points to cylinder
	nrow_mat <- nrow(mat)
	distances <- rep(NA, nrow_mat)
	for(i in 1:nrow_mat){
		
		# Find angle from closest point on elliptic cylinder
		ec_ang <- avec(ecylinder$U, mat[i, ]-pt_ca[i, ], axis=ecylinder$N, about.axis=TRUE)
		
		# Find radius corresponding to angle
		ec_rad <- sqrt(sum((ecylinder$R1*cos(ec_ang)*ecylinder$U + ecylinder$R2*sin(ec_ang)*ecylinder$V)^2))
		
		# Find difference from radius
		distances[i] <- abs(ec_rad - dist_ca[i])
	}

	# Return mean distance
	mean(distances)
}