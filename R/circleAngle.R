circleAngle <- function(circle, p){

	# Math from: https://www.myphysicslab.com/springs/trig-identity-en.html

	# Finds angle on circle given point
	if(is.matrix(p)){
		ret.val <- rep(NA, nrow(p))
		for(i in 1:nrow(p)) ret.val[i] <- circleAngle(circle, p[i,])
		return(ret.val)
	}
	
	# Could add check that point is on circle - will return error at end if not because no angle will be found
	#cat(paste0('Distance point to circle: ', signif(distPointToCircle(circle, p), 3), '\n'))
	
	# For each dimension
	p2row <- matrix(p, 2, 3, byrow=TRUE)
	for(i in 1:3){

		## Seems to work fine without needing to try multiple angles per dimension
		# Calculate angle
		#angles <- c(
		#	-atan2(circle$U[i], circle$V[i]) + asin( ((1/circle$R)*(p[i] - circle$C[i])) / sqrt(circle$U[i]^2 + circle$V[i]^2)),
		#	-atan(circle$U[i] / circle$V[i]) + asin( ((1/circle$R)*(p[i] - circle$C[i])) / sqrt(circle$U[i]^2 + circle$V[i]^2))
		#)
	
		# Find point corresponding to angle
		#try_angles <- circlePoint(circle, T=angles)
	
		# Compare point to input
		#dist_pts <- rowSums(abs(p2row - try_angles))

		# If close enough to point, return angle
		#if(dist_pts[1] < 1e-12) return(angles[1])
		#if(dist_pts[2] < 1e-12) return(angles[2])

		# Calculate angle
		angle <- -atan2(circle$U[i], circle$V[i]) + asin( ((1/circle$R)*(p[i] - circle$C[i])) / sqrt(circle$U[i]^2 + circle$V[i]^2))

		# Find point corresponding to angle
		try_angle <- circlePoint(circle, T=angle)
		
		# If close enough to point, return angle
		if(sum(abs(p2row - try_angle) < 1e-12)) return(angle)
	}

	stop("No angle on circle found that returns input point within specified precision.")
}