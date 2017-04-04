rtd <- function(m1, m2, tvec = NULL, na.rm = FALSE, positive.only = TRUE){

	# Decomposes rotation and translation. If tvec is NULL translation is along AoR. If 
	# tvec is specified, translation is parallel to tvec

	# Find instantaneous axis of rotation
	inst_rotate1 <- instRotate(m1, m2, na.rm = na.rm, positive.only = positive.only, CoR = c(0,0,0))

	# Find body centroids
	centroid_m1 <- colMeans(m1, na.rm=na.rm)
	centroid_m2 <- colMeans(m2, na.rm=na.rm)

	# Find vector from m1 to plane of m2 centroid
	if(is.null(tvec)){

		# Find minimum length translation vector (from m1 centroid to m2 centroid plane)
		tvec <- pointPlaneProj(centroid_m1, centroid_m2, inst_rotate1$AoR) - centroid_m1
		
	}else{

		# Find vector parallel to tvec to rotation plane (intersection of line and plane)
		tvec <- intersectPlaneLine(centroid_m2, inst_rotate1$AoR, centroid_m1, centroid_m1+tvec) - centroid_m1
	}

	# Translate m1 into m2 plane
	m1t <- m1 + matrix(tvec, nrow(m1), 3, byrow=TRUE)
	
	# Find CoR
	inst_rotate2 <- instRotate(m1t, m2, AoR=inst_rotate1$AoR, na.rm = na.rm, positive.only = positive.only)

	## Test that combination of rotation and translation gives 0 error
	# Translation first
	#m1r <- rotateBody(m1t, p=inst_rotate2$CoR, v=inst_rotate2$AoR, a=inst_rotate2$angle)
	#mean_error <- mean(sqrt(rowSums((m1r - m2)^2))); print(mean_error)

	# Rotation first - CoR must be translated
	#m1r <- rotateBody(m1, p=inst_rotate2$CoR - tvec, v=inst_rotate2$AoR, a=inst_rotate2$angle) + matrix(tvec, nrow(m1), 3, byrow=TRUE)
	#mean_error <- mean(sqrt(rowSums((m1r - m2)^2))); print(mean_error)

	rlist <- list(
		'aor'=inst_rotate1$AoR,
		'cor'=inst_rotate2$CoR,
		'angle'=inst_rotate2$angle,
		'tvec'=tvec
	)
}