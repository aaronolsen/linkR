angleOnCircleFromPoint <- function(circle, dist, P, point_compare=NULL){

	# GIVEN A CIRCLE, DISTANCE AND POINT P THIS FUNCTION FINDS THE ANGLE OF THE CIRCLE, SPECIFYING A
	#	POINT ON THE CIRCLE AT THE GIVEN DISTANCE TO POINT P. THERE WILL EITHER BE ZERO, TWO OR AN
	#	INFINITE NUMBER OF SOLUTIONS. FOR A TWO-POINT SOLUTION, IF point_compare IS GIVEN THEN THE
	#	POINT CLOSEST TO point_compare IS RETURNED.
	# http://mathforum.org/library/drmath/view/65138.html

	if(is.na(P[1])) stop("'P' has NA values.")
	
	# Point is on circle
	if(dist == 0){

		# Check that point is in circle plane
		#if(distPointToPlane(P, circle$N, circle$C) > 1e-7) stop("If dist is 0 then point must lie in circle plane")

		# Check that distance is equal to radius
		#if(distPointToPoint(circle$C, P) - circle$R > 1e-7) stop("If dist is 0 then distance from point to circle center must be equal to the circle radius")

		# Find angle between U-vector and vector to point
		a_vec <- avec(circle$U, P-circle$C, axis=circle$N, about.axis=TRUE)

		# Check that rotation returns point
		#circle$R*rotateBody(circle$U, circle$N, -a_vec) + circle$C
		#print(circlePoint(circle, -a_vec))

		return(-a_vec)
	}

	a <- 2*circle$R*(sum(circle$V*(circle$C-P)))
	b <- 2*circle$R*(sum(circle$U*(circle$C-P)))
	c <- (dist^2 - (circle$R^2) - sum(P^2) - sum(circle$C^2) + 2*sum(circle$C*P))

	# CHECK FOR NO SOLUTION
	if(abs(-c / sqrt(a^2 + b^2)) > 1 || abs(-a / sqrt(a^2 + b^2)) > 1) return(rep(NA, 2))

	t <- c(
		asin(-c / sqrt(a^2 + b^2)) - acos(-a / sqrt(a^2 + b^2)), 
		asin(c / sqrt(a^2 + b^2)) + acos(a / sqrt(a^2 + b^2)),
		asin(c / sqrt(a^2 + b^2)) - acos(a / sqrt(a^2 + b^2)),
		asin(-c / sqrt(a^2 + b^2)) + acos(-a / sqrt(a^2 + b^2))
		)

	# FIND DISTANCES FOR SOLUTIONS
	d <- distPointToPoint(P, circlePoint(circle, t))
	
	# REMOVE SOLUTIONS THAT DO NOT MATCH INPUT DISTANCE
	t <- t[abs(d - dist) < 1e-10]

	if(is.na(t[1])) return(rep(NA, 2))

	d <- distPointToPoint(P, circlePoint(circle, t))

	if(distPointToPoint(d[1], dist) > 1e-10 || distPointToPoint(d[2], dist) > 1e-10)
		warnings("Warning: Solution does not conform to specified distance\n")

	if(!is.null(point_compare)){

		# FIND POINTS CORRESPONDING TO OUTPUT ANGLES
		output_points <- circlePoint(circle, t)
	
		# FIND POINT CLOSEST TO PREVIOUS POINT AND CORRESPONDING ANGLE
		dist_to_point_compare <- distPointToPoint(output_points, point_compare)
		if(identical(dist_to_point_compare[1], dist_to_point_compare[2])) return(t[1])
		return(t[which(dist_to_point_compare == min(dist_to_point_compare))[1]])
	}

	return(t)
}