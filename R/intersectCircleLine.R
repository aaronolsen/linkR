intersectCircleLine <- function(circle, l1, l2){

	# FIND CLOSEST POINT ON LINE FROM CIRCLE CENTER
	mid_point <- pointNormalOnLine(circle$C, l1, l2)

	## LINE ANGLE CHECK
	# IF LINE PASSES THROUGH CENTER OF CIRCLE
	if(sum(abs(mid_point - circle$C)) < 1e-8){

		# CHECK THAT LINE IS PERPENDICULAR TO CIRCLE N VECTOR
		ang_line <- abs(avec(l2-l1, circle$N))
		
		if(abs(ang_line - pi/2) > 1e-8) stop("Line input to intersectCircleLine() must be parallel to circle plane.")

	}else{

		# CHECK THAT LINE IS PARALLEL TO CROSS PRODUCT OF CIRCLE N VECTOR AND VECTOR TO LINE
		ang_line <- abs(avec(l2-l1, cprod(circle$N, mid_point-circle$C)))

		if(min(abs(ang_line), abs(ang_line-pi)) > 1e-8) stop("Line input to intersectCircleLine() must be parallel to circle plane.")
	}
	
	# CHECK THAT LINE OVERLAPS WITH CIRCLE
	dist_mid_point <- distPointToPoint(mid_point, circle$C)
	if(dist_mid_point > circle$R){
	
		return(NULL)

	}else if(dist_mid_point == circle$R){

		return(list('p1'=mid_point))

	}else{

		# GET VECTOR MAGNITUDE FROM MID POINT
		vmag <- sqrt(circle$R^2 - dist_mid_point^2)

		# GET VECTOR ORIENTATION
		l12_vec <- uvector(l1-l2)

		# GET INTERSECTION POINTS USING VECTOR
		return(list('p1'=mid_point+vmag*l12_vec, 'p2'=mid_point-vmag*l12_vec))
	}
}