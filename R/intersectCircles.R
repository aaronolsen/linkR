intersectCircles <- function(circle1, circle2){
	# http://math.stackexchange.com/questions/938701/how-to-find-the-intersection-points-of-two-circles-in-3d
	# TO ADD: ONE CIRCLE WITHIN THE OTHER, NO OVERLAP IN CIRCUMFERENCE
	
	if(avec(circle1$N, circle2$N, max.pi=TRUE)){

		#stop(paste0("Currently only finds intersection of circles that are co-planar. Input circles are not coplanar; difference in angle between normal vectors: ", signif(avec(circle1$N, circle2$N), 3)))
		
		## Circles are not co-planar
		
		# Find the intersection of the two circle planes
		int_planes <- intersectPlanes(p1=circle1$C, n1=circle1$N, p2=circle2$C, n2=circle2$N)

		# Find intersection of line and circles
		int_c1_line <- intersectCircleLine(circle=circle1, l1=int_planes$l1, l2=int_planes$l2)
		
		# Check that this is not NULL
		if(is.null(int_c1_line)){
			return(list('type'='non-coincident'))
		}

		# Find intersection of line and circles
		int_c2_line <- intersectCircleLine(circle=circle2, l1=int_planes$l1, l2=int_planes$l2)
		
		# Check that this is not NULL
		if(is.null(int_c2_line)){
			return(list('type'='non-coincident'))
		}
		
		# Get mean radius to scale "zero"
		mean_rad <- mean(c(circle1$R, circle2$R))

		if(is.null(int_c1_line$p2) && is.null(int_c2_line$p2)){

			d12 <- distPointToPoint(int_c1_line$p1 - int_c2_line$p1)
			
			if(d12 < mean_rad*1e-5) return(list(int_c1_line$p1, 'type'='one'))

		}else if(!is.null(int_c1_line$p2) && !is.null(int_c2_line$p2)){

			# Two intersections for both circles
			dint <- c(distPointToPoint(int_c1_line$p1 - int_c2_line$p1),
				distPointToPoint(int_c1_line$p2 - int_c2_line$p1),
				distPointToPoint(int_c1_line$p1 - int_c2_line$p2),
				distPointToPoint(int_c1_line$p2 - int_c2_line$p2))

			rlist <- list()
			if(dint[1] < mean_rad*1e-5) rlist[[1]] <- int_c1_line$p1
			if(dint[2] < mean_rad*1e-5) rlist[[length(rlist)+1]] <- int_c1_line$p2
			if(dint[3] < mean_rad*1e-5) rlist[[length(rlist)+1]] <- int_c1_line$p1
			if(dint[4] < mean_rad*1e-5) rlist[[length(rlist)+1]] <- int_c1_line$p2
			
			if(length(rlist) == 0){
				return(list('type'='non-coincident'))
			}else if(length(rlist) == 1){
				return(list(rlist[[1]], 'type'='one'))
			}else{
				return(list(rlist[[1]], rlist[[2]], 'type'='two'))
			}

		}else{
			stop('Write handling for this condition')
		}
		
		return(NULL)

	}else{

		## Circles are co-planar
		# FIND DISTANCE BETWEEN CENTERS
		center_dist <- distPointToPoint(circle1$C, circle2$C)
	
		if(center_dist == 0){
		
			# CIRCLES PERFECTLY COINCIDENT
			if(circle1$R == circle2$R) return(list('type'='coincident'))

			# CIRCLES ARE CONCENTRIC
			if(circle1$R != circle2$R) return(list('type'='concentric'))
		}
	
		# CHECK THAT INTERSECTION IS POSSIBLE
		if(center_dist > circle1$R + circle2$R) return(list('type'='non-coincident'))

		# CHECK IF ONLY ONE SOLUTION EXISTS
		if(center_dist == circle1$R + circle2$R) return(list(circle1$C + uvector(circle2$C - circle1$C)*circle1$R, 'type'='one'))

		# FIND AREA OF TRIANGLE USING HERONS FORMULA
		p <- (circle1$R + circle2$R + center_dist) / 2
		area_sq <- p*(p - circle1$R)*(p - circle2$R)*(p - center_dist)

		# IF AREA IS NAN, MAYBE CIRCLE IS INSIDE THE OTHER (PLOT CONFIRMS ON ONE OCCASION)
		if(area_sq < 0) return(list('type'='inside'))	

		# FIND AREA
		area <- sqrt(area_sq)

		# FIND TRIANGLE HEIGHT
		h <- area / (0.5*center_dist)

		# FIND LENGTH FROM FIRST CENTER TO INTERSECTION MIDPOINT
		dist_c1m <- sqrt(circle1$R^2 - h^2)

		# PROJECT FROM FIRST CENTER TO INTERSECTION MIDPOINT
		inter_midpt <- circle1$C + uvector(circle2$C - circle1$C)*dist_c1m
	
		# DIRECTION VECTOR FROM INTERSECTION MIDPOINT TO INTERSECTIONS
		dir_vec <- uvector(circle2$C - circle1$C) %*% tMatrixEP(v=circle2$N, a=pi/2)

		# FIND TENTATIVE INTERSECTS
		intersects <- list(c(inter_midpt + dir_vec*h), c(inter_midpt + -dir_vec*h), 'type'='two')
	
		# MAKE SURE THAT INTERSECTION MIDPOINT WAS PROJECTED IN THE RIGHT DIRECTION
		if(abs(distPointToPoint(circle2$C, intersects[[1]]) - circle2$R) > 1e-8){
		
			# IF NOT, PROJECT IN OTHER DIRECTION
			inter_midpt <- circle1$C + -uvector(circle2$C - circle1$C)*dist_c1m
	
			# DIRECTION VECTOR FROM INTERSECTION MIDPOINT TO INTERSECTIONS
			dir_vec <- uvector(circle2$C - circle1$C) %*% tMatrixEP(v=circle2$N, a=pi/2)

			# FIND TENTATIVE INTERSECTS
			intersects <- list(c(inter_midpt + dir_vec*h), c(inter_midpt + -dir_vec*h), 'type'='two')
		}
	}

	intersects
}