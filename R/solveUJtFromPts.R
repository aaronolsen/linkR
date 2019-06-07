solveUJtFromPts <- function(center, axes, p1, p2, tp1 = NULL, tp2 = NULL){

	## 2019-06-04
	## For a U-joint, given a position of a point on a sphere (with the U-joint center as 
	## 	the sphere center) there is not a unique pose for that point on the sphere. There 
	##	will be at most (I think) four poses that will place the point on the specified 
	##	target. A second toggle point can be used to select one of these four transformations.
	##	There will only be one transformation that satisfies a second toggle point constraint.
	##	The four transformations can be accessed by flipping the axes and by using different 
	##	circle intersection points to calculate the rotations.


	## Finds rotations about axes of U joint to transform p1 to p2
	##  A U-joint can be used to transform one point anywhere on the surface of a sphere (p1)
	##	to any other point on the surface of a sphere (p2). This transformation can also be 
	##	accomplished with just two rotations about each U-joint axis. This function uses 
	##	the intersection of circles representing the rotations about each axis to find the 
	##	U-joint component rotations
	
	# Get U-joint spherical radius for points
	u_rads <- distPointToPoint(center, rbind(p1, p2))
	
	# Check that radii are the same
	if(abs(u_rads[2]-u_rads[1]) > 1e-4) stop(paste0('Points are not the same distance from center: ', paste0(signif(u_rads, 5), collapse=', '), '.'))
	
	# 
	save_inputs <- list('center'=center, 'axes'=axes, 'p1'=p1, 'p2'=p2, 'tp1'=tp1, 'tp2'=tp2)

	# Set common transformation matrices
	tmat1 <- tmat3 <- diag(4)
	tmat1[1:3, 4] <- center
	tmat3[1:3, 4] <- -center

	# 
	angles <- list(NULL, NULL, NULL, NULL)
	tmats <- list(NULL, NULL, NULL, NULL)

	toggle_dist <- rep(NA, 4)
	all_null <- TRUE
	for(i in 1:2){
	
		#cat(i, '\n')

		# Flip axes for last 2 runs
		if(i == 2) axes <- axes[2:1,]

		# Define circle that contains second point and is orthogonal to axis1
		axis1_circle <- defineCircle(center=center, nvector=axes[1,], point_on_radius=p2)
	
		# Find circle formed by axis2
		axis2_circle <- defineCircle(center=center, nvector=axes[2,], point_on_radius=p1)

		# Find intersection of circles - where p1 will be transformed to by axis2
		int_circles <- int_circles1 <- intersectCircles(axis1_circle, axis2_circle)

		# If no intersection, skip
		if(int_circles$type == 'non-coincident') next
		
		# Mark that there are some non-NULL
		all_null <- FALSE

		for(j in 1:2){
		
			# If only a single intersection
			if(is.null(int_circles[[j]])) break
		
			# Set sequential list index
			list_idx <- j + 2*i - 2

			#cat('\t', list_idx, '\n')

			# Get circle intersection
			p12 <- int_circles[[j]]
			
			# Get angles
			angles[[list_idx]] <- c(
				'a2'=avec(p12-center, p1-center, axis=axes[2,], about.axis=TRUE),	# Find rotation about axis2 to transform p1 to p12
				'a1'=avec(p2-center, p12-center, axis=axes[1,], about.axis=TRUE)	# Find rotation about axis1 to transform p12 to p2
			)
		
			# Set rotation transformation matrix
			tmat2 <- diag(4)
			tmat2[1:3, 1:3] <- tMatrixEP(axes[1,], -angles[[list_idx]][['a1']]) %*% tMatrixEP(axes[2,], -angles[[list_idx]][['a2']])

			# Set final transformation
			tmats[[list_idx]] <- tmat1 %*% tmat2 %*% tmat3

			if(is.null(tp1)){
	
				# If no toggle point just return first result - not recommended!
				tmat <- tmats[[list_idx]]
				a1 <- angles[[list_idx]][['a1']]
				a2 <- angles[[list_idx]][['a2']]
				break
			}
		}

		if(is.null(tp1)) break
	}

	if(!is.null(tp1)){

		# For each new position of p1
		for(i in 1:4){

			if(is.null(tmats[[i]])) next

			# Transform toggle point
			tp1_t <- applyTransform(tp1, tmats[[i]])

			# Measure distance
			toggle_dist[i] <- distPointToPoint(tp1_t, tp2)
		}

		#cat('\n\n')
		#print(toggle_dist)

		# Use a second (toggle) point to select the transformation that places the second point closest to the input position
		#print(toggle_dist)
		which_min <- which.min(toggle_dist)
		tmat <- tmats[[which_min]]
		a1 <- angles[[which_min]][['a1']]
		a2 <- angles[[which_min]][['a2']]
	}
	
	#print(angles)
	#print(toggle_dist)

	# If there was no intersection (should always be some intersections unless input parameters are wrong)
	if(all_null){
		cat('\nInputs to function:\n')
		print(save_inputs)
		stop('Circles not coincident.')
	}

	return(list('a1'=a1, 'a2'=a2, 'tmat'=tmat))
}