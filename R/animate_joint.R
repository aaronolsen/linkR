animate_joint <- function(type, cons, param, coor, lcs = TRUE, check.joint.cons = TRUE, 
	print.progress = FALSE){

	## In-progress function for simulating single joint kinematics. Will eventually be 
	## 	folded into animateLinkage.

	if(print.progress) cat('animate_joint\n')

	# Standardize input parameter to matrix, each row is an iteration
	if(is.vector(param)) param <- matrix(param, nrow=length(param), ncol=1)

	# Get number of iterations
	n_iter <- nrow(param)

	# Standardize contraint into matrix, each row is an iteration
	if(is.vector(cons)) cons <- matrix(cons, nrow=n_iter, ncol=length(cons), byrow=TRUE)

	# Check joint constraints
	if(check.joint.cons){
		if(type %in% c('P')) for(iter in 1:n_iter) if(abs(avec(cons[iter, 1:3], cons[iter, 4:6]) - pi/2) > 1e-7) stop("P-joint axes are not perpendicular")
		if(type %in% c('U')) for(iter in 1:n_iter) if(abs(avec(cons[iter, 4:6], cons[iter, 7:9]) - pi/2) > 1e-7) stop("U-joint axes are not perpendicular")
		if(type %in% c('S')){
			for(iter in 1:n_iter){
				if(abs(avec(cons[iter, 4:6], cons[iter, 7:9]) - pi/2) > 1e-7) stop("S-joint axes are not perpendicular")
				if(abs(avec(cons[iter, 4:6], cons[iter, 10:12]) - pi/2) > 1e-7) stop("S-joint axes are not perpendicular")
				if(abs(avec(cons[iter, 7:9], cons[iter, 10:12]) - pi/2) > 1e-7) stop("S-joint axes are not perpendicular")
			}
		}
		if(type %in% c('T')){
			for(iter in 1:n_iter){
				if(abs(avec(cons[iter, 1:3], cons[iter, 4:6]) - pi/2) > 1e-7) stop("T-joint axes are not perpendicular")
				if(abs(avec(cons[iter, 1:3], cons[iter, 7:9]) - pi/2) > 1e-7) stop("T-joint axes are not perpendicular")
				if(abs(avec(cons[iter, 4:6], cons[iter, 7:9]) - pi/2) > 1e-7) stop("T-joint axes are not perpendicular")
			}
		}
	}

	# Standardize coordinates into matrix
	if(is.vector(coor)) coor <- matrix(coor, nrow=1, ncol=3)
	
	# Create local coordinate system
	if(length(lcs) == 1){ # lcs == TRUE
		center <- colMeans(coor, na.rm=TRUE)
		lcs <- rbind(center, center+c(1,0,0), center+c(0,1,0), center+c(0,0,1))
	}
	
	# Create single array for all points
	if(!is.null(lcs)){pmat <- rbind(coor, lcs)}else{pmat <- coor}
	tarr <- array(diag(4), dim=c(4,4,n_iter))
	translations <- matrix(0, n_iter, 3)

	# Get rotational and translational DoFs
	dof <- joint_types()$dof[type, ]

	if(print.progress) cat('\tCreating transformation matrices\n')

	# Animate body
	for(iter in 1:n_iter){

		# Set null transformation matrix
		tmat <- diag(4)
		
		# Set rotation dof skip
		if(dof[1] == 0){rdof_skip <- 0}else{rdof_skip <- (dof[1]+1)*3}

		# Apply translations
		if(dof[2] > 0) for(i in 1:dof[2]){
			tmat[1:3, 4] <- tmat[1:3, 4] + param[iter, dof[1]+i]*uvector(cons[iter, (rdof_skip+i*3+1-3):(rdof_skip+i*3)])
		}
		
		# Save translations
		translations[iter, ] <- tmat[1:3, 4]
		
		# Apply rotations
		if(dof[1] > 0){
			
			# Move points back
			tmat <- tmat %*% cbind(rbind(diag(3), rep(0,3)), c(cons[iter, 1:3], 1))

			# Apply rotations
			# 	The negative sign is needed to ensure rotation matrix follows the right-hand 
			#	rule - because the EP rotation matrix is put in a transformation 
			#	matrix that precedes the point matrix in the matrix multiplication. If the 
			#	EP rotation matrix came after the point matrix in the matrix multiplication, 
			#	the input angle would need to be positive
			for(i in dof[1]:1){
				tmat <- tmat %*% cbind(rbind(tMatrixEP(v=cons[iter, (i*3+1):(i*3+3)], a=-param[iter, i]), rep(0,3)), c(0,0,0,1))
			}

			# Make rotation point origin
			tmat <- tmat %*% cbind(rbind(diag(3), rep(0,3)), c(-cons[iter, 1:3], 1))
		}
		
		tarr[, , iter] <- tmat
	}

	if(print.progress) cat('\tApplying transformation matrices\n')

	# Apply transformations
	tcoor <- applyTransform(pmat, tarr)

	# Apply translations to constraint vector
	if(dof[1] > 0 && dof[2] > 0) cons[, 1:3] <- cons[, 1:3] + translations

	# Check that joint constraints hold
	if(check.joint.cons && dim(tcoor)[3] > 1){
	
		if(dof[2] <= 2){

			if(print.progress) cat('\tChecking joint constraints\n')

			# Get body size
			coor_size <- centroidSize(coor)

			# Create distance matrix
			dist_mat <- matrix(NA, nrow=dim(tcoor)[1], ncol=dim(tcoor)[3], dimnames=list(dimnames(tcoor)[[1]], NULL))

			# Check rotational constraints
			if(dof[1] > 0 && dof[2] <= 2){
			
				if(print.progress) cat('\t\tChecking rotational constraints\n')

				# Find distance of each point from the center of rotation at each iteration
				for(iter in 1:dim(tcoor)[3]) dist_mat[, iter] <- distPointToPoint(tcoor[, , iter], cons[iter, 1:3])

				# Check that distances are constant over iterations
				if(sum(apply(dist_mat, 1, 'sd') / coor_size > 1e-8) > 0) warning("Point deviates from rotational constraint.")
			}

			if(dof[1] == 0 && dof[2] > 0){
		
				if(print.progress) cat('\t\tChecking translational constraints\n')

				# Fill matrix
				if(dof[2] == 1){
					for(iter in 1:dim(tcoor)[3]) dist_mat[, iter] <- distPointToLine(tcoor[, , iter], c(0,0,0), cons[iter, 1:3])
				}else if(dof[2] == 2){
					for(iter in 1:dim(tcoor)[3]) dist_mat[, iter] <- distPointToPlane(tcoor[, , iter], cprod(cons[iter, 1:3], cons[iter, 4:6]), tcoor[1, , iter])
				}

				# Check that distances are constant over iterations
				if(sum(apply(dist_mat, 1, 'sd') / coor_size > 1e-8) > 0) warning("Point deviates from translational constraint.")
			}
		}
	}

	# Get transformed coordinates and lcs, if specified
	if(!is.null(lcs)){
		rcoor <- tcoor[1:(dim(tcoor)[1]-4), , ]
		rlcs <- tcoor[(dim(tcoor)[1]-3):dim(tcoor)[1], , ]
	}else{
		rcoor <- tcoor
		rlcs <- NULL
	}

	list(
		'coor'=rcoor,
		'lcs'=rlcs,
		'cons'=cons,
		'dof'=dof
	)
}
