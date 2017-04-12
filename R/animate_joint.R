animate_joint <- function(type, cons, param, coor, lcs = TRUE, input.ref = 'initial', 
	check.joint.cons = TRUE, print.progress = FALSE){

	## In-progress function for simulating single joint kinematics. Will eventually be 
	## folded into animateLinkage.

	if(print.progress){
		cat('animate_joint\n')
		cat(paste0('\tJoint type: ', type, '\n'))
	}

	# Standardize input parameter to matrix, each row is an iteration
	if(is.vector(param)) param <- matrix(param, nrow=length(param), ncol=1)

	n_iter <- nrow(param)+1												# Get number of iterations, add additional iteration for initial position
	dof <- joint_types()$dof[type, ]									# Get rotational and translational DoFs
	has_u <- ifelse(grepl('u', type, ignore.case=TRUE), TRUE, FALSE)	# Has U-joint component (first axis remains fixed, second axis rotates when applying first axis rotation)
	has_x <- ifelse(grepl('x', type, ignore.case=TRUE), TRUE, FALSE)	# Has X-joint component (both axes remain fixed)

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
	
	# Set rotation dof skip
	if(dof[1] == 0){rdof_skip <- 0}else{rdof_skip <- (dof[1]+1)*3}

	if(print.progress){
		cat(paste0('\tJoint constraints:\n'))
		if(dof[1] > 0){
			cat(paste0('\t\tCoR: {', paste0(round(cons[1, 1:3], 3), collapse=', '), '}\n'))
			for(i in 1:dof[1]){
				cat(paste0('\t\tAoR ', i, ': {', paste0(round(uvector(cons[1, (i*3+1):(i*3+3)]), 3), collapse=', '), '}\n'))
			}
		}
		if(dof[2] > 0){
			for(i in 1:dof[2]){
				cat(paste0('\t\tTranslation Axis ', i, ': {', paste0(round(uvector(cons[1, (rdof_skip+i*3+1-3):(rdof_skip+i*3)]), 3), collapse=', '), '}\n'))
			}
		}
	}

	# Standardize coordinates into matrix
	if(is.vector(coor)) coor <- matrix(coor, nrow=1, ncol=3)
	
	# Set number of coordinates
	n_coor <- dim(coor)[1]

	# Create local coordinate system
	if(length(lcs) == 1){ # lcs == TRUE
		center <- colMeans(coor, na.rm=TRUE)
		lcs <- rbind(center, center+c(1,0,0), center+c(0,1,0), center+c(0,0,1))
	}
	
	# Create single array for all points
	if(!is.null(lcs)){pmat <- rbind(coor, lcs); n_coor <- nrow(pmat)}else{pmat <- coor}

	# Create array for transformed coordinates
	tcoor <- array(NA, dim=c(dim(pmat), n_iter))
	tcoor[, , 1] <- pmat
	
	# Create array for transformation matrices for body
	tmarr <- array(diag(4), dim=c(4, 4, n_iter))

	if(print.progress) cat('\tAnimating body\n')

	# Animate body
	for(iter in 2:n_iter){
	
		# Get reference coordinates
		if(input.ref == 'previous'){
			tcoor_mat <- tcoor[, , iter-1]
			cons_vec <- cons[iter-1, ]
		}else{
			tcoor_mat <- tcoor[, , 1]
			cons_vec <- cons[1, ]
		}
		
		# Add translations
		translations <- c(0,0,0)
		if(dof[2] > 0){
			for(i in 1:dof[2]) translations <- translations + param[iter-1, dof[1]+i]*uvector(cons_vec[(rdof_skip+i*3+1-3):(rdof_skip+i*3)])
		}
		tcoor_mat <- tcoor_mat + matrix(translations, n_coor, 3, byrow=TRUE)

		# Add translation to transformation matrix
		tmarr[1:3, 4, iter] <- tmarr[1:3, 4, iter] + translations

		# Apply translation to CoR
		if(dof[1] > 0) cons_vec[1:3] <- cons_vec[1:3] + translations

		# Apply rotations
		if(dof[1] > 0){

			# Move points to CoR
			tcoor_mat <- tcoor_mat - matrix(cons_vec[1:3], n_coor, 3, byrow=TRUE)

			# Add translation to transformation matrix
			tmarr[, , iter] <- tmarr[, , iter] %*% cbind(rbind(diag(3), rep(0,3)), c(cons[iter, 1:3], 1))

			# Apply rotations
			# 	The negative sign is needed to ensure rotation matrix follows the right-hand 
			#	rule - because the EP rotation matrix is put in a transformation 
			#	matrix that precedes the point matrix in the matrix multiplication. If the 
			#	EP rotation matrix came after the point matrix in the matrix multiplication, 
			#	the input angle would need to be positive
			if(has_u){
				
				# For a U-joint the order of rotations doesn't matter because the second rotational axis moves with the body
				# So unlike Euler angles, rotations do not change the relationship between the body and its rotational axis
				# Find rotation about AoR1
				RM1 <- tMatrixEP(v=cons_vec[4:6], a=param[iter-1, 1])
				
				# Apply rotation to coordinates
				tcoor_mat <- tcoor_mat %*% RM1
				
				# Apply rotation to transformation matrix
				tmarr[, , iter] <- tmarr[, , iter] %*% cbind(rbind(tMatrixEP(v=cons_vec[4:6], a=-param[iter-1, 1]), rep(0,3)), c(0,0,0,1))

				# For U-joint, rotation must also be applied to AoR2
				cons_vec[7:9] <- cons_vec[7:9] %*% RM1
				
				# Apply second rotation (about rotated AoR2) to coordinates
				tcoor_mat <- tcoor_mat %*% tMatrixEP(v=cons_vec[7:9], a=param[iter-1, 2])

				# Apply rotation to transformation matrix
				tmarr[, , iter] <- tmarr[, , iter] %*% cbind(rbind(tMatrixEP(v=cons_vec[7:9], a=-param[iter-1, 2]), rep(0,3)), c(0,0,0,1))
			}
			if(!has_u){
				for(i in dof[1]:1){
					tcoor_mat <- tcoor_mat %*% tMatrixEP(v=cons_vec[(i*3+1):(i*3+3)], a=param[iter-1, i])

					# Apply rotation to transformation matrix
					tmarr[, , iter] <- tmarr[, , iter] %*% cbind(rbind(tMatrixEP(v=cons_vec[(i*3+1):(i*3+3)], a=-param[iter-1, i]), rep(0,3)), c(0,0,0,1))
				}
			}
			#print(tMatrixEP(v=cons[iter, 4:6], a=-param[iter-1, 1]) %*% tMatrixEP(v=cons[iter, 7:9], a=-param[iter-1, 2]))
			#tcoor_mat <- tcoor_mat %*% tMatrixEP(v=cons[iter, 4:6], a=-param[iter-1, 1]) %*% tMatrixEP(v=cons[iter, 7:9], a=-param[iter-1, 2])

			# Move points back from CoR
			tcoor_mat <- tcoor_mat + matrix(cons_vec[1:3], n_coor, 3, byrow=TRUE)

			# Add translation to transformation matrix
			tmarr[, , iter] <- tmarr[, , iter] %*% cbind(rbind(diag(3), rep(0,3)), c(-cons[iter, 1:3], 1))
		}

		# Save transformed coordinates
		tcoor[, , iter] <- tcoor_mat
		cons[iter, ] <- cons_vec
	}

	# Remove first iteration (was only present to make easy looping)
	tcoor <- tcoor[, , 2:n_iter]
	cons <- cons[2:n_iter, ]
	tmarr <- tmarr[, , 2:n_iter]
	n_iter <- n_iter - 1

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
	
	# Check that transformation matrices returns the same result
	#tcoor_tm <- applyTransform(coor, tmarr)
	#print(sum(abs(tcoor_tm - rcoor)))

	# Add rownames
	dimnames(rcoor)[[1]] <- rownames(coor)

	list(
		'coor'=rcoor,
		'lcs'=rlcs,
		'cons'=cons,
		'dof'=dof,
		'tmarr'=tmarr
	)
}
