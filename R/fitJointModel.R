fitJointModel <- function(coor, type, max.iter.use = 30, ref.iter = 1, optim.to.prop = 0.00001, 
	print.progress = FALSE){

	# Get DoF of joint type
	dof <- joint_types()$dof[type, ]

	# If number of iterations exceeds max.iter.use, limit to evenly spaced subset
	use_idx <- 1:dim(coor)[3]
	if(dim(coor)[3] > max.iter.use){

		# Get evenly spaced subset
		use_idx <- floor(seq(1, dim(coor)[3], length=max.iter.use))
		
		# Make sure subset includes ref.iter
		if(!ref.iter %in% use_idx) use_idx <- sort(c(ref.iter, floor(seq(1, dim(coor)[3], length=max.iter.use-1))))
	}

	# Get subset
	coor_s <- coor[, , use_idx]

	# Fit shape to point position over time
	fit_joint <- fitJointConstraint(coor_s, type)
	joint_cons <- fit_joint$cons

return(1)

	# Optimize initial pose and input parameters
	# Align coordinates across all time points using generalized procrustes analysis to 
	# find mean (consensus) shape scaled to mean centroid size
	ccoor <- procAlign(coor_s)

	# Align consensus to reference time point
	pose_init <- findBestAlignment(coor[, , ref.iter], ccoor$mean.scaled)$mat

	# Find full range of coordinates
	coor_range <- apply(coor, 2, 'range')
	
	# Set translation limit based on coordinate range
	translation_limit <- max(apply(coor_range, 2, 'diff'))*2

	# Set starting guess for input parameter values
	if(dof['R'] > 0){
		r_input <- matrix(0.1, length(use_idx), dof['R'])
		r_lower <- rep(-pi, length=dof['R'])
		r_upper <- rep(pi, length=dof['R'])
	}else{
		r_input <- r_lower <- r_upper <- NULL
	}
	if(dof['T'] > 0){
		t_input <- matrix(translation_limit*0.05, length(use_idx), dof['T'])
		t_lower <- rep(-translation_limit, length=dof['T'])
		t_upper <- rep(translation_limit, length=dof['T'])
	}else{
		t_input <- t_lower <- t_upper <- NULL
	}
	
	# Set starting values for optimizing parameters
	optim_param_values <- cbind(r_input, t_input)

	# Create matrix to hold optimized input parameters
	optim_param <- matrix(NA, dim(coor)[3], ncol=ncol(optim_param_values))

	# Fill in matrix for optimize subset
	optim_param[use_idx, ] <- optim_param_values
	
	# Set input parameters to 0 for reference (initial) iteration
	optim_param[ref.iter, ] <- 0

	# Create vector for errors (RMSD)
	objectives <- rep(NA, length(use_idx))
	
	# Find error of consensus at reference time point
	objectives[ref.iter] <- sqrt(mean((pose_init - coor[, , ref.iter])^2))

	# Set difference threshold at which optimization will stop, as proportion of mean centroid size
	opt_to_diff <- optim.to.prop*mean(ccoor$Csize)
	
	# Initial setting of optimization sequence parameters
	optim_iter <- 0
	optim_errors <- c(translation_limit*1000, translation_limit*1000-opt_to_diff*2)

	# Set input limits
	input_lower <- c(r_lower, t_lower)
	input_upper <- c(r_upper, t_upper)

	# Set pose limits
	pose_lower <- c(rep(-pi,3), rep(-translation_limit,3))
	pose_upper <- c(rep(pi,3), rep(translation_limit,3))

	# Go back and forth between optimizing the input parameters and body pose until error 
	# changes less than difference threshold between consecutive optimization steps (like 
	# in generalized Procrustes analysis)
	while(abs(diff(tail(optim_errors, 2))) > opt_to_diff && optim_iter < 5){

		# Optimize input parameters
		for(i in 1:length(use_idx)){
	
			if(use_idx[i] == ref.iter) next

			input_fit <- tryCatch(
				expr={
					nlminb(start=optim_param[use_idx[i],], objective=animate_joint_error, 
						lower=input_lower, upper=input_upper, type=type, cons=joint_cons, coor=pose_init, 
						coor.compare=coor[, , use_idx[i]]) 
				},
				error=function(cond) {print(cond);return(NULL)},
				warning=function(cond) {print(cond);return(NULL)}
			)

			# Save results
			optim_param[use_idx[i], ] <- input_fit$par
			objectives[i] <- input_fit$objective
			
			# Set next input parameter based on current
			if(i+1 <= length(use_idx) && use_idx[i+1] != ref.iter) optim_param[use_idx[i+1], ] <- input_fit$par
		}

		# Set transform starting parameters
		start_transform <- c(0.1,0.1,0.1,translation_limit*0.05,translation_limit*0.05,translation_limit*0.05)

		# Optimize pose
		pose_fit <- tryCatch(
			expr={
				nlminb(start=start_transform, objective=body_pose_error, 
					lower=pose_lower, upper=pose_upper, 
					type=type, cons=joint_cons, coor=pose_init, 
					coor.compare=coor_s, param=optim_param[use_idx, ])
			},
			error=function(cond) return(NULL),
			warning=function(cond) return(NULL)
		)
		
		# Save error after optimization
		optim_errors <- c(optim_errors, pose_fit$objective)
	
		# Apply optimized transformation to initial pose
		centroid_mat <- matrix(colMeans(pose_init), nrow(pose_init), ncol(pose_init), byrow=TRUE)
		pose_init <- ((pose_init - centroid_mat) %*% rotationMatrixZYX(pose_fit$par[1:3])) + centroid_mat
		pose_init <- pose_init + matrix(pose_fit$par[4:6], nrow(coor), ncol(coor), byrow=TRUE)

		# Advance iteration tracker
		optim_iter <- optim_iter + 1
	}

	# Remove first two errors (only used to make for easy looping)
	optim_errors <- optim_errors[3:length(optim_errors)]

	# Check that sequence of errors decreases
	if(print.progress) cat(paste0('\tInput parameter and pose optimization error sequence:\n\t\t', paste0(round(optim_errors, 7), collapse='\n\t\t'), '\n'))

	# Fill in missing guesses for starting parameters with preceding parameter (could use 
	# interpolation if necessary but this seems to work pretty well)
	for(i in 1:nrow(optim_param)){

		# Save last non-NA value
		if(!is.na(optim_param[i, 1])) {seq_i <- optim_param[i, ];next}

		# Add last non-NA value
		optim_param[i, ] <- seq_i
	}

	# Fill in remaining input parameters (not in use_idx) with optimized parameter
	for(i in 1:dim(coor)[3]){

		if(i %in% use_idx) next

		input_fit <- tryCatch(
			expr={
				nlminb(start=optim_param[i,], objective=animate_joint_error, 
					lower=input_lower, upper=input_upper, type=type, cons=joint_cons, coor=pose_init, 
					coor.compare=coor[, , i]) 
			},
			error=function(cond) {print(cond);return(NULL)},
			warning=function(cond) {print(cond);return(NULL)}
		)

		optim_param[i, ] <- input_fit$par
	}

	# Run model with optimized input parameters
	anim <- animate_joint(type=type, cons=joint_cons, param=optim_param, coor=pose_init, 
		check.joint.cons=TRUE)

	# Compare final simulated coordinates to actual
	rmse <- sqrt(mean((anim$coor - coor)^2))
	
	# Print errors
	if(print.progress){
		cat(paste0('\nfitJointConstraint\n'))
		cat(paste0('\tRMS error: ', round(rmse, 5), '\n'))
	}

	# Create return list
	rlist <- list(
		'type'=type,
		'cons'=joint_cons,
		'param'=optim_param,
		'coor'=pose_init,
		'rmse'=rmse,
		'animate'=anim
	)

	return(rlist)

	#
	if(FALSE){
		svg.new(file='Test.html')
		svg.frame(coor)
		svg.points(pose_init, col=c('red', 'green', 'blue', 'black'), cex=1)
		for(i in 1:dim(coor)[3]){
			if(i == ref.iter){
				svg.pointsC(coor[, , ref.iter], cex=7, col.stroke=c('red', 'green', 'blue', 'black'), col.fill='none', close=TRUE)
			}else{
				svg.pointsC(coor[, , i], cex=7, opacity.stroke=0.3, col.stroke=c('red', 'green', 'blue', 'black'), col.fill='none', close=TRUE)
			}
		}
		for(i in 1:dim(anim$coor)[3]) svg.pointsC(anim$coor[, , i], col.stroke=c('red', 'green', 'blue', 'black'), col.stroke.C='orange', cex=1, close=TRUE)
		for(i in 1:dim(anim$coor)[1]) svg.pointsC(t(anim$coor[i, , ]), col.stroke.C='orange')
		svg.close()
	}

	# 
	if(FALSE){
		svg.new(file='Test.html')

		svg.pointsC(t(coor_s[i, , ]), cex=3, col.stroke='green')
		svg.pointsC(circlePoint(fit_circle_3d, seq(0,2*pi,length=80)), cex=0, col.stroke='orange')
		svg.arrows(rbind(fit_circle_3d$C, fit_circle_3d$C+0.25*uvector(fit_circle_3d$N)), col='blue')

		svg.points(CoR, cex=3, col='orange')
		svg.arrows(rbind(CoR, CoR+AoR), col='orange')
	
		svg.arrows(rbind(c(1,0.2,0), c(1,0.2,0)+0.5*uvector(c(0.3,0.3,1))), col='purple')
		svg.close()
	}
}