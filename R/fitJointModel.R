fitJointModel <- function(coor, type, cons = NULL, pose.init = NULL, optim.frame.max = 100, 
	optim.iter.max = 5, optim.pose = TRUE, optim.cons = TRUE, optim.to.prop = 0.00001, print.progress = FALSE){

	control <- list(
		'subsample.method'='Evenly spaced',		# or 'Maximum dispersion'
		'max.disp.prop.R'=0.2,					# Proportion of time points used in max dispersion sampling
		'max.disp.prop.S'=0.2,					#
		'max.disp.prop.U'=0.2					#
	)

	# Set reference iteration
	ref.iter <- 1

	# If number of iterations exceeds optim.frame.max, limit to optim.frame.max
	optim_use <- 1:dim(coor)[3]
	if(dim(coor)[3] > optim.frame.max){

		if(control$subsample.method == 'Maximum dispersion'){		# Try sorting to see if that changes error - shouldn't

			# Find centroid size of each point "position cloud" over time (for weights)
			Csizes_time <- apply(coor, 1, 'centroidSize', transpose=TRUE)

			# Find maximally disperse points among input
			optim_use <- whichMaxDisperse(t(coor[which.max(Csizes_time), , ]), n=optim.frame.max)
			
			# Make sure subset includes ref.iter
			if(!ref.iter %in% optim_use) optim_use <- c(1, optim_use[1:(length(optim_use)-1)])

		}else if(control$subsample.method == 'Evenly spaced'){

			# Get evenly spaced subset
			optim_use <- floor(seq(1, dim(coor)[3], length=optim.frame.max))

			# Make sure subset includes ref.iter
			if(!ref.iter %in% optim_use) optim_use <- sort(c(ref.iter, floor(seq(1, dim(coor)[3], length=optim.frame.max-1))))
		}
	}

	if(print.progress){
		cat(paste0('\nfitJointModel\n'))
		cat(paste0('\tType of joint model fit: ', type, '\n'))
		cat(paste0('\tTotal time points: ', dim(coor)[3], '\n'))
		cat(paste0('\tNumber of time points used in optimization: ', length(optim_use), '\n'))
		cat(paste0('\t\tMethod of sub-sampling: ', control$subsample.method, '\n'))
	}

	# Get DoF of joint type
	dof <- joint_types()$dof[type, ]

	# Estimate joint constraints
	if(is.null(cons)){
		if(print.progress) cat(paste0('\tEstimating joint constraints...\n'))
		fit_joint_cons <- fitJointConstraint(coor, type, control=control, print.progress=print.progress)
		joint_cons <- fit_joint_cons$cons
	}else{
		joint_cons <- cons
	}
	
	# Set rotation dof skip
	if(dof['R'] == 0){rdof_skip <- 0}else{rdof_skip <- (dof['R']+1)*3}

	if(print.progress){
		cat(paste0('\tEstimated joint constraints:\n'))
		if(dof['R'] > 0){
			cat(paste0('\t\tCoR: {', paste0(round(joint_cons[1:3], 3), collapse=', '), '}\n'))
			for(i in 1:dof['R']){
				cat(paste0('\t\tAoR ', i, ': {', paste0(round(uvector(joint_cons[(i*3+1):(i*3+3)]), 3), collapse=', '), '}\n'))
			}
		}
		if(dof['T'] > 0){
			for(i in 1:dof['T']){
				cat(paste0('\t\tTranslation Axis ', i, ': {', paste0(round(uvector(joint_cons[(rdof_skip+i*3+1-3):(rdof_skip+i*3)]), 3), collapse=', '), '}\n'))
			}
		}
	}

	# Get subset
	coor_s <- coor[, , optim_use]

	# Align coordinates across all time points using generalized procrustes analysis to 
	# find mean (consensus) shape scaled to mean centroid size
	ccoor <- procAlign(coor_s)

	# Align consensus to reference time point
	if(is.null(pose.init)){
		pose_init <- bestAlign(coor[, , ref.iter], ccoor$mean.scaled)$mat
	}else{
		pose_init <- pose.init
	}

	# Find full range of coordinates
	coor_range <- apply(coor, 2, 'range')
	
	# Set translation limit based on coordinate range
	translation_limit <- max(apply(coor_range, 2, 'diff'))*2

	# Set starting guess for input parameter values
	if(dof['R'] > 0){
		r_input <- matrix(0.1, length(optim_use), dof['R'])
		r_lower <- rep(-6*pi, length=dof['R'])
		r_upper <- rep(6*pi, length=dof['R'])
	}else{
		r_input <- r_lower <- r_upper <- NULL
	}
	if(dof['T'] > 0){
		t_input <- matrix(translation_limit*0.05, length(optim_use), dof['T'])
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
	optim_param[optim_use, ] <- optim_param_values
	
	# Set initial value for reference iteration to 0 - this will also be optimized
	optim_param[ref.iter, ] <- 0

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
	
	# Set joint constraint limits
	cons_lower <- c()
	cons_upper <- c()
	cor_limit <- 0.2*translation_limit
	if(dof['R'] > 0){
		
		# Center of rotation limits
		cons_lower <- c(cons_lower, joint_cons[1:3] - cor_limit)
		cons_upper <- c(cons_upper, joint_cons[1:3] + cor_limit)

		# Euler angle limits for first axis
		cons_lower <- c(cons_lower, c(-pi,-pi,-pi))
		cons_upper <- c(cons_upper, c(pi,pi,pi))

		# Euler angle limit for second axis since this stays orthogonal to the first (rotation about first axis)
		for(i in 2:dof['R']){
			cons_lower <- c(cons_lower, -pi)
			cons_upper <- c(cons_upper, pi)
		}
	}
	
	# But for S-joint, no need to optimize axes, only center of rotation
	if(dof['R'] == 3){
		cons_lower <- cons_lower[1:3]
		cons_upper <- cons_upper[1:3]
	}
	
	if(dof['T'] > 0){

		# Euler angle limits for first axis
		cons_lower <- c(cons_lower, c(-pi,-pi,-pi))
		cons_upper <- c(cons_upper, c(pi,pi,pi))

		# Euler angle limit for second axis since this stays orthogonal to the first (rotation about first axis)
		for(i in 2:dof['T']){
			cons_lower <- c(cons_lower, -pi)
			cons_upper <- c(cons_upper, pi)
		}
	}

	if(print.progress){
		if(optim.cons && optim.pose){
			cat(paste0('\tOptimizing input parameters, initial body pose, and constraint parameters...\n'))
		}else if(optim.cons && !optim.pose){
			cat(paste0('\tOptimizing input parameters and constraint parameters...\n'))
		}else if(!optim.cons && optim.pose){
			cat(paste0('\tOptimizing initial body pose...\n'))
		}else if(!optim.cons && !optim.pose){
			cat(paste0('\tOptimizing input parameters...\n'))
		}
	}

	# Go back and forth between optimizing the input parameters and body pose until error 
	# changes less than difference threshold between consecutive optimization steps (like 
	# in generalized Procrustes analysis)
	while(abs(diff(tail(optim_errors, 2))) > opt_to_diff && optim_iter < optim.iter.max){

		#if(print.progress) cat('', optim_iter+1)
		if(print.progress) cat('\t\t')

		# Set previous pose
		pose_prev <- pose_init

		## Optimize input parameters
		input_fit_errors <- rep(NA, length(optim_use))
		for(i in 1:length(optim_use)){

			input_fit <- tryCatch(
				expr={
					nlminb(start=optim_param[optim_use[i],], objective=animate_joint_error, 
						lower=input_lower, upper=input_upper, type=type, cons=joint_cons, coor=pose_prev, 
						coor.compare=coor[, , optim_use[i]]) 
				},
				error=function(cond) {print(cond);return(NULL)},
				warning=function(cond) {print(cond);return(NULL)}
			)

			# Save results
			optim_param[optim_use[i], ] <- input_fit$par
			input_fit_errors[i] <- input_fit$objective

			# Use parameter to run iteration of model
			#pose_prev <- animate_joint(type=type, cons=joint_cons, 
			#	param=rbind(rep(0, length(input_fit$par)), input_fit$par), coor=pose_prev, 
			#	lcs=NULL, check.joint.cons=FALSE)$coor[, , 2]
			
			# Save parameters for next iteration
			next_iter_param <- input_fit$par
			
			# Make sure angles stay on -2*pi,2*pi interval
			if(dof['R'] > 0){
				angles <- next_iter_param[1:dof['R']]
				out_range <- abs(angles) > 2*pi
				angles[out_range] <- sign(angles[out_range])*(abs(angles[out_range]) %% (2*pi))
				next_iter_param[1:dof['R']] <- angles
			}
			
			# Set next input parameter based on current
			if(i+1 <= length(optim_use) && optim_use[i+1] != ref.iter) optim_param[optim_use[i+1], ] <- next_iter_param
		}

		if(print.progress) cat(paste0('', round(mean(input_fit_errors, na.rm=TRUE), 5)))

		# Set transform starting parameters
		start_transform <- c(0.1,0.1,0.1,translation_limit*0.05,translation_limit*0.05,translation_limit*0.05)

		## Optimize pose
		if(optim.pose){
			pose_fit <- tryCatch(
				expr={
					nlminb(start=start_transform, objective=body_pose_error, 
						lower=pose_lower, upper=pose_upper, type=type, cons=joint_cons, 
						coor=pose_init, coor.compare=coor_s, param=optim_param[optim_use, ])
				},
				error=function(cond) {print(cond);return(NULL)},
				warning=function(cond) {print(cond);return(NULL)}
			)

			# Apply optimized transformation to initial pose
			centroid_mat <- matrix(colMeans(pose_init), nrow(pose_init), ncol(pose_init), byrow=TRUE)
			pose_init <- ((pose_init - centroid_mat) %*% rotationMatrixZYX(pose_fit$par[1:3])) + centroid_mat
			pose_init <- pose_init + matrix(pose_fit$par[4:6], nrow(coor), ncol(coor), byrow=TRUE)

			if(print.progress) cat(paste0(', ', round(pose_fit$objective, 5)))
		}else{
			pose_fit <- list()
			pose_fit$objective <- mean(input_fit_errors, na.rm=TRUE)
		}

		## Optimize constraint parameters
		if(optim.cons && type != 'T'){
		
			# Set starting parameters (Euler angles to rotate vectors, initially same as input)
			start_cons_param <- rep(0, length(cons_lower))
			
			# But keep CoR as regular vector
			if(dof['R'] > 0) start_cons_param[1:3] <- joint_cons[1:3]

			# Optimize constraint parameters
			constraint_fit <- tryCatch(
				expr={
					nlminb(start=start_cons_param, objective=constraint_error, 
						lower=cons_lower, upper=cons_upper, type=type, coor=pose_init, 
						coor.compare=coor_s, param=optim_param[optim_use, ], 
						joint.cons.init=joint_cons, dof=dof)
				},
				error=function(cond) {print(cond);return(NULL)},
				warning=function(cond) {print(cond);return(NULL)}
			)
			
			# Apply optimized transformations to constraint parameters
			new_cons <- apply_cons_optim_transforms(constraint_fit$par, joint_cons, type, dof)

			#print(pose_fit$objective)
			#print(constraint_fit)
			#print(rbind(cons_lower, cons_upper))
			#print(constraint_fit$objective)
			if(print.progress) cat(paste0(', ', round(constraint_fit$objective, 5)))
			
			# Save new joint constraints and error (if lower than previous)
			if(!is.null(constraint_fit) && constraint_fit$objective < pose_fit$objective){
				joint_cons <- new_cons
				pose_fit$objective <- constraint_fit$objective
			}
		}

		if(print.progress) cat('\n')
		
		# Save error after optimization
		optim_errors <- c(optim_errors, pose_fit$objective)
	
		# Advance iteration tracker
		optim_iter <- optim_iter + 1

		# If near perfect fit stop - for perfect input testing
		if(pose_fit$objective < 1e-7) break
		
		# If only input parameters are optimized nothing changes with loop so only one iteration needed
		if(!optim.cons && !optim.pose) break
	}

	# Remove first two errors (only used to make for easy looping)
	optim_errors <- optim_errors[3:length(optim_errors)]

	# Check that sequence of errors decreases
	#if(print.progress) cat(paste0('\tInput parameter and pose optimization error sequence:\n\t\t', paste0(round(optim_errors, 7), collapse='\n\t\t'), '\n'))

	# Fill in missing guesses for starting parameters with preceding parameter (could use 
	# interpolation if necessary but this seems to work pretty well)
	for(i in 1:nrow(optim_param)){

		# Save last non-NA value
		if(!is.na(optim_param[i, 1])) {seq_i <- optim_param[i, ];next}

		# Add last non-NA value
		optim_param[i, ] <- seq_i
	}

	# Create optimized coordinate array
	optim_coor <- coor*NA
	optim_coor[, , ref.iter] <- pose_init

	# Get indices not previously used
	not_optim_use <- (1:dim(coor)[3])[!(1:dim(coor)[3]) %in% optim_use]

	# Fill in all input parameters with optimized parameter
	for(i in not_optim_use){

		input_fit <- tryCatch(
			expr={
				nlminb(start=optim_param[i,], objective=animate_joint_error, 
					lower=input_lower, upper=input_upper, type=type, cons=joint_cons, 
					coor=pose_init, coor.compare=coor[, , i]) 
			},
			error=function(cond) {print(cond);return(NULL)},
			warning=function(cond) {print(cond);return(NULL)}
		)

		# Save optimized parameters
		optim_param[i, ] <- input_fit$par
	}

	# Use optimized parameters to run model
	for(i in 1:dim(coor)[3]){
		optim_coor[, , i] <- animate_joint(type=type, cons=joint_cons, 
			param=rbind(rep(0, length(optim_param[i, ])), optim_param[i, ]), coor=pose_init, 
			lcs=NULL, check.joint.cons=FALSE)$coor[, , 2]
	}

	# Compare final simulated coordinates to actual
	rmse <- sqrt(apply((optim_coor - coor)^2, 3, 'rowSums'))
	rmse_mean <- mean(rmse)

	if(print.progress){
		if(optim.cons){
			cat(paste0('\tFinal estimated joint constraints:\n'))
			if(dof['R'] > 0){
				cat(paste0('\t\tCoR: {', paste0(round(joint_cons[1:3], 3), collapse=', '), '}\n'))
				for(i in 1:dof['R']){
					cat(paste0('\t\tAoR ', i, ': {', paste0(round(uvector(joint_cons[(i*3+1):(i*3+3)]), 3), collapse=', '), '}\n'))
				}
			}
			if(dof['T'] > 0){
				for(i in 1:dof['T']){
					cat(paste0('\t\tTranslation Axis ', i, ': {', paste0(round(uvector(joint_cons[(rdof_skip+i*3+1-3):(rdof_skip+i*3)]), 3), collapse=', '), '}\n'))
				}
			}
		}
		
		# Print errors
		cat(paste0('\tRMS error for model across all iterations: ', round(rmse_mean, 7), '\n'))
	}

	# Create return list
	rlist <- list(
		'type'=type,
		'cons'=joint_cons,
		'param'=optim_param,
		'pose.init'=pose_init,
		'rmse.mean'=rmse_mean,
		'rmse'=rmse
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