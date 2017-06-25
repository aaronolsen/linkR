fitMechanism <- function(joint.types, body.conn, fit.points, body.assoc, input.param, 
	input.joint, input.body, fit.wts = NULL, joint.names = NULL, fixed.body = 'Fixed', 
	print.progress = FALSE){

	# Start run time
	time1 <- proc.time()

	if(print.progress) cat(paste0('fitMechanism()\n'))
	
	control <- list(
		'joint.coor.bounds.factor'=0.1,
		'fit.points.bounds.factor'=0.1,
		'optim.frame.max'=10,
		'optim.to.prop'=0.001,
		'optim.iter.max'=1
	)

	# Use motion between two bodies connected by each joint to generate initial joint 
	# constraint estimate

	# Make sure that row names are unique
	if(!is.null(rownames(fit.points))) if(length(unique(rownames(fit.points))) != nrow(fit.points)) stop("Row names in 'fit.points' must be unique.")

	# Get number of iterations
	n_iter <- dim(fit.points)[3]
	
	# Set reference iteration - shouldn't change fit when completed
	ref.iter <- 1
	
	# Get number of joints
	n_joints <- length(joint.types)

	# Set NA joint coordinates
	joint.coor <- matrix(NA, nrow=n_joints, ncol=3, dimnames=list(joint.names, NULL))
	
	# Set NA joint constraints
	joint.cons <- setNames(as.list(rep(NA, n_joints)), joint.names)

	# Use define linkage just to get body.conn.num and other mechanism properties (skip path finding)
	mechanism <- defineMechanism(joint.coor=joint.coor, joint.types=joint.types, 
		joint.cons=joint.cons, body.conn=body.conn, find.paths=FALSE)

	# Create numeric body association
	body_assoc_num <- rep(NA, length(body.assoc))
	for(i in 1:length(body_assoc_num)) body_assoc_num[i] <- which(body.assoc[i] == mechanism$body.names)

	# If number of iterations exceeds optim.frame.max, limit to optim.frame.max
	if(n_iter <= control$optim.frame.max){
		optim_use <- 1:n_iter
	}else{
		optim_use <- round(seq(1, n_iter, length=control$optim.frame.max))
	}

	# Set number of optimization iterations
	n_optim <- length(optim_use)

	if(print.progress){
		cat(paste0('\tTotal time points: ', n_iter, '\n'))
		cat(paste0('\tNumber of time points used in optimization: ', n_optim, '\n'))
	}

	## Estimate initial joint constraints
	if(print.progress) cat(paste0('\tEstimating joint constraints...\n'))
	for(joint_num in 1:nrow(mechanism$body.conn.num)){
		
		# Get two bodies connected by joint
		bodies <- mechanism$body.conn.num[joint_num, ]
		
		# Get points associated with each body
		body_points <- list(which(body_assoc_num == bodies[1]), which(body_assoc_num == bodies[2]))

		# If fixed link among bodies
		if(1 %in% bodies){

			# Choose other body as mobile
			mobile_body <- which(bodies != 1)
			
			# Get fit points
			fit_points <- fit.points[body_points[[mobile_body]], , ]
			
			# Get fixed body points
			fixed_points <- fit.points[body_points[[which(bodies == 1)]], , ]

		# If no fixed link
		}else{

			# Fix body2 points relative to body1 (need to input body1+body2 points)
			fit_points_rel <- immobilize(fit.points[unlist(body_points), , ], fit.points[body_points[[1]], , 1])
			
			# Save body2 points only
			fit_points <- fit_points_rel[dimnames(fit.points)[[1]][body_points[[2]]], , ]
			fixed_points <- fit_points_rel[dimnames(fit.points)[[1]][body_points[[1]]], , ]
		}
		
		# Get initial joint constraint estimate
		# **** Could use the angles recovered from fitJointConstraint to fill initial input parameter estimates
		fit_joint_cons <- fitJointConstraint(coor=fit_points, type=joint.types[joint_num], print.progress=FALSE)

		if(joint.types[joint_num] == 'R'){
			joint.coor[joint_num, ] <- fit_joint_cons$cons[1:3]
			joint.cons[[joint_num]] <- fit_joint_cons$cons[4:6]
		}
		if(joint.types[joint_num] == 'S'){
			joint.coor[joint_num, ] <- fit_joint_cons$cons[1:3]
			joint.cons[[joint_num]] <- diag(3)
		}
	}
	
	# Re-define linkage with initial joint coordinate and constraints
	mechanism <- defineMechanism(joint.coor=joint.coor, joint.types=joint.types, 
		joint.cons=joint.cons, body.conn=body.conn)

	# If input.joint is non-numeric, convert to numeric equivalent
	if(!is.numeric(input.joint[1])){
		if(sum(!input.joint %in% mechanism$joint.names) > 0) stop("'input.joint' names do not match joint names.")
		input_joint_num <- rep(NA, length(input.joint))
		for(i in 1:length(input.joint)) input_joint_num[i] <- which(mechanism$joint.names == input.joint[i])
		input.joint <- input_joint_num
	}

	## Prepare input parameter optimization vectors
	# Create vector of input parameters (start with first iteration)
	input_optim_vec <- c()
	input_optim_names <- c()

	# Create matrix of bounds for input parameters
	input_optim_bounds <- matrix(NA, nrow=2, ncol=0, dimnames=list(c('lower', 'upper'), NULL))

	# Create vector for number of input parameters per input
	n_input <- rep(NA, length(input.param))
	
	# Create optimization input parameter
	input_param_optim <- list()

	# Fill vector with initial values
	for(i in 1:length(input_joint_num)){
		
		# Set number of input parameters per joint
		if(is.vector(input.param[[i]])){
			n_input[i] <- 1
		}else{
			n_input[i] <- ncol(input.param[[i]])
		}

		# Create optimization input parameters
		input_param_optim[[i]] <- matrix(input.param[[i]], nrow=n_optim, ncol=n_input[i])
		
		# Set input value
		if(joint.types[input_joint_num[i]] %in% c('R', 'S')){
			input_value <- -0.1
			input_bounds <- c(-6*pi, 6*pi)
		}

		# Fill vectors and matrices
		input_optim_vec <- c(input_optim_vec, rep(input_value, n_input[i]))
		input_optim_names <- c(input_optim_names, paste0(joint.types[input_joint_num[i]], input_joint_num[i], '_', 1:n_input[i]))
		input_optim_bounds <- cbind(input_optim_bounds, rbind(rep(input_bounds[1], n_input[i]), rep(input_bounds[2], n_input[i])))
	}
	
	# Convert to matrix with row for each iteration
	input_optim <- matrix(NA, nrow=n_iter, ncol=length(input_optim_vec), byrow=TRUE, dimnames=list(NULL, input_optim_names))

	# Add column names to bounds matrix
	colnames(input_optim_bounds) <- colnames(input_optim)

	# Set rows that will not be used in optimization to NA
	input_optim[optim_use, ] <- input_optim_vec

	# Set initial value for reference iteration to 0 - this will also be optimized
	input_optim[ref.iter, ] <- 0


	## Prepare joint coordinate optimization vectors
	# Get range of joint coordinate positions along each axis
	joint_coor_range <- apply(apply(mechanism$joint.coor, 2, 'range'), 2, 'diff')
	
	# If one dimension is 0, set to minimum of other dimensions
	if(sum(abs(joint_coor_range) < 1e-3) > 0) joint_coor_range[abs(joint_coor_range) < 1e-3] <- min(joint_coor_range[joint_coor_range !=0])
	
	# Set amount to add to current values to set lower and upper bounds based on coordinate range
	coor_optim_bounds_add <- rep(joint_coor_range*control$joint.coor.bounds.factor, n_joints)

	# Create sequence of values to add to joint coordinates
	coor_optim <- c()
	for(i in 1:n_joints){
		if(joint.types[i] == 'R') coor_optim <- c(coor_optim, rep(0,2))
		if(joint.types[i] == 'S') coor_optim <- c(coor_optim, rep(0,3))
	}

	## Prepare joint constraint optimization vectors
	# Create vector for number of constraint parameters per joint
	n_cons <- rep(0, n_joints)
	for(i in 1:n_joints){
		if(joint.types[i] == 'S') next
		if(joint.types[i] == 'R') n_cons[i] <- 3
	}

	## Prepare fit point pose optimization vectors
	# Get range of joint coordinate positions along each axis
	fit_points_range <- apply(apply(fit.points[, , 1], 2, 'range'), 2, 'diff')
	
	# If one dimension is 0, set to minimum of other dimensions
	if(sum(abs(fit_points_range) < 1e-3) > 0) fit_points_range[abs(fit_points_range) < 1e-3] <- min(fit_points_range[fit_points_range !=0])

	# Set bound for translation
	pose_optim_bounds_add <- fit_points_range*control$fit.points.bounds.factor
	

	## Align coordinates across all time points using generalized procrustes analysis to 
	# find mean (consensus) shape scaled to mean centroid size
	#ccoor <- procAlign(coor_s)
	#consensus <- fit.points[, , ref.iter]

	# Align consensus to reference time point
	#pose_init <- findBestAlignment(fit.points[, , ref.iter], consensus)$mat

	# Set reference pose
	pose_ref <- fit.points[, , ref.iter]

	# Add fit points to mechanism as body points
	for(body_name in mechanism$body.names){
	
		# Get fit points associated with body
		fit_assoc <- which(body.assoc == body_name)

		# Set points connect
		if(length(fit_assoc) == 2){
			points_connect <- list(c(1,2))
		}else if(length(fit_assoc) == 3){
			points_connect <- list(c(1,2), c(2,3), c(1,3))
		}
		
		# Associate fit points with body
		mechanism <- associatePoints(mechanism, points=pose_ref[fit_assoc, ], body=body_name, 
			points.connect=points_connect)
	}
	
	# Sort fit point weights to match mechanism$body.points
	if(!is.null(fit.wts)){
		names(fit.wts) <- dimnames(fit.points)[[1]]
		fit.wts <- fit.wts[rownames(mechanism$body.points)]
	}else{
		fit.wts <- setNames(rep(1), rownames(mechanism$body.points))
	}

	# Sort fit points to match mechanism$body.points
	fit.points=fit.points[rownames(mechanism$body.points), , ]

	# Draw animation
	#drawMechanism(mechanism, file=paste0('Fit mechanism.html'), 
	#	window.title='Fit mechanism', animate.reverse=FALSE)

	if(print.progress) cat(paste0('\tOptimizing input parameters, joint coordinates, constraint parameters, and fit point reference poses...\n'))

	# Set difference threshold at which optimization will stop, as proportion of mean fit point range
	opt_to_diff <- control$optim.to.prop*mean(fit_points_range)

	# Initial setting of optimization sequence parameters
	optim_iter <- 0
	optim_errors <- c(mean(fit_points_range)*1000, mean(fit_points_range)*1000-opt_to_diff*2)

#if(FALSE){
	# Cycle optimizing the input parameters, joint constraints and coordinates, and body 
	#	pose until error changes less than difference threshold between consecutive 
	# 	optimization steps
	while(abs(diff(tail(optim_errors, 2))) > opt_to_diff && optim_iter < control$optim.iter.max){
	
		if(print.progress) cat('\t\tErrors: ')

		## Optimize input parameters
		# Create vectors for initial and final error
		input_fit_errors_i <- rep(NA, n_optim)
		input_fit_errors_f <- rep(NA, n_optim)

		# Run optimization
		for(i in 1:n_optim){
		
			# Set iteration
			iter <- optim_use[i]

			# Find initial error
			if(print.progress) input_fit_errors_i[i] <- animate_mechanism_error(input_optim[iter, ], 
				replace='input.param', fit.points=fit.points[, , iter], mechanism=mechanism, 
				input.param=input.param, input.joint=input.joint, input.body=input.body, fit.wts=fit.wts, 
				n.input=n_input)

			# Run optimization
			input_fit <- tryCatch(
				expr={
					nlminb(start=input_optim[iter, ], objective=animate_mechanism_error, 
						lower=input_optim_bounds['lower', ], upper=input_optim_bounds['upper', ], 
						replace='input.param', fit.points=fit.points[, , iter], 
						mechanism=mechanism, input.param=input.param, input.joint=input.joint, 
						input.body=input.body, fit.wts=fit.wts, n.input=n_input)
				},
				error=function(cond) {print(cond);return(NULL)},
				warning=function(cond) {print(cond);return(NULL)}
			)
			
			# Save error
			input_fit_errors_f[i] <- input_fit$objective

			# Save optimized parameters
			input_optim[optim_use[i], ] <- input_fit$par
			
			# If first cycle, set next input parameters to current values
			if(optim_iter == 0 && i < n_optim) input_optim[optim_use[i+1], ] <- input_fit$par
		}

		# Print error
		if(print.progress) cat(paste0('', round(mean(input_fit_errors_i, na.rm=TRUE), 5), '->', round(mean(input_fit_errors_f, na.rm=TRUE), 5)))

		# Add input parameters into list
		n <- 1
		for(j in 1:length(input_param_optim)){
			input_param_optim[[j]] <- matrix(input_optim[optim_use, n:(n + n_input[j] - 1)], nrow=n_optim, ncol=n_input[j])
			n <- n + n_input[j]
		}


		## Optimize joint coordinates
		# Create list for vector components
		coor_vectors <- as.list(rep(NA, n_joints))
		for(j in 1:n_joints){
			if(joint.types[j] == 'R'){
				vo <- vorthogonal(mechanism$joint.cons[[j]][, , 1])
				coor_vectors[[j]] <- rbind(vo, cprod(vo, mechanism$joint.cons[[j]][, , 1]))
			}
		}
		
		# Get initial error
		if(print.progress) coor_fit_error_i <- animate_mechanism_error(coor_optim, 
			replace='joint.coor', fit.points=fit.points[, , optim_use], mechanism=mechanism, 
			input.param=input_param_optim, input.joint=input.joint, input.body=input.body, 
			fit.wts=fit.wts, n.input=n_input, coor.vectors=coor_vectors)

		# Run optimization
		coor_fit <- tryCatch(
			expr={
				nlminb(start=coor_optim, objective=animate_mechanism_error, 
					lower=-coor_optim_bounds_add, upper=coor_optim_bounds_add, 
					replace='joint.coor', fit.points=fit.points[, , optim_use], 
					mechanism=mechanism, input.param=input_param_optim, input.joint=input.joint, 
					input.body=input.body, fit.wts=fit.wts, n.input=n_input, coor.vectors=coor_vectors)
			},
			error=function(cond) {print(cond);return(NULL)},
			warning=function(cond) {print(cond);return(NULL)}
		)
		
		# Print error
		if(print.progress) cat(paste0(', ', round(coor_fit_error_i, 5), '->', round(coor_fit$objective, 5)))

		# Replace previous with optimized joint coordinates
		j <- 1
		for(i in 1:mechanism$num.joints){
			if(mechanism$joint.types[i] == 'R'){
				mechanism$joint.coor[i, ] <- mechanism$joint.coor[i, ] + colSums(coor_fit$par[j:(j+1)]*coor_vectors[[i]])
				j <- j + 2
			}else{
				mechanism$joint.coor[i, ] <- mechanism$joint.coor[i, ] + coor_fit$par[j:(j+2)]
				j <- j + 3
			}
		}


		## Optimize joint constraints
		# Convert joint constraint list into vector
		cons_optim <- c()
		cons_names <- c()
		for(i in 1:n_joints){
			if(n_cons[i] == 0) next
			cons_optim <- c(cons_optim, c(mechanism$joint.cons[[i]][, , 1]))
			cons_names <- c(cons_names, paste0(mechanism$joint.names[i], '_', 1:n_cons[i]))
		}
		
		# Set constraint vector names
		names(cons_optim) <- cons_names
		
		# Get initial error
		if(print.progress) cons_fit_error_i <- animate_mechanism_error(cons_optim, 
			replace='joint.cons', fit.points=fit.points[, , optim_use], mechanism=mechanism, 
			input.param=input_param_optim, input.joint=input.joint, input.body=input.body, 
			fit.wts=fit.wts, n.cons=n_cons)

		# Run optimization
		cons_fit <- tryCatch(
			expr={
				nlminb(start=cons_optim, objective=animate_mechanism_error, 
					lower=rep(-1, length(cons_optim)), upper=rep(1, length(cons_optim)), 
					replace='joint.cons', fit.points=fit.points[, , optim_use], 
					mechanism=mechanism, input.param=input_param_optim, input.joint=input.joint, 
					input.body=input.body, fit.wts=fit.wts, n.cons=n_cons)
			},
			error=function(cond) {print(cond);return(NULL)},
			warning=function(cond) {print(cond);return(NULL)}
		)

		# Print error
		if(print.progress) cat(paste0(', ', round(cons_fit_error_i, 5), '->', round(cons_fit$objective, 5)))
		
		# Replace previous with optimized joint coordinates
		i_params <- 1
		for(i in 1:n_joints){
			if(n_cons[i] == 0) next
			mechanism$joint.cons[[i]][, , 1] <- cons_fit$par[i_params:(i_params+n_cons[i]-1)]
			i_params <- i_params + n_cons[i]
		}


		## Optimize reference pose of each body
		# 	Don't have to run model, just create optimization function that takes input 
		# 	transformation parameters for each set of fit.points and at each iteration 
		# 	applies the corresponding transformation arrays from the mechanism run and 
		# 	compares the error
		# Run model to get updated transformations
		tmarr <- suppressWarnings(animateMechanism(mechanism, input.param=input_param_optim, 
			input.joint=input.joint, input.body=input.body, print.progress=FALSE, check.inter.joint.dist=FALSE, 
			check.joint.cons=FALSE)$tmarr)

		# Create vectors for initial and final error
		pose_fit_errors_i <- rep(NA, mechanism$num.bodies)
		pose_fit_errors_f <- rep(NA, mechanism$num.bodies)

		# Run optimization
		for(i in 1:mechanism$num.bodies){
			
			# Find initial error
			if(print.progress) pose_fit_errors_i[i] <- ref_transform_error(rep(0, 6), 
				ref.points=mechanism$body.points[mechanism$points.assoc[[i]], ],
				fit.points=fit.points[mechanism$points.assoc[[i]], , optim_use], transform=tmarr[, , i, ], 
				fit.wts=fit.wts[mechanism$points.assoc[[i]]])
			
			# Run optimization
			pose_fit <- tryCatch(
				expr={
					nlminb(start=rep(0, 6), objective=ref_transform_error, 
						lower=c(rep(-2*pi, 3), -rep(pose_optim_bounds_add, 3)), 
						upper=c(rep(2*pi, 3), rep(pose_optim_bounds_add, 3)), 
						ref.points=mechanism$body.points[mechanism$points.assoc[[i]], ],
						fit.points=fit.points[mechanism$points.assoc[[i]], , optim_use], 
						transform=tmarr[, , i, ], fit.wts=fit.wts[mechanism$points.assoc[[i]]])
				},
				error=function(cond) {print(cond);return(NULL)},
				warning=function(cond) {print(cond);return(NULL)}
			)
			
			# Save error
			pose_fit_errors_f[i] <- pose_fit$objective
			
			# Apply optimized parameters
			tmat <- diag(4)
			tmat[1:3, 1:3] <- rotationMatrixZYX(pose_fit$par[1:3])
			tmat[1:3, 4] <- pose_fit$par[4:6]
			mechanism$body.points[mechanism$points.assoc[[i]], ] <- applyTransform(mechanism$body.points[mechanism$points.assoc[[i]], ], tmat)	
		}
		
		# Set final error
		final_fit_error <- mean(pose_fit_errors_f, na.rm=TRUE)

		# Print error
		if(print.progress) cat(paste0(', ', round(mean(pose_fit_errors_i, na.rm=TRUE), 5), '->', round(final_fit_error, 5)))

		if(print.progress) cat('\n')

		# Save error after optimization
		optim_errors <- c(optim_errors, final_fit_error)

		# If near perfect fit stop - for perfect input testing
		if(final_fit_error < 1e-7) break

		# Advance iteration tracker
		optim_iter <- optim_iter + 1
	}

#	mechanism_g <<- mechanism
#	input_param_g <<- input.param
#	input_optim_g <<- input_optim
#}else{

#	input.param <- input_param_g
#	mechanism <- mechanism_g
#	input_optim <- input_optim_g
#}

	## Final optimization across all input parameters (will change slightly since coordinates, constraints, and pose fits have improved
	if(print.progress) cat(paste0('\tFinal input parameter optimization...\n'))

	# Fill in missing guesses for starting parameters with preceding parameter (could use 
	# interpolation if necessary but this seems to work pretty well)
	if(n_optim < n_iter){
		for(i in 1:nrow(input_optim)){

			# Save last non-NA value
			if(!is.na(input_optim[i, 1])) {seq_i <- input_optim[i, ];next}

			# Add last non-NA value
			input_optim[i, ] <- seq_i
		}
	}

	# Create vectors for initial and final error
	input_fit_errors_i <- rep(NA, n_iter)
	input_fit_errors_f <- rep(NA, n_iter)

	for(iter in 1:n_iter){

		# Find initial error
		if(print.progress) input_fit_errors_i[iter] <- animate_mechanism_error(input_optim[iter, ], 
			replace='input.param', fit.points=fit.points[, , iter], mechanism=mechanism, 
			input.param=input.param, input.joint=input.joint, input.body=input.body, 
			fit.wts=fit.wts, n.input=n_input)

		# Run optimization
		input_fit <- tryCatch(
			expr={
				nlminb(start=input_optim[iter, ], objective=animate_mechanism_error, 
					lower=input_optim_bounds['lower', ], upper=input_optim_bounds['upper', ], 
					replace='input.param', fit.points=fit.points[, , iter], 
					mechanism=mechanism, input.param=input.param, input.joint=input.joint, 
					input.body=input.body, fit.wts=fit.wts, n.input=n_input)
			},
			error=function(cond) {print(cond);return(NULL)},
			warning=function(cond) {print(cond);return(NULL)}
		)

		# Save error
		input_fit_errors_f[iter] <- input_fit$objective

		# Save optimized parameters
		input_optim[iter, ] <- input_fit$par
	}

	# Add input parameters into list
	n <- 1
	for(j in 1:length(input.param)){
		input.param[[j]] <- matrix(input_optim[, n:(n + n_input[j] - 1)], nrow=n_iter, ncol=n_input[j])
		n <- n + n_input[j]
	}

	# Print error
	if(print.progress) cat(paste0('\t\tError: ', round(mean(input_fit_errors_i, na.rm=TRUE), 5), '->', round(mean(input_fit_errors_f, na.rm=TRUE), 5), '\n'))
	

	## Standardize joint coordinates equivalent 
	# Slide joint coordinates, when applicable, to closest points between connected bodies
	for(i in 1:n_joints){

		if(joint.types[i] == 'S') next
		
		# Get points of bodies connected by joint
		body1_pts <- mechanism$body.points[mechanism$body.assoc == mechanism$body.conn.num[i, 1], ]
		body2_pts <- mechanism$body.points[mechanism$body.assoc == mechanism$body.conn.num[i, 2], ]

		# Find nearest points on connected bodies
		nearest_pts <- nearestPointsOnBodies(body1_pts, body2_pts)
		
		# Find mean point
		nearest_pt <- colMeans(nearest_pts)
		
		if(joint.types[i] == 'R'){

			# Find point on R-joint axis closest to nearest point between bodies
			mechanism$joint.coor[i, ] <- pointNormalOnLine(nearest_pt, mechanism$joint.coor[i, ], mechanism$joint.coor[i, ]+mechanism$joint.cons[[i]][, , 1])
		}
	}
	
	# Get run time
	time2 <- proc.time()
	proc_time <- (time2 - time1)['user.self']

	# Print run time
	if(print.progress) cat(paste0('\tTotal run time: ', round(proc_time, 2), ' sec (', round(floor(proc_time / 60)), 'm ', round(proc_time %% 60, 2),'s)\n'))

	list(
		'mechanism'=mechanism,
		'input.param'=input.param,
		'rmse.error'=input_fit_errors_f
	)
}