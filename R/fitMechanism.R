fitMechanism <- function(joint.types, body.conn, fit.points, body.assoc, input.param, 
	input.joint = NULL, input.body = NULL, joint.coor = NULL, joint.cons = NULL, 
	joint.optim = rep(TRUE, nrow(joint.coor)), fit.wts = NULL, joint.names = NULL, 
	planar = FALSE, fixed.body = 'Fixed', use.ref.as.prev = FALSE, print.progress = FALSE, 
	control = NULL, print.level = 0){

	# Start run time
	time1 <- proc.time()

	# Set indent
	indent <- '\t'
	
	if(print.progress) cat(paste0(paste0(rep(indent, print.level), collapse=''), 'fitMechanism()\n'))
	
	control_default <- list(
		'joint.coor.bounds'=0.1,
		'fit.points.bounds.factor'=0.1,
		'optim.frame.max'=21,
		'optim.to.percent'=0.001,
		'optim.iter.max'=30
	)
	
	# Overwrite default controls with inputs, if applicable
	if(is.null(control)){
		control <- control_default
	}else{
		for(i in 1:length(control))
			if(is.null(control[[names(control_default)[i]]])) control[[names(control_default)[i]]] <- control_default[[names(control_default)[i]]]
	}

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
	if(is.null(joint.coor)) joint.coor <- matrix(NA, nrow=n_joints, ncol=3, dimnames=list(joint.names, NULL))
	
	# Set joint coordinates to optimize
	joints_optim <- which(joint.optim)
	
	# Set logical whether joint coordinates will be optimized
	if(length(joints_optim) > 0) optim_joint_coor <- TRUE

	# Set NA joint constraints if no set values specified
	if(is.null(joint.cons)){
		joint.cons <- list()
		for(joint_num in 1:n_joints){
			if(joint.types[joint_num] %in% c('R', 'L', 'T')) joint.cons[[joint_num]] <- matrix(NA, 1, 3)
			if(joint.types[joint_num] %in% c('X', 'U', 'P')) joint.cons[[joint_num]] <- matrix(NA, 2, 3)
			if(joint.types[joint_num] %in% c('S')) joint.cons[[joint_num]] <- matrix(NA, 3, 3)
		}
	}

	# Save input joint constraints
	joint_cons_input <- joint.cons

	# Use define linkage just to get body.conn.num and other mechanism properties (skip path finding)
	mechanism <- defineMechanism(joint.coor=joint.coor, joint.types=joint.types, 
		joint.cons=joint.cons, body.conn=body.conn, fixed.body=fixed.body, find.paths=FALSE)

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
		cat(paste0(paste0(rep(indent, print.level+1), collapse=''), 'Total time points: ', n_iter, '\n'))
		cat(paste0(paste0(rep(indent, print.level+1), collapse=''), 'Number of time points used in optimization: ', n_optim, '\n'))
	}

	## Estimate initial joint constraints
	if(print.progress) cat(paste0(paste0(rep(indent, print.level+1), collapse=''), 'Estimating joint constraints...\n'))
	for(joint_num in 1:n_joints){
	
		if(!is.na(joint.coor[joint_num, 1]) && joint.types[joint_num] == 'S'){
			joint.cons[[joint_num]] <- diag(3)
			next
		}
		
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
		
		# Set initial fit type
		init_fit_type <- joint.types[joint_num]
		if(joint.types[joint_num] == 'U') init_fit_type <- 'R'
		
		# Get initial joint constraint estimate
		# **** Could use the angles recovered from fitJointConstraint to fill initial input parameter estimates
		fit_joint_cons <- fitJointConstraint(coor=fit_points, type=init_fit_type, print.progress=FALSE)

		# Set center of rotation
		if(joint.types[joint_num] %in% c('R', 'S', 'X', 'U'))
			if(is.na(joint.coor[joint_num, 1])) joint.coor[joint_num, ] <- fit_joint_cons$cons[1:3]

		# Set axes of rotation, filling in where initial joint constraints are not already specified
		if(joint.types[joint_num] == 'R'){
			if(is.na(joint.cons[[joint_num]][1, 1])) joint.cons[[joint_num]] <- matrix(fit_joint_cons$cons[4:6], 1, 3)
		}

		if(joint.types[joint_num] %in% c('X', 'U')){
			if(is.na(joint.cons[[joint_num]][1, 1])) joint.cons[[joint_num]] <- rbind(fit_joint_cons$cons[4:6], rep(NA, 3))
			if(is.na(joint.cons[[joint_num]][2, 1])) joint.cons[[joint_num]][2, ] <- vorthogonal(joint.cons[[joint_num]][1, ])
		}

		if(joint.types[joint_num] == 'S'){

			if(is.na(joint.cons[[joint_num]][1, 1])){

				# No input joint constraint vectors - use standard
				joint.cons[[joint_num]] <- diag(3)
				joint_cons_input[[joint_num]] <- diag(3)

			}else{

				if(is.na(joint.cons[[joint_num]][2, 1])){
					joint.cons[[joint_num]][2, ] <- vorthogonal(joint.cons[[joint_num]][1, ])
					joint.cons[[joint_num]][3, ] <- cprod(joint.cons[[joint_num]][1, ], joint.cons[[joint_num]][2, ])
				}
			}
		}
	}

	# If planar is TRUE, set joint coordinates into same plane and R-joint axes perpendicular to plane
	if(planar){
		
		if(sum(!joint.optim) <= 1){

			# Find normal vector to plane
			v123 <- cprod(joint.coor[3, ]-joint.coor[1, ], joint.coor[2, ]-joint.coor[1, ])
			v124 <- cprod(joint.coor[4, ]-joint.coor[1, ], joint.coor[3, ]-joint.coor[2, ])

			plane_nvector <- uvector(colMeans(rbind(v123, v124)))

		}else{
		
			# ****** Temp fix
			joints_plane <- which(!joint.optim)
			if(length(joints_plane) == 3){
				v123 <- joint.coor[joints_plane[3], ] - joint.coor[joints_plane[1], ]
				v124 <- joint.coor[joints_plane[2], ] - joint.coor[joints_plane[1], ]
			}else{
				v123 <- joint.coor[joints_plane[4], ] - joint.coor[joints_plane[1], ]
				v124 <- joint.coor[joints_plane[3], ] - joint.coor[joints_plane[1], ]
			}

			plane_nvector <- uvector(cprod(v123, v124))
		}
		
		# Check if a joint is fixed
		if(sum(!joint.optim) > 0){

			# Set joint center as point in plane
			plane_point <- joint.coor[which(!joint.optim)[1], ]

		}else{

			# Set joint center as point in plane
			plane_point <- colMeans(joint.coor)
		}

		# Set all R-joint axes to normal vector
		for(joint_num in 1:n_joints){

			# Project joint coordinates into plane
			joint.coor[joint_num, ] <- pointPlaneProj(joint.coor[joint_num, ], plane_point, plane_nvector)

			if(joint.types[joint_num] == 'R') joint.cons[[joint_num]] <- plane_nvector
		}
	}

	# Re-define linkage with initial joint coordinate and constraints
	mechanism <- defineMechanism(joint.coor=joint.coor, joint.types=joint.types, 
		joint.cons=joint.cons, body.conn=body.conn, fixed.body=fixed.body)

	# If input.joint is NULL and a single joint, set sole joint as input
	if(is.null(input.joint)){
		if(n_joints > 1) stop("If the mechanism has more than one joint 'input.joint' must be specified.")
		input.joint <- 1
	}

	# If input.joint is non-numeric, convert to numeric equivalent
	if(!is.numeric(input.joint[1])){
		if(sum(!input.joint %in% mechanism$joint.names) > 0) stop("'input.joint' names do not match joint names.")
		input_joint_num <- rep(NA, length(input.joint))
		for(i in 1:length(input.joint)) input_joint_num[i] <- which(mechanism$joint.names == input.joint[i])
		input.joint <- input_joint_num
	}

	## Prepare input parameter optimization vectors
	if(print.progress) cat(paste0(paste0(rep(indent, print.level+1), collapse=''), 'Preparing optimization parameters...\n'))

	# Create vector of input parameters (start with first iteration)
	input_optim_vec <- c()
	input_optim_names <- c()

	# Create matrix of bounds for input parameters
	input_optim_bounds <- matrix(NA, nrow=2, ncol=0, dimnames=list(c('lower', 'upper'), NULL))

	# Convert input.param into list of matrices for consistency
	if(class(input.param) == 'numeric') input.param <- list(matrix(input.param, nrow=n_inputs, ncol=1))
	if(class(input.param) == 'matrix') input.param <- list(input.param)
	if(class(input.param) == 'list'){
		for(i in 1:length(input.param)) if(is.vector(input.param[[i]])) input.param[[i]] <- matrix(input.param[[i]], nrow=length(input.param[[i]]), ncol=1)
	}

	# Create vector for number of input parameters per input
	n_input <- rep(NA, length(input.param))
	
	# Create optimization input parameter
	input_param_optim <- list()

	# Fill vector with initial values
	for(i in 1:length(input.joint)){
		
		# Set number of input parameters per joint
		if(is.vector(input.param[[i]])){
			n_input[i] <- 1
		}else{
			n_input[i] <- ncol(input.param[[i]])
		}

		# Create optimization input parameters
		input_param_optim[[i]] <- matrix(NA, nrow=n_optim, ncol=n_input[i])
		
		# Set input value
		if(joint.types[input.joint[i]] %in% c('R', 'S', 'U', 'X')){
			input_value <- -0.1
			input_bounds <- c(-6*pi, 6*pi)
		}

		# Fill vectors and matrices
		input_optim_vec <- c(input_optim_vec, rep(input_value, n_input[i]))
		input_optim_names <- c(input_optim_names, paste0(joint.types[input.joint[i]], input.joint[i], '_', 1:n_input[i]))
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
	# Create boundary matrix for each joint in joints_optim
	joints_optim_initial <- matrix(mechanism$joint.coor[joints_optim, ], nrow=joints_optim, ncol=3, dimnames=list(joint.names[joints_optim], NULL))

	coor_optim <- c()
	for(i in joints_optim){
	
		# Create sequence of values to add to joint coordinates
		if(planar){
			if(joint.types[i] == 'R') coor_optim <- c(coor_optim, rep(0,3))
		}else{
			if(joint.types[i] == 'R'){
				coor_optim <- c(coor_optim, rep(0,2))
			}else{
				coor_optim <- c(coor_optim, rep(0,3))
			}
		}
	}

	## Prepare joint constraint optimization vectors
	# Create vector to specify which parameters will be optimized
	cons_fill <- c()
	cons_optim <- matrix(NA, nrow=0, ncol=3)
	for(i in 1:n_joints){
		for(j in 1:nrow(joint_cons_input[[i]])){
			if(is.na(joint_cons_input[[i]][j, 1])){

				if(joint.types[i] == 'R'){
					cons_fill <- c(cons_fill, 'v')
					cons_optim <- rbind(cons_optim, joint.cons[[i]][j, ])
				}
				if(joint.types[i] %in% c('X', 'U') && j == 1){
					cons_fill <- c(cons_fill, 'v')
					cons_optim <- rbind(cons_optim, joint.cons[[i]][j, ])
				}
				if(joint.types[i] %in% c('U', 'X', 'S') && j == 2){
					cons_fill <- c(cons_fill, 'vo')
					cons_optim <- rbind(cons_optim, c(0, NA, NA))
				}
				if(joint.types[i] %in% c('S') && j == 3){
					cons_fill <- c(cons_fill, 'cprod')
					cons_optim <- rbind(cons_optim, c(NA, NA, NA))
				}

			}else{

				cons_fill <- c(cons_fill, 'n')
				cons_optim <- rbind(cons_optim, rep(NA, 3))
			}
		}
	}
	

	# Create vector of upper and lower bounds
	cons_optim_bounds_low <- matrix(NA, nrow=length(cons_fill), ncol=3)
	cons_optim_bounds_upp <- matrix(NA, nrow=length(cons_fill), ncol=3)
	for(i in 1:length(cons_fill)){
		if(cons_fill[i] == 'v'){
			cons_optim_bounds_low[i, ] <- -1
			cons_optim_bounds_upp[i, ] <- 1
		}
		if(cons_fill[i] == 'vo'){
			cons_optim_bounds_low[i, 1] <- -6*pi
			cons_optim_bounds_upp[i, 1] <- 6*pi
		}
	}
	
	# Determine whether joint constraints will be optimized
	if(sum(!cons_fill %in% c('n', 'cprod')) > 0){ optim_joint_cons <- TRUE }else { optim_joint_cons <- FALSE }

	# Print joint constraint optimization parameters
	if(print.progress){
		cat(paste0(paste0(rep(indent, print.level+2), collapse=''), 'Joint constraint optimization:\n'))
		n <- 1
		for(i in 1:n_joints){
			for(j in 1:nrow(joint_cons_input[[i]])){
				cat(paste0(paste0(rep(indent, print.level+3), collapse=''), joint.names[i], '-', j, ': '))
				if(cons_fill[n] %in% c('n', 'cprod')){
					if(cons_fill[n] == 'n') cat(paste0('Using input values {', paste0(round(joint.cons[[i]][j, ], 3), collapse=','), '}'))
					if(cons_fill[n] == 'cprod') cat(paste0('Will be filled with cross product of ', joint.names[i], '-', j-2, ' and ', joint.names[i], '-', j-1))
				}else{
					if(cons_fill[n] == 'v') cat('Optimizing 3-unit vector')
					if(cons_fill[n] == 'vo') cat(paste0('Optimizing vector orthogonal to ', joint.names[i], '-', j-1))
					cat(paste0(' with initial value(s) {', paste0(round(na.omit(cons_optim[n, ]), 3), collapse=','), '}'))
					cat(paste0(', lower bounds {', paste0(round(na.omit(cons_optim_bounds_low[n, ]), 3), collapse=','), '}'))
					cat(paste0(', and upper bounds {', paste0(round(na.omit(cons_optim_bounds_upp[n, ]), 3), collapse=','), '}'))
				}
				cat('\n')
				n <- n + 1
			}
		}
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
		fit.wts <- setNames(rep(1, nrow(mechanism$body.points)), rownames(mechanism$body.points))
	}

	# Sort fit points to match mechanism$body.points
	fit.points=fit.points[rownames(mechanism$body.points), , ]

	# Draw animation
	#drawMechanism(mechanism, file=paste0('Fit mechanism.html'), 
	#	window.title='Fit mechanism', animate.reverse=FALSE)

	if(print.progress){
		optimizing_which <- c(TRUE, optim_joint_coor, optim_joint_cons, TRUE)
		cat(paste0(paste0(rep(indent, print.level+1), collapse=''), 'Optimizing ', 
			paste0(c('input parameters', 'joint coordinates', 'constraint parameters', 'fit point reference poses')[optimizing_which], collapse=', '), 
			'...\n'))
	}

	# Set difference threshold at which optimization will stop, as proportion of mean fit point range
	#opt_to_diff <- control$optim.to.percent*mean(fit_points_range)

	# Initial setting of optimization sequence parameters
	optim_iter <- 0
	#optim_errors <- c(mean(fit_points_range)*1000, mean(fit_points_range)*1000-opt_to_diff*2)
	optim_errors <- c(10000, 1000)

#if(FALSE){
	# Cycle optimizing the input parameters, joint constraints and coordinates, and body 
	#	pose until error changes less than difference threshold between consecutive 
	# 	optimization steps
	while(abs(diff(tail(optim_errors, 2)) / optim_errors[length(optim_errors)-1])*100 > control$optim.to.percent && optim_iter < control$optim.iter.max){
	
		if(print.progress) cat(paste0(paste0(rep(indent, print.level+2), collapse=''), optim_iter+1, ') Errors: '))

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
				n.input=n_input, use.ref.as.prev=use.ref.as.prev)

			# Run optimization
			input_fit <- tryCatch(
				expr={
					nlminb(start=input_optim[iter, ], objective=animate_mechanism_error, 
						lower=input_optim_bounds['lower', ], upper=input_optim_bounds['upper', ], 
						replace='input.param', fit.points=fit.points[, , iter], 
						mechanism=mechanism, input.param=input.param, input.joint=input.joint, 
						input.body=input.body, fit.wts=fit.wts, n.input=n_input, 
						use.ref.as.prev=use.ref.as.prev)
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

		#return(list('mechanism'=mechanism, 'input.param'=input_param_optim, 'rmse.error'=input_fit_errors_f))

		## Optimize mechanism orientation
		if(planar && FALSE){

			# Find initial error
			if(print.progress) mtfm_fit_error_i <- mechanism_transform_error(rep(0, 1), 
				fit.points=fit.points[, , optim_use], mechanism=mechanism, 
				input.param=input_param_optim, input.joint=input.joint, input.body=input.body, 
				fit.wts=fit.wts, center=mechanism$joint.coor[!joint.optim, ])

			# Run optimization
			mtfm_fit <- tryCatch(
				expr={
					nlminb(start=rep(0, 1), objective=mechanism_transform_error, 
						lower=c(rep(-2*pi, 1)), 
						upper=c(rep(2*pi, 1)), 
						fit.points=fit.points[, , optim_use], mechanism=mechanism, 
						input.param=input_param_optim, input.joint=input.joint, input.body=input.body, 
						fit.wts=fit.wts, center=mechanism$joint.coor[!joint.optim, ])
				},
				error=function(cond) {print(cond);return(NULL)},
				warning=function(cond) {print(cond);return(NULL)}
			)
			
			# Print error
			if(print.progress) cat(paste0(', ', round(mtfm_fit_error_i, 5), '->', round(mtfm_fit$objective, 5)))
			
			# Apply optimized parameters
			tmat1 <- tmat2 <- tmat3 <- diag(4)
			tmat1[1:3, 4] <- mechanism$joint.coor[which(!joint.optim)[1], ]
			tmat2[1:3, 1:3] <- tMatrixEP(mechanism$joint.coor[which(!joint.optim)[4], ]-mechanism$joint.coor[which(!joint.optim)[1], ], mtfm_fit$par)
			tmat3[1:3, 4] <- -mechanism$joint.coor[which(!joint.optim)[1], ]
			tmat <- tmat1 %*% tmat2 %*% tmat3
			
			# Apply transformation to joint constraints
			for(i in 1:mechanism$num.joints){
				if(mechanism$joint.types[i] == 'R'){
					cons_vec <- rbind(mechanism$joint.coor[i,], mechanism$joint.coor[i,]+mechanism$joint.cons[[i]][, , 1])
					cons_vec_t <- applyTransform(cons_vec, tmat)
					mechanism$joint.cons[[i]][, , 1] <- cons_vec_t[2, ] - cons_vec_t[1, ]
				}
			}

			# Apply transformation to joint coordinates
			mechanism$joint.coor <- applyTransform(mechanism$joint.coor, tmat)	
		}

		## Optimize joint coordinates
		if(optim_joint_coor){

			# Create list for vector components
			coor_vectors <- as.list(rep(NA, n_joints))
			coor_optim_bounds_low <- c()
			coor_optim_bounds_upp <- c()
			for(j in joints_optim){

				if(planar){
					#vo <- vorthogonal(mechanism$joint.cons[[j]][, , 1])
					#coor_vectors[[j]] <- rbind(uvector(vo), uvector(cprod(vo, mechanism$joint.cons[[j]][, , 1])))
				}else{

					if(joint.types[j] == 'R'){
						vo <- vorthogonal(mechanism$joint.cons[[j]][, , 1])
						coor_vectors[[j]] <- rbind(uvector(vo), uvector(cprod(vo, mechanism$joint.cons[[j]][, , 1])))
					}else{
						coor_vectors[[j]] <- diag(3)
					}
				}

				# Set upper and lower bound for parameter to add to vector
				for(k in 1:nrow(coor_vectors[[j]])){
				
					# Project initial position onto vector
					pnl <- pointNormalOnLine(joints_optim_initial[j, ], mechanism$joint.coor[j, ], mechanism$joint.coor[j, ]+coor_vectors[[j]][k, ])
					
					# Find bounds on vector from projection
					int_sl <- rbind(
						pnl - control$joint.coor.bounds*coor_vectors[[j]][k, ],
						pnl + control$joint.coor.bounds*coor_vectors[[j]][k, ]
					)
					
					# Find distance from current position and bounds
					dpp <- distPointToPoint(mechanism$joint.coor[j, ], int_sl)
					
					# Try scaling vector toward one intersection
					int_scale <- mechanism$joint.coor[j, ] + dpp[1]*coor_vectors[[j]][k, ]

					# Set upper and lower bounds in each direction, depending on which direction the positive vector points
					if(distPointToPoint(int_scale, int_sl[1, ]) < 1e-10){
						coor_optim_bounds_low <- c(coor_optim_bounds_low, -dpp[2])
						coor_optim_bounds_upp <- c(coor_optim_bounds_upp, dpp[1])
					}else{
						coor_optim_bounds_low <- c(coor_optim_bounds_low, -dpp[1])
						coor_optim_bounds_upp <- c(coor_optim_bounds_upp, dpp[2])
					}
				}
			}
			
			# Get initial error
			if(print.progress) coor_fit_error_i <- animate_mechanism_error(coor_optim, 
				replace='joint.coor', planar=planar, fit.points=fit.points[, , optim_use], mechanism=mechanism, 
				input.param=input_param_optim, input.joint=input.joint, input.body=input.body, 
				fit.wts=fit.wts, coor.vectors=coor_vectors, joint.optim=joints_optim, 
				use.ref.as.prev=use.ref.as.prev)

			# Run optimization
			coor_fit <- tryCatch(
				expr={
					nlminb(start=coor_optim, objective=animate_mechanism_error, 
						lower=coor_optim_bounds_low, upper=coor_optim_bounds_upp, 
						replace='joint.coor', planar=planar, fit.points=fit.points[, , optim_use], 
						mechanism=mechanism, input.param=input_param_optim, input.joint=input.joint, 
						input.body=input.body, fit.wts=fit.wts, coor.vectors=coor_vectors, 
						joint.optim=joints_optim, use.ref.as.prev=use.ref.as.prev)
				},
				error=function(cond) {print(cond);return(NULL)},
				warning=function(cond) {print(cond);return(NULL)}
			)
		
			# Print error
			if(print.progress) cat(paste0(', ', round(coor_fit_error_i, 5), '->', round(coor_fit$objective, 5)))

			# Replace previous with optimized joint coordinates
			j <- 1
			for(i in joints_optim){
				if(length(coor_vectors[[i]]) == 1){
					mechanism$joint.coor[i, ] <- mechanism$joint.coor[i, ] + coor_fit$par[j:(j+2)]
					j <- j + 3
				}else{
					if(nrow(coor_vectors[[i]]) == 1){
						mechanism$joint.coor[i, ] <- mechanism$joint.coor[i, ] + colSums(coor_fit$par[j]*coor_vectors[[i]])
						j <- j + 1
					}else if(nrow(coor_vectors[[i]]) == 2){
						mechanism$joint.coor[i, ] <- mechanism$joint.coor[i, ] + colSums(coor_fit$par[j:(j+1)]*coor_vectors[[i]])
						j <- j + 2
					}
				}

				if(planar){
					mechanism$joint.coor[i, ] <- pointPlaneProj(mechanism$joint.coor[i, ], 
						p=mechanism$joint.coor[1, ], n=mechanism$joint.cons[[i]][, , 1])
				}

				#print(distPointToPoint(joints_optim_initial[i, ], mechanism$joint.coor[i, ]))
			}
			
			# No need to update coor_optim because joint coordinates are updated and coor_optim restarts at 0
		}

		#return(list('mechanism'=mechanism, 'input.param'=input_param_optim, 'rmse.error'=input_fit_errors_f))

		## Optimize joint constraints
		if(!planar && optim_joint_cons){
		
			# Convert optimization parameters to vector
			cons_optim_v <- na.omit(c(cons_optim))
			cons_optim_bounds_low_v <- na.omit(c(cons_optim_bounds_low))
			cons_optim_bounds_upp_v <- na.omit(c(cons_optim_bounds_upp))

			# Get initial error
			if(print.progress) cons_fit_error_i <- animate_mechanism_error(cons_optim_v, 
				replace='joint.cons', fit.points=fit.points[, , optim_use], mechanism=mechanism, 
				input.param=input_param_optim, input.joint=input.joint, input.body=input.body, 
				fit.wts=fit.wts, cons.fill=cons_fill, use.ref.as.prev=use.ref.as.prev)

			# Run optimization
			cons_fit <- tryCatch(
				expr={
					nlminb(start=cons_optim_v, objective=animate_mechanism_error, 
						lower=cons_optim_bounds_low_v, upper=cons_optim_bounds_upp_v, 
						replace='joint.cons', fit.points=fit.points[, , optim_use], 
						mechanism=mechanism, input.param=input_param_optim, input.joint=input.joint, 
						input.body=input.body, fit.wts=fit.wts, cons.fill=cons_fill, use.ref.as.prev=use.ref.as.prev)
				},
				error=function(cond) {print(cond);return(NULL)},
				warning=function(cond) {print(cond);return(NULL)}
			)

			# Print error
			if(print.progress) cat(paste0(', ', round(cons_fit_error_i, 5), '->', round(cons_fit$objective, 5)))
			#if(print.progress) cat(paste0('(', paste0(round(cons_fit$par,3), collapse=','), ')'))
	
			# Replace previous with optimized joint constraints
			k <- 1
			n <- 1
			for(i in 1:length(mechanism$joint.cons)){
				for(j in 1:nrow(mechanism$joint.cons[[i]])){
					if(cons_fill[k] == 'v'){
						mechanism$joint.cons[[i]][j, , 1] <- cons_fit$par[n:(n+2)]
						n <- n + 3
					}else if(cons_fill[k] == 'vo'){
						mechanism$joint.cons[[i]][j, , 1] <- rotateBody(m=mechanism$joint.cons[[i]][j, , 1], v=mechanism$joint.cons[[i]][j-1, , 1], a=cons_fit$par[n])
						n <- n + 1
					}else if(cons_fill[k] == 'cprod'){
						mechanism$joint.cons[[i]][j, , 1] <- cprod(mechanism$joint.cons[[i]][j-2, , 1], mechanism$joint.cons[[i]][j-1, , 1])
					}
					k <- k + 1
				}
			}
		}

		#return(list('mechanism'=mechanism, 'input.param'=input_param_optim, 'rmse.error'=input_fit_errors_f))


		## Optimize reference pose of each body
		# 	Don't have to run model, just create optimization function that takes input 
		# 	transformation parameters for each set of fit.points and at each iteration 
		# 	applies the corresponding transformation arrays from the mechanism run and 
		# 	compares the error
		# Run model to get updated transformations
		tmarr <- suppressWarnings(animateMechanism(mechanism, input.param=input_param_optim, 
			input.joint=input.joint, input.body=input.body, use.ref.as.prev=use.ref.as.prev, 
			print.progress=FALSE, check.inter.joint.dist=FALSE, check.joint.cons=FALSE)$tmarr)

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

		# Save error after optimization
		optim_errors <- c(optim_errors, final_fit_error)

		if(print.progress && optim_iter > 0) cat(paste0(' (', round(abs(diff(tail(optim_errors, 2)) / optim_errors[length(optim_errors)-1])*100, 2), '% change)'))

		if(print.progress) cat('\n')

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
	if(print.progress) cat(paste0(paste0(rep(indent, print.level+1), collapse=''), 'Final input parameter optimization...\n'))

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
			fit.wts=fit.wts, n.input=n_input, use.ref.as.prev=use.ref.as.prev)

		# Run optimization
		input_fit <- tryCatch(
			expr={
				nlminb(start=input_optim[iter, ], objective=animate_mechanism_error, 
					lower=input_optim_bounds['lower', ], upper=input_optim_bounds['upper', ], 
					replace='input.param', fit.points=fit.points[, , iter], 
					mechanism=mechanism, input.param=input.param, input.joint=input.joint, 
					input.body=input.body, fit.wts=fit.wts, n.input=n_input, 
					use.ref.as.prev=use.ref.as.prev)
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
	if(print.progress) cat(paste0(paste0(rep(indent, print.level+2), collapse=''), 'Error: ', round(mean(input_fit_errors_i, na.rm=TRUE), 5), '->', round(mean(input_fit_errors_f, na.rm=TRUE), 5), '\n'))

#	mechanism_g <<- mechanism
#}

#mechanism <- mechanism_g

	## Standardize joint coordinates equivalent 
	# Slide joint coordinates, when applicable, to closest points between connected bodies
	if(!planar){
		for(i in 1:n_joints){

			if(!joint.types[i] %in% c('R')) next
		
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
	}
	
	# Get run time
	time2 <- proc.time()
	proc_time <- (time2 - time1)['user.self']

	# Print run time
	
	if(print.progress){
		optim_iter_exceeded <- ''
		if(optim_iter >= control$optim.iter.max) optim_iter_exceeded <- ' (maximum number of iterations met)'
		cat(paste0(paste0(rep(indent, print.level+1), collapse=''), 'Total number of optimization iterations: ', optim_iter ,'', optim_iter_exceeded, '\n'))
		cat(paste0(paste0(rep(indent, print.level+1), collapse=''), 'Total run time: ', round(proc_time, 2), ' sec (', round(floor(proc_time / 60)), 'm ', round(proc_time %% 60, 2),'s)\n'))
	}

	list(
		'mechanism'=mechanism,
		'fit.points'=fit.points,
		'input.param'=input.param,
		'rmse.error'=input_fit_errors_f,
		'input.joint'=input.joint, 
		'input.body'=input.body,
		'use.ref.as.prev'=use.ref.as.prev
	)
}