fitMechanism <- function(joint.types, body.conn, fit.points, body.assoc, input.param, 
	input.joint, input.body, joint.names = NULL, fixed.body = 'Fixed', optim.frame.max = 100, 
	print.progress = FALSE){
	
	# Use motion between two bodies connected by each joint to generate initial joint 
	# constraint estimate

	# Make sure that row names are unique
	if(!is.null(rownames(fit.points))) if(length(unique(rownames(fit.points))) != nrow(fit.points)) stop("Row names in 'fit.points' must be unique.")

	# Get number of iterations
	n_iter <- dim(fit.points)[3]
	
	# Set reference iteration
	ref.iter <- 1

	# Set NA joint coordinates
	joint.coor <- matrix(NA, nrow=length(joint.types), ncol=3, dimnames=list(joint.names, NULL))
	
	# Set NA joint constraints
	joint.cons <- setNames(as.list(rep(NA, length(joint.types))), joint.names)

	# Use define linkage just to get body.conn.num and other mechanism properties (skip path finding)
	mechanism <- defineMechanism(joint.coor=joint.coor, joint.types=joint.types, 
		joint.cons=joint.cons, body.conn=body.conn, find.paths=FALSE)

	# Create numeric body association
	body_assoc_num <- rep(NA, length(body.assoc))
	for(i in 1:length(body_assoc_num)) body_assoc_num[i] <- which(body.assoc[i] == mechanism$body.names)

	# If number of iterations exceeds optim.frame.max, limit to optim.frame.max
	optim_use <- 1:dim(fit.points)[3]
	#if(dim(fit.points)[3] > optim.frame.max){ }

	# Estimate initial joint constraints
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

		# If no fixed link
		}else{

			# Fix body2 points relative to body1 (need to input body1+body2 points)
			fit_points_rel <- immobilize(fit.points[unlist(body_points), , ], fit.points[body_points[[1]], , 1])
			
			# Save body2 points only
			fit_points <- fit_points_rel[dimnames(fit.points)[[1]][body_points[[2]]], , ]
		}

		# Get initial joint constraint estimate
		fit_joint_cons <- fitJointConstraint(coor=fit_points, 
			type=joint.types[joint_num], print.progress=print.progress)

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
		if(sum(!input.joint %in% joint.names) > 0) stop("'input.joint' names do not match joint names.")
		input_joint_num <- rep(NA, length(input.joint))
		for(i in 1:length(input.joint)) input_joint_num[i] <- which(joint.names == input.joint[i])
		input.joint <- input_joint_num
	}

	# Create vector of input parameters (start with first iteration)
	start_init_vec <- c()
	start_init_names <- c()

	# Create matrix of bounds for input parameters
	start_init_bounds <- matrix(NA, nrow=2, ncol=0, dimnames=list(c('lower', 'upper'), NULL))

	# Fill vector with initial values
	for(i in 1:length(input_joint_num)){
		if(joint.types[input_joint_num[i]] == 'R'){
			start_init_vec <- c(start_init_vec, 0.1)
			start_init_names <- c(start_init_names, paste0(joint.types[input_joint_num[i]], input_joint_num[i], '_', 1))
			start_init_bounds <- cbind(start_init_bounds, c(-6*pi, 6*pi))
		}
		if(joint.types[input_joint_num[i]] == 'S'){
			start_init_vec <- c(start_init_vec, rep(0.1, 3))
			start_init_names <- c(start_init_names, paste0(joint.types[input_joint_num[i]], input_joint_num[i], '_', 1:3))
			start_init_bounds <- cbind(start_init_bounds, rbind(rep(-6*pi, 3), rep(-6*pi, 3)))
		}
	}
	
	# Convert to matrix with row for each iteration
	start_init <- matrix(start_init_vec, nrow=n_iter, ncol=length(start_init_vec), byrow=TRUE, dimnames=list(NULL, start_init_names))
	
	# Add column names to bounds matrix
	colnames(start_init_bounds) <- colnames(start_init)

	# Set initial value for reference iteration to 0 - this will also be optimized
	start_init[ref.iter, ] <- 0

	# Draw animation
	#drawMechanism(mechanism, file=paste0('Fit mechanism.html'), 
	#	window.title='Fit mechanism', animate.reverse=FALSE)

	# Cycle optimizing the input parameters, joint constraints and coordinates, and body 
	#	pose until error changes less than difference threshold between consecutive 
	# 	optimization steps
	n_run <- 0
	while(n_run < 1){
	
		## Optimize input parameters
		input_fit_errors <- rep(NA, length(optim_use))
		for(i in 1:length(optim_use)){
		
			# Set iteration
			iter <- optim_use[i]

			# Run optimization
			input_fit <- tryCatch(
				expr={
					nlminb(start=start_init[iter,], objective=animate_mechanism_error, 
						lower=start_init_bounds['lower', ], upper=start_init_bounds['upper', ], 
						fit.points=fit.points[, , iter], mechanism=mechanism, 
						input.joint=input.joint, input.body=input.body)
				},
				error=function(cond) {print(cond);return(NULL)},
				warning=function(cond) {print(cond);return(NULL)}
			)
			
			break
		}

return(1)
		n_run <- n_run + 1

		if(TRUE){


			# Save results
			optim_param[optim_use[i], ] <- input_fit$par
			input_fit_errors[i] <- input_fit$objective
		}

		## Optimize joint constraints and coordinates

		## Optimize body pose
		
	}

	# Optimize:
	#	joint constraints and coordinates
	#	input parameters
	#	fit points pose for each body
}