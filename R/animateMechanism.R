animateMechanism <- function(mechanism, input.param, input.joint = NULL, input.body = NULL, 
	check.inter.joint.dist = TRUE, check.joint.cons = TRUE, check.inter.point.dist = TRUE, 
	print.progress = FALSE){
	
	# CONVERT INPUT.PARAM INTO LIST OF MATRICES FOR CONSISTENCY ACROSS LINKAGES WITH DIFFERING DEGREES OF FREEDOM
	if(class(input.param) == 'numeric') input.param <- list(matrix(input.param, nrow=length(input.param), ncol=1))
	if(class(input.param) == 'matrix') input.param <- list(input.param)
	if(class(input.param) == 'list'){
		for(i in 1:length(input.param)) if(is.vector(input.param[[i]])) input.param[[i]] <- matrix(input.param[[i]], nrow=length(input.param[[i]]), ncol=1)
	}
	
	# SET NUMBER OF ITERATIONS
	n_iter <- nrow(input.param[[1]])
	
	# SET ITERATIONS TO PRINT WITH PRINT PROGRESS
	if(print.progress){
		print.progress.iter <- 1
		indent <- '  '
	}
	
	# SET NUMBER OF JOINTS
	n_joints <- nrow(mechanism$joint.coor)

	# SET NUMBER OF BODIES
	n_bodies <- mechanism$num.bodies

	# CONVERT ARRAY TO MATRIX - COPY OVER LAST DIMENSION OF ARRAY
	if(length(dim(mechanism$joint.coor)) == 3) mechanism$joint.coor <- mechanism$joint.coor[, , dim(mechanism$joint.coor)[3]]

	# CONVERT JOINT CONSTRAINTS INTO ARRAYS FOR CHANGING CONSTRAINT VECTORS
	# ADD ITERATIONS TO JOINT CONSTRAINT ARRAY FOR CHANGING CONSTRAINT PARAMETERS
	joint_cons <- list()
	for(i in 1:length(mechanism$joint.cons)){
		if(is.na(mechanism$joint.cons[[i]][1])){joint_cons[[i]] <- NULL;next}
		joint_cons[[i]] <- array(mechanism$joint.cons[[i]], dim=c(dim(mechanism$joint.cons[[i]])[1:2], n_iter))
	}

	# GET JOINT NAMES
	joint_names <- rownames(mechanism$joint.coor)

	# COPY COORDINATES AND CONVERT TO ARRAY
	joint_coor <- array(mechanism$joint.coor, dim=c(dim(mechanism$joint.coor), n_iter), dimnames=list(joint_names, colnames(mechanism$joint.coor), NULL))
	if(!is.null(mechanism$body.points)) body_points <- array(mechanism$body.points, dim=c(dim(mechanism$body.points), n_iter), dimnames=list(rownames(mechanism$body.points), NULL, NULL))

	# IF INPUT.JOINT IS NON-NUMERIC, CONVERT TO NUMERIC EQUIVALENT
	if(is.null(input.joint)){
		if(n_joints > 1) stop("If the mechanism has more than one joint 'input.joint' must be specified.")
		input.joint <- 1
	}
	if(!is.numeric(input.joint[1])){
		if(sum(!input.joint %in% joint_names) > 0) stop("'input.joint' names do not match joint names.")
		input_joint_num <- rep(NA, length(input.joint))
		for(i in 1:length(input.joint)) input_joint_num[i] <- which(joint_names == input.joint[i])
		input.joint <- input_joint_num
	}

	# CHECK THAT INPUT.PARAM LENGTH MATCHES INPUT.JOINT LENGTH
	if(length(input.param) != length(input.joint)) stop(paste0("The length of input.param (", length(input.param), ") must match the number of input.joint (", length(input.joint), ")."))

	# IF INPUT BODY IS NULL
	if(is.null(input.body)){

		# CHECK WHETHER THERE ARE OPEN CHAIN JOINTS
		if(!is.null(mechanism$joints.open)){
		
			# FIND JOINTS IN OPEN CHAIN
			joints_open_in <- input.joint[input.joint %in% mechanism$joints.open]

			# REMAINING JOINTS
			joints_remaining <- input.joint[!input.joint %in% joints_open_in]

			if(length(joints_remaining) > 0){
				if(sum(!joints_remaining %in% mechanism$fixed.joints) > 0) stop("If 'input.body' is NULL all joints in 'input.joint' must be either connected to the fixed body or be in an open joint chain.")
			}

		}else{

			# CHECK THAT ALL INPUT JOINTS ARE CONNECTED TO FIXED BODY
			if(sum(!input.joint %in% mechanism$fixed.joints) > 0) stop("If 'input.body' is NULL all joints in 'input.joint' must be connected to the fixed body so that the input.body can be designated as the corresponding mobile body.")
		}

		# FIND MOBILE LINK ATTACHED TO JOINT
		input.body <- rep(NA, length(input.joint))
		for(i in 1:length(input.joint)) input.body[i] <- max(mechanism$body.conn.num[input.joint[i], ])

	}else{

		if(!is.numeric(input.body[1])){
			
			# MAKE SURE THAT ALL BODY NAMES ARE FOUND
			if(sum(!input.body %in% mechanism$body.names) > 0) stop("Names in 'input.body' do not match body names.")
			
			# FIND NUMBER CORRESPONDING TO BODY
			for(i in 1:length(input.body)) input.body[i] <- which(input.body[i] == mechanism$body.names)
			input.body <- as.numeric(input.body)
		}
	}

	# SET DEFAULT LINKAGE SIZE
	linkage_size <- 1
	
	# GET LINKAGE SIZE (IF MORE THAN ONE JOINT AND JOINTS ARE NOT THE SAME
	if(n_joints > 1 && sum(apply(mechanism$joint.coor, 2, 'sd', na.rm=TRUE)) > 1e-5){
		linkage_size <- mean(sqrt(rowSums((mechanism$joint.coor - matrix(colMeans(mechanism$joint.coor), nrow=n_joints, ncol=3, byrow=TRUE))^2)))
	}

	if(print.progress) cat(paste0('animateMechanism()\n'))

	# SET PREVIOUS ITERATION
	prev_iter <- 1
	
	# CREATE ARRAY OF TRANSFORMATION MATRICES FOR EACH BODY AND ITERATION
	tmarr <- array(diag(4), dim=c(4,4,n_bodies,n_iter))

	# CREATE INPUT BODY LIST
	input_body_list <- as.list(input.body)

	# ADD OPEN CHAIN BODIES TO TRANSFORM FOR EACH INPUT PARAMETER
	if(!is.null(mechanism$body.transform)){
		for(i in 1:length(input.joint)){
			body_transform <- sort(unique(c(input_body_list[[i]], mechanism$body.transform[[input.joint[i]]])))
			input_body_list[[i]] <- body_transform[!is.na(body_transform)]
		}
	}

	# DEFAULT
	print_progress_iter <- FALSE

	#
	for(iter in 1:n_iter){
	
		# SET WHETHER TO PRINT PROGRESS FOR CURRENT ITERATION
		if(print.progress && iter %in% print.progress.iter){
			print_progress_iter <- TRUE
		}else{
			print_progress_iter <- FALSE
		}
	
		# PRINT ITERATION
		if(print_progress_iter) cat(paste0(paste0(rep(indent, 1), collapse=''), 'Iteration: ', iter, '\n'))

		# RESET TRANSFORM VECTORS
		joint_change <- setNames(rep(FALSE, n_joints), joint_names)

		if(print_progress_iter) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Input transformations\n'))

		# APPLY INPUT PARAMETERS
		for(input_num in 1:length(input.param)){
		
			# RESET TRANSFORMATION MATRICES
			tmat1 <- tmat2 <- tmat3 <- diag(4)

			if(print_progress_iter) cat(paste0(paste0(rep(indent, 3), collapse=''), 'Input transformation at joint ', joint_names[input.joint[input_num]], '(', input.joint[input_num], ') (type:', mechanism$joint.types[input.joint[input_num]], ') '))
			
			# CREATE TRANSFORMATION MATRIX
			if(mechanism$joint.types[input.joint[input_num]] == 'N'){

				# TRANSLATE TO CENTER OF ROTATION (JOINT)
				tmat1 <- cbind(rbind(diag(3), rep(0,3)), c(joint_coor[input.joint[input_num], , iter], 1))

				# APPLY ROTATION TO TRANSFORMATION MATRIX
				tmat2 <- cbind(rbind(rotationMatrixZYX(-input.param[[input_num]][iter, 4:6]), rep(0,3)), c(0,0,0,1))

				# TRANSLATE BACK FROM CENTER OF ROTATION
				tmat3 <- cbind(rbind(diag(3), rep(0,3)), c(-joint_coor[input.joint[input_num], , iter]+input.param[[input_num]][iter, 1:3], 1))

			}else if(mechanism$joint.types[input.joint[input_num]] == 'R'){

				if(print_progress_iter) cat(paste0('{CoR:', paste0(round(joint_coor[input.joint[input_num], , iter], 2), collapse=','), '; AoR:', paste0(round(joint_cons[[input.joint[input_num]]][1, , iter], 2), collapse=','), '; angle:', round(input.param[[input_num]][iter, 1], 2),'} '))

				# TRANSLATE TO CENTER OF ROTATION (JOINT)
				tmat1 <- cbind(rbind(diag(3), rep(0,3)), c(joint_coor[input.joint[input_num], , iter], 1))

				# APPLY ROTATION TO TRANSFORMATION MATRIX
				tmat2 <- cbind(rbind(tMatrixEP(joint_cons[[input.joint[input_num]]][, , iter], -input.param[[input_num]][iter, 1]), rep(0,3)), c(0,0,0,1))

				# TRANSLATE BACK FROM CENTER OF ROTATION
				tmat3 <- cbind(rbind(diag(3), rep(0,3)), c(-joint_coor[input.joint[input_num], , iter], 1))

			}else if(mechanism$joint.types[input.joint[input_num]] == 'L'){

				# TRANSLATE TO CENTER OF ROTATION (JOINT)
				tmat1 <- cbind(rbind(diag(3), rep(0,3)), c(input.param[[input_num]][iter, 1]*joint_cons[[input.joint[input_num]]][, , iter], 1))
			}

			# COMBINE TRANSFORMATION MATRICES
			tmat <- tmat1 %*% tmat2 %*% tmat3

			if(print_progress_iter) cat('\n')

			if(print_progress_iter) cat(paste0(paste0(rep(indent, 4), collapse=''), 'Apply to joint(s): ', paste0(sort(joint_names[mechanism$joints.tform[[input.joint[input_num]]]]), collapse=', '), '\n'))

			# APPLY TO JOINTS IN SAME BODY AND IN DESCENDANT OPEN CHAIN
			joint_coor[mechanism$joints.tform[[input.joint[input_num]]], , iter] <- applyTransform(joint_coor[mechanism$joints.tform[[input.joint[input_num]]], , iter], tmat)

			#
			joint_change[mechanism$joints.tform[[input.joint[input_num]]]] <- TRUE

			if(print_progress_iter) cat(paste0(paste0(rep(indent, 4), collapse=''), 'Apply to body/bodies: ', paste0(sort(mechanism$body.names[input_body_list[[input_num]]]), collapse=', '), '\n'))

			# APPLY TO EACH BODY
			for(body_num in input_body_list[[input_num]]) tmarr[, , body_num, iter] <- tmat %*% tmarr[, , body_num, iter]
		}

		if(print_progress_iter) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Joint(s) transformed by input parameters: ', paste0(sort(names(joint_change)[joint_change]), collapse=', '), '\n'))
		if(print_progress_iter) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Path solving\n'))

		# 
		path_cycle <- 1
		while(path_cycle < 4){

			unknown_changed <- FALSE

			for(i in 1:length(mechanism$paths.closed)){
		
				# SET PATH
				path <- mechanism$paths.closed[[i]]
				
				# SKIP IF ALL JOINTS ARE KNOWN
				if(sum(joint_change[path]) == length(path)) next

				# PRINT PATH DETAILS
				if(print_progress_iter) cat(paste0(paste0(rep(indent, 3), collapse=''), 'Path ', i, ': '))
				
				# SET SOLVE JOINT PATH
				solve_joint_path <- solveJointPath(joint.types=mechanism$joint.types[path], 
					joint.change=joint_change[path], joint.coor=joint_coor[path, , iter], 
					joint.cons=joint_cons[path], joint.names=dimnames(mechanism$joint.coor)[[1]][path],
					joint.dist=mechanism$paths.closed.dist[[i]], joint.prev=joint_coor[path, , prev_iter],
					joint.init=joint_coor[path, , 1], iter=iter, print.progress=print_progress_iter, indent=indent)

				if(is.null(solve_joint_path)) next
				
				if(print_progress_iter){
					apply_to_path_joints <- c()
					apply_to_bodies <- c()
				}
				
				# APPLY JOINT TRANSFORMATIONS
				for(j in 1:length(solve_joint_path$joint.tmat)){

					if(length(solve_joint_path$joint.tmat[[j]]) == 1) next

					if(print_progress_iter) apply_to_path_joints <- c(apply_to_path_joints, path[j])
					
					# APPLY TO PATH JOINTS
					joint_coor[path[j], , iter] <- applyTransform(joint_coor[path[j], , iter], solve_joint_path$joint.tmat[[j]])

					# APPLY TO JOINT CONSTRAINT
					joint_cons_point <- rbind(joint_coor[path[j], , iter], joint_coor[path[j], , iter]+joint_cons[[path[j]]][, , iter])
					joint_cons_point <- applyTransform(joint_cons_point, solve_joint_path$joint.tmat[[j]])
					joint_cons[[path[j]]][, , iter] <- joint_cons_point[2, ]-joint_cons_point[1, ]
				}

				if(print_progress_iter){
					cat(paste0(paste0(rep(indent, 4), collapse=''), 'Apply to path joint(s): '))
					cat(paste0(paste0(sort(joint_names[apply_to_path_joints]), collapse=', '), '\n'))
					apply_to_joints <- c()
				}

				# APPLY BODY TRANSFORMATIONS
				for(j in 1:length(solve_joint_path$body.tmat)){
				
					if(length(solve_joint_path$body.tmat[[j]]) == 1) next

					# APPLY TO BODY BETWEEN JOINTS IN PATH
					tmarr[, , mechanism$paths.closed.bodies[[i]][j], iter] <- solve_joint_path$body.tmat[[j]] %*% tmarr[, , mechanism$paths.closed.bodies[[i]][j], iter]
					
					if(print_progress_iter) apply_to_bodies <- c(apply_to_bodies, mechanism$paths.closed.bodies[[i]][j])

					# GET JOINTS ASSOCIATED WITH BODY
					body_joints <- mechanism$body.joints[[mechanism$paths.closed.bodies[[i]][j]]]
					
					# REMOVE JOINTS IN PATH
					body_joints <- body_joints[!body_joints %in% path]

					if(length(body_joints) == 0) next

					# APPLY TO JOINTS ASSOCIATED WITH BODY BUT NOT IN PATH
					joint_coor[body_joints, , iter] <- applyTransform(joint_coor[body_joints, , iter], solve_joint_path$body.tmat[[j]])

					if(print_progress_iter) apply_to_joints <- c(apply_to_joints, body_joints)
				}

				if(print_progress_iter){
					cat(paste0(paste0(rep(indent, 4), collapse=''), 'Apply to body/bodies: '))
					cat(paste0(paste0(sort(mechanism$body.names[apply_to_bodies]), collapse=', '), '\n'))
					cat(paste0(paste0(rep(indent, 4), collapse=''), 'Apply to body-associated joint(s): '))
					cat(paste0(paste0(sort(joint_names[apply_to_joints]), collapse=', '), '\n'))
				}
			}

			path_cycle <- path_cycle + 1

			if(!unknown_changed) break
		}

		# SET PREVIOUS ITERATION
		prev_iter <- iter
	}

	# APPLY BODY TRANSFORMATIONS TO POINTS
	if(!is.null(mechanism$body.points)){

		for(body_num in 1:length(mechanism$points.assoc)){

			if(is.na(mechanism$points.assoc[[body_num]][1])) next
			
			body_points[mechanism$points.assoc[[body_num]], , ] <- applyTransform(body_points[mechanism$points.assoc[[body_num]], , ], tmarr[, , body_num, ])
		}
	}

	mechanism_r <- mechanism
	
	mechanism_r$joint.coor <- joint_coor
	mechanism_r$joint.cons <- joint_cons
	mechanism_r$body.points <- body_points

	return(mechanism_r)
}
