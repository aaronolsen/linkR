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
		print.progress.iter <- 2
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

	## FIND DEFAULT INPUT BODIES
	# CHECK WHETHER THERE ARE OPEN CHAIN JOINTS
	if(!is.null(mechanism$joints.open)){
	
		# FIND JOINTS IN OPEN CHAIN
		joints_open_in <- input.joint[input.joint %in% mechanism$joints.open]

		# REMAINING JOINTS
		joints_remaining <- input.joint[!input.joint %in% joints_open_in]

		#if(length(joints_remaining) > 0){
		#	if(sum(!joints_remaining %in% mechanism$fixed.joints) > 0) stop("If 'input.body' is NULL all joints in 'input.joint' must be either connected to the fixed body or be in an open joint chain.")
		#}

	}else{

		# CHECK THAT ALL INPUT JOINTS ARE CONNECTED TO FIXED BODY
		#if(sum(!input.joint %in% mechanism$fixed.joints) > 0) stop("If 'input.body' is NULL all joints in 'input.joint' must be connected to the fixed body so that the input.body can be designated as the corresponding mobile body.")
	}

	# IF INPUT BODY IS NULL CREATE LIST
	if(is.null(input.body)){
		input_body <- as.list(rep(NA, length(input.joint)))
	}else{

		# CHECK THAT THE LENGTH OF INPUT.BODY MATCHES THE LENGTH OF INPUT.JOINT
		if(length(input.body) != length(input.joint)) stop(paste0("The length of input.body (", length(input.body), ") should match the length of input.joint (", length(input.joint), ")"))

		input_body <- input.body
	}
	
	# FILL EMPTY ELEMENTS IN LIST
	for(i in 1:length(input_body)){
	
		if(!is.na(input_body[[i]][1])){

			for(j in 1:length(input_body[[i]])){

				if(is.numeric(input_body[[i]][j])) next

				# GET NON-NA VALUES
				input_body_nna <- input_body[!is.na(input_body)]
			
				# MAKE SURE THAT ALL BODY NAMES ARE FOUND
				if(!input_body[[i]][j] %in% mechanism$body.names) stop(paste0("Name '", input_body[[i]][j], "' in 'input.body' not found in body names."))
			
				# FIND NUMBER CORRESPONDING TO BODY
				input_body[[i]][j] <- which(input_body[[i]][j] == mechanism$body.names)
			}

			# MAKE NUMERIC
			input_body[[i]] <- as.numeric(input_body[[i]])

			next
		}

		if(1 %in% mechanism$body.conn.num[input.joint[i], ]){
		
			# LINK OTHER THAN 1 (FIXED)
			input_body[[i]] <- max(mechanism$body.conn.num[input.joint[i], ])
		}else{

			# CHOOSE MAX FOR NOW - PERHAPS NOT NECESSARY IN MOST CASES SINCE INPUT AT JOINT 
			# WITHIN CLOSED LOOP LIKELY USED FOR SPECIFIC PATH FRAGMENTS IN SOLVING PATH
			input_body[[i]] <- max(mechanism$body.conn.num[input.joint[i], ])
		}

		if(is.null(mechanism$body.transform)) next

		# ADD OPEN CHAIN BODIES TO TRANSFORM FOR EACH INPUT PARAMETER
		body_transform <- sort(unique(c(input_body[[i]], mechanism$body.transform[[input.joint[i]]])))
		input_body[[i]] <- body_transform[!is.na(body_transform)]
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

	# DEFAULT
	print_progress_iter <- FALSE

	# CREATE LIST FOR INPUT RESOLVE PARAMETERS
	input_resolve <- list()
	for(i in 1:n_joints) input_resolve[[i]] <- NA

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
		joint_known <- setNames(rep(FALSE, n_joints), joint_names)
		joint_change <- setNames(rep(FALSE, n_joints), joint_names)

		if(print_progress_iter) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Input transformations\n'))

		# APPLY INPUT PARAMETERS
		for(input_num in 1:length(input.param)){
		
			# RESET TRANSFORMATION MATRICES
			tmat1 <- tmat2 <- tmat3 <- tmat4 <- diag(4)

			if(print_progress_iter) cat(paste0(paste0(rep(indent, 3), collapse=''), 'Input transformation at joint ', joint_names[input.joint[input_num]], '(', input.joint[input_num], ') (type:', mechanism$joint.types[input.joint[input_num]], ') '))
			
			# CREATE TRANSFORMATION MATRIX
			if(mechanism$joint.types[input.joint[input_num]] %in% c('N', 'S')){

				# TRANSLATE TO CENTER OF ROTATION (JOINT)
				tmat1 <- cbind(rbind(diag(3), rep(0,3)), c(joint_coor[input.joint[input_num], , iter], 1))

				# TRANSLATE BACK FROM CENTER OF ROTATION
				tmat3 <- cbind(rbind(diag(3), rep(0,3)), c(-joint_coor[input.joint[input_num], , iter], 1))

				if(mechanism$joint.types[input.joint[input_num]] == 'N'){

					# APPLY ROTATION TO TRANSFORMATION MATRIX
					tmat2[1:3, 1:3] <- rotationMatrixZYX(-input.param[[input_num]][iter, 4:6])

					# APPLY TRANSLATION
					tmat4 <- cbind(rbind(diag(3), rep(0,3)), c(input.param[[input_num]][iter, 1:3], 1))

				}else if(mechanism$joint.types[input.joint[input_num]] == 'S'){
				
					if(!is.null(joint_cons[[input.joint[input_num]]])){
						#if(print_progress_iter) cat(paste0('{CoR:', paste0(round(joint_coor[input.joint[input_num], , iter], 2), collapse=','), ' AoR:'))

						if(print_progress_iter){
							cat(paste0('{CoR:', paste0(round(joint_coor[input.joint[input_num], , iter], 2), collapse=','), ''))
							for(i in 1:dim(joint_cons[[input.joint[input_num]]])[1]){
								cat(paste0('; AoR:', paste0(round(joint_cons[[input.joint[input_num]]][i, , iter], 2), collapse=','), ' angle:', round(input.param[[input_num]][iter, i], 2)))
							}
							cat(paste0('}'))
						}
					}

					# LOOP THROUGH EACH COLUMNN OF INPUT PARAMETERS
					for(i in 1:ncol(input.param[[input_num]])){

						if(is.null(joint_cons[[input.joint[input_num]]]) || i > nrow(joint_cons[[input.joint[input_num]]])){

							# SAVE INPUT PARAMETERS FOR SOLVING JOINT PATH
							input_resolve[[input.joint[input_num]]] <- matrix(input.param[[input_num]][, i], ncol=1)

							if(print_progress_iter) cat(paste0('\n', paste0(rep(indent, 4), collapse=''), 'Input will be used to resolve multiple solutions during solving.'))

						}else{

							# APPLY ROTATION ABOUT SINGLE JOINT CONSTRAINT VECTOR
							tmat2[1:3, 1:3] <- tmat2[1:3, 1:3] %*% tMatrixEP(joint_cons[[input.joint[input_num]]][i, , iter], -input.param[[input_num]][iter, i])
						}
					}
				}

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
			tmat <- tmat1 %*% tmat2 %*% tmat3 %*% tmat4
			
			# SET JOINTS TO APPLY TRANSFORMATION TO
			joints_transform <- unique(unlist(mechanism$body.joints[input_body[[input_num]]]))

			if(print_progress_iter) cat(paste0('\n', paste0(rep(indent, 4), collapse=''), 'Apply to joint(s): ', paste0(sort(joint_names[joints_transform]), collapse=', '), '\n'))

			# APPLY TO JOINTS IN SAME BODY AND IN DESCENDANT OPEN CHAIN
			joint_coor[joints_transform, , iter] <- applyTransform(joint_coor[joints_transform, , iter], tmat)

			# APPLY TO JOINT CONSTRAINTS
			for(joint_num in joints_transform){

				if(is.null(joint_cons[[joint_num]][, , iter])) next
				if(mechanism$joint.types[joint_num] == 'S') next

				joint_cons_point <- rbind(joint_coor[joint_num, , iter], joint_coor[joint_num, , iter]+joint_cons[[joint_num]][, , iter])
				joint_cons_point <- applyTransform(joint_cons_point, tmat)
				joint_cons[[joint_num]][, , iter] <- joint_cons_point[2, ]-joint_cons_point[1, ]
			}

			# CHECK WHICH JOINTS HAVE CHANGED
			joint_coor_diff <- abs(joint_coor[joints_transform, , iter] - mechanism$joint.init[joints_transform, ])
			if(is.vector(joint_coor_diff)){
				joint_coor_change <- sum(joint_coor_diff) > 1e-8
			}else{
				joint_coor_change <- rowSums(joint_coor_diff) > 1e-8
			}

			#
			#if(25 %in% joints_transform[joint_coor_change]) joint_change[25] <- TRUE
			
			#
			#joints_transform[joint_coor_change] <- joints_transform[joint_coor_change][joints_transform[joint_coor_change] != 25]

			# SET JOINTS THAT DIFFER FROM REFERENCE AS CHANGED
			joint_known[joints_transform[joint_coor_change]] <- TRUE
			joint_change[joints_transform[joint_coor_change]] <- TRUE

			if(print_progress_iter) cat(paste0(paste0(rep(indent, 4), collapse=''), 'Apply to body/bodies: ', paste0(sort(mechanism$body.names[input_body[[input_num]]]), collapse=', '), '\n'))

			# APPLY TO EACH BODY
			for(body_num in input_body[[input_num]]) tmarr[, , body_num, iter] <- tmat %*% tmarr[, , body_num, iter]
		}

		if(print_progress_iter) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Joint(s) transformed to "known" by input parameters: ', paste0(sort(names(joint_known)[joint_known]), collapse=', '), '\n'))
		if(print_progress_iter) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Joint(s) transformed to "changed" by input parameters: ', paste0(sort(names(joint_change)[joint_change]), collapse=', '), '\n'))
		if(print_progress_iter) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Path solving\n'))

		# 
		path_cycle <- 1
		while(path_cycle < 4){

			unknown_changed <- FALSE

			for(i in 1:length(mechanism$paths.closed)){
		
				# SET PATH
				path <- mechanism$paths.closed[[i]]
				
				# SKIP IF ALL JOINTS ARE KNOWN
				if(sum(joint_known[path]) == length(path)) next

				# PRINT PATH DETAILS
				if(print_progress_iter) cat(paste0(paste0(rep(indent, 3), collapse=''), 'Path ', i, ': '))
				
				# SET SOLVE JOINT PATH
				solve_joint_path <- solveJointPath(joint.types=mechanism$joint.types[path], 
					joint.known=joint_known[path], joint.change=joint_change[path], joint.coor=joint_coor[path, , iter], 
					joint.cons=joint_cons[path], joint.names=dimnames(mechanism$joint.coor)[[1]][path],
					joint.dist=mechanism$paths.closed.dist[[i]], joint.prev=joint_coor[path, , prev_iter],
					joint.init=mechanism$joint.coor[path, ], input.resolve=input_resolve[path], 
					iter=iter, print.progress=print_progress_iter, indent=indent)

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

					# CHECK THAT JOINT CONSTRAINT IS NOT NULL
					if(path[j] > length(joint_cons) || is.null(joint_cons[[path[j]]])) next

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

	# CHECK THAT DISTANCES WITHIN LINKS HAVE NOT CHANGED
	if(check.inter.joint.dist && n_joints > 1){

		joint_pairs_checked <- c()
		
		for(path_num in 1:length(mechanism$paths.closed)){
			for(path_joint in 1:(length(path)-1)){
				
				# GET INDICES FOR JOINTS TO MEASURE DISTANCE
				jt_idx <- c(mechanism$paths.closed[[path_num]][path_joint], mechanism$paths.closed[[path_num]][path_joint+1])

				# CREATE STRING TO SAVE SO INTERJOINT DISTANCE IS ONLY MEASURED ONCE
				path_pair_idx <- paste0(sort(jt_idx), collapse='-')

				# CHECK IF JOINT PAIR HAS ALREADY BEEN CHECKED
				if(path_pair_idx %in% joint_pairs_checked) next

				# ADD TO JOINT PAIRS CHECKED
				joint_pairs_checked <- c(joint_pairs_checked, path_pair_idx)

				# GET JOINT TYPES
				path_pair_types <- mechanism$joint.types[jt_idx]

				# REFERENCE DISTANCE
				ref_dist <- mechanism$paths.closed.dist[[path_num]][path_joint]
				
				# FIND DISTANCE BETWEEN JOINTS OVER ANIMATION
				anim_dist <- apply(joint_coor[jt_idx, , ], 3, distPointToPoint)
				
				# FIND SD OF DIFFERENCE IN DISTANCE
				dist_sd <- abs(sd(anim_dist - ref_dist) / linkage_size)

				# SKIP NA
				if(is.na(dist_sd)) next

				# ALL DISTANCES CONSTANT
				if(dist_sd < 1e-7) next
				
				#print(path_pair_idx)
				#print(path_pair_types)
				#print(dist_sd)

				# PRINT DISTANCES THAT CHANGE
				warning(paste0("The distance between joints ", paste0(sort(joint_names[jt_idx]), collapse=" and "), " is non-constant (", round(dist_sd, 6), ")."))
			}
		}
	}

	mechanism_r <- mechanism
	
	mechanism_r$joint.coor <- joint_coor
	mechanism_r$joint.cons <- joint_cons
	mechanism_r$body.points <- body_points

	return(mechanism_r)
}