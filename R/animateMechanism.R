animateMechanism <- function(mechanism, input.param, input.joint = NULL, input.body = NULL, 
	check.inter.joint.dist = TRUE, check.joint.cons = TRUE, check.inter.point.dist = TRUE, 
	print.progress = FALSE){

	if(FALSE){
		tA <- 0
		tAA <- 0
		tAB <- 0
		tABA <- 0
		tABB <- 0
		tABBA <- 0
		tABBB <- 0
		tABBC <- 0
		tAC <- 0
		tAD <- 0
		tA1 <- proc.time()['user.self']
		tAA1 <- proc.time()['user.self']
	}

	if(print.progress) cat(paste0('animateMechanism()\n'))

	# SET NUMBER OF INPUT PARAMETERS
	n_inputs <- length(input.param)

	# CONVERT INPUT.PARAM INTO LIST OF MATRICES FOR CONSISTENCY ACROSS LINKAGES WITH DIFFERING DEGREES OF FREEDOM
	if(class(input.param) == 'numeric') input.param <- list(matrix(input.param, nrow=n_inputs, ncol=1))
	if(class(input.param) == 'matrix') input.param <- list(input.param)
	if(class(input.param) == 'list'){
		for(i in 1:n_inputs) if(is.vector(input.param[[i]])) input.param[[i]] <- matrix(input.param[[i]], nrow=length(input.param[[i]]), ncol=1)
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

	# CREATE JOINT COORDINATE AND CONSTRAINT REFERENCE
	joint_ref <- mechanism$joint.coor
	joint_cons_ref <- mechanism$joint.cons

	# GET JOINT NAMES
	joint_names <- rownames(mechanism$joint.coor)

	# CONVERT JOINT CONSTRAINTS INTO ARRAYS FOR CHANGING CONSTRAINT VECTORS
	# ADD ITERATIONS TO JOINT CONSTRAINT ARRAY FOR CHANGING CONSTRAINT PARAMETERS
	# COPY CONSTRAINTS TO TWO SEPARATE ARRAYS (EACH JOINT MOVED WITH BOTH BODIES)
	joint_cons <- list()
	joint_consn <- list()
	for(i in 1:length(mechanism$joint.cons)){
		if(is.na(mechanism$joint.cons[[i]][1])){joint_cons[[i]] <- NULL;next}
		joint_cons[[i]] <- array(mechanism$joint.cons[[i]], dim=c(dim(mechanism$joint.cons[[i]])[1:2], n_iter))
		joint_consn[[i]] <- array(mechanism$joint.cons[[i]], dim=c(dim(mechanism$joint.cons[[i]])[1:2], n_iter, 2))
	}

	# COPY COORDINATES TO TWO SEPARATE JOINT COORDINATE ARRAYS (EACH JOINT MOVED WITH BOTH BODIES)
	joint_coorn <- array(mechanism$joint.coor, dim=c(dim(mechanism$joint.coor), n_iter, 2), dimnames=list(joint_names, colnames(mechanism$joint.coor), NULL, NULL))
	
	# CREATE MATRIX TO TRACK JOINT STATUS
	joint_status_init <- matrix('', nrow=n_joints, ncol=2, dimnames=list(joint_names, colnames(mechanism$body.conn.num)))
	joint_status_init[mechanism$body.conn.num == 1] <- 'f'

	# SET JOINT CHANGE STATUS DEFAULT
	joint_status_change <- joint_status_init
	joint_status_change[joint_status_change == ''] <- 't'

	# COPY BODY POINTS AND CONVERT TO ARRAY
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
	if(n_inputs != length(input.joint)) stop(paste0("The length of input.param (", n_inputs, ") must match the number of input.joint (", length(input.joint), ")."))

	## FIND DEFAULT INPUT BODIES
	# CHECK WHETHER THERE ARE OPEN CHAIN JOINTS
	if(!is.null(mechanism$joints.open)){
	
		# FIND JOINTS IN OPEN CHAIN
		joints_open_in <- input.joint[input.joint %in% mechanism$joints.open]

		# REMAINING JOINTS
		joints_remaining <- input.joint[!input.joint %in% joints_open_in]
	}

	# IF INPUT BODY IS NULL CREATE LIST
	if(is.null(input.body)){
		input_body <- as.list(rep(NA, length(input.joint)))
	}else{

		# CHECK THAT THE LENGTH OF INPUT.BODY MATCHES THE LENGTH OF INPUT.JOINT
		if(length(input.body) != length(input.joint)) stop(paste0("The length of input.body (", length(input.body), ") should match the length of input.joint (", length(input.joint), ")"))

		input_body <- input.body
	}

	# FILL IN UNKNOWNS IN INPUT BODY LIST
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

	# SET JOINTS TRANSFORMED BY EACH BODY TRANSFORMATION
	joints_transform <- list(setNames(as.list(rep(NA, n_bodies)), mechanism$body.names), setNames(as.list(rep(NA, n_bodies)), mechanism$body.names))

	# FILL INPUT JOINT LIST (JOINTS TRANSFORMED BY EACH BODY)
	for(jt_set in 1:2){
		for(body_num in 1:n_bodies){
			joints_transform[[jt_set]][[body_num]] <- which(mechanism$body.conn.num[, jt_set] %in% body_num)
		}
	}
	
	# SET DEFAULT LINKAGE SIZE
	linkage_size <- 1
	
	# GET LINKAGE SIZE (IF MORE THAN ONE JOINT AND JOINTS ARE NOT THE SAME
	if(n_joints > 1 && sum(apply(mechanism$joint.coor, 2, 'sd', na.rm=TRUE)) > 1e-5){
		linkage_size <- mean(sqrt(rowSums((mechanism$joint.coor - matrix(colMeans(mechanism$joint.coor), nrow=n_joints, ncol=3, byrow=TRUE))^2)))
	}
	
	# CREATE LIST OF INPUT PARAMETERS BY JOINT
	joint.input <- setNames(as.list(rep(NA, n_joints)), joint_names)
	joint.input[input.joint] <- input.param
	
	# CREATE NEW PATH LISTS TO INCLUDE INPUT TRANSFORMATIONS
	new_path_len <- n_inputs + length(mechanism$paths.closed)
	paths <- as.list(rep(NA, new_path_len))
	paths_dist <- as.list(rep(NA, new_path_len))
	paths_bodies <- as.list(rep(NA, new_path_len))
	paths_input <- as.list(rep(NA, new_path_len))

	# ADD PATHS TO NEW PATH LISTS
	paths[(n_inputs+1):new_path_len] <- mechanism$paths.closed
	paths_dist[(n_inputs+1):new_path_len] <- mechanism$paths.closed.dist
	paths_bodies[(n_inputs+1):new_path_len] <- mechanism$paths.closed.bodies

	# ADD INPUT PARAMETERS TO PATHS
	paths[1:n_inputs] <- input.joint
	paths_bodies[1:n_inputs] <- input_body
	
	#
	for(i in 1:length(paths)){
		if(length(paths[[i]]) == 1){
			paths_input[i] <- joint.input[paths[[i]]]
		}else{
			paths_input[i] <- list(joint.input[paths[[i]]])
		}
	}		

	# SET PREVIOUS ITERATION
	prev_iter <- 1
	
	# CREATE ARRAY OF TRANSFORMATION MATRICES FOR EACH BODY AND ITERATION
	tmarr <- array(diag(4), dim=c(4,4,n_bodies,n_iter))

	# DEFAULT
	print_progress_iter <- FALSE

	#tAA <- tAA + proc.time()['user.self'] - tAA1
	#tAB1 <- proc.time()['user.self']

	# LOOP THROUGH EACH ITERATION
	for(iter in 1:n_iter){
	
		#tABA1 <- proc.time()['user.self']

		# SET WHETHER TO PRINT PROGRESS FOR CURRENT ITERATION
		if(print.progress && iter %in% print.progress.iter){
			print_progress_iter <- TRUE
		}else{
			print_progress_iter <- FALSE
		}
	
		# PRINT ITERATION
		if(print_progress_iter) cat(paste0(paste0(rep(indent, 1), collapse=''), 'Iteration: ', iter, '\n'))

		# RESET TRANSFORM VECTORS
		joint_status <- joint_status_init

		#tABA <- tABA + proc.time()['user.self'] - tABA1
		#tABB1 <- proc.time()['user.self']

		# CYCLE THROUGH PATHS
		path_cycle <- 1
		while(path_cycle < 4){

			# RESET STATUS CHANGED FLAG
			status_changed <- FALSE
			
			# SET STARTING PATH
			if(path_cycle == 1){ start_path <- 1 }else{ start_path <- n_inputs + 1 }

			for(i in start_path:length(paths)){
		
				#tABBA1 <- proc.time()['user.self']

				# SET PATH
				path <- paths[[i]]
				
				# SET PATH AS INPUT
				is_input <- ifelse(i <= n_inputs, TRUE, FALSE)
				
				# PRINT PATH DETAILS
				if(print_progress_iter){
					if(i == n_inputs+1) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Path solving (Cycle ', path_cycle, ')\n'))
					if(path_cycle == 1 && i == 1) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Input transformations\n'))
					if(is_input){
						path_print <- paste0('Input transformation ', tolower(as.roman(i)), ' to ', mechanism$body.names[paths_bodies[[i]]], '(', paths_bodies[[i]], ') body at joint ', dimnames(mechanism$joint.coor)[[1]][path], '(', path, ') of type: ', mechanism$joint.types[path], ' ')
					}else{
						path_print <- paste0('Path ', i - n_inputs, ': ', paste0(mechanism$joint.types[path], collapse='-'), ' (', paste0(joint_names[path], collapse='-'), ')\n')
					}
					cat(paste0(paste0(rep(indent, 3), collapse=''), path_print))
				}

				# SKIP IF ALL JOINTS ARE KNOWN
				#if(sum(joint_status[path, ] == '') == 0){
				#	next
				#}

				# COPY TRANSFORMATIONS ACROSS '_,t' JOINTS IN PATH
				for(j in 1:length(path)){

					# CHECK THAT STATUS IS _,t
					if(sum(joint_status[path[j], ] == '') != 1 || sum(joint_status[path[j], ] == 't') != 1) next
					
					# FIND TRANSFORMED BODY
					t_body <- mechanism$body.conn.num[path[j], which(joint_status[path[j], ] == 't')]

					# FIND UNTRANSFORMED BODY
					u_body <- mechanism$body.conn.num[path[j], which(joint_status[path[j], ] == '')]
					
					# COPY TRANSFORMATION TO UNTRANSFORMED BODY
					tmarr[, , u_body, iter] <- tmarr[, , t_body, iter]

					# EXTEND TRANSFORMATION ACROSS _,i JOINTS
					# If transforming body at an input joint having a '_,t' status (or if any 
					# joints in the path have that status), apply transformation that was 
					# applied to input joint to input body first
					extend <- extendTransformation(tmarr=tmarr, body.num=u_body, iter=iter, 
						joints.transform=joints_transform, joint.coor=joint_coorn, joint.cons=joint_consn, 
						joint.ref=joint_ref, joint.cons.ref=joint_cons_ref, joint.types=mechanism$joint.types, joint.status=joint_status, status.to='c', 
						body.names=mechanism$body.names, joint.names=joint_names, 
						indent=indent, indent.level=4,  print.progress=print_progress_iter)
					
					# SAVE RESULT
					joint_coorn <- extend$joint.coor
					joint_consn <- extend$joint.cons
					joint_status <- extend$joint.status
					tmarr <- extend$tmarr

					if(print_progress_iter){
						joint_status_print <- joint_status
						joint_status_print[joint_status_print == ''] <- '_'
						cat('')
						for(k in 1:nrow(joint_status_print)) cat(paste0(paste0(rep(indent, 5), collapse=''), joint_names[k], ': ', paste0(joint_status_print[k,], collapse=','), '\n'))
					}
					
					status_changed <- TRUE
				}

				#tABBA <- tABBA + proc.time()['user.self'] - tABBA1
				#tABBB1 <- proc.time()['user.self']

				# SOLVE OR APPLY INPUT TRANSFORMATIONS
				solve_joint_path <- solveJointPath(joint.types=mechanism$joint.types[path], 
					joint.status=joint_status[path, ], joint.coor=joint_coorn[path, , iter, ], 
					joint.cons=joint_consn[path], body.num=paths_bodies[[i]], 
					input=paths_input[[i]],
					body.conn=mechanism$body.conn.num[path, ], 
					joint.names=dimnames(mechanism$joint.coor)[[1]][path],
					joint.prev=joint_coorn[path, , prev_iter, 1],						#joint.dist=paths_dist[[i]], 
					joint.ref=mechanism$joint.coor[path, ], iter=iter, 
					print.progress=print_progress_iter, indent=indent)
				
				#tABBB <- tABBB + proc.time()['user.self'] - tABBB1

				# IF NO TRANSFORMATION FOUND, SKIP
				if(is.null(solve_joint_path)) next

				#tABBC1 <- proc.time()['user.self']
				
				# SET OUTPUT JOINT STATUS
				joint_status_out <- joint_status_change
				joint_status_out[path, ] <- solve_joint_path$joint.status

				# APPLY BODY TRANSFORMATIONS
				for(j in 1:length(solve_joint_path$body.tmat)){
				
					if(length(solve_joint_path$body.tmat[[j]]) == 1 && is.na(solve_joint_path$body.tmat[[j]])) next

					# APPLY TO BODY BETWEEN JOINTS IN PATH
					tmarr[, , paths_bodies[[i]][j], iter] <- solve_joint_path$body.tmat[[j]] %*% tmarr[, , paths_bodies[[i]][j], iter]

					# EXTEND TRANSFORMATION ACROSS _,i JOINTS
					extend <- extendTransformation(tmarr=tmarr, body.num=paths_bodies[[i]][j], iter=iter, 
						joints.transform=joints_transform, joint.coor=joint_coorn, joint.cons=joint_consn, 
						joint.ref=joint_ref, joint.cons.ref=joint_cons_ref, joint.types=mechanism$joint.types, joint.status=joint_status, status.to=joint_status_out, 
						body.names=mechanism$body.names, joint.names=joint_names, 
						indent=indent, indent.level=4,  print.progress=print_progress_iter)

					# SAVE RESULT
					joint_coorn <- extend$joint.coor
					joint_consn <- extend$joint.cons
					joint_status <- extend$joint.status
					tmarr <- extend$tmarr

					status_changed <- TRUE
				}

				if(print_progress_iter){
					#joint_status_print <- joint_status
					#joint_status_print[joint_status_print == ''] <- '_'
					#for(k in 1:nrow(joint_status_print)) cat(paste0(paste0(rep(indent, 5), collapse=''), joint_names[k], ': ', paste0(joint_status_print[k,], collapse=','), '\n'))
				}

				#tABBC <- tABBC + proc.time()['user.self'] - tABBC1
			}

			# INCREASE PATH CYCLE COUNT
			path_cycle <- path_cycle + 1

			# BREAK IF STATUS DID NOT CHANGE
			if(!status_changed) break
		}

		# SET PREVIOUS ITERATION
		prev_iter <- iter

		#tABB <- tABB + proc.time()['user.self'] - tABB1
	}

	#tAB <- tAB + proc.time()['user.self'] - tAB1
	#tAC1 <- proc.time()['user.self']

	# APPLY BODY TRANSFORMATIONS TO POINTS
	if(!is.null(mechanism$body.points)){

		for(body_num in 1:length(mechanism$points.assoc)){

			if(is.na(mechanism$points.assoc[[body_num]][1])) next
			
			body_points[mechanism$points.assoc[[body_num]], , ] <- applyTransform(body_points[mechanism$points.assoc[[body_num]], , ], tmarr[, , body_num, ])
		}
	}

	#tAC <- tAC + proc.time()['user.self'] - tAC1
	#tAD1 <- proc.time()['user.self']
	
	# ****ADD***** CHECK THAT BOTH JOINT COORDINATE SETS ARE THE SAME

	# COMBINE TWO JOINT COORDINATE SETS INTO ONE
	joint_coor <- joint_coorn[, , , 1]

	# IF SINGLE ITERATION, CONVERT TO 3 DIM ARRAY
	if(length(dim(joint_coor)) == 2) joint_coor <- array(joint_coor, dim=c(dim(joint_coor), 1), dimnames=list(dimnames(joint_coor)[[1]], NULL, NULL))

	joint_cons <- joint_consn
	for(i in 1:length(joint_cons)){
		if(is.na(mechanism$joint.cons[[i]][1])){joint_cons[[i]] <- NULL;next}
		if(length(dim(joint_consn[[i]][, , , 1])) == 2){
			joint_cons[[i]] <- array(joint_consn[[i]][, , , 1], dim=c(1,3,n_iter))
		}else{
			joint_cons[[i]] <- joint_consn[[i]][, , , 1]
		}
	}
	
	# If single iteration convert to 3D array
	for(i in 1:length(joint_cons)){
		if(length(dim(joint_cons[[i]])) == 0){
			joint_cons[[i]] <- array(joint_cons[[i]], dim=c(1, length(joint_cons[[i]]), 1))
		}else if(length(dim(joint_cons[[i]])) == 2){
			joint_cons[[i]] <- array(joint_cons[[i]], dim=c(dim(joint_cons[[i]]), 1))
		}
	}

	# CHECK THAT LINK MOTIONS OBEY JOINT CONSTRAINTS
	if(check.joint.cons && dim(joint_coor)[3] > 1){
		
		for(body_num in 2:n_bodies){
			
			# FIND JOINTS ASSOCIATED WITH BODY
			assoc_joints <- c(joints_transform[[1]][[body_num]], joints_transform[[2]][[body_num]])
			
			# CHECK DISTANCE OF JOINTS IN SAME BODY FROM R-JOINT AXIS OVER TIME
			if('R' %in% mechanism$joint.types[assoc_joints]){
			
				# CREATE VECTOR WITH ONLY ASSOCIATED JOINT TYPES
				joint_types <- mechanism$joint.types
				joint_types[!1:length(joint_types) %in% assoc_joints] <- NA

				# GET R JOINTS
				R_joints <- which(joint_types == 'R')
				
				for(R_joint in R_joints){
					
					# GET POINTS ON R AXIS
					R_axis1 <- t(joint_coor[R_joint, , ])
					R_axis2 <- t(joint_coor[R_joint, , ]) + matrix(joint_cons[[R_joint]], n_iter, 3, byrow=TRUE)
					
					# GET DISTANCES FROM EACH JOINT TO R-JOINT AXIS
					for(joint_num in assoc_joints){

						distances <- rep(NA, length=n_iter)

						for(iter in 1:n_iter){
							distances[iter] <- distPointToLine(joint_coor[joint_num, , iter], R_axis1[iter, ], R_axis2[iter, ])
						}

						if(sd(distances, na.rm=TRUE) > 1e-10) warning(paste0('R-joint motion constraint not obeyed: distance between joint ', joint_names[joint_num], ' and R-axis of joint ', joint_names[R_joint], ' is non-constant.'))
					}
				}
			}
		}
	}

	# CHECK THAT DISTANCES WITHIN LINKS HAVE NOT CHANGED
	if(check.inter.joint.dist && n_joints > 1 && dim(joint_coor)[3] > 1){

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
	mechanism_r$joint.coorn <- joint_coorn
	mechanism_r$joint.cons <- joint_cons
	mechanism_r$joint.consn <- joint_consn
	mechanism_r$body.points <- body_points
	mechanism_r$tmarr <- tmarr

	#tAD <- tAD + proc.time()['user.self'] - tAD1
	#tA <- tA + proc.time()['user.self'] - tA1

	#user_times <<- rbind(user_times, c('tA'=tA, 'tAA'=tAA, 'tAB'=tAB, 'tABA'=tABA, 'tABB'=tABB, 'tABBA'=tABBA, 'tABBB'=tABBB, 'tABBC'=tABBC, 'tAC'=tAC, 'tAD'=tAD))

	return(mechanism_r)
}