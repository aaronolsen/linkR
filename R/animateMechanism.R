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
		joint_trsfm <- setNames(rep(FALSE, n_joints), joint_names)

		if(print_progress_iter) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Input transformations\n'))

		# APPLY INPUT PARAMETERS
		for(input_num in 1:length(input.param)){
		
			# RESET TRANSFORMATION MATRICES
			tmat1 <- tmat2 <- tmat3 <- tmat4 <- diag(4)

			if(print_progress_iter) cat(paste0(paste0(rep(indent, 3), collapse=''), 'Input transformation at joint ', joint_names[input.joint[input_num]], '(', input.joint[input_num], ') (type:', mechanism$joint.types[input.joint[input_num]], ') '))
			
			# CREATE TRANSFORMATION MATRIX
			if(mechanism$joint.types[input.joint[input_num]] == 'N'){

				# TRANSLATE TO CENTER OF ROTATION (JOINT)
				tmat1 <- cbind(rbind(diag(3), rep(0,3)), c(joint_coor[input.joint[input_num], , iter], 1))

				# APPLY ROTATION TO TRANSFORMATION MATRIX
				tmat2 <- cbind(rbind(rotationMatrixZYX(-input.param[[input_num]][iter, 4:6]), rep(0,3)), c(0,0,0,1))

				# TRANSLATE BACK FROM CENTER OF ROTATION
				tmat3 <- cbind(rbind(diag(3), rep(0,3)), c(-joint_coor[input.joint[input_num], , iter], 1))
				
				# APPLY TRANSLATION
				tmat4 <- cbind(rbind(diag(3), rep(0,3)), c(input.param[[input_num]][iter, 1:3], 1))
			}

			# CREATE TRANSFORMATION MATRIX
			if(mechanism$joint.types[input.joint[input_num]] == 'R'){

				if(print_progress_iter) cat(paste0('{CoR:', paste0(round(joint_coor[input.joint[input_num], , iter], 2), collapse=','), '; AoR:', paste0(round(joint_cons[[input.joint[input_num]]][1, , iter], 2), collapse=','), '; angle:', round(input.param[[input_num]][iter, 1], 2),'} '))

				# TRANSLATE TO CENTER OF ROTATION (JOINT)
				tmat1 <- cbind(rbind(diag(3), rep(0,3)), c(joint_coor[input.joint[input_num], , iter], 1))

				# APPLY ROTATION TO TRANSFORMATION MATRIX
				tmat2 <- cbind(rbind(tMatrixEP(joint_cons[[input.joint[input_num]]][, , iter], -input.param[[input_num]][iter, 1]), rep(0,3)), c(0,0,0,1))

				# TRANSLATE BACK FROM CENTER OF ROTATION
				tmat3 <- cbind(rbind(diag(3), rep(0,3)), c(-joint_coor[input.joint[input_num], , iter], 1))
			}

			# COMBINE TRANSFORMATION MATRICES
			tmat <- tmat1 %*% tmat2 %*% tmat3 %*% tmat4

			if(print_progress_iter) cat('\n')

			if(print_progress_iter) cat(paste0(paste0(rep(indent, 4), collapse=''), 'Apply to joint(s): ', paste0(joint_names[mechanism$joints.tform[[input.joint[input_num]]]], collapse=', '), '\n'))

			# APPLY TO JOINTS IN SAME BODY AND IN DESCENDANT OPEN CHAIN
			joint_coor[mechanism$joints.tform[[input.joint[input_num]]], , iter] <- applyTransform(joint_coor[mechanism$joints.tform[[input.joint[input_num]]], , iter], tmat)

			#
			joint_trsfm[mechanism$joints.tform[[input.joint[input_num]]]] <- TRUE

			if(print_progress_iter) cat(paste0(paste0(rep(indent, 4), collapse=''), 'Apply to body/bodies: ', paste0(mechanism$body.names[input_body_list[[input_num]]], collapse=', '), '\n'))

			# APPLY TO EACH BODY
			for(body_num in input_body_list[[input_num]]) tmarr[, , body_num, iter] <- tmat %*% tmarr[, , body_num, iter]
		}

		if(print_progress_iter) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Joint(s) transformed by input parameters: ', paste0(names(joint_trsfm)[joint_trsfm], collapse=', '), '\n'))
		if(print_progress_iter) cat(paste0(paste0(rep(indent, 2), collapse=''), 'Path solving\n'))

		# 
		path_cycle <- 1
		while(path_cycle < 4){

			unknown_changed <- FALSE

			for(i in 1:length(mechanism$paths.closed)){
		
				# SET PATH
				path <- mechanism$paths.closed[[i]]
				
				# SKIP IF ALL JOINTS ARE KNOWN
				if(sum(joint_trsfm[path]) == length(path)) next

				# PRINT PATH DETAILS
				if(print_progress_iter) cat(paste0(paste0(rep(indent, 3), collapse=''), 'Path ', i, ': '))
				
				# SET SOLVE JOINT PATH
				solveJointPath(joint.types=mechanism$joint.types[path], 
					joint.transform=joint_trsfm[path], joint.coor=mechanism$joint_coor[, , iter], 
					joint.cons=joint_cons[path], joint.names=dimnames(mechanism$joint.coor)[[1]][path],
					joint.dist=mechanism$paths.closed.dist[[i]], joint.prev=joint_coor[, , prev_iter],
					joint.init=joint_coor[, , 1], print.progress=print_progress_iter, indent=indent)
			}

			path_cycle <- path_cycle + 1

			if(!unknown_changed) break
		}

		# SET PREVIOUS ITERATION
		prev_iter <- iter
	}

#print(joint_coor['J21', , ])
#print(mechanism$points.assoc)
#print(mechanism$body.names)

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



	for(itr in 1:n_iter){

		# CLEAR UNKNOWN JOINTS VECTOR
		joints_unknown <- setNames(rep("rtp", n_joints), joint_names)

		# CLEAR TRANSFORMED POINTS VECTOR
		link_points_tform <- setNames(rep(FALSE, mechanism$num.links), mechanism$link.names)

		# SET L AND P JOINTS AS UNKNOWN POSITION
		joints_unknown[mechanism$joint.types %in% c('L', 'P')] <- 't'

		# SET S JOINTS AS UNKNOWN POSITION
		joints_unknown[mechanism$joint.types %in% c('S')] <- 'p'

		# SET NON-GROUND R JOINTS AS UNKNOWN POSITION AND ROTATION
		joints_unknown[mechanism$joint.types %in% c('R', 'U')] <- 'rp'

		# SET R GROUND JOINTS AS UNKNOWN ROTATION
		joints_unknown[mechanism$fixed.joints[mechanism$joint.types[mechanism$fixed.joints] %in% c('R', 'U', 'S')]] <- 'r'

		# SET S GROUND JOINTS AS KNOWN ROTATION AND POSITION
		#joints_unknown[mechanism$fixed.joints[mechanism$joint.types[mechanism$fixed.joints] %in% c('S')]] <- ''

		# GET POINT FOR COMPARISON FROM PREVIOUS ITERATION
		if(itr == 1){joints.prev <- mechanism$joint.coor}else{joints.prev <- linkage_r$joint.coor[, , itr-1]}

		#print(joints_unknown)

		path_cycle <- 1
		while(path_cycle < 4){

			unknown_changed <- FALSE

			for(i in 1:length(mechanism$joint.paths)){
		
				# SET PATH
				path <- mechanism$joint.paths[[i]]
				
				# SKIP IF ALL JOINTS ARE KNOWN
				if(sum(joints_unknown[path] == '') == length(path)) next

				if(print.progress){
					cat(paste0('      Path ', i, ': '))
					cat(paste0(paste(mechanism$joint.types[path], collapse='-'), '; '))
					#cat(paste0(paste(joints_unknown[path], collapse='-')))
					for(k in 1:length(path)){
						#if(path[k] == 0) next
						if(joints_unknown[path[k]] == ''){cat('_')}else{cat(joints_unknown[path[k]])}
						#cat(paste0(mechanism$joint.types[path[k]], joints_unknown[path[k]]))
						#cat(paste0(mechanism$joint.types[path[k]], joints_unknown[path[k]]))
						if(k < length(path)) cat('-')
					}
					cat(paste0('; '))
					cat(paste0(paste(joint_names[path], collapse='-'), '; '))
					cat('\n')
				}
				
				#if(paste0(path, collapse='') == '234') next
				#if(paste0(path, collapse='') == '432') next
				#if(paste0(path, collapse='') == '765') next
				#if(paste0(path, collapse='') == '567') next

				solve_chain <- NULL
				if(length(path) == 1){

					# CHECK THAT INPUT JOINT IS CONNECTED TO GROUND
					if(!path[1] %in% mechanism$fixed.joints)
						stop(paste0("linkR currently only supports input parameters for joints associated with ground (", paste(joint_names[mechanism$fixed.joints], collapse=', '), ")."))

					if(print.progress) cat(paste0('         Apply input at ', mechanism$joint.types[path[1]], '-joint:\n'))

					# PATH WITH SINGLE JOINT IS INPUT PARAMETER
					if(mechanism$joint.types[path[1]] == 'R') solve_chain <- list(list('r' = input.param[[i]][itr, ]))
					if(mechanism$joint.types[path[1]] == 'L') solve_chain <- list(list('t' = uvector(mechanism$joint.cons[[path[1]]])*input.param[[i]][itr, 1]))
					if(mechanism$joint.types[path[1]] == 'P') solve_chain <- list(list('t' = input.param[[i]][itr, ]))
					if(mechanism$joint.types[path[1]] == 'U') solve_chain <- list(list('r' = input.param[[i]][itr, ]))
					if(mechanism$joint.types[path[1]] == 'S') solve_chain <- list(list('r' = input.param[[i]][itr, ]))

				}else{

					# SOLVE POSITION
					solve_chain <- tryCatch(
						expr={
							solveKinematicChain(joint.types=mechanism$joint.types[path], joints.unknown=joints_unknown[path], 
								joint.coor=linkage_r$joint.coor[path, , itr], joint.cons=joint_cons[path], 
								joints.dist=path_joint_lengths[[i]], joints.prev=joints.prev[path, ], joint.init=mechanism$joint.init[path, ],
								print.progress=print.progress)
						},
						error=function(cond){return(0)},
						warning=function(cond) return(NULL)
					)
				}

				# CHECK IF CHAIN COULD NOT BE SOLVED
				if(is.null(solve_chain)){
					#cat(paste0('\t\t\tNo solution for path\n'))
					next
				}

				# CHECK IF ERROR WAS RETURNED
				if(!is.list(solve_chain)){
					if(solve_chain == 0){
						joint_types_in <- paste(mechanism$joint.types[path], collapse='')
						if(joint_types_in == 'SSR' || joint_types_in == 'RSS') solve_chain <- list(list('r'=NA))
						#if(joint_types_in == 'LSS') solve_chain <- list(list('t'=NA))
					}
				}

				if(print.progress){
					for(k in 1:length(solve_chain)){
						cat(paste0('\t\t\t\t', mechanism$joint.types[path[k]], ''))
						for(scname in names(solve_chain[[k]])){
							cat(paste0('\t', scname, ': ', paste(round(solve_chain[[k]][[scname]], 3), collapse=', ')))
						}
						cat('\n')
					}
					#print(solve_chain)
				}
				#cat(paste0('\t', paste(mechanism$joint.types[path], collapse='', sep=''), '\t', paste(joints_unknown[path], collapse=',', sep=''), '\n'));
				#if(is.na(solve_chain[[1]][['r']])) return(1)
				
				# APPLY SOLVE TO JOINTS AND POINTS
				apply_solve_chain <- applySolveChain(linkage=linkage, linkage_r=linkage_r, 
					solve_chain=solve_chain, path=path, itr=itr, joint_cons=joint_cons,
					joints_unknown=joints_unknown, link_points_tform=link_points_tform, 
					print.progress=print.progress)

				#cat(i,'\n')

				#print(linkage_r$joint.coor[, , itr])

				linkage_r <- apply_solve_chain$linkage_r
				joint_cons <- apply_solve_chain$joint_cons
				unknown_changed <- apply_solve_chain$unknown_changed
				joints_unknown <- apply_solve_chain$joints_unknown
				link_points_tform <- apply_solve_chain$link_points_tform

				# SAVE DYNAMIC JOINT CONSTRAINT VECTORS
				for(j in 1:length(joint_cons_dyn)){
					if(is.null(joint_cons_dyn[[j]])) next
					joint_cons_dyn[[j]][itr, ] <- joint_cons[[j]]
				}
			}
			
			#cat('\n')

			path_cycle <- path_cycle + 1

			if(!unknown_changed) break
		}

		# TRANSFORM POINTS ASSOCIATED WITH UNTRANSFORMED LINKS, SKIPPING GROUND
		if(print.progress) cat('\t\tCopy transformation to points and JCSs associated with untransformed links:\n')
		for(i in 2:length(link_points_tform)){

			# SKIP ALREADY TRANSFORMED LINK POINTS
			if(link_points_tform[i]) next
			
			if(print.progress) cat(paste0('\t\t\t', mechanism$link.names[i], '\n'))
			
			# GET POINTS ASSOCIATED WITH LINK
			points_t <- mechanism$point.assoc[[mechanism$link.names[i]]]
		
			# FIND JOINTS ASSOCIATED WITH LINK
			joints_assoc <- unique(c(mechanism$joint.links[mechanism$joint.links[, 'Link.idx'] == i-1, c('Joint1', 'Joint2')]))

			# SKIP IF NO ASSOCIATED POINTS
			#if(is.null(points_t)) next
			
			# GET JOINTS FOR COPYING TRANSFORMATION
			if(length(joints_assoc) > 3){
				
				### FIX
				# SELECT JOINTS IF GREATER THAN THREE (EG TO AVOID COINCIDENT POINTS)
				joints_assoc <- joints_assoc[1:3]
			}

			# MAKE SURE THAT JOINTS ARE NOT COINCIDENT
			if(nrow(mechanism$joint.coor) > 1 && sum(abs(mechanism$joint.coor[joints_assoc, ] - matrix(colMeans(mechanism$joint.coor[joints_assoc, ]), nrow=length(joints_assoc), ncol=3, byrow=TRUE))) < 1e-7){
				joints_assoc <- joints_assoc[1]
				#stop(paste0("Joints used to copy transformation to points associated with '", mechanism$link.names[i], "' are coincident"))
			}

			# TRANSFORM LONG-AXIS ROTATION CONSTRAINTS
			if(!is.null(linkage_r$lar.cons[[mechanism$link.names[i]]]) && length(joints_assoc) == 2){
			
				# COPY TRANSFORMATION
				mr <- copyTransformation(m1=mechanism$joint.coor[joints_assoc, ], 
					m2=linkage_r$joint.coor[joints_assoc, , itr], 
					mn=rbind(mechanism$link.points[points_t, ], mechanism$link.lcs[[mechanism$link.names[i]]], mechanism$lar.cons[[mechanism$link.names[i]]]$point.i),
					lar.cons=linkage_r$lar.cons[[mechanism$link.names[i]]], 
					lar.compare=lar_points[[mechanism$link.names[i]]][prev_itr, ])

				# ADD TRANSFORMED POINTS
				if(!is.null(points_t)) linkage_r$link.points[points_t, , itr] <- mr[1:(nrow(mr)-5), ]

				# ADD TRANSFORMED ASSOCIATED LOCAL COORDINATE SYSTEM
				linkage_r$link.lcs[[mechanism$link.names[i]]][, , itr] <- mr[(nrow(mr)-4):(nrow(mr)-1), ]
				
				# ADD LONG-AXIS ROTATION REFERENCE POINT
				lar_points[[mechanism$link.names[i]]][itr, ] <- mr[nrow(mr), ]

			}else{

				# COPY TRANSFORMATION
				if(!is.null(points_t)) linkage_r$link.points[points_t, , itr] <- copyTransformation(m1=mechanism$joint.coor[joints_assoc, ], 
					m2=linkage_r$joint.coor[joints_assoc, , itr], mn=mechanism$link.points[points_t, ])

				# TRANSFORM ASSOCIATED LOCAL COORDINATE SYSTEMS
				linkage_r$link.lcs[[mechanism$link.names[i]]][, , itr] <- copyTransformation(m1=mechanism$joint.coor[joints_assoc, ], 
					m2=linkage_r$joint.coor[joints_assoc, , itr], mn=mechanism$link.lcs[[mechanism$link.names[i]]])
			}
		}
	}

	# ADD EXTRA REFERENCE RESULTS TO RETURN LIST
	linkage_r$joint.cons.dyn <- joint_cons_dyn

	# CHECK THAT DISTANCES WITHIN LINKS HAVE NOT CHANGED
	if(check.inter.joint.dist && dim(linkage_r$joint.coor)[3] > 1){
	
		# EACH PAIR OF JOINED JOINTS
		for(i in 1:nrow(mechanism$joint.links)){

			# SKIP LINKS TO GROUND JOINTS
			if(mechanism$joint.links[i, 'Joint1'] == 0 || mechanism$joint.links[i, 'Joint2'] == 0) next

			# GET JOINT PAIR POSITIONS
			joint1 <- linkage_r$joint.coor[mechanism$joint.links[i, 'Joint1'], , ]
			joint2 <- linkage_r$joint.coor[mechanism$joint.links[i, 'Joint2'], , ]

			### FIX!
			# FOR NOW SKIP TRANSLATION ALONG ROTATING LINK
			if(sum(c('R', 'L') %in% linkage_r$joint.types[c(mechanism$joint.links[i, 'Joint1'], mechanism$joint.links[i, 'Joint2'])]) == 2) next
			
			#print(joint_names[mechanism$joint.links[i, c('Joint1', 'Joint2')]])

			# COMPARE TO INITIAL JOINT PAIR POSITIONS
			d <- abs(mechanism$joint.links[i, 'Length'] - sqrt(colSums((joint1 - joint2)^2)))
			#cat(mechanism$joint.links[i, 'Joint1'], '-', mechanism$joint.links[i, 'Joint2'], '\n')

			# CHANGE IN DISTANCE
			dist_sd <- abs(sd(d) / linkage_size)

			# SKIP NA
			if(is.na(dist_sd)) next

			# ALL DISTANCES CONSTANT
			if(dist_sd < 1e-7) next

			#print(sqrt(colSums((joint1 - joint2)^2)))

			# PRINT DISTANCES THAT CHANGE
			warning(paste0("The distance between joints ", joint_names[mechanism$joint.links[i, 'Joint1']], " and ", joint_names[mechanism$joint.links[i, 'Joint2']], " is non-constant (", sd(d), ")."))
		}
	}

	# CHECK THAT JOINT CONSTRAINTS HOLD
	if(check.joint.cons && dim(linkage_r$joint.coor)[3] > 1){

		for(i in 1:n_joints){
		
			if(mechanism$joint.types[i] %in% c('R', 'U', 'S') && i %in% mechanism$fixed.joints){
			
				# FIND DISTANCES FROM FIRST JOINT POSITION TO ALL SUBSEQUENT POSITIONS
				diff <- linkage_r$joint.coor[i, , ] - matrix(linkage_r$joint.coor[i, , 1], nrow=dim(linkage_r$joint.coor)[2], ncol=dim(linkage_r$joint.coor)[3])
				d <- sqrt(colSums((diff)^2))

				# ALL DISTANCES CONSTANT
				if(abs(sd(d) / linkage_size) < 1e-7) next

				# PRINT DISTANCES THAT CHANGE
				warning(paste0("Joint ", i, " is non-stationary (", sd(d), ")."))
			}

			if(mechanism$joint.types[i] == 'L' && i %in% mechanism$fixed.joints){

				# FIND DISTANCES FROM JOINT TO LINE
				d <- abs(distPointToLine(t(linkage_r$joint.coor[i, , ]), linkage_r$joint.coor[i, , 1], linkage_r$joint.coor[i, , 1]+linkage_r$joint.cons[[i]]))

				# ALL DISTANCES CONSTANT
				if(max(d) / linkage_size < 1e-7) next

				# PRINT DISTANCES THAT CHANGE
				warning(paste0("Joint ", i, " deviates from the linear constraint (max: ", max(d), ")."))
			}

			if(mechanism$joint.types[i] == 'P' && i %in% mechanism$fixed.joints){

				# FIND DISTANCES FROM JOINT TO PLANE
				d <- abs(distPointToPlane(t(linkage_r$joint.coor[i, , ]), linkage_r$joint.cons[[i]], linkage_r$joint.coor[i, , 1]))

				# ALL DISTANCES CONSTANT
				if(max(d) / linkage_size < 1e-7) next

				# PRINT DISTANCES THAT CHANGE
				warning(paste0("Joint ", i, " deviates from the planar constraint (max: ", max(d), ")."))
			}
		}
	}

	# CHECK THAT DISTANCES AMONG POINTS ASSOCIATED WITH THE SAME LINK DO NOT CHANGE
	if(check.inter.point.dist && !is.null(linkage_r$link.points) && dim(linkage_r$link.points)[3] > 1){
		for(points_assoc in mechanism$point.assoc){

			if(length(points_assoc) < 2) next
		
			# GET ALL POINTS ASSOCIATED WITH BODY
			n <- linkage_r$link.points[points_assoc, , ]

			# GENERATE PAIRS
			r1 <- r2 <- c(1,dim(n)[1])
			p <- matrix(NA, nrow=0, ncol=2)
			for(i in r1[1]:r1[2]){
				for(j in r2[1]:r2[2]){
					if(j < i && r2[2] >= r1[2]){next}
					if(i < j && r2[2] < r1[2]){next}
					if(j == i){next}

					p <- rbind(p, c(i, j))
				}
			}

			# DISTANCE MATRIX
			d <- matrix(NA, nrow=nrow(p), ncol=dim(n)[3])

			# FIND DISTANCES BETWEEN PAIRS OF POINTS
			for(j in 1:dim(n)[3]) d[, j] <- distPointToPoint(n[p[, 1], , j], n[p[, 2], , j])

			# FIND SD OF EACH ROW
			d_sd <- apply(d, 1, sd)
		
			# ALL DISTANCES CONSTANT
			if(sum(na.omit(d_sd) > 1e-8) == 0) next

			# PRINT DISTANCES THAT CHANGE
			warning("Interpoint distance within link are non-constant.")
		}
	}
	
	class(linkage_r) <- 'animate_linkage'

	linkage_r
}
