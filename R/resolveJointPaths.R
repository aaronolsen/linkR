resolveJointPaths <- function(mechanism, iter, print.progress = FALSE, indent = '\t', indent.level=3){

	# Path syntax
	# 	J/D: Jointed/disjointed

	if(print.progress) cat(paste0(paste0(rep(indent, indent.level), collapse=''), 'resolveJointPaths()\n'))

	if(mechanism[['num.paths.closed']] == 0){
		if(print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'No closed paths\n'))
		return(mechanism)
	}

	# Set max number of recursive loops
	ct_max <- 20
	
	# Create vector of solved paths
	if(is.null(mechanism[['paths.solved']])){
	
		# Start by searching all paths
		save_paths_solved <- TRUE
		path_search_idx <- 1:mechanism[['num.paths.closed']]
		path_search_len <- mechanism[['num.paths.closed']]

		if(print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Searching all paths: 1-', tail(path_search_idx, 1), '\n'))
	}else{

		# Only search solved paths from previous run
		save_paths_solved <- FALSE
		path_search_idx <- mechanism[['paths.solved']]
		path_search_len <- length(mechanism[['paths.solved']])

		if(print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Searching previously solved paths: ', paste0(path_search_idx, collapse=','), '\n'))
	}

	#if(print.progress) print_joint_status(mechanism, indent, indent.level+2)

	#if(iter == 1) print.progress <- TRUE

	ct <- 0
	while(ct < ct_max){
	
		# Keep track if path was solved
		path_solved <- FALSE
	
		# For each path
		i <- 1
		while(i <= path_search_len){
		
			# Set path index
			path_idx <- path_search_idx[i]
	
			# Get joint indices
			joint_idx <- mechanism[['paths.closed']][[path_idx]]

			if(print.progress) if(ct == ct_max-1){ ct_print <- 'max' }else{ ct_print <- ct }

			# Skip if any joints are already solved
			if(any(rowSums(mechanism[['status']][['solved']][joint_idx, ] > 0) > 1)){
				if(print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 
					'Skipping path ', path_idx, ' with solved joints, run num=', ct_print, '\n'))
				i <- i + 1
				next
			}
			
			# Get path bodies
			path_bodies <- mechanism[['paths.closed.bodies']][[path_idx]]

			# Create joint-body string
			path_strings <- createJointPathString(bodies=path_bodies, mechanism=mechanism, 
				mode=c('ts', 'tj', 'tjs'), joints=joint_idx, path=path_idx, print.progress=print.progress)
			
			# Check string against solvable joint type and status strings
			solvable_ts <- ifelse(path_strings[['ts']] %in% linkR_sp[['str']][['ts']], TRUE, FALSE)
			
			#print(linkR_sp[['str']][['ts']])
	
			# If no match, skip
			if(!solvable_ts){ 
				if(print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 
					'Skipping unsolvable path ', path_strings[['p']], ', [', path_strings[['ts']], '], path num=', path_idx, ', run num=', ct_print, '\n'))
				i <- i + 1
				next
			}

			# Check string against solvable joint type, jointed and status strings
			solvable_tjs <- ifelse(path_strings[['tjs']] %in% linkR_sp[['str']][['tjs']], TRUE, FALSE)

			# Save solved path index
			if(save_paths_solved) mechanism[['paths.solved']] <- c(mechanism[['paths.solved']], path_idx)

			if(!solvable_tjs){

				path_standardized <- FALSE

				if(print.progress) print_progress_standardize <- paste0(paste0(rep(indent, indent.level+1), collapse=''), 
					'Standardizing path ', path_strings[['p']], ', path num=', path_idx, ', run num=', ct_print, '\n')

				if(path_strings[['tj']] %in% c('P(D)-1-S(J)-2-U(D)-3-S(J)-4-S(J)')){
					
					if(print.progress) cat(paste0(print_progress_standardize, paste0(rep(indent, indent.level+2), collapse=''), 
						'Standardizing path by joining ', mechanism[['joint.names']][joint_idx[3]], ' (', joint_idx[3], ')\n'))

					# Find transformation to restore disjointed joint and then extend across any subsequent disjoints
					mechanism <- extendTransformation(mechanism, joint=joint_idx[3], body=path_bodies[2],
						body.excl=path_bodies[2], iter=iter, recursive=TRUE, print.progress=print.progress, indent=indent, 
						indent.level=indent.level+2)

					path_standardized <- TRUE
				}

				if(path_strings[['tj']] %in% c('R(J)-1-R(D)-2-R(D)', 'R(J)-1-R(D)-2-R(J)')){
					
					if(print.progress) cat(paste0(print_progress_standardize, paste0(rep(indent, indent.level+2), collapse=''), 
						'Standardizing path by joining ', mechanism[['joint.names']][joint_idx[2]], ' (', joint_idx[2], ')\n'))

					# Find transformation to restore disjointed joint and then extend across any subsequent disjoints
					mechanism <- extendTransformation(mechanism, joint=joint_idx[2], body=path_bodies[2],
						body.excl=path_bodies[1], iter=iter, recursive=TRUE, print.progress=print.progress, indent=indent, 
						indent.level=indent.level+2)

					path_standardized <- TRUE
				}

				if(path_strings[['tj']] %in% c('R(D)-1-S(J)-2-P(D)-2-S(J)-3-S(J)', 'U(D)-1-R(J)-2-S(J)', 'R(D)-1-S(J)-2-S(J)', 'R(D)-1-R(J)-2-R(D)', 'R(D)-1-R(D)-2-R(D)', 'R(D)-1-R(J)-2-R(J)')){

					if(print.progress) cat(paste0(print_progress_standardize, paste0(rep(indent, indent.level+2), collapse=''), 
						'Standardizing path by joining ', mechanism[['joint.names']][joint_idx[1]], ' (', joint_idx[1], ')\n'))

					# Find transformation to restore disjointed joint and then extend across any subsequent disjoints
					mechanism <- extendTransformation(mechanism, joint=joint_idx[1], body=path_bodies[1],
						body.excl=path_bodies[1], iter=iter, recursive=TRUE, print.progress=print.progress, indent=indent, 
						indent.level=indent.level+2)
			
					path_standardized <- TRUE

					#if(path_strings[['p']] == 'R2(DSN)-R3(JNN)-R4(DNS)'){ ct_max <- 21; break }
				}
				
				if(!path_standardized) i <- i + 1
				
				next
			}

			# Print solving path
			if(print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 
				'Solving path ', path_strings[['p']], ', [', path_strings[['tjs']], '], path num=', path_idx, ', run num=', ct_print, '\n'))
	
			# Get whether halves are solved
			joint_sets <- mechanism[['paths.closed.set']][[path_idx]]

			# Get original distances between joints in path (used for solving some paths)
			path_dists <- mechanism[['paths.closed.dist']][[path_idx]]

			# Apply current transformation to get current joint coordinate and constraint vectors
			joint_current <- applyJointTransform(mechanism, joint=joint_idx, iter=iter)

			# Get previous point for toggle position comparison
			if(path_strings[['tjs']] %in% c('U(JSN)-1-R(JNN)-2-S(DNS)', 'R(JSN)-1-S(JNN)-2-P(DNS)', 
				'R(JSN)-1-S(JNN)-2-S(DNS)', 'R(JSN)-1-S(JNN)-2-S(DSN)', 'R(JSN)-1-R(JNN)-2-R(DNS)', 
				'R(JSN)-1-S(JNN)-2-P(DNS)-2-S(JNN)-3-S(DNS)')){
				
				if(path_strings[['tjs']] == 'R(JSN)-1-S(JNN)-2-P(DNS)-2-S(JNN)-3-S(DNS)'){
					which_jt <- 3
				}else{
					which_jt <- 2
				}
				
				if(iter == 1){
					if(is.null(mechanism[['joint.compare']])){
						toggle_jt_compare <- mechanism[['joint.coor']][joint_idx[which_jt], ]
					}else{
						toggle_jt_compare <- mechanism[['joint.compare']][joint_idx[which_jt],,1]
					}
				}else{
					if(is.null(mechanism[['joint.compare']])){
						toggle_jt_compare <- applyJointTransform(mechanism, joint=joint_idx[which_jt], iter=iter-1)$coor[1, , 1]
					}else{
						toggle_jt_compare <- mechanism[['joint.compare']][joint_idx[which_jt],,iter]
					}
				}
			}

			if(path_strings[['tjs']] %in% c('S(JSN)-1-S(DNS)')){

				# Explain solution
				if(print.progress){
					solve_str <- paste0(paste0(rep(indent, indent.level+2), collapse=''), 
						'Finding rotation at S-joint ', mechanism[['joint.names']][joint_idx[1]], 
						'(', joint_idx[1], ') to bring S-joint ', mechanism[['joint.names']][joint_idx[2]], 
						'(', joint_idx[2], ') into alignment\n')
					cat(solve_str)
				}

				## Find transformation of B4, rotation about joint 5
				V_pre <- joint_current$coor[2,,joint_sets[2,1]] - joint_current$coor[1,,1]
				V_new <- joint_current$coor[2,,joint_sets[2,2]] - joint_current$coor[1,,1]

				# Get axis to rotate body about
				J_axis <- cprod(V_pre, V_new)

				# Get rotation matrix
				RM <- tMatrixEP(J_axis, avec(V_pre, V_new, axis=J_axis, about.axis=TRUE))

				# Get transformation
				tmat_B_1 <- tmat_B_2 <- tmat_B_3 <- diag(4)
				tmat_B_1[1:3, 4] <- joint_current$coor[1,,1]
				tmat_B_2[1:3, 1:3] <- RM
				tmat_B_3[1:3, 4] <- -joint_current$coor[1,,1]
				tmat_B <- tmat_B_1 %*% tmat_B_2 %*% tmat_B_3

				# Transform first body and extend transformation
				mechanism <- extendTransformation(mechanism, body=path_bodies[1], tmat=tmat_B, 
					iter=iter, recursive=TRUE, joint=joint_idx[1], status.solved.to=1, 
					body.excl=path_bodies[1], print.progress=print.progress, indent=indent, 
					indent.level=indent.level+2)

				# Set as jointed
				mechanism[['status']][['jointed']][joint_idx[2]] <- TRUE
				mechanism[['status']][['transformed']][joint_idx[2], ] <- FALSE

				#if(print.progress) print('hi')
			}

			# Solve path
			if(path_strings[['tjs']] %in% c('U(JSN)-1-R(JNN)-2-S(DNS)')){
			
				# Example: Solving path LegL_FootL(DSN)-1-FemurL_LegL(JNN)-2-Pelvis_FemurL(JNS)

				# Find distance from J2 to J3
				J23_length <- distPointToPoint(mechanism[['joint.coor']][joint_idx[2], ], mechanism[['joint.coor']][joint_idx[3], ])
				
				# Set params
				params <- list('U_axes'=rbind(joint_current$cons[[1]][1,,1], joint_current$cons[[1]][2,,2]), 
					'U_cor'=joint_current$coor[1,,1], 'R_mat'=rbind('R_cor'=joint_current$coor[2,,1], 
					'R_axis_pt'=joint_current$coor[2,,1]+joint_current$cons[[2]][1,,1], 'R_pt'=joint_current$coor[3,,joint_sets[3,1]]), 
					'S_cor_S'=joint_current$coor[3,,joint_sets[3,2]], 'J2_compare'=toggle_jt_compare, 
					'J23_length'=J23_length, 'return.J2'=FALSE, 'return.tmat'=FALSE)
				
				# Explain optimization
				if(print.progress){
					solve_str <- paste0(paste0(rep(indent, indent.level+2), collapse=''), 
						'Finding optimal rotations of U-joint ', mechanism[['joint.names']][joint_idx[1]], 
						'(', joint_idx[1], ') to minimize the distance of joint ', mechanism[['joint.names']][joint_idx[3]], 
						'(', joint_idx[3], ') from the circle defined by the hinge axis and center of rotation of joint ',
						mechanism[['joint.names']][joint_idx[2]], '\n')
					cat(solve_str)
				}
				# Set starting values to try - does not converge for some starting values
				try_start <- list(c(0.1,0.2), c(0.1,-0.2), c(0,0), c(-0.2,-0.1))

				# Run optimization with several starting values
				list_optim_runs <- list()
				toggle_dist <- rep(NA, length(try_start))
				list_optim_errs <- rep(NA, length(try_start))
				for(try_num in 1:length(try_start)){

					u_jt_fit <- tryCatch(
						expr={
							nlminb(start=try_start[[try_num]], objective=path_min_j3dtc, lower=rep(-2*pi, 2), 
								upper=rep(2*pi, 2), params=params)
						},
						error=function(cond) {return(NULL)},
						warning=function(cond) {return(NULL)}
					)

					# Save optimization and error
					list_optim_runs[[try_num]] <- u_jt_fit
					list_optim_errs[try_num] <- u_jt_fit$objective
					
					#
					params[['return.J2']] <- TRUE
					toggle_dist[try_num] <- distPointToPoint(toggle_jt_compare, path_min_j3dtc(u_jt_fit$par, params))
					params[['return.J2']] <- FALSE
					
					# If low error is reached stop
					#if(u_jt_fit$objective < 1e-5) break
					#if(sum(abs(u_jt_fit$par - try_start[[try_num]])) > 1e-5) break
				}
				
				# Find best guess of closest toggle distance (min for now - should look for lowest value occurring at least twice?)
				min_toggle_dist <- min(toggle_dist, na.rm=TRUE)

				# Limit search to fits where toggle is lower
				runs_at_min_toggle <- which(abs(toggle_dist-min_toggle_dist) < 1e-3)
				list_optim_runs <- list_optim_runs[runs_at_min_toggle]
				list_optim_errs <- list_optim_errs[runs_at_min_toggle]

				# Save fit having lowest error and closest position to toggle
				u_jt_fit <- list_optim_runs[[which.min(list_optim_errs)]]
				
				# Report optimization error
				if(print.progress){
					solve_str <- paste0(paste0(rep(indent, indent.level+2), collapse=''), 
						'Error after optimization: ', signif(u_jt_fit$objective, 3), '\n')
					cat(solve_str)
				}

				# Get optimal transformation
				params[['return.tmat']] <- TRUE
				body_1_tmat <- path_min_j3dtc(u_jt_fit$par, params)

				# Transform bodies and extend transformation
				mechanism <- extendTransformation(mechanism, body=path_bodies[1], tmat=body_1_tmat, 
					iter=iter, recursive=TRUE, joint=joint_idx[1], status.solved.to=1, 
					print.progress=print.progress, indent=indent, indent.level=indent.level+2)

				# Apply current transformation to get current joint coordinate and constraint vectors
				joint_current <- applyJointTransform(mechanism, joint=joint_idx, iter=iter)
				
				## Find transformation of B4, rotation about joint 5
				V_pre <- joint_current$coor[3,,joint_sets[3,1]] - joint_current$coor[2,,1]
				V_new <- joint_current$coor[3,,joint_sets[3,2]] - joint_current$coor[2,,1]

				# Get rotation matrix
				RM <- tMatrixEP(joint_current$cons[[2]][,,1], avec(V_pre, V_new, axis=joint_current$cons[[2]][,,1], about.axis=TRUE))

				# Get transformation
				tmat_B2_1 <- tmat_B2_2 <- tmat_B2_3 <- diag(4)
				tmat_B2_1[1:3, 4] <- joint_current$coor[2,,1]
				tmat_B2_2[1:3, 1:3] <- RM
				tmat_B2_3[1:3, 4] <- -joint_current$coor[2,,1]
				tmat_B2 <- tmat_B2_1 %*% tmat_B2_2 %*% tmat_B2_3

				# Transform second body and extend transformation
				mechanism <- extendTransformation(mechanism, body=path_bodies[2], tmat=tmat_B2, 
					iter=iter, recursive=TRUE, joint=joint_idx[2], status.solved.to=1, 
					print.progress=print.progress, indent=indent, 
					indent.level=indent.level+2)

				# Set as jointed
				mechanism[['status']][['jointed']][joint_idx[3]] <- TRUE
				mechanism[['status']][['transformed']][joint_idx[3], ] <- FALSE

				path_solved <- TRUE
			}

			# Solve path
			if(path_strings[['tjs']] %in% c('R(JSN)-1-S(JNN)-2-P(DNS)-2-S(JNN)-3-S(DNS)')){
			
				# Check that all middle joints are coincident
				if(sum(apply(mechanism[['joint.coor']][joint_idx[2:4], ], 2, 'sd') > 1e-10)){
					stop(paste0('Joints "', paste0(joint_idx[2:4], collapse=','), '" (', 
						paste0(mechanism[['joint.names']][2:4], collapse=', '), 
						') must be coincident for current version of path solver.'))
				}
				
				# Get R and P vectors
				R_vec <- joint_current$cons[[1]][, , 1]
				P_vec <- joint_current$cons[[3]][3, , joint_sets[3,2]]

				# Check that R and P vectors are parallel
				if(avec(R_vec, P_vec, max.pi=TRUE) > 1e-10){
					stop(paste0('Axis of joint ', joint_idx[1], ' (', mechanism[['joint.names']][joint_idx[1]], 
						') and normal vector at joint ', joint_idx[3], ' (', mechanism[['joint.names']][joint_idx[3]], 
						') must be parallel.'))
				}
				
				if(print.progress){

					solve_str <- paste0(paste0(rep(indent, indent.level+2), collapse=''), 
						'Finding ', mechanism[['joint.names']][joint_idx[3]], '(', joint_idx[3], 
						') in plane defined by point {', paste0(signif(joint_current$coor[3, , joint_sets[3,2]], 3), collapse=','), 
						'} and normal vector {', paste0(signif(P_vec, 3), collapse=','), '} from R-joint and S-joint\n')
					cat(solve_str)
				}


				# Find original distance from joint centers in P-plane
				J1_o_proj <- pointPlaneProj(mechanism[['joint.coor']][joint_idx[1], ], 
					mechanism[['joint.coor']][joint_idx[3], ], mechanism[['joint.cons']][[joint_idx[3]]][3,])
				J12_o_dist <- distPointToPoint(J1_o_proj, mechanism[['joint.coor']][joint_idx[3], ])
				
				# Project new point into plane
				J1_c_proj <- pointPlaneProj(joint_current$coor[1, , 1], joint_current$coor[3, , joint_sets[3,2]], P_vec)

				# Find circle from R-joint side
				R_circle <- defineCircle(center=J1_c_proj, nvector=R_vec, radius=J12_o_dist)

				# Find intersection of sphere and plane for S-joint side (circles in plane)
				int_sph_pln <- intersectSpherePlane(c=joint_current$coor[5, , joint_sets[5,2]], 
					r=path_dists[4], p=joint_current$coor[3, , joint_sets[3,2]], n=P_vec)
				S_circle <- defineCircle(center=int_sph_pln$center, nvector=int_sph_pln$nvector, radius=int_sph_pln$radius)
				
				# Find intersection of circles
				int_circles <- intersectCircles(R_circle, S_circle)

				if(int_circles$type %in% c('two', 'one')){

					if(int_circles$type == 'two'){

						# Find point closest to compare point (toggle)
						which_min <- which.min(distPointToPoint(rbind(int_circles[[1]], int_circles[[2]]), toggle_jt_compare))

						# Set solved joint
						J3_solve <- int_circles[[which_min]]

					}else{

						# Set solved joint
						J3_solve <- int_circles[[1]]
					}
				
					## Find transformation of B1, rotation about J1
					V_pre <- joint_current$coor[2, , 1] - J1_c_proj
					V_new <- J3_solve - J1_c_proj
				
					# Get axis to rotate link about
					J_axis <- cprod(V_pre, V_new)

					# Get rotation matrix
					RM <- tMatrixEP(J_axis, avec(V_pre, V_new, axis=J_axis, about.axis=TRUE))

					# Get transformation
					tmat_B1_1 <- tmat_B1_2 <- tmat_B1_3 <- diag(4)
					tmat_B1_1[1:3, 4] <- joint_current$coor[1,,1]
					tmat_B1_2[1:3, 1:3] <- RM
					tmat_B1_3[1:3, 4] <- -joint_current$coor[1,,1]
					tmat_B1 <- tmat_B1_1 %*% tmat_B1_2 %*% tmat_B1_3
					#tmat_B1 <- diag(4)

					# Transform first body and extend transformation
					mechanism <- extendTransformation(mechanism, body=path_bodies[1], tmat=tmat_B1, 
						iter=iter, recursive=TRUE, joint=joint_idx[1], status.solved.to=1, 
						body.excl=path_bodies[1], print.progress=print.progress, indent=indent, 
						indent.level=indent.level+2)

					# Set as jointed
					mechanism[['status']][['jointed']][joint_idx[1]] <- TRUE
					mechanism[['status']][['transformed']][joint_idx[1], ] <- FALSE

					# Apply current transformation to get current joint coordinate and constraint vectors
					joint_current <- applyJointTransform(mechanism, joint=joint_idx, iter=iter)

					## Find transformation of B3, rotation about J3
					V_pre <- joint_current$coor[5, , joint_sets[5,1]] - J3_solve
					V_new <- joint_current$coor[5, , joint_sets[5,2]] - J3_solve

					# Get axis to rotate link about
					J_axis <- cprod(V_pre, V_new)

					# Get rotation matrix
					RM <- tMatrixEP(J_axis, avec(V_pre, V_new, axis=J_axis, about.axis=TRUE))

					# Get transformation
					tmat_B3_1 <- tmat_B3_2 <- tmat_B3_3 <- diag(4)
					tmat_B3_1[1:3, 4] <- J3_solve
					tmat_B3_2[1:3, 1:3] <- RM
					tmat_B3_3[1:3, 4] <- -J3_solve
					tmat_B3 <- tmat_B3_1 %*% tmat_B3_2 %*% tmat_B3_3

					# Transform first body and extend transformation
					# path_bodies has 4 elements - middle body is repeated
					mechanism <- extendTransformation(mechanism, body=path_bodies[4], tmat=tmat_B3, 
						iter=iter, recursive=TRUE, joint=joint_idx[4], status.solved.to=1, 
						body.excl=path_bodies[4], print.progress=print.progress, indent=indent, 
						indent.level=indent.level+2)

					# Apply current transformation to get current joint coordinate and constraint vectors
					joint_current <- applyJointTransform(mechanism, joint=joint_idx, iter=iter)

					# Set as jointed
					mechanism[['status']][['jointed']][joint_idx[5]] <- TRUE
					mechanism[['status']][['transformed']][joint_idx[5], ] <- FALSE

					path_solved <- TRUE
				}
			}

			# Solve path
			if(path_strings[['tjs']] %in% c('P(DSN)-1-S(JNN)-2-U(JNS)-3-S(DSN)-4-S(JNN)')){

				# Example: Solving path LJSYM_NEURO(DSN)-1-LJAWL_LJSYM(JNN)-2-SUSPL_LJAWL(DNS)-3-OPERL_INOPL(JSN)-4-INOPL_LJAWL(JNN)

				# Find distance from J2 to J1-plane
				dptp_J12 <- distPointToPlane(p=mechanism[['joint.coor']][joint_idx[2], ], n=mechanism[['joint.cons']][[joint_idx[1]]], q=mechanism[['joint.coor']][joint_idx[1], ])

				# Find distance from J4 to J5
				J45_length <- distPointToPoint(mechanism[['joint.coor']][joint_idx[4], ], mechanism[['joint.coor']][joint_idx[5], ])

				# Find point in plane of final J2
				pt_in_J2_plane <- joint_current$coor[1,,joint_sets[1,1]] + dptp_J12*joint_current$cons[[1]][, , joint_sets[1,1]]
				
				# Set other params
				params <- list('U_axes'=rbind(joint_current$cons[[3]][1,,1], joint_current$cons[[3]][2,,2]), 
					'CoR'=joint_current$coor[3,,1], 'joints'=joint_current$coor[c(2,5),,1],
					'J2_plane_p'=pt_in_J2_plane, 'J2_plane_n'=joint_current$cons[[1]][, , joint_sets[1,1]],
					'J4'=joint_current$coor[4,,joint_sets[4,1]], 'J45_length'=J45_length, 
					'return.tmat'=FALSE)

				# Explain optimization
				if(print.progress){
					solve_str <- paste0(paste0(rep(indent, indent.level+2), collapse=''), 
						'Finding optimal rotations of U-joint ', mechanism[['joint.names']][joint_idx[3]], 
						'(', joint_idx[3], ') to minimize the distance of joint ', mechanism[['joint.names']][joint_idx[2]], 
						'(', joint_idx[2], ') from the plane and the keep the length between joints ',
						mechanism[['joint.names']][joint_idx[4]], '(', joint_idx[4], ') and ', 
						mechanism[['joint.names']][joint_idx[5]], '(', joint_idx[5], ') close to the initial length of ', 
						signif(J45_length, 3), '\n')
					cat(solve_str)
				}
				
				# Set starting values to try - does not converge for some starting values
				try_start <- list(c(0.1,0.2), c(0,0), c(-0.2,-0.1), c(0.1,-0.2))

				# Run optimization with several starting values
				list_optim_runs <- list()
				list_optim_errs <- rep(NA, length(try_start))
				for(try_num in 1:length(try_start)){

					u_jt_fit <- tryCatch(
						expr={
							nlminb(start=try_start[[try_num]], objective=path_min_j2ptp_u_j45ptp, lower=rep(-2*pi, 2), 
								upper=rep(2*pi, 2), params=params)
						},
						error=function(cond) {return(NULL)},
						warning=function(cond) {return(NULL)}
					)

					# Save optimization and error
					list_optim_runs[[try_num]] <- u_jt_fit
					list_optim_errs[try_num] <- u_jt_fit$objective
					
					# If low error is reached stop
					if(u_jt_fit$objective < 1e-5) break
					#if(sum(abs(u_jt_fit$par - try_start[[try_num]])) > 1e-5) break
				}

				# Save fit having lowest error
				u_jt_fit <- list_optim_runs[[which.min(list_optim_errs)]]
				
				# Report optimization error
				if(print.progress){
					# Get error for rotation of 0
					#u_jt_err <- path_min_j2ptp_u_j45ptp(p=c(0,0), params)

					solve_str <- paste0(paste0(rep(indent, indent.level+2), collapse=''), 
						'Error after optimization: ', signif(u_jt_fit$objective, 3), '\n')
					cat(solve_str)
				}

				# Get optimal transformation
				params[['return.tmat']] <- TRUE
				body_2_tmat <- path_min_j2ptp_u_j45ptp(u_jt_fit$par, params)

				# Transform bodies and extend transformation
				mechanism <- extendTransformation(mechanism, body=path_bodies[2], tmat=body_2_tmat, 
					iter=iter, recursive=TRUE, joint=joint_idx[3], status.solved.to=1, 
					body.excl=path_bodies[2], print.progress=print.progress, indent=indent, 
					indent.level=indent.level+2)

				# Apply current transformation to get current joint coordinate and constraint vectors
				joint_current <- applyJointTransform(mechanism, joint=joint_idx, iter=iter)
				
				# Get initial position of J2
				# *** This wont work if B1 has been re-oriented in space
				# Will need to save transformations of bodies to reconstruct original position of J2 relative to J1
				J2_coor_i <- (mechanism[['joint.coor']][joint_idx[2], ] - mechanism[['joint.coor']][joint_idx[1], ]) + joint_current$coor[1,,joint_sets[1,1]]
				
				# Find translation vector
				B1_t_vec <- joint_current$coor[2,,1] - J2_coor_i

				# Get two coordinate sets to compare 1:translation only, 2: translation and rotation
				B1_t_tmat <- diag(4)
				B1_t_tmat[1:3,4] <- B1_t_vec
				B1_tfm1 <- applyTransform(diag(3), B1_t_tmat)
				B1_tfm2 <- applyTransform(diag(3), mechanism[['tmat']][,,path_bodies[1],iter])
				
				# Find transformation at J2 of B1 so that B1 only translates
				best_align <- bestAlign(B1_tfm1, B1_tfm2)
				
				# Extend transformation
				mechanism <- extendTransformation(mechanism, body=path_bodies[1], tmat=best_align$tmat, 
					iter=iter, recursive=TRUE, joint=joint_idx[2], status.solved.to=1, 
					body.excl=path_bodies[1], print.progress=print.progress, indent=indent, 
					indent.level=indent.level+2)

				# Set as jointed
				mechanism[['status']][['jointed']][joint_idx[1]] <- TRUE
				mechanism[['status']][['transformed']][joint_idx[1], ] <- FALSE
				

				## Find transformation of B4, rotation about joint 5
				V_pre <- joint_current$coor[4,,joint_sets[4,2]] - joint_current$coor[5,,1]
				V_new <- joint_current$coor[4,,joint_sets[4,1]] - joint_current$coor[5,,1]
				
				# Get axis to rotate link about
				J_axis <- cprod(V_pre, V_new)

				# Get rotation matrix
				RM <- tMatrixEP(J_axis, avec(V_pre, V_new, axis=J_axis, about.axis=TRUE))

				# Get transformation
				tmat_B4_1 <- tmat_B4_2 <- tmat_B4_3 <- diag(4)
				tmat_B4_1[1:3, 4] <- joint_current$coor[5,,1]
				tmat_B4_2[1:3, 1:3] <- RM
				tmat_B4_3[1:3, 4] <- -joint_current$coor[5,,1]
				tmat_B4 <- tmat_B4_1 %*% tmat_B4_2 %*% tmat_B4_3

				# Transform second body and extend transformation
				mechanism <- extendTransformation(mechanism, body=path_bodies[4], tmat=tmat_B4, 
					iter=iter, recursive=TRUE, joint=joint_idx[5], status.solved.to=1, 
					body.excl=path_bodies[4], print.progress=print.progress, indent=indent, 
					indent.level=indent.level+2)

				# Set as jointed
				mechanism[['status']][['jointed']][joint_idx[4]] <- TRUE
				mechanism[['status']][['transformed']][joint_idx[4], ] <- FALSE

				path_solved <- TRUE
			}

			# Solve path
			if(path_strings[['tjs']] %in% c('R(JSN)-1-S(JNN)-2-P(DNS)')){

				# Get current circle radius, center and normal vector
				circle_pt_on_rad <- joint_current$coor[2, , 1]		# Also point in plane
				circle_center <- joint_current$coor[1, , 1]
				circle_norm <- joint_current$cons[[1]][, , 1]

				# Get point in plane
				plane_point <- joint_current$coor[3, , 2]

				# Get plane normal vector
				plane_norm <- joint_current$cons[[3]][3, , 2]
				
				# Check whether vectors are parallel - if they are then there is not a unique solution
				a_vecs <- avec(circle_norm, plane_norm, max.pi=TRUE)
				
				if(a_vecs == 0){
				
					# Vectors are parallel - there is no unique solution

				}else{

					# 
					if(print.progress){
						solve_str <- paste0(paste0(rep(indent, indent.level+2), collapse=''), 
							'Finding ', mechanism[['joint.names']][joint_idx[2]], '(', joint_idx[2], 
							') on circle with center {', paste0(signif(circle_center, 3), collapse=','), 
							'} at ', mechanism[['joint.names']][joint_idx[1]], 
							'(', joint_idx[1], '), ', 'point on radius {', paste0(signif(circle_pt_on_rad, 3), collapse=','), '}, and normal {' , 
							paste0(signif(circle_norm, 3), collapse=','), '}', ' at intersection with plane defined by point={', 
							paste0(signif(plane_point, 3), collapse=','), '} and normal vector {', 
							paste0(signif(uvector(plane_norm), 3), collapse=','), '} and closest to point {', 
							paste0(signif(toggle_jt_compare, 3), collapse=','), '}', '\n')
						cat(solve_str)
					}

					# Define circle
					circle <- defineCircle(center=circle_center, nvector=circle_norm, 
						point_on_radius=circle_pt_on_rad, redefine_center=TRUE)

					# Find intersection of circle and plane
					int_cp <- intersectCirclePlane(circle=circle, P=plane_point, N=plane_norm)

					# Find point on circle
					J2_solve <- circlePoint(circle=circle, T=int_cp)

					# If no solution, return NULL
					if((is.vector(J2_solve) && is.na(J2_solve[1])) || (!is.vector(J2_solve) && is.na(J2_solve[1,1]))){
						if(print.progress) cat(paste0(paste0(rep(indent, indent.level+2), collapse=''), 'No solution found.\n'))
						return(mechanism) 
					}

					# Find point closest to compare point
					which_min <- which.min(distPointToPoint(J2_solve, toggle_jt_compare))
				
					# Set solved joint
					J2_solve <- J2_solve[which_min,]

					# Get vectors and axis to find first body transformation
					V_pre <- joint_current$coor[2, , 1] - joint_current$coor[1, , 1]
					V_new <- J2_solve - joint_current$coor[1, , 1]

					# Get axis to rotate link about
					J_axis <- joint_current$cons[[1]][, , 1]

					# Get rotation matrix
					RM <- tMatrixEP(J_axis, avec(V_pre, V_new, axis=J_axis, about.axis=TRUE))

					# Get transformation of first body
					tmat_B_1 <- tmat_B_2 <- tmat_B_3 <- diag(4)
					tmat_B_1[1:3, 4] <- -joint_current$coor[1, , 1]
					tmat_B_2[1:3, 1:3] <- RM
					tmat_B_3[1:3, 4] <- joint_current$coor[1, , 1]
					tmat_B1 <- tmat_B_3 %*% tmat_B_2 %*% tmat_B_1

					# Find vector for second body transformation
					tmat_B2 <- diag(4)
					tmat_B2[1:3, 4] <- J2_solve - circle_pt_on_rad

					# Transform second body and extend transformation
					mechanism <- extendTransformation(mechanism, body=path_bodies[2], tmat=tmat_B2, 
						iter=iter, recursive=TRUE, joint=joint_idx[2], status.solved.to=1, 
						body.excl=path_bodies[1:2], print.progress=print.progress, indent=indent, 
						indent.level=indent.level+2)

					# Transform first body and extend transformation
					mechanism <- extendTransformation(mechanism, body=path_bodies[1], joint=joint_idx[1], 
						status.solved.to=1, tmat=tmat_B1, iter=iter, recursive=TRUE, 
						body.excl=path_bodies[1], print.progress=print.progress, indent=indent, indent.level=indent.level+2)

					# Set last joint as jointed
					mechanism[['status']][['jointed']][joint_idx[3]] <- TRUE
					mechanism[['status']][['transformed']][joint_idx[3], ] <- FALSE

					path_solved <- TRUE
				}
			}

			# Solve path
			if(path_strings[['tjs']] %in% c('R(JSN)-1-L(JNN)-2-R(DNS)', 'S(JSN)-1-L(JNN)-2-S(DNS)')){

				# Find target length between solved halves of rotational joints
				len_target <- distPointToPoint(joint_current$coor[1, , 1], joint_current$coor[3, , joint_sets[3,2]])
				
				# Get previous point for toggle position comparison
				# **** Should this be iter-1???
				# **** Also need to add in joint.compare
				if(iter == 1){
					t_target_prev <- mechanism[['joint.coor']][joint_idx[3], ]
				}else{
					t_target_prev <- joint_current$coor[3, , 1]
				}
				
				# Get translation vector
				tvec <- joint_current$cons[[2]][1, , 1]

				# Find intersection of sphere and line
				t_target <- intersectSphereLine(c=joint_current$coor[1, , 1], r=len_target, 
					x=joint_current$coor[3, , joint_sets[3,1]], l=tvec, point.compare=t_target_prev)
				
				# Get translation magnitude
				tmag <- distPointToPoint(t_target, joint_current$coor[3, , joint_sets[3,1]])
				
				# Set magnitude sign
				tpos <- sum(abs((joint_current$coor[3, , joint_sets[3,1]] + tmag*tvec - t_target)))
				tneg <- sum(abs((joint_current$coor[3, , joint_sets[3,1]] - tmag*tvec - t_target)))
				if(tpos < tneg){ tmag <- tmag }else { tmag <- -tmag }

				# Get transformation of second body
				tmat_B <- diag(4)
				tmat_B[1:3, 4] <- tmag*tvec

				# Transform second body and extend transformation
				mechanism <- extendTransformation(mechanism, body=path_bodies[2], tmat=tmat_B, 
					iter=iter, recursive=TRUE, joint=joint_idx[2], status.solved.to=0, 
					body.excl=path_bodies[1:2], print.progress=print.progress, indent=indent, 
					indent.level=indent.level+2)

				# Update joint coordinates after transformation
				joint_current <- applyJointTransform(mechanism, joint=joint_idx, iter=iter)

				# Get vectors and axis to find first body transformation
				V_pre <- joint_current$coor[3, , joint_sets[3,1]]-joint_current$coor[1, , 1]
				V_new <- joint_current$coor[3, , joint_sets[3,2]]-joint_current$coor[1, , 1]

				# Get axis to rotate link about
				if(path_strings[['tjs']] %in% c('R(JSN)-1-L(JNN)-2-R(DNS)')) J_axis <- joint_current$cons[[1]][, , 1]
				if(path_strings[['tjs']] %in% c('S(JSN)-1-L(JNN)-2-S(DNS)')) J_axis <- cprod(V_pre, V_new)

				# Get rotation matrix
				RM <- tMatrixEP(J_axis, avec(V_pre, V_new, axis=J_axis, about.axis=TRUE))

				# Get transformation of first and second body
				tmat_B_1 <- tmat_B_2 <- tmat_B_3 <- diag(4)
				tmat_B_1[1:3, 4] <- joint_current$coor[1, , 1]
				tmat_B_2[1:3, 1:3] <- RM
				tmat_B_3[1:3, 4] <- -joint_current$coor[1, , 1]
				tmat_B <- tmat_B_1 %*% tmat_B_2 %*% tmat_B_3

				# Transform first body and extend transformation
				mechanism <- extendTransformation(mechanism, body=path_bodies[1], joint=joint_idx[1], 
					status.solved.to=1, tmat=tmat_B, iter=iter, recursive=TRUE, 
					body.excl=path_bodies[1], print.progress=print.progress, indent=indent, indent.level=indent.level+2)

				# Update joint coordinates after transformation
				joint_current <- applyJointTransform(mechanism, joint=joint_idx, iter=iter)
				len_current <- distPointToPoint(joint_current$coor[1, , 1], joint_current$coor[3, , joint_sets[3,1]])

				# Set remaining joints as jointed and solved
				mechanism[['status']][['jointed']][joint_idx[3]] <- TRUE
				mechanism[['status']][['transformed']][joint_idx[3], ] <- FALSE
				mechanism[['status']][['solved']][joint_idx[2], ] <- 1
				mechanism[['status']][['solved']][joint_idx[3], joint_sets[3,1]] <- 1

				path_solved <- TRUE
			}

			# Solve path
			if(path_strings[['tjs']] %in% c('R(JSN)-1-S(JNN)-2-S(DSN)', 'R(JSN)-1-S(JNN)-2-S(DNS)', 'R(JSN)-1-R(JNN)-2-R(DNS)')){
				
				# Set 3rd joint set to use
				if(grepl('2-S[(]DNS[)]', path_strings[['tjs']])){
					joint_set3_use <- c(1,2)
				}else{
					joint_set3_use <- c(2,1)
				}

				# Get current line length -- easiest to calculate in real time in case link is composite of other joints
				line_len <- distPointToPoint(joint_current$coor[2, , 1], joint_current$coor[3, , joint_sets[3,joint_set3_use[1]]])

				# Get point at distance from circle
				line_point <- joint_current$coor[3, , joint_sets[3,joint_set3_use[2]]]

				# Get current circle radius, center and normal vector
				circle_pt_on_rad <- joint_current$coor[2, , 1]
				circle_center <- joint_current$coor[1, , 1]
				circle_norm <- joint_current$cons[[1]][, , 1]

				# 
				if(print.progress){
					solve_str <- paste0(paste0(rep(indent, indent.level+2), collapse=''), 
						'Finding ', mechanism[['joint.names']][joint_idx[2]], '(', joint_idx[2], 
						') on circle with center {', paste0(signif(circle_center, 3), collapse=','), 
						'} at ', mechanism[['joint.names']][joint_idx[1]], 
						'(', joint_idx[1], '), ', 'point on radius={', paste0(signif(circle_pt_on_rad, 3), collapse=','), '}, and normal={' , 
						paste0(signif(circle_norm, 3), collapse=','), '}', ' at distance=', 
						signif(line_len, 3), ' from {', paste0(signif(line_point, 3), collapse=','), 
						'} at ', mechanism[['joint.names']][joint_idx[3]], '(', joint_idx[3], 
						') and closest to point {', paste0(signif(toggle_jt_compare, 3), collapse=','), '}\n')
					cat(solve_str)
				}

				# Define circle
				circle <- defineCircle(center=circle_center, nvector=circle_norm, 
					point_on_radius=circle_pt_on_rad, redefine_center=TRUE)

				# Find angle on circle at distance from point
				circle_t <- angleOnCircleFromPoint(circle=circle, dist=line_len, P=line_point, point_compare=toggle_jt_compare)

				# Find point on circle
				J2_solve <- circlePoint(circle=circle, T=circle_t)

				# If no solution, return NULL
				if((is.vector(J2_solve) && is.na(J2_solve[1])) || (!is.vector(J2_solve) && is.na(J2_solve[1,1]))){
					if(print.progress) cat(paste0(paste0(rep(indent, indent.level+2), collapse=''), 'No solution found.\n'))
					return(mechanism) 
				}

				# Get vectors and axis to find first body transformation
				V_pre <- joint_current$coor[2, , 1] - joint_current$coor[1, , 1]
				V_new <- J2_solve - joint_current$coor[1, , 1]

				# Get axis to rotate link about
				J_axis <- joint_current$cons[[1]][, , 1]

				# Get rotation matrix
				RM <- tMatrixEP(J_axis, avec(V_pre, V_new, axis=J_axis, about.axis=TRUE))

				# Get transformation of first body
				tmat_B_1 <- tmat_B_2 <- tmat_B_3 <- diag(4)
				tmat_B_1[1:3, 4] <- -joint_current$coor[1, , 1]
				tmat_B_2[1:3, 1:3] <- RM
				tmat_B_3[1:3, 4] <- joint_current$coor[1, , 1]
				tmat_B <- tmat_B_3 %*% tmat_B_2 %*% tmat_B_1

				# Transform first body and extend transformation
				mechanism <- extendTransformation(mechanism, body=path_bodies[1], joint=joint_idx[1], 
					status.solved.to=1, tmat=tmat_B, iter=iter, recursive=TRUE, 
					body.excl=path_bodies[1], print.progress=print.progress, indent=indent, indent.level=indent.level+2)

				# Update 3rd joint coordinate and constraint after transformation
				joint_current3 <- applyJointTransform(mechanism, joint=joint_idx[3], iter=iter)

				# Get vectors and axis to find second body transformation
				V_pre <- joint_current3$coor[1, , joint_sets[3, joint_set3_use[1]]] - J2_solve
				V_new <- line_point - J2_solve
				
				# Get axis to rotate link about
				if(path_strings[['tjs']] %in% c('R(JSN)-1-R(JNN)-2-R(DNS)')){
					joint_current2 <- applyJointTransform(mechanism, joint=joint_idx[2], iter=iter)
					J_axis <- joint_current2$cons[[1]][, , 1]
				}
				if(path_strings[['tjs']] %in% c('R(JSN)-1-S(JNN)-2-S(DNS)', 'R(JSN)-1-S(JNN)-2-S(DSN)')) J_axis <- cprod(V_pre, V_new)
				
				# Get rotation matrix
				RM <- tMatrixEP(J_axis, avec(V_pre, V_new, axis=J_axis, about.axis=TRUE))

				# Get transformation of second body
				tmat_B_1 <- tmat_B_2 <- tmat_B_3 <- diag(4)
				tmat_B_1[1:3, 4] <- J2_solve
				tmat_B_2[1:3, 1:3] <- RM
				tmat_B_3[1:3, 4] <- -J2_solve
				tmat_B <- tmat_B_1 %*% tmat_B_2 %*% tmat_B_3

				# Transform second body and extend transformation
				mechanism <- extendTransformation(mechanism, body=path_bodies[2], tmat=tmat_B, 
					iter=iter, recursive=TRUE, joint=joint_idx[2], status.solved.to=1, 
					body.excl=path_bodies[1:2], print.progress=print.progress, indent=indent, 
					indent.level=indent.level+2)

				#print(line_point)
				#print(applyJointTransform(mechanism, joint=joint_idx[3], iter=iter)$coor[, , joint_sets[3,joint_set3_use[2]]])

				# Set last joint as jointed
				mechanism[['status']][['jointed']][joint_idx[3]] <- TRUE
				mechanism[['status']][['transformed']][joint_idx[3], ] <- FALSE

				path_solved <- TRUE
			}

			if(!path_solved && print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 
				'Could not find solver for path [', path_strings[['tjs']], '], path num=', path_idx, ', run num=', ct_print, '\n'))

			# Extend solved status through mechanism
			mechanism <- extendSolvedStatus(mechanism, print.progress=print.progress, indent=indent, indent.level=indent.level+2)

			#if(print.progress) print_joint_status(mechanism, indent, indent.level+2)
			
			#if(path_strings[['p']] == 'R2(JSN)-R3(JNN)-R4(DNS)'){ path_solved <- FALSE; break }

			i <- i + 1
		}
		
		# If no path was solved, break
		if(!path_solved){

			if(print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 
				'No additional paths found to solve during previous run, stopping path solving\n'))

			break
		}
		
		# Advance count
		ct <- ct + 1
	}

	# Save solved path index
	if(save_paths_solved) mechanism[['paths.solved']] <- unique(mechanism[['paths.solved']])

	#if(print.progress) print_joint_status(mechanism, indent)

	mechanism
}