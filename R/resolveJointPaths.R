resolveJointPaths <- function(mechanism, iter, print.progress = FALSE, indent = '\t', indent.level=3){

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
	
			# If no match, skip
			if(!solvable_ts){ 
				if(print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 
					'Skipping unsolvable path ', path_strings[['p']], ', path num=', path_idx, ', run num=', ct_print, '\n'))
				i <- i + 1
				next
			}

			# Check string against solvable joint type, jointed and status strings
			solvable_tjs <- ifelse(path_strings[['tjs']] %in% linkR_sp[['str']][['tjs']], TRUE, FALSE)
			
			# Save solved path index
			if(save_paths_solved) mechanism[['paths.solved']] <- c(mechanism[['paths.solved']], path_idx)

			if(!solvable_tjs){

				path_standardized <- FALSE

				if(print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 
					'Standardizing path ', path_strings[['p']], ', path num=', path_idx, ', run num=', ct_print, '\n'))

				if(path_strings[['tj']] %in% c('R(J)-1-R(D)-2-R(D)', 'R(J)-1-R(D)-2-R(J)')){
					
					if(print.progress) cat(paste0(paste0(rep(indent, indent.level+2), collapse=''), 
						'Standardizing path by joining ', mechanism[['joint.names']][joint_idx[2]], ' (', joint_idx[2], ')\n'))

					# Find transformation to restore disjointed joint and then extend across any subsequent disjoints
					mechanism <- extendTransformation(mechanism, joint=joint_idx[2], body=path_bodies[2],
						body.excl=path_bodies[1], iter=iter, recursive=TRUE, print.progress=print.progress, indent=indent, 
						indent.level=indent.level+2)

					path_standardized <- TRUE
				}

				if(path_strings[['tj']] %in% c('R(D)-1-S(J)-2-S(J)', 'R(D)-1-R(J)-2-R(D)', 'R(D)-1-R(D)-2-R(D)', 'R(D)-1-R(J)-2-R(J)')){

					if(print.progress) cat(paste0(paste0(rep(indent, indent.level+2), collapse=''), 
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
				'Solving path ', path_strings[['p']], ', path num=', path_idx, ', run num=', ct_print, '\n'))
	
			# Get whether halves are solved
			joint_sets <- mechanism[['paths.closed.set']][[path_idx]]

			# Get original distances between joints in path (used for solving some paths)
			path_dists <- mechanism[['paths.closed.dist']][[path_idx]]

			# Apply current transformation to get current joint coordinate and constraint vectors
			joint_current <- applyJointTransform(mechanism, joint=joint_idx, iter=iter)

			# Get previous point for toggle position comparison
			if(path_strings[['tjs']] %in% c('R(JSN)-1-S(JNN)-2-P(DNS)', 'R(JSN)-1-S(JNN)-2-S(DNS)', 'R(JSN)-1-R(JNN)-2-R(DNS)')){
				if(iter == 1){
					if(is.null(mechanism[['joint.compare']])){
						J2_compare <- mechanism[['joint.coor']][joint_idx[2], ]
					}else{
						J2_compare <- mechanism[['joint.compare']][joint_idx[2],,1]
					}
				}else{
					if(is.null(mechanism[['joint.compare']])){
						J2_compare <- applyJointTransform(mechanism, joint=joint_idx[2], iter=iter-1)$coor[1, , 1]
					}else{
						J2_compare <- mechanism[['joint.compare']][joint_idx[2],,iter]
					}
				}
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
				plane_norm <- cprod(joint_current$cons[[3]][1, , 2], joint_current$cons[[3]][2, , 2])

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
						paste0(signif(J2_compare, 3), collapse=','), '}', '\n')
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
				which_min <- which.min(distPointToPoint(J2_solve, J2_compare))
				
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
			if(path_strings[['tjs']] %in% c('R(JSN)-1-S(JNN)-2-S(DNS)', 'R(JSN)-1-R(JNN)-2-R(DNS)')){
				
				# Get current line length -- easiest to calculate in real time in case link is composite of other joints
				line_len <- distPointToPoint(joint_current$coor[2, , 1], joint_current$coor[3, , joint_sets[3,1]])

				# Get point at distance from circle
				line_point <- joint_current$coor[3, , joint_sets[3,2]]

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
						') and closest to point {', paste0(signif(J2_compare, 3), collapse=','), '}\n')
					cat(solve_str)
				}

				# Define circle
				circle <- defineCircle(center=circle_center, nvector=circle_norm, 
					point_on_radius=circle_pt_on_rad, redefine_center=TRUE)

				# Find angle on circle at distance from point
				circle_t <- angleOnCircleFromPoint(circle=circle, dist=line_len, P=line_point, point_compare=J2_compare)

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
				V_pre <- joint_current3$coor[1, , joint_sets[3,1]] - J2_solve
				V_new <- line_point - J2_solve
				
				# Get axis to rotate link about
				if(path_strings[['tjs']] %in% c('R(JSN)-1-R(JNN)-2-R(DNS)')){
					joint_current2 <- applyJointTransform(mechanism, joint=joint_idx[2], iter=iter)
					J_axis <- joint_current2$cons[[1]][, , 1]
				}
				if(path_strings[['tjs']] %in% c('R(JSN)-1-S(JNN)-2-S(DNS)')) J_axis <- cprod(V_pre, V_new)
				
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
				#print(applyJointTransform(mechanism, joint=joint_idx[3], iter=iter)$coor[, , joint_sets[3,2]])

				# Set last joint as jointed
				mechanism[['status']][['jointed']][joint_idx[3]] <- TRUE
				mechanism[['status']][['transformed']][joint_idx[3], ] <- FALSE

				path_solved <- TRUE
			}

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