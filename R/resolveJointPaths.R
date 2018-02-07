resolveJointPaths <- function(mechanism, iter, print.progress = FALSE, indent = '\t', indent.level=3){

	if(print.progress) cat(paste0(paste0(rep(indent, indent.level), collapse=''), 'resolveJointPaths()\n'))

	if(mechanism[['num.paths.closed']] == 0){
		if(print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'No closed paths\n'))
		return(mechanism)
	}

	# Set max number of recursive loops
	n_max <- 10

	#if(print.progress) print_joint_status(mechanism, indent, indent.level+2)

	n <- 0
	while(n < n_max){
	
		# Keep track if path was solved
		path_solved <- FALSE
	
		# For each path
		for(i in 1:mechanism[['num.paths.closed']]){
	
			# Get joint indices
			joint_idx <- mechanism[['paths.closed']][[i]]
			
			# Get path length
			path_length <- length(joint_idx)

			# Get joint types
			joint_types <- mechanism[['joint.types']][joint_idx]

			# Get whether joints are jointed
			joint_jointed <- rep('D', path_length)
			joint_jointed[mechanism[['status']][['jointed']][joint_idx]] <- 'J'
	
			# Get whether halves are solved
			joint_sets <- mechanism[['paths.closed.set']][[i]]

			# Get whether joint is solved/fixed
			joint_solved_set1 <- joint_solved_set2 <- rep('N', path_length)
			for(j in 1:length(joint_idx)){
				if(mechanism[['status']][['solved']][joint_idx[j], joint_sets[j,1]] > 0) joint_solved_set1[j] <- 'S'
				if(mechanism[['status']][['solved']][joint_idx[j], joint_sets[j,2]] > 0) joint_solved_set2[j] <- 'S'
			}

			# Create string
			path_str <- paste0(joint_types, '(', joint_jointed, joint_solved_set1, joint_solved_set2 ,')', collapse='-')
	
			# **** Check string against solvable path strings
			solvable <- ifelse(path_str %in% c('R(JSN)-R(JNN)-R(DNS)'), TRUE, FALSE)
	
			if(print.progress){

				if(n == n_max-1){ n_print <- 'max' }else{ n_print <- n }

				path_str_print <- paste0(mechanism[['joint.names']][joint_idx], '(', joint_jointed, joint_solved_set1, joint_solved_set2 ,')', collapse='-')
				if(solvable){
					cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Solving path:  ', path_str_print, ', n=', n_print, '\n'))
				}else{
					#cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Skipping path: ', path_str_print, ', n=', n_print, '\n'))
				}
			}

			# If not solvable, skip
			if(!solvable) next

			# Get path bodies
			path_bodies <- mechanism[['paths.closed.bodies']][[i]]

			# Get original distances between joints in path (used for solving some paths)
			path_dists <- mechanism[['paths.closed.dist']][[i]]

			#
			if(path_str %in% c('R(JSN)-R(JNN)-R(DNS)')){
	
				# Get line length
				line_len <- path_dists[1]
				line_point <- mechanism[['joint.coor.anim']][joint_idx[1], , iter, 1]
				if(iter == 1){
					line_point_prev <- mechanism[['joint.coor']][joint_idx[2], ]
				}else{
					line_point_prev <- mechanism[['joint.coor.anim']][joint_idx[2], , iter-1, 1]
				}
				
				circle_rad <- path_dists[2]
				circle_center <- mechanism[['joint.coor.anim']][joint_idx[3], , iter, joint_sets[3,2]]
				circle_norm <- mechanism[['joint.cons.anim']][[joint_idx[3]]][, , iter, joint_sets[3,2]]

				# 
				if(print.progress){
					solve_str <- paste0(paste0(rep(indent, indent.level+2), collapse=''), 
						#paste0('By point on circle at distance from point\n', paste0(rep(indent, indent.level+2), collapse=''), 
						'Finding point on circle with center {', paste0(signif(circle_center, 3), collapse=','), 
						'}, ', 'radius \'', signif(circle_rad, 3), '\', & normal {' , 
						paste0(signif(circle_norm, 3), collapse=','), '}', ' at distance \'', 
						signif(line_len, 3), '\' from point {', paste0(signif(line_point, 3), collapse=','), 
						'} & closest to point {', paste0(signif(line_point_prev, 3), collapse=','), '}\n')
					cat(solve_str)
				}

				# Define circle
				circle <- defineCircle(center=circle_center, nvector=circle_norm, radius=circle_rad)

				# Find angle on circle at distance from point
				circle_t <- angleOnCircleFromPoint(circle=circle, dist=line_len, P=line_point, point_compare=line_point_prev)

				# Find point on circle
				J2_solve <- circlePoint(circle=circle, T=circle_t)

				# If no solution, return NULL
				if(is.vector(J2_solve)){
					if(is.na(J2_solve[1])) return(mechanism)
				}else{
					if(is.na(J2_solve[1,1])) return(mechanism)
				}

				# Get vectors and axis to find first body transformation
				V_pre <- mechanism[['joint.coor.anim']][joint_idx[2], , iter, 1] - mechanism[['joint.coor.anim']][joint_idx[1], , iter, 1]
				V_new <- J2_solve - mechanism[['joint.coor.anim']][joint_idx[1], , iter, 1]
				J_axis <- mechanism[['joint.cons.anim']][[joint_idx[1]]][, , iter, 1]

				# Get transformation of first body
				tmat_B_1 <- tmat_B_2 <- tmat_B_3 <- diag(4)
				tmat_B_1[1:3, 4] <- -mechanism[['joint.coor.anim']][joint_idx[1], , iter, 1]
				tmat_B_2[1:3, 1:3] <- tMatrixEP(J_axis, avec(V_pre, V_new, axis=J_axis, about.axis=TRUE))
				tmat_B_3[1:3, 4] <- mechanism[['joint.coor.anim']][joint_idx[1], , iter, 1]
				tmat_B <- tmat_B_3 %*% tmat_B_2 %*% tmat_B_1
		
				# Transform first body
				mechanism <- transformBody(mechanism, body=path_bodies[1], 
					tmat=tmat_B, iter=iter, at.joint=joint_idx[1], status.solved.to=1, print.progress=print.progress, 
					indent=indent, indent.level=indent.level+2)

				# Extend transformation
				mechanism <- extendTransformation2(mechanism, tmat=tmat_B, iter=iter, recursive=TRUE, 
					print.progress=print.progress, indent=indent, indent.level=indent.level+2)

				# Get vectors and axis to find second body transformation
				V_pre <- mechanism[['joint.coor.anim']][joint_idx[3], , iter, joint_sets[3,1]] - J2_solve
				V_new <- circle_center - J2_solve
				J_axis <- mechanism[['joint.cons.anim']][[joint_idx[3]]][, , iter, 1]

				# Get transformation of second body
				tmat_B_1 <- tmat_B_2 <- tmat_B_3 <- diag(4)
				tmat_B_1[1:3, 4] <- J2_solve
				tmat_B_2[1:3, 1:3] <- tMatrixEP(J_axis, avec(V_pre, V_new, axis=J_axis, about.axis=TRUE))
				tmat_B_3[1:3, 4] <- -J2_solve
				tmat_B <- tmat_B_1 %*% tmat_B_2 %*% tmat_B_3

				# Solve body 2
				mechanism <- transformBody(mechanism, body=path_bodies[2], 
					tmat=tmat_B, iter=iter, at.joint=joint_idx[2], status.solved.to=1, print.progress=print.progress, 
					indent=indent, indent.level=indent.level+2)

				# Extend transformation
				mechanism <- extendTransformation2(mechanism, tmat=tmat_B, iter=iter, recursive=TRUE, 
					print.progress=print.progress, indent=indent, indent.level=indent.level+2)
			}

			path_solved <- TRUE
		}
		
		# If no path was solved, break
		if(!path_solved) break
		
		# Advance count
		n <- n + 1
	}

	#if(print.progress) print_joint_status(mechanism, indent)

	mechanism
}