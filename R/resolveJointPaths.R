resolveJointPaths <- function(mechanism, iter, print.progress = FALSE, indent = '\t', indent.level=3){

	if(print.progress) cat(paste0(paste0(rep(indent, indent.level), collapse=''), 'resolveJointPaths()\n'))

	if(mechanism[['num.paths.closed']] == 0){
		if(print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'No closed paths\n'))
		return(mechanism)
	}

	# Set max number of recursive loops
	ct_max <- 10

	#if(print.progress) print_joint_status(mechanism, indent, indent.level+2)

	ct <- 0
	while(ct < ct_max){
	
		# Keep track if path was solved
		path_solved <- FALSE
	
		# For each path
		i <- 1
		while(i <= mechanism[['num.paths.closed']]){
	
			# Get joint indices
			joint_idx <- mechanism[['paths.closed']][[i]]
			
			# Get whether halves are solved
			joint_sets <- mechanism[['paths.closed.set']][[i]]
			
			# Skip if any joints are already solved
			if(any(rowSums(mechanism[['status']][['solved']][joint_idx, ] > 0) > 1)){ i <- i + 1; next }

			# Create string
			path_strings <- createJointPathString(mechanism, joint_idx, i, print.progress)
			path_str <- path_strings[['test']]
	
			# **** Check string against solvable path strings
			solvable <- ifelse(path_str %in% c('R(JSN)-R(JNN)-R(DNS)', 'R(JSN)-R(DNN)-R(DNS)', 
				'R(DSN)-R(JNN)-R(DNS)', 'R(JSN)-R(DNN)-R(JNS)', 'R(DSN)-R(DNN)-R(DNS)'), TRUE, FALSE)
	
			if(print.progress){
				if(ct == ct_max-1){ ct_print <- 'max' }else{ ct_print <- ct }
				if(solvable){
					cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Solving path:  ', path_strings[['print']], ', n=', i, ', ct=', ct_print, '\n'))
				}else{
					cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Skipping path: ', path_strings[['print']], ', n=', ct_print, '\n'))
				}
			}

			# If not solvable, skip
			if(!solvable){ i <- i + 1; next }

			# Get path bodies
			path_bodies <- mechanism[['paths.closed.bodies']][[i]]

			# Get original distances between joints in path (used for solving some paths)
			path_dists <- mechanism[['paths.closed.dist']][[i]]

			if(path_str %in% c('R(JSN)-R(DNN)-R(DNS)', 'R(JSN)-R(DNN)-R(JNS)')){

				#if(print.progress) print_joint_status(mechanism, indent)

				if(print.progress) cat(paste0(paste0(rep(indent, indent.level+2), collapse=''), 
					'Standardizing path by joining ', mechanism[['joint.names']][joint_idx[2]], 
					' (', joint_idx[2], ')\n'))

				if(TRUE){

					# Extend transformation across joint joint_idx[1] by transforming path_bodies[1]
					# and then extending that same joining transformation across all disjointed joints
					mechanism <- extendTransformation2(mechanism, joint=joint_idx[2], body=path_bodies[2],
						body.excl=path_bodies[1], iter=iter, recursive=TRUE, print.progress=print.progress, indent=indent, 
						indent.level=indent.level+2)

				}else{
					# Find inverse transformation that returns body to initial pose
					itmat <- solve(mechanism[['tmat']][, , path_bodies[2], iter])
				
					# Get combined transformation of returning body to initial pose and adding transformation of joined body
					# This will 
					# **** not sure of order here!
					etmat <- mechanism[['tmat']][, , path_bodies[1], iter] %*% itmat

					# Apply transformation of first body to second body to rejoin the bodies
					mechanism <- transformBody(mechanism, body=path_bodies[2], 
						tmat=mechanism[['tmat']][, , path_bodies[1], iter], iter=iter, 
						at.joint=joint_idx[2], replace=TRUE, status.solved.to=0, status.jointed.to=TRUE, 
						print.progress=print.progress, indent=indent, indent.level=indent.level+2)

					# Extend transformation (removes any transformation of path_bodies[2] and then applies new transformation that joins joint 2)
					mechanism <- extendTransformation2(mechanism, tmat=etmat, 
						body.excl=path_bodies[1], iter=iter, recursive=TRUE, print.progress=print.progress, indent=indent, 
						indent.level=indent.level+2)
				}

				next
			}

			if(path_str %in% c('R(DSN)-R(JNN)-R(DNS)', 'R(DSN)-R(DNN)-R(DNS)')){

				#if(print.progress) print_joint_status(mechanism, indent)

				if(print.progress) cat(paste0(paste0(rep(indent, indent.level+2), collapse=''), 
					'Standardizing path by joining ', mechanism[['joint.names']][joint_idx[1]], 
					' (', joint_idx[1], ')\n'))

				# 
				if(TRUE){

					# Extend transformation across joint joint_idx[1] by transforming path_bodies[1]
					# and then extending that same joining transformation across all disjointed joints
					mechanism <- extendTransformation2(mechanism, joint=joint_idx[1], body=path_bodies[1],
						body.excl=path_bodies[1], iter=iter, recursive=TRUE, print.progress=print.progress, indent=indent, 
						indent.level=indent.level+2)

				}else{

					# Find body across first joint
					body_0 <- mechanism[['body.conn.num']][joint_idx[1], ]
					body_0 <- body_0[body_0 != path_bodies[1]]

					# Find inverse transformation that returns body to initial pose
					itmat <- solve(mechanism[['tmat']][, , path_bodies[1], iter])
					etmat <- mechanism[['tmat']][, , body_0, iter] %*% itmat

					# Apply transformation of first body to second body to rejoin the bodies
					mechanism <- transformBody(mechanism, body=path_bodies[1], 
						tmat=mechanism[['tmat']][, , body_0, iter], iter=iter, 
						at.joint=joint_idx[1], replace=TRUE, status.solved.to=0, status.jointed.to=TRUE, 
						print.progress=print.progress, indent=indent, indent.level=indent.level+2)

					# Extend transformation
					mechanism <- extendTransformation2(mechanism, tmat=etmat, 
						body.excl=path_bodies[1], iter=iter, recursive=TRUE, print.progress=print.progress, indent=indent, 
						indent.level=indent.level+2)
				}

				next
			}

			#
			if(path_str %in% c('R(JSN)-R(JNN)-R(DNS)')){
				#break
	
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
						'Finding ', mechanism[['joint.names']][joint_idx[2]], '(', joint_idx[2], 
						') on circle with center at ', mechanism[['joint.names']][joint_idx[3]], 
						'(', joint_idx[3], '), ', 'radius=', signif(circle_rad, 3), ', and normal={' , 
						paste0(signif(circle_norm, 3), collapse=','), '}', ' at distance=', 
						signif(line_len, 3), ' from ', mechanism[['joint.names']][joint_idx[1]], 
						'(', joint_idx[1], ') and closest to point {', paste0(signif(line_point_prev, 3), collapse=','), '}\n')
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

				# Extend transformations across second joint and throughout
				mechanism <- extendTransformation2(mechanism, tmat=tmat_B, iter=iter, recursive=TRUE, 
					body.excl=path_bodies[1], print.progress=print.progress, indent=indent, indent.level=indent.level+2)

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
				mechanism <- transformBody(mechanism, body=path_bodies[2], tmat=tmat_B, 
					iter=iter, at.joint=joint_idx[2], status.solved.to=1, print.progress=print.progress, 
					indent=indent, indent.level=indent.level+2)

				# Set last joint as jointed
				mechanism[['status']][['jointed']][joint_idx[3]] <- TRUE
				mechanism[['status']][['transformed']][joint_idx[3], ] <- FALSE

				# Extend transformation
				mechanism <- extendTransformation2(mechanism, tmat=tmat_B, iter=iter, recursive=TRUE, 
					body.excl=path_bodies[1:2], print.progress=print.progress, indent=indent, 
					indent.level=indent.level+2)
			}

			# Extend solved status through mechanism
			mechanism <- extendSolvedStatus(mechanism, print.progress=print.progress, 
				indent=indent, indent.level=indent.level+2)

			if(print.progress) print_joint_status(mechanism, indent, indent.level+2)

			path_solved <- TRUE
			i <- i + 1
		}
		
		# If no path was solved, break
		if(!path_solved) break
		
		# Advance count
		ct <- ct + 1
	}

	#if(print.progress) print_joint_status(mechanism, indent)

	mechanism
}