findSolvablePaths <- function(joint.conn, body.conn, input.joint, input.dof, fixed.joints, open.joints, joint.types, 
	body.names = NULL, joint.names = NULL, indent = '\t', indent.level = 1, print.progress = FALSE){

	if(print.progress) cat(paste0(paste0(rep(indent, indent.level), collapse=''), 'findSolvablePaths()'))

	# Which joints are fully input (all DoFs specified)
	input_joint_is_full <- rep(TRUE, length(input.joint))
	for(i in 1:length(input.joint)) if(any(is.na(input.dof[[i]]))) input_joint_is_full[i] <- FALSE
	
	# Vector of just those input joints that have all DoFs specified
	input_joint_full <- input.joint[input_joint_is_full]

	# Create list for saving solvable paths
	solv_path_joints <- list()
	solv_path_bodies <- list()
	added_solv_path_joints <- c()

	# Get number of joints
	num_joints <- nrow(body.conn)

	# If all open joints return NULL
	if(length(open.joints) == num_joints){
		return(list(
			'paths.joints'=NULL,
			'paths.bodies'=NULL
		))
	}

	# Vector to save which joints are open
	is_open <- rep(TRUE, num_joints)

	# By default print for all joints
	joint_start_print <- 1:num_joints
	#joint_start_print <- 5

	# For each joint
	for(joint_start in 1:num_joints){
	
		if(joint_start %in% open.joints){
			if(print.progress && joint_start %in% joint_start_print) cat(paste0('\n', paste0(rep(indent, indent.level+1), collapse=''), 'Skipping open joint ', joint_start))
			next
		}

		if(joint_start %in% input_joint_full){
			if(print.progress && joint_start %in% joint_start_print) cat(paste0('\n', paste0(rep(indent, indent.level+1), collapse=''), 'Skipping input joint ', joint_start))
			next
		}

		if(print.progress && joint_start %in% joint_start_print){
			cat(paste0('\n', paste0(rep(indent, indent.level+1), collapse=''), 'Starting at joint ', joint_start))
			if(!is.null(joint.names)) cat(paste0(' (', joint.names[joint_start], ')'))
		}

		# Start lists
		joint_path <- list(joint_start)
		body_path <- list()
		
		# Set joint path indices for looping
		joint_path_idx <- 1:length(joint_path)
		
		it <- 0
		while(it < 100){

			if(print.progress && joint_start %in% joint_start_print) cat(paste0('\n', paste0(rep(indent, indent.level+2), collapse=''), 'Iter ', it, ':'))

			# Logical to test whether something has changed
			something_changed <- FALSE
			
			# 
			for(i in joint_path_idx){
			
				if(print.progress && joint_start %in% joint_start_print){
					cat(paste0('\n', paste0(rep(indent, indent.level+3), collapse=''), 'Main ', i, ') '))
				}

				# Get last joint
				last_joint <- tail(joint_path[[i]], 1)

				# If NA, remove index and skip
				if(last_joint == 0){
					if(print.progress && joint_start %in% joint_start_print){
						cat(paste0(paste0(joint_path[[i]], collapse=','), ' (', paste0(body.names[body_path[[i]]], collapse=','), ')'))
					}
					#joint_path_idx <- joint_path_idx[joint_path_idx != i]
					next
				}

				# Find adjacent joints
				adj_joints <- joint.conn[[last_joint]]

				# Remove any joints already in path
				adj_joints <- adj_joints[!adj_joints %in% joint_path[[i]]]

				# Remove any open joints
				adj_joints <- adj_joints[!adj_joints %in% open.joints]

				# Remove any adjacent joints that are adjacent to last two joints in path
				# Prevent search from getting more than two joints attached to the same body
				# This is just running through each add_body to do a check, a final 
				# add_body is set later
				if(length(adj_joints) > 0){

					# Find body shared by last joint and adjacent joint
					for(j in 1:length(adj_joints)){

						# Get body to add
						body_table <- table(body.conn[c(last_joint, adj_joints[j]), ])

						# Find body connected by both joints
						add_body <- as.numeric(names(body_table)[body_table == 2])

						if(length(body_path) > 0){
							
							# Stop if body is the previous two bodies in path
							if(tail(body_path[[i]],1) == add_body && tail(body_path[[i]],2) == add_body){
								if(print.progress && joint_start %in% joint_start_print){
									cat(paste0('\n', paste0(rep(indent, indent.level+4), collapse=''), 'Same body is already in path twice immediately preceding new body.'))
								}
								adj_joints[j] <- NA
							}
							
							# Stop if body is in path but not immediately preceding
							if(tail(body_path[[i]],1) != add_body && add_body %in% body_path[[i]]){
								if(print.progress && joint_start %in% joint_start_print){
									cat(paste0('\n', paste0(rep(indent, indent.level+4), collapse=''), 'Same body already added earlier in path.'))
								}
								adj_joints[j] <- NA
							}
						}
					}

					# Remove any NAs
					adj_joints <- adj_joints[!is.na(adj_joints)]
				}

				# Mark that something changed
				something_changed <- TRUE

				# If no adjacent joints, add NA to stop path
				if(length(adj_joints) == 0){

					# Add NA
					joint_path[[i]][length(joint_path[[i]])+1] <- NA
					joint_path_idx <- joint_path_idx[joint_path_idx != i]

					if(print.progress && joint_start %in% joint_start_print){
						cat(paste0(paste0(joint_path[[i]], collapse=','), ' (', paste0(body.names[body_path[[i]]], collapse=','), ')'))
					}

					next
				}

				# Set paths before any additions
				joint_path_i <- joint_path[[i]]
				if(length(body_path) == 0){ body_path_i <- NULL }else{ body_path_i <- body_path[[i]] }
		
				# Then create new paths with the rest
				for(j in 1:length(adj_joints)){

					# Get joint to add
					add_joint <- adj_joints[j]
				
					# Get body to add
					body_table <- table(body.conn[c(last_joint, add_joint), ])

					# Find body connected by both joints
					add_body <- as.numeric(names(body_table)[body_table == 2])
				
					# Add 0 if fixed
					if(add_joint %in% fixed.joints){
						if(last_joint %in% fixed.joints || add_joint %in% input_joint_full){
							add_joint <- 0
							add_body <- 1
						}else{
							#add_joint <- c(add_joint, 0)
							#add_body <- c(add_body, 1)
						}
					}
				
					# Add first joint to path, create new paths with any subsequent joints
					if(j == 1){
						add_at <- i
					}else{
						add_at <- length(joint_path)+1
						joint_path_idx <- c(joint_path_idx, add_at)

						if(print.progress && joint_start %in% joint_start_print){
							cat(paste0('\n', paste0(rep(indent, indent.level+3), collapse=''), 'Spur ', add_at, ') '))
						}
					}
					
					# Add to lists
					joint_path[[add_at]] <- c(joint_path_i, add_joint)
					if(is.null(body_path_i)){
						body_path[[add_at]] <- add_body
					}else{
						body_path[[add_at]] <- c(body_path_i, add_body)
					}

					if(print.progress && joint_start %in% joint_start_print){
						#cat(paste0(paste0(joint_path[[add_at]], collapse=','), ' (', paste0(body.names[body_path[[add_at]]], collapse=','), ')'))
						cat(paste0('\n', paste0(rep(indent, indent.level+4), collapse=''), paste0(joint_path[[add_at]], collapse=','), ' (', paste0(body.names[body_path[[add_at]]], collapse=','), ')'))
					}

					if(tail(add_joint, 1) == 0){
					
						# Check if there are additional joints

						# Since reached fixed, mark path as done
						joint_path_idx <- joint_path_idx[joint_path_idx != add_at]
						
						# Not adding a new joint so no need to proceed to search
						if(length(add_joint) == 1){
							if(print.progress && joint_start %in% joint_start_print){
								cat(paste0('\n', paste0(rep(indent, indent.level+4), collapse=''), 'End of path, stop'))
							}
							next
						}

						# If only one joint and end, do not need to check solvable paths
						if(length(joint_path[[add_at]]) == 2){
							if(print.progress && joint_start %in% joint_start_print){
								cat(paste0('\n', paste0(rep(indent, indent.level+4), collapse=''), 'Single joint to fixed path, non-solvable path'))
							}
							next
						}
					}
					
					# Set any joints to 
					ignore_joints <- which(joint_path[[add_at]] %in% input_joint_full)

					# Create joint-body string
					jt_str <- createJointPathString(joint.types=joint.types[joint_path[[add_at]]], 
						bodies=body_path[[add_at]], mode = c('t', 'f'), ignore.joints=ignore_joints)

					if(print.progress && joint_start %in% joint_start_print){
						cat(paste0(': ', jt_str[['t']], ' [', jt_str[['f']], ']'))
					}
					
					# Check string against solvable paths
					if(jt_str[['t']] %in% linkR_sp[['str']][['t']]){

						# Remove any NAs or 0s
						final_joint_path <- joint_path[[add_at]]
						final_joint_path <- final_joint_path[final_joint_path != 0]
						final_joint_path <- final_joint_path[!is.na(final_joint_path)]

						# Remove fixed body
						final_body_path <- body_path[[add_at]]
						final_body_path <- final_body_path[final_body_path != 1]
						
						# Remove bodies after any input joints (skip last joint here since body vector has length one less than joints)
						final_joint_path_short <- final_joint_path[1:(length(final_joint_path)-1)]
						final_body_path <- final_body_path[!final_joint_path_short %in% input_joint_full]

						# If last joint is input joint remove last body
						if(tail(final_joint_path, 1) %in% input_joint_full) final_body_path <- final_body_path[1:(length(final_body_path)-1)]

						# Remove input joints
						final_joint_path <- final_joint_path[!final_joint_path %in% input_joint_full]
						
						# Create joint string to compare against previous strings
						joint_collapse <- paste0(final_joint_path, collapse='-')

						if(joint_collapse %in% added_solv_path_joints){
							if(print.progress && joint_start %in% joint_start_print){
								cat(paste0('\n', paste0(rep(indent, indent.level+4), collapse=''), 'Joint string matches previously found string'))
							}
							next
						}

						# Add joints to paths list
						solv_path_joints[[length(solv_path_joints)+1]] <- final_joint_path
						solv_path_bodies[[length(solv_path_bodies)+1]] <- final_body_path
						added_solv_path_joints <- c(added_solv_path_joints, joint_collapse)

						if(print.progress && joint_start %in% joint_start_print){
							cat(paste0('\n', paste0(rep(indent, indent.level+4), collapse=''), 'Joint-body string matches solvable path'))
						}

					}else{

						if(tail(add_joint, 1) == 0){
							if(print.progress && joint_start %in% joint_start_print){
								cat(paste0('\n', paste0(rep(indent, indent.level+4), collapse=''), 'No match and reached end of path, stop'))
							}
							next
						}

						# Check whether string matches start of any paths
						grepl_match <- grepl(paste0('^', jt_str[['t']]), linkR_sp[['str']][['t']])
						
						#
						if(any(grepl_match)){
							if(print.progress && joint_start %in% joint_start_print){
								cat(paste0('\n', paste0(rep(indent, indent.level+4), collapse=''), 'Joint-body string matches start of solvable path, continue'))
							}
						}else{
							joint_path_idx <- joint_path_idx[joint_path_idx != add_at]
							if(print.progress && joint_start %in% joint_start_print){
								cat(paste0('\n', paste0(rep(indent, indent.level+4), collapse=''), 'Joint-body string does not match start of any solvable path, stop'))
							}
						}
					}
				}
			}
			
			if(!something_changed){
				if(print.progress && joint_start %in% joint_start_print) cat(' No change')
				break
			}
			
			it <- it + 1
		}
		
		#break
	}

	if(print.progress) cat('\n')

	#print(linkR_sp[['str']][['t']])
	
	# If no paths found, set as NULL
	if(length(solv_path_joints) == 0) solv_path_joints <- NULL
	if(length(solv_path_bodies) == 0) solv_path_bodies <- NULL

	return(list(
		'paths.joints'=solv_path_joints,
		'paths.bodies'=solv_path_bodies
	))
}