findOpenJoints <- function(joint.conn, body.conn, fixed.joints, body.names = NULL, indent = '\t', indent.level = 1, print.progress = FALSE){

	if(print.progress) cat(paste0(paste0(rep(indent, indent.level), collapse=''), 'findOpenJoints()'))

	# Get number of joints
	num_joints <- nrow(body.conn)

	## Find "sole joints" (joints that are the only joint connected to a particular body)
	sole_joints <- c()

	# Get bodies that are in body connection matrix just once
	not_in_joint_conn <- which(table(body.conn) == 1)

	# Remove fixed body
	not_in_joint_conn <- not_in_joint_conn[!not_in_joint_conn == 1]

	# Get joints connected to body
	if(length(not_in_joint_conn) > 0){
		for(i in 1:num_joints){
			if(body.conn[i,1] %in% not_in_joint_conn || body.conn[i,2] %in% not_in_joint_conn) sole_joints <- c(sole_joints, i)
		}
	}

	if(print.progress){
		cat(paste0('\n', paste0(rep(indent, indent.level+1), collapse=''), 'Sole joints (joints that are the only joint connected to a particular body):'))
		if(length(sole_joints) == 0){
			cat(' none')
		}else{
			cat(paste0(' ', paste0(sole_joints, collapse=', ')))
		}
	}

	## Find open joints
	# Vector to save which joints are open
	is_open <- rep(TRUE, num_joints)

	# For each joint
	for(joint_start in 1:num_joints){
	
		if(print.progress) cat(paste0('\n', paste0(rep(indent, indent.level+1), collapse=''), 'Starting at joint ', joint_start))

		#
		if(joint_start %in% sole_joints){
			if(print.progress) cat(paste0('\n', paste0(rep(indent, indent.level+2), collapse=''), 'Joint is the only joint connected to a particular body, joint ', joint_start, ' is open'))
			next
		}

		# Start list of joint path
		joint_path <- list(joint_start)
		body_path <- list()

		# Set joint path indices for looping
		joint_path_idx <- 1:length(joint_path)

		it <- 0
		while(it < 100){

			if(print.progress) cat(paste0('\n', paste0(rep(indent, indent.level+2), collapse=''), it, ':'))

			# Logical to test whether something has changed
			something_changed <- FALSE
			
			#
			for(i in joint_path_idx){

				if(print.progress) cat(paste0('\n', paste0(rep(indent, indent.level+3), collapse=''), i, ') '))
			
				# Get last joint
				last_joint <- tail(joint_path[[i]], 1)

				# If NA, skip
				if(last_joint == 0){
					if(print.progress) cat(paste0(paste0(joint_path[[i]], collapse=','), ' (', paste0(body.names[body_path[[i]]], collapse=','), ')'))
					next
				}
				
				# Find adjacent joints
				adj_joints <- joint.conn[[last_joint]]
				
				# Remove any joints already in path
				adj_joints <- adj_joints[!adj_joints %in% joint_path[[i]]]

				# Remove any adjacent joints that are adjacent to last two joints in path
				# Prevent search from getting more than two joints attached the same body
				if(length(adj_joints) > 0){

					# Find body shared by last joint and adjacent joint
					for(j in 1:length(adj_joints)){

						# Get body to add
						body_table <- table(body.conn[c(last_joint, adj_joints[j]), ])

						# Find body connected by both joints
						add_body <- as.numeric(names(body_table)[body_table == 2])

						# If body is already in path, mark adjacent joint as NA to skip						
						if(length(body_path) > 0 && add_body %in% body_path[[i]]) adj_joints[j] <- NA
					}

					# Remove any NAs
					adj_joints <- adj_joints[!is.na(adj_joints)]
				}

				# Mark that something changed
				something_changed <- TRUE

				# Add NA to stop path
				if(length(adj_joints) == 0){
					joint_path[[i]][length(joint_path[[i]])+1] <- NA
					joint_path_idx <- joint_path_idx[joint_path_idx != i]
					if(print.progress) cat(paste0(paste0(joint_path[[i]], collapse=','), ' (', paste0(body.names[body_path[[i]]], collapse=','), ')'))
					next
				}
				
				# Set paths before any additions
				joint_path_i <- joint_path[[i]]
				if(length(body_path) == 0){ body_path_i <- NULL }else{ body_path_i <- body_path[[i]] }
			
				# Then create new paths with the rest
				for(j in 1:length(adj_joints)){

					# Get body to add
					body_table <- table(body.conn[c(last_joint, adj_joints[j]), ])

					# Find body connected by both joints
					add_body <- as.numeric(names(body_table)[body_table == 2])
					
					# Get joint to add
					add_joint <- adj_joints[j]
					
					# Add 0 if fixed
					if(add_joint %in% fixed.joints){
						if(last_joint %in% fixed.joints){
							add_joint <- 0
							add_body <- 1
						}else{
							add_joint <- c(add_joint, 0)
							add_body <- c(add_body, 1)
						}
					}
					
					# Add first joint to path, create new paths with any subsequent joints
					if(j == 1){
						add_at <- i
					}else{
						add_at <- length(joint_path)+1
						joint_path_idx <- c(joint_path_idx, add_at)
					}

					# If reached fixed, mark path as done
					if(tail(add_joint, 1) == 0) joint_path_idx <- joint_path_idx[joint_path_idx != add_at]

					# Add to lists
					joint_path[[add_at]] <- c(joint_path_i, add_joint)
					if(is.null(body_path_i)){
						body_path[[add_at]] <- add_body
					}else{
						body_path[[add_at]] <- c(body_path_i, add_body)
					}

					if(print.progress){
						if(add_at == i){
							cat(paste0(paste0(joint_path[[add_at]], collapse=','), ' (', paste0(body.names[body_path[[add_at]]], collapse=','), ')'))
						}else{
							if(print.progress) cat(paste0('\n', paste0(rep(indent, indent.level+3), collapse=''), add_at, ') '))
							cat(paste0(paste0(joint_path[[add_at]], collapse=','), ' (', paste0(body.names[body_path[[add_at]]], collapse=','), ')'))
						}
					}
				}
			}
			
			# Find number of paths that end in 0
			path_end <- unlist(lapply(joint_path, tail, 1))
			
			# If more than one path 
			if(sum(path_end == 0, na.rm=TRUE) > 1){

				# Paths to fixed link
				end_0_paths <- which(path_end == 0)
				
				# Get first bodies from all paths to fixed link
				first_bodies <- unlist(lapply(body_path[end_0_paths], head, 1))
				
				# If more than one unique body, then joint is open
				if(length(unique(first_bodies)) > 1){
					is_open[joint_start] <- FALSE
					if(print.progress) cat(paste0('\n', paste0(rep(indent, indent.level+2), collapse=''), 'More than one body leading to fixed, joint ', joint_start, ' is closed'))
					break
				}
			}
			
			if(!something_changed){
				if(print.progress) cat(paste0('\n', paste0(rep(indent, indent.level+3), collapse=''), 'No change'))
				break
			}
			
			it <- it + 1
		}
	}

	if(print.progress) cat('\n')

	which_open <- which(is_open)
	
	if(length(which_open) == 0) return(NULL)
	
	which_open
}