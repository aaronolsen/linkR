findOpenJoints <- function(joint.conn, body.conn, fixed.joints, body.names = NULL, indent = '\t', indent.level = 1, print.progress = FALSE){

	if(print.progress) cat(paste0(paste0(rep(indent, indent.level), collapse=''), 'findOpenJoints()\n'))

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
		cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Sole joints (joints that are the only joint connected to a particular body):'))
		if(length(sole_joints) == 0){
			cat(' none\n')
		}else{
			cat(paste0(' ', paste0(sole_joints, collapse=', '), '\n'))
		}
	}

	# Vector to save which joints are open
	is_open <- rep(TRUE, num_joints)

	# For each joint
	for(joint_start in 1:num_joints){
	
		if(print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Starting at joint ', joint_start, '\n'))

		#
		if(joint_start %in% sole_joints){
			if(print.progress) cat(paste0(paste0(rep(indent, indent.level+2), collapse=''), 'Joint is the only joint connected to a particular body, joint ', joint_start, ' is open\n'))
			next
		}

		# Start list of joint chain
		joint_chain <- list()
		body_chain <- list()

		# Get adjacent joints
		adj_joints <- joint.conn[[joint_start]]

		# If joint is fixed, remove any adjacent fixed joints (don't go through fixed body)
		if(joint_start %in% fixed.joints) adj_joints <- adj_joints[!adj_joints %in% fixed.joints]

		# If only fixed adjacent joints, joint must be open
		if(length(adj_joints) == 0){
			if(print.progress) cat(paste0(paste0(rep(indent, indent.level+2), collapse=''), 'Only fixed adjacent joints, joint ', joint_start, ' is open\n'))
			next
		}

		# For each adjacent joint
		for(i in 1:length(adj_joints)){
			
			# Create new joint chain
			joint_chain[[i]] <- c(joint_start, adj_joints[i])

			# Get bodies connected to joints
			body_table <- table(body.conn[c(joint_start, adj_joints[i]), ])

			# Find body connected by both joints
			body_chain[[i]] <- as.numeric(names(body_table)[body_table == 2])
			
			if(adj_joints[i] %in% fixed.joints){
				joint_chain[[i]] <- c(joint_chain[[i]], 0)
				body_chain[[i]] <- c(body_chain[[i]], 1)
			}
		}
		
		# If fixed joint add chain with 0
		if(joint_start %in% fixed.joints){
			joint_chain[[length(joint_chain)+1]] <- c(joint_start, 0)
			body_chain[[length(body_chain)+1]] <- 1
		}
		
		it <- 0
		while(it < 100){

			# Logical to test whether something has changed
			something_changed <- FALSE
			
			#
			for(i in 1:length(joint_chain)){
			
				# Get last joint
				last_joint <- tail(joint_chain[[i]], 1)

				# If NA, skip
				if(is.na(last_joint)) next
				if(last_joint == 0) next
				
				# Find adjacent joints
				adj_joints <- joint.conn[[last_joint]]
				
				# Remove any joints already in chain
				adj_joints <- adj_joints[!adj_joints %in% joint_chain[[i]]]

				# Remove any adjacent joints that are adjacent to last two joints in chain
				# Prevent search from getting more than two joints attached the same body
				if(length(adj_joints) > 0){

					# Find body shared by last joint and adjacent joint
					for(j in 1:length(adj_joints)){

						# Get body to add
						body_table <- table(body.conn[c(last_joint, adj_joints[j]), ])

						# Find body connected by both joints
						add_body <- as.numeric(names(body_table)[body_table == 2])

						# If body is already in chain, mark adjacent joint as NA to skip						
						if(add_body %in% body_chain[[i]]) adj_joints[j] <- NA
					}

					# Remove any NAs
					adj_joints <- adj_joints[!is.na(adj_joints)]
				}

				# Mark that something changed
				something_changed <- TRUE

				# Add NA to stop chain
				if(length(adj_joints) == 0){
					joint_chain[[i]][length(joint_chain[[i]])+1] <- NA
					next
				}
				
				# Set chains before any additions
				joint_chain_i <- joint_chain[[i]]
				body_chain_i <- body_chain[[i]]
			
				# Then create new chains with the rest
				for(j in 1:length(adj_joints)){

					# Get body to add
					body_table <- table(body.conn[c(last_joint, adj_joints[j]), ])

					# Find body connected by both joints
					add_body <- as.numeric(names(body_table)[body_table == 2])
					
					# Get joint to add
					add_joint <- adj_joints[j]
					
					# Add 0 if fixed
					if(add_joint %in% fixed.joints){
						add_joint <- c(add_joint, 0)
						add_body <- c(add_body, 1)
					}
					
					# Add first joint to chain, create new chains with any subsequent joints
					if(j == 1){ add_at <- i }else{ add_at <- length(joint_chain)+1 }

					joint_chain[[add_at]] <- c(joint_chain_i, add_joint)
					body_chain[[add_at]] <- c(body_chain_i, add_body)
				}
			}
			
			# Find number of paths that end in 0
			chain_end <- unlist(lapply(joint_chain, tail, 1))
			
			# If more than one chain 
			if(sum(chain_end == 0, na.rm=TRUE) > 1){

				# Paths to fixed link
				end_0_chains <- which(chain_end == 0)
				
				#print(joint_chain[end_0_chains])
				#print(body_chain[end_0_chains])
				#cat('********\n')

				# Get first bodies from all paths to fixed link
				first_bodies <- unlist(lapply(body_chain[end_0_chains], head, 1))
				
				# If more than one unique body, then joint is open
				if(length(unique(first_bodies)) > 1){
					is_open[joint_start] <- FALSE
					if(print.progress) cat(paste0(paste0(rep(indent, indent.level+2), collapse=''), 'More than one body leading to fixed, joint ', joint_start, ' is closed\n'))
					break
				}
			}
			
			if(!something_changed) break
			
			it <- it + 1
		}

		if(print.progress){
			for(i in 1:length(joint_chain)){
				cat(paste0(paste0(rep(indent, indent.level+2), collapse=''), paste0(joint_chain[[i]], collapse=','), '\n'))
				if(is.null(body.names)){
					cat(paste0(paste0(rep(indent, indent.level+3), collapse=''), paste0(body_chain[[i]], collapse=','), '\n'))
				}else{
					cat(paste0(paste0(rep(indent, indent.level+3), collapse=''), paste0(body.names[body_chain[[i]]], collapse=','), '\n'))
				}
			}
		}
		
		#break
	}

	which_open <- which(is_open)
	
	if(length(which_open) == 0) return(NULL)
	
	which_open
}