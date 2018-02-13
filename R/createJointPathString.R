createJointPathString <- function(bodies, joint.types = NULL, jointed = NULL, mechanism = NULL, 
	status = NULL, joints = NULL, path = NULL, mode = c('t'), ignore.joints = NULL, print.progress = FALSE){

	#
	if(print.progress) mode <- c(mode, 'p')
	
	# If no indices, set to default
	if(is.null(joints)) joints <- 1:length(joint.types)
	
	# Get path length
	path_length <- length(joints)

	# Get joint types
	if(is.null(joint.types)) joint.types <- mechanism[['joint.types']][joints]

	# Get whether joints are jointed
	if(is.null(jointed) && ('tj' %in% mode || 'tjs' %in% mode)){
		jointed <- rep('D', path_length)
		jointed[mechanism[['status']][['jointed']][joints]] <- 'J'
	}
	
	if('p' %in% mode) joint_names <- mechanism[['joint.names']][joints]

	# Get whether joints are jointed
	if(is.null(status) && ('tjs' %in% mode || 'ts' %in% mode)){

		# Get whether halves are solved
		joint_sets <- mechanism[['paths.closed.set']][[path]]

		# Get whether joint is solved/fixed
		joint_solved_set1 <- joint_solved_set2 <- rep('N', path_length)
		for(j in 1:length(joints)){
			if(mechanism[['status']][['solved']][joints[j], joint_sets[j,1]] > 1) joint_solved_set1[j] <- 'S'
			if(mechanism[['status']][['solved']][joints[j], joint_sets[j,2]] > 1) joint_solved_set2[j] <- 'S'
		}

		# Create status
		status <- paste0(joint_solved_set1, joint_solved_set2)
	}
	
	# Create full string of all joints, including ignored ones
	if('f' %in% mode){

		str_f <- ''

		# Replace bodies with ordered index
		unique_bodies <- unique(bodies)
		bodies_idx <- rep(NA, length(bodies))
		for(i in 1:length(bodies)) bodies_idx[i] <- which(bodies[i] == unique_bodies)

		for(i in 1:length(joints)){
			if(i < length(joints)){
				if('f' %in% mode) str_f <- paste0(str_f, joint.types[i], '-', bodies_idx[i], '-')
			}else{
				if('f' %in% mode)  str_f <- paste0(str_f, joint.types[i])
			}
		}

	}else{
		str_f <- NULL
	}

	# 
	if(!is.null(ignore.joints)){
		all_joints <- 1:(length(joints)-1)
		include_joints <- all_joints[!all_joints %in% ignore.joints]
		bodies <- bodies[include_joints]

		#
		joints <- joints[!joints %in% ignore.joints]
		if('p' %in% mode) joint_names <- mechanism[['joint.names']][joints]
		joint.types <- joint.types[joints]
		jointed <- jointed[joints]
		status <- status[joints]
	}

	# Replace bodies with ordered index
	unique_bodies <- unique(bodies)
	bodies_idx <- rep(NA, length(bodies))
	for(i in 1:length(bodies)) bodies_idx[i] <- which(bodies[i] == unique_bodies)
	
	if('t' %in% mode){ str_t <- '' }else{ str_t <- NULL }
	if('tj' %in% mode){ str_tj <- '' }else{ str_tj <- NULL }
	if('ts' %in% mode){ str_ts <- '' }else{ str_ts <- NULL }
	if('tjs' %in% mode){ str_tjs <- '' }else{ str_tjs <- NULL }
	if('p' %in% mode){ str_p <- '' }else{ str_p <- NULL }

	for(i in 1:length(joints)){
		
		if(i < length(joints)){
			if('t' %in% mode) str_t <- paste0(str_t, joint.types[i], '-', bodies_idx[i], '-')
			if('tj' %in% mode) str_tj <- paste0(str_tj, joint.types[i], '(', jointed[i], ')-', bodies_idx[i], '-')
			if('ts' %in% mode) str_ts <- paste0(str_ts, joint.types[i], '(', status[i], ')-', bodies_idx[i], '-')
			if('tjs' %in% mode) str_tjs <- paste0(str_tjs, joint.types[i], '(', jointed[i], status[i], ')-', bodies_idx[i], '-')
			if('p' %in% mode) str_p <- paste0(str_p, joint_names[i], '(', jointed[i], status[i], ')-', bodies_idx[i], '-')
		}else{
			if('t' %in% mode)  str_t <- paste0(str_t, joint.types[i])
			if('tj' %in% mode) str_tj <- paste0(str_tj, joint.types[i], '(', jointed[i], ')')
			if('ts' %in% mode) str_ts <- paste0(str_ts, joint.types[i], '(', status[i], ')')
			if('tjs' %in% mode) str_tjs <- paste0(str_tjs, joint.types[i], '(', jointed[i], status[i], ')')
			if('p' %in% mode) str_p <- paste0(str_p, joint_names[i], '(', jointed[i], status[i], ')')
		}
	}
	
	return(list('t'=str_t, 'tj'=str_tj, 'ts'=str_ts, 'tjs'=str_tjs, 'p'=str_p, 'f'=str_f))
}
