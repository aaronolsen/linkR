resolveDisjoints <- function(mechanism, iter, joint = NULL, recursive = FALSE, print.progress = FALSE, indent = '\t', indent.level=3){

	if(print.progress) cat(paste0(paste0(rep(indent, indent.level), collapse=''), 'resolveDisjoints()\n'))

	# Set max number of recursive loops
	if(recursive){ n_max <- 5 }else{ n_max <- 1 }

	#if(print.progress) print(mechanism[['status']])

	n <- 0
	while(n < n_max){

		# Find any disjointed joints
		disjointeds <- which(!mechanism[['status']][['jointed']])

		# If no disjointed joints stop
		if(!any(disjointeds)) break
		
		for(disjointed in disjointeds){

			# Find transformed joint sets
			jt_set_t <- which(mechanism[['status']][['transformed']][disjointed, ])
		
			# If both are transformed skip
			if(length(jt_set_t) == 2) next

			# Find untransformed joint sets
			jt_set_u <- which(!mechanism[['status']][['transformed']][disjointed, ])

			# Find transformed and untransformed bodies
			body_t <- mechanism[['body.conn.num']][disjointed, jt_set_t]
			body_u <- mechanism[['body.conn.num']][disjointed, jt_set_u]

			if(print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 
				'Resolve disjoint across joint \'', mechanism[['joint.names']][disjointed], 
				'\' (', disjointed, ')\n'))

			# Transformation body
			mechanism <- transformBody(mechanism, status.solved.to=NULL, body=body_u, 
				tmat=mechanism[['tmat']][, , body_t, iter], iter=iter, print.progress=print.progress, 
				indent=indent, indent.level=indent.level+2)
			
			# Change status
			mechanism[['status']][['jointed']][disjointed] <- TRUE
			mechanism[['status']][['transformed']][disjointed, ] <- FALSE

			# Apply transformation 'across' joint 
			if(print.progress){
				#print(disjointed)
				#print(jt_set_t)
				#print(body_t)
				#print(body_u)
			}

			if(print.progress) print_joint_status(mechanism, indent, indent.level+2)
		
			break
		}
		
		#break

		n <- n + 1
	}

	# Search for disjointed joints

	mechanism
}