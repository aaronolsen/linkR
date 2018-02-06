resolveDisjoints <- function(mechanism, iter, joint = NULL, recursive = FALSE, print.progress = FALSE, indent = '\t', indent.level=3){

	if(print.progress) cat(paste0(paste0(rep(indent, indent.level), collapse=''), 'resolveDisjoints()\n'))

	# Set max number of recursive loops
	if(recursive){ n_max <- 6 }else{ n_max <- 1 }

	#if(print.progress) print_joint_status(mechanism, indent, indent.level+2)

	n <- 0
	while(n < n_max){

		if(!is.null(joint)){

			# Set disjointed joint
			disjointeds <- joint

		}else{

			# Find any disjointed joints
			disjointeds <- which(!mechanism[['status']][['jointed']])

			# If no disjointed joints stop
			if(!any(disjointeds)) break
		}
		
		any_disjointed <- FALSE
		for(disjointed in disjointeds){

			# Find transformed joint sets
			jt_set_t <- which(mechanism[['status']][['transformed']][disjointed, ])
		
			# If both are transformed, skip
			if(length(jt_set_t) == 2) next

			# If either set is fixed, skip
			if(any(mechanism[['status']][['solved']][disjointed, ] == 2)) next

			# Find untransformed joint sets
			jt_set_u <- which(!mechanism[['status']][['transformed']][disjointed, ])

			# Find transformed and untransformed bodies
			body_t <- mechanism[['body.conn.num']][disjointed, jt_set_t]
			body_u <- mechanism[['body.conn.num']][disjointed, jt_set_u]

			if(print.progress){
				if(n == n_max-1){
					n_print <- 'max'
				}else{
					n_print <- n
				}
				cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 
					'Resolve disjoint across joint \'', mechanism[['joint.names']][disjointed], 
					'\' (', disjointed, '), n=', n, '\n'))
			}

			# Transformation body
			mechanism <- transformBody(mechanism, status.solved.to=NULL, body=body_u, 
				tmat=mechanism[['tmat']][, , body_t, iter], iter=iter, print.progress=print.progress, 
				indent=indent, indent.level=indent.level+2)
			
			# Change status
			mechanism[['status']][['jointed']][disjointed] <- TRUE
			mechanism[['status']][['transformed']][disjointed, ] <- FALSE
			
			# Mark if a joint is disjointed
			any_disjointed <- TRUE

			#if(print.progress) print_joint_status(mechanism, indent, indent.level+2)
		
			break
		}
		
		if(!any_disjointed) break

		n <- n + 1
	}

	mechanism
}