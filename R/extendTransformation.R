extendTransformation <- function(mechanism, iter, tmat = NULL, joint = NULL, body = NULL, 
	recursive = FALSE, body.excl = NULL, replace = FALSE, reverse = FALSE, status.solved.to = NULL, 
	print.progress = FALSE, indent = '\t', indent.level=3){

	if(print.progress) cat(paste0(paste0(rep(indent, indent.level), collapse=''), 'extendTransformation()\n'))

	# Set max number of recursive loops
	if(recursive){ ct_max <- 100 }else{ ct_max <- 2 }

	# Find any already disjointed joints (transformations shouldn't be extended across these joints 
	# because it would be different than the original transformation that caused the joint to be disjointed
	joint.excl <- which(!mechanism[['status']][['jointed']])

	# Input joint is jointed and tmat is given; transform input body with tmat, then extend transformation across disjointed joints
	if(mechanism[['status']][['jointed']][joint] && !is.null(tmat)){

		if(print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), '0) '))

		# Transform body
		mechanism <- transformBody(mechanism, body=body, tmat=tmat, iter=iter, at.joint=joint, 
			status.solved.to=status.solved.to, print.progress=print.progress, indent.level=0)
	}

	# Input joint is disjointed and no tmat is given; find transformation that restores disjointed joint
	# and then find tmat to apply to all subsequently disjointed joints
	if(!mechanism[['status']][['jointed']][joint] && is.null(tmat)){

		if(print.progress) cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), '0) '))

		# Find body across joint
		body_0 <- mechanism[['body.conn.num']][joint, ]
		body_0 <- body_0[body_0 != body]

		# Set transformation for all other bodies
		tmat <- mechanism[['tmat']][, , body_0, iter] %*% solve(mechanism[['tmat']][, , body, iter])

		# Apply transformation of first body to second body to rejoin the bodies
		mechanism <- transformBody(mechanism, body=body, 
			tmat=mechanism[['tmat']][, , body_0, iter], iter=iter, 
			at.joint=joint, replace=TRUE, status.solved.to=0, status.jointed.to=TRUE, 
			print.progress=print.progress, indent.level=0)
	}
	
	# Set initial matrix of transformed joints
	local_transformed <- matrix(FALSE, nrow=mechanism[['num.joints']], ncol=2)

	ct <- 1
	while(ct < ct_max){

		# Find any disjointed joints
		disjointeds <- which(!mechanism[['status']][['jointed']])
		
		# Remove excluded joints
		disjointeds <- disjointeds[!disjointeds %in% joint.excl]

		# If no disjointed joints stop
		if(!any(disjointeds)) break

		# Start check whether any joint has been joined (to know when to stop cycles)
		any_joined <- FALSE

		# Loop through disjointed joints
		for(disjointed in disjointeds){
			
			# If fixed joint, skip
			#if(disjointed %in% mechanism[['fixed.joints']]) next

			# Find transformed joint sets
			jt_set_t <- which(mechanism[['status']][['transformed']][disjointed, ])
	
			# If both are transformed, skip
			if(length(jt_set_t) == 2) next

			# If either set is fixed, skip
			#if(any(mechanism[['status']][['solved']][disjointed, ] == 2)) next

			# Find untransformed joint sets
			jt_set_u <- which(!mechanism[['status']][['transformed']][disjointed, ])

			# Find untransformed body
			body_u <- mechanism[['body.conn.num']][disjointed, jt_set_u]

			# If fixed body, skip (do not want to transform fixed body)
			if(body_u == 1) next

			# Skip if body is in body exclude list			
			if(!is.null(body.excl) && body_u %in% body.excl) next

			# Get joints in untransformed body
			body_joints <- mechanism[['body.joints']][[body_u]]

			# Remove current (disjointed) joint
			body_joints <- body_joints[body_joints != disjointed]

			# Transformations should not end with any solved joints disjointed. So check 
			# that transformation can be extended across any solved joints in transformed body
			will_disjoint_solved_joint <- FALSE

			if(length(body_joints) > 0){

				for(i in 1:length(body_joints)){

					# If joint is not solved, skip
					if(sum(mechanism[['status']][['solved']][body_joints[i], ] > 0) < 2) next
				
					# If disjoints fixed joint then do no transform - not sure if this is needed
					# May help algorithm by having a particular joint type consistently not disjointed?
					if(body_joints[i] %in% mechanism[['fixed.joints']]){
						will_disjoint_solved_joint <- TRUE
						break
					}

					# If joint is fixed, then will leave disjointed because body across joint cannot move
					if(any(mechanism[['status']][['solved']][body_joints[i], ] == 2)){
						will_disjoint_solved_joint <- TRUE
						break
					}

					# Get adjoining body
					body_conn <- mechanism[['body.conn.num']][body_joints[i], ]
					body_adj <- body_conn[body_conn != body_u]

					# Skip if body across solved joint is in body exclude list
					if(body_adj %in% body.excl){
						will_disjoint_solved_joint <- TRUE
						break
					}
				}
			}

			# Transformation will leave solve joint disjointed - do no proceed
			if(will_disjoint_solved_joint) next

			# Get transformed state before transformBody()
			transformed_pre <- mechanism[['status']][['transformed']]

			if(print.progress){
				if(ct == ct_max-1){ ct_print <- 'max' }else{ ct_print <- ct }
				cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), ct_print, ') '))
			}

			# Transformation body
			mechanism <- transformBody(mechanism, status.solved.to=NULL, body=body_u, 
				tmat=tmat, at.joint=disjointed, replace=replace, reverse=reverse, 
				iter=iter, print.progress=print.progress, 
				indent.level=0)

			mechanism[['status']][['jointed']][disjointed] <- TRUE
			mechanism[['status']][['transformed']][disjointed, ] <- FALSE
			
			# Update local transformed states
			local_transformed[(mechanism[['status']][['transformed']] - transformed_pre) == TRUE] <- TRUE
			
			# Find any joints where both sets have been transformed locally (within extendTransformation)
			both_sets_transformed <- which(rowSums(local_transformed) == 2)
			
			# Set these as jointed (since same transformation has been applied to both sets, joint will be jointed again)
			if(length(both_sets_transformed) > 0){
				mechanism[['status']][['jointed']][both_sets_transformed] <- TRUE
				mechanism[['status']][['transformed']][both_sets_transformed, ] <- FALSE
			}
			
			# Mark that a joint has been joined
			any_joined <- TRUE

			#if(print.progress) print_joint_status(mechanism, indent, indent.level+2)
		
			# Once disjointed joint is found break to find new set of disjointed joints
			break
		}
		
		if(!any_joined) break

		ct <- ct + 1
	}	

	mechanism
}