transformBody <- function(mechanism, status.solved.to, body, tmat, iter, print.progress, indent){

	if(print.progress) cat(paste0(paste0(rep(indent, 3), collapse=''), 'transformBody()\n'))

	# Apply transformation to current body transformation
	mechanism[['body.tmat']][, , body, iter] <- tmat %*% mechanism[['body.tmat']][, , body, iter]

	# Find associated joints and sets
	body_joints <- mechanism[['joint.transform']][[body]]
	jt_set <- mechanism[['joint.set.transform']][[body]]
	
	#
	if(print.progress) apply_to_joints <- c()

	# Check if joints
	if(length(body_joints) > 0){

		# Add joint to list of transformed joints
		if(print.progress) apply_to_joints <- c(apply_to_joints, paste0(mechanism[['joint.names']][body_joints], '(', body_joints, ')-', jt_set))

		for(i in 1:length(body_joints)){
			
			# Get joint index
			jt_idx <- body_joints[i]
			
			# Change statuses
			if(!jt_idx %in% mechanism[['fixed.joints']]){
				mechanism[['status']][['jointed']][jt_idx] <- FALSE
				mechanism[['status']][['transformed']][jt_idx, jt_set[i]] <- TRUE
			}

			# Set joint solved status
			mechanism[['status']][['solved']][[jt_idx, jt_set[i]]] <- status.solved.to

			# Apply to joints associated with body
			mechanism[['joint.coor.anim']][jt_idx, , iter, jt_set[i]] <- applyTransform(mechanism[['joint.coor']][jt_idx, ], tmat)

			# Skip if no constraint
			if(is.null(mechanism[['joint.cons']][[jt_idx]])) next
			
			# Create point vector matrix
			cons_origin <- matrix(mechanism[['joint.coor']][jt_idx, ], 
				nrow=nrow(mechanism[['joint.cons']][[jt_idx]]), ncol=3)

			# Translate vectors so base is at origin
			cons_point <- cons_origin + mechanism[['joint.cons']][[jt_idx]]
			
			# Transform origin and vectors
			cons_origin_trfm <- applyTransform(cons_origin, tmat)
			cons_point_trfm <- applyTransform(cons_point, tmat)
			
			# Move back to origin
			mechanism[['joint.cons.anim']][[jt_idx]][, , iter, jt_set] <- cons_point_trfm - cons_origin_trfm
		}

		if(print.progress){
			cat(paste0(paste0(rep(indent, 4), collapse=''), 'Transform joints: ', paste0(apply_to_joints, collapse=', ')))
		}
	}

	if(print.progress) cat('\n')

	mechanism
}