transformBody <- function(mechanism, body, tmat, iter, replace = FALSE, reverse = FALSE, status.solved.to = NULL, 
	status.jointed.to = NULL, at.joint = NULL, print.progress = FALSE, indent = '\t', indent.level = 3){

	if(print.progress) cat(paste0(paste0(rep(indent, indent.level), collapse=''), 'transformBody() '))

	if(replace){
		# Replace any transformations with current
		mechanism[['tmat']][, , body, iter] <- tmat
	}else{
		if(reverse){
			# Apply transformation to current body transformation
			mechanism[['tmat']][, , body, iter] <- mechanism[['tmat']][, , body, iter] %*% tmat
		}else{
			# Apply transformation to current body transformation
			mechanism[['tmat']][, , body, iter] <- tmat %*% mechanism[['tmat']][, , body, iter]
		}
	}

	# Find associated joints and sets
	body_joints <- mechanism[['joint.transform']][[body]]
	jt_set <- mechanism[['joint.set.transform']][[body]]
	
	# Check if joints
	if(length(body_joints) > 0){

		#if(print.progress) print(mechanism[['joint.coor.anim']][body_joints, , iter, ])

		for(i in 1:length(body_joints)){
			
			# Get joint index
			jt_idx <- body_joints[i]
			
			# Change statuses
			if(!jt_idx %in% at.joint){
				mechanism[['status']][['jointed']][jt_idx] <- FALSE
				mechanism[['status']][['transformed']][jt_idx, jt_set[i]] <- TRUE
			}else{
				if(!is.null(status.jointed.to)){
					if(status.jointed.to){
						mechanism[['status']][['jointed']][jt_idx] <- TRUE
						mechanism[['status']][['transformed']][jt_idx, ] <- FALSE
					}else{
						# Not sure if this condition is possible/necessary
					}
				}
			}

			# Set joint solved status
			if(!is.null(status.solved.to)){
				if(!is.null(at.joint) && jt_idx %in% at.joint){
					if(status.solved.to > mechanism[['status']][['solved']][jt_idx, 1]) mechanism[['status']][['solved']][jt_idx, 1] <- status.solved.to
					if(status.solved.to > mechanism[['status']][['solved']][jt_idx, 2]) mechanism[['status']][['solved']][jt_idx, 2] <- status.solved.to
				}else{
					if(status.solved.to > mechanism[['status']][['solved']][jt_idx, jt_set[i]]) mechanism[['status']][['solved']][jt_idx, jt_set[i]] <- status.solved.to
				}
			}

			# Apply to joints associated with body
			mechanism[['joint.coor.anim']][jt_idx, , iter, jt_set[i]] <- applyTransform(mechanism[['joint.coor']][jt_idx, ], mechanism[['tmat']][, , body, iter])

			# Skip if no constraint
			if(is.null(mechanism[['joint.cons']][[jt_idx]])) next
			
			# Create point vector matrix
			cons_origin <- matrix(mechanism[['joint.coor']][jt_idx, ], 
				nrow=nrow(mechanism[['joint.cons']][[jt_idx]]), ncol=3)

			# Translate vectors so base is at origin
			cons_point <- cons_origin + mechanism[['joint.cons']][[jt_idx]]
			
			# Transform origin and vectors
			cons_origin_trfm <- applyTransform(cons_origin, mechanism[['tmat']][, , body, iter])
			cons_point_trfm <- applyTransform(cons_point, mechanism[['tmat']][, , body, iter])
			
			# Move back to origin
			mechanism[['joint.cons.anim']][[jt_idx]][, , iter, jt_set] <- cons_point_trfm - cons_origin_trfm
		}

		if(print.progress){

			# Joints transform
			joints_transform <- body_joints[body_joints != at.joint]
			jt_set_transform <- jt_set[body_joints != at.joint]

			# Add joint to list of transformed joints
			apply_to_joints <- paste0(mechanism[['joint.names']][joints_transform], '(', joints_transform, ')-', jt_set_transform)
		}

		if(print.progress){
			#cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Transform body \'', mechanism[['body.names']][body], '\' (', body, ') and associated joints: ', paste0(apply_to_joints, collapse=', ')))
			cat(paste0('\'', mechanism[['body.names']][body], '\' (', body, ')'))
			if(!is.null(at.joint)) cat(paste0(' at joint ', mechanism[['joint.names']][at.joint], '(', at.joint, ')'))
			cat(paste0(' and associated joints: ', paste0(apply_to_joints, collapse=', ')))
			#print(mechanism[['joint.coor.anim']][body_joints, , iter, ])
		}
	}

	if(print.progress) cat('\n')

	mechanism
}