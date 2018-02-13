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
		}

		if(print.progress){

			# Joints transform
			joints_transform <- body_joints[body_joints != at.joint]
			jt_set_transform <- jt_set[body_joints != at.joint]

			# Add joint to list of transformed joints
			if(length(joints_transform) > 0){
				apply_to_joints <- paste0(mechanism[['joint.names']][joints_transform], '(', joints_transform, ')-', jt_set_transform)
			}else{
				apply_to_joints <- NULL
			}

			#cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Transform body \'', mechanism[['body.names']][body], '\' (', body, ') and associated joints: ', paste0(apply_to_joints, collapse=', ')))
			cat(paste0('\'', mechanism[['body.names']][body], '\' (', body, ')'))
			if(!is.null(at.joint)) cat(paste0(' at joint ', mechanism[['joint.names']][at.joint], '(', at.joint, ')'))
			if(!is.null(apply_to_joints)) cat(paste0(' and associated joints: ', paste0(apply_to_joints, collapse=', ')))
		}
	}

	if(print.progress) cat('\n')

	mechanism
}