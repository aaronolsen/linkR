applyJointTransform <- function(mechanism, joint = NULL, iter = NULL){

	if(!is.null(joint)){
	
		# Get animated joint coordinate structure
		joint_coor <- array(mechanism[['joint.coor.anim']][joint, , iter, ], dim=c(length(joint), 3, 2), 
			dimnames=list(mechanism[['joint.names']][joint], NULL, NULL))

		joint_cons <- list()
		for(i in 1:length(joint)){

			if(is.null(mechanism[['joint.cons']][[joint[i]]])){

				# If no constraints, set as NULL
				joint_cons[[i]] <- NULL

			}else{
				
				# Get animated joint constraint structure
				joint_cons[[i]] <- array(mechanism[['joint.cons.anim']][[joint[i]]][, , iter, ], 
					dim=c(dim(mechanism[['joint.cons.anim']][[joint[i]]])[1:2], 2))

				# Create point vector matrix
				cons_origin <- matrix(mechanism[['joint.coor']][joint[i], ], nrow=nrow(mechanism[['joint.cons']][[joint[i]]]), ncol=3)

				# Translate vectors so base is at origin
				cons_point <- cons_origin + mechanism[['joint.cons']][[joint[i]]]
			}

			for(set in 1:2){

				# Apply transformation to joint coordinate
				joint_coor[i, , set] <- applyTransform(mechanism[['joint.coor']][joint[i], ], 
					mechanism[['tmat']][, , mechanism[['body.conn']][joint[i], set], iter])

				# Transform origin and vectors
				cons_origin_trfm <- applyTransform(cons_origin, mechanism[['tmat']][, , mechanism[['body.conn']][joint[i], set], iter])
				cons_point_trfm <- applyTransform(cons_point, mechanism[['tmat']][, , mechanism[['body.conn']][joint[i], set], iter])

				# Move back to origin
				joint_cons[[i]][, , set] <- cons_point_trfm - cons_origin_trfm
			}

			# If U-joint, set axis in other set to NA
			if(mechanism[['joint.types']][joint[i]] %in% c('U')){
				joint_cons[[i]][2, , 1] <- NA
				joint_cons[[i]][1, , 2] <- NA
			}
		}

		# Set names for joint constraint list
		names(joint_cons) <- mechanism[['joint.names']][joint]

		return(list('coor'=joint_coor, 'cons'=joint_cons))

	}else{

		for(iter in 1:mechanism[['num.iter']]){
			for(body_num in 1:mechanism[['num.bodies']]){

				# Get joints and sets
				for(i in 1:length(mechanism[['joint.transform']][[body_num]])){

					# Find associated joints and sets
					jt_idx <- mechanism[['joint.transform']][[body_num]][i]
					jt_set <- mechanism[['joint.set.transform']][[body_num]][i]

					# Apply to joints associated with body
					mechanism[['joint.coor.anim']][jt_idx, , iter, jt_set] <- applyTransform(mechanism[['joint.coor']][jt_idx, ], 
						mechanism[['tmat']][, , body_num, iter])

					# Skip if no constraint
					if(is.null(mechanism[['joint.cons']][[jt_idx]])) next
				
					# Create point vector matrix
					cons_origin <- matrix(mechanism[['joint.coor']][jt_idx, ], 
						nrow=nrow(mechanism[['joint.cons']][[jt_idx]]), ncol=3)

					# Translate vectors so base is at origin
					cons_point <- cons_origin + mechanism[['joint.cons']][[jt_idx]]
		
					# Transform origin and vectors
					cons_origin_trfm <- applyTransform(cons_origin, mechanism[['tmat']][, , body_num, iter])
					cons_point_trfm <- applyTransform(cons_point, mechanism[['tmat']][, , body_num, iter])
		
					# Move back to origin
					mechanism[['joint.cons.anim']][[jt_idx]][, , iter, jt_set] <- cons_point_trfm - cons_origin_trfm

					# If U-joint, set axis in other set to NA
					if(mechanism[['joint.types']][jt_idx] %in% c('U')){
						mechanism[['joint.cons.anim']][[jt_idx]][2, , iter, 1] <- NA
						mechanism[['joint.cons.anim']][[jt_idx]][1, , iter, 2] <- NA
					}
				}
			}
		}
		
		return(mechanism)
	}

}
