extendTransformation <- function(tmarr, body.num, iter, joints.transform, joint.coor, 
	joint.cons, joint.ref, joint.cons.ref, joint.types, joint.status, status.to, body.names = NULL, joint.names = NULL, 
	indent = '', indent.level=1, print.progress = FALSE){

	if(print.progress){
		cat(paste0(paste0(rep(indent, indent.level), collapse=''), 'extendTransformation\n'))
		cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Apply to body: '))
		cat(paste0(paste0(sort(body.names[body.num]), collapse=', '), '\n'))
		apply_to_joints <- c()
	}

	if(length(status.to) == 1) status.to <- matrix('c', nrow(joint.status), ncol(joint.status))

	# GET JOINTS ASSOCIATED WITH BODY
	for(jt_set in 1:2){

		# FIND ASSOCIATED JOINTS
		body_joints <- joints.transform[[jt_set]][[body.num]]
		
		if(length(body_joints) == 0) next

		# ADD TO LIST OF TRANSFORMED JOINTS
		if(print.progress) apply_to_joints <- c(apply_to_joints, paste0(joint.names[body_joints], '(', body_joints, ')-', jt_set))

		# APPLY TO JOINTS ASSOCIATED WITH BODY BUT NOT IN PATH
		joint.coor[body_joints, , iter, jt_set] <- applyTransform(joint.ref[body_joints, ], tmarr[, , body.num, iter])

		# UPDATE JOINT STATUS
		joint.status[body_joints, jt_set] <- status.to[body_joints, jt_set]

		for(joint_num in body_joints){

			# SKIP IF NO CONSTRAINT
			if(is.null(joint.cons.ref[[joint_num]])) next
			
			# TRANSFORM JOINT CONSTRAINTS
			if(is.matrix(joint.cons.ref[[joint_num]][, , 1])){
				for(i in 1:nrow(joint.cons.ref[[joint_num]][, , 1])){

					# Skip row if joint constraint is NA (when joint constraints are split between joined bodies)
					if(is.na(joint.cons[[joint_num]][i, 1, iter, jt_set])) next

					joint_cons_point <- rbind(joint.ref[joint_num, ], joint.ref[joint_num, ]+joint.cons.ref[[joint_num]][i, , 1])
					joint_cons_point <- applyTransform(joint_cons_point, tmarr[, , body.num, iter])
					joint.cons[[joint_num]][i, , iter, jt_set] <- joint_cons_point[2, ]-joint_cons_point[1, ]
				}
			}else{
				joint_cons_point <- rbind(joint.ref[joint_num, ], joint.ref[joint_num, ]+joint.cons.ref[[joint_num]][, , 1])
				joint_cons_point <- applyTransform(joint_cons_point, tmarr[, , body.num, iter])
				joint.cons[[joint_num]][, , iter, jt_set] <- joint_cons_point[2, ]-joint_cons_point[1, ]
			}

		}

		# IF TRANSFORMED JOINT IS _,i APPLY TRANSFORMATION THROUGH THAT JOINT
	}
	
	if(print.progress){
		cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 'Apply to joint(s): '))
		cat(paste0(paste0(sort(apply_to_joints), collapse=', '), '\n'))
	}

	list(
		'tmarr'=tmarr, 
		'joint.coor'=joint.coor, 
		'joint.cons'=joint.cons, 
		'joint.status'=joint.status
	)
}