extendSolvedStatus <- function(mechanism, print.progress = FALSE, indent = '\t', indent.level=3){

	## For any body with fixed, solved joints make all joints fixed

	if(print.progress) cat(paste0(paste0(rep(indent, indent.level), collapse=''), 'extendSolvedStatus()\n'))

	# Set max number of recursive loops
	ct_max <- 6

	ct <- 0
	while(ct < ct_max){

		# 
		changed_status <- FALSE
		
		for(i in 1:mechanism[['num.joints']]){

			# Check that whole joint is solved
			if(any(mechanism[['status']][['solved']][i,] == 0)) next

			# Check that only one is fixed
			if(sum(mechanism[['status']][['solved']][i,] == 2) != 1) next

			# Check that joint is jointed
			if(!mechanism[['status']][['jointed']][i]) next
			
			# Set that status will be changed
			changed_status <- TRUE

			# Find set with '1' solved status
			set1 <- which(mechanism[['status']][['solved']][i,] == 1)
			
			# Find body with set 1
			body_num <- mechanism[['body.conn.num']][i,set1]

			# Find all joints associated with body
			body_joints <- mechanism[['body.joints']][[body_num]]

			# Set all joint halves in body to '2' (fixed)
			if(print.progress) apply_to_joints <- c()
			for(j in 1:length(body_joints)){

				# If joint is solved
				#if(sum(mechanism[['status']][['solved']][body_joints[j],] > 0) == 2){

					#mechanism[['status']][['solved']][body_joints[j], ] <- 2

					#if(print.progress) apply_to_joints <- c(apply_to_joints, 
					#	paste0(mechanism[['joint.names']][body_joints[j]], '(', body_joints[j], ')'))
				#}else{

					joint_set <- which(mechanism[['body.conn.num']][body_joints[j], ] == body_num)
					mechanism[['status']][['solved']][body_joints[j],joint_set] <- 2

					if(print.progress) apply_to_joints <- c(apply_to_joints, 
						paste0(mechanism[['joint.names']][body_joints[j]], '(', body_joints[j], ')-', joint_set))
				#}
			}

			if(print.progress){
				if(ct == ct_max-1){ ct_print <- 'max' }else{ ct_print <- ct }
				cat(paste0(paste0(rep(indent, indent.level+1), collapse=''), 
					'Extend fixed status across joint ', mechanism[['joint.names']][i], '(', i, 
					') to joints ', paste0(apply_to_joints, collapse=', '), ' of body \'', 
					mechanism[['body.names']][body_num], '\' (', body_num, '), ct=', ct_print, '\n'))
			}
		}
		
		if(!changed_status) break

		ct <- ct + 1
	}

	mechanism
}