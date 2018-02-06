resolveJointPaths <- function(mechanism, iter, print.progress = FALSE, indent = '\t', indent.level=3){

	if(print.progress) cat(paste0(paste0(rep(indent, indent.level), collapse=''), 'resolveJointPaths()\n'))

	for(i in 1:mechanism[['num.paths.closed']]){
		
		# Get joint indices
		joint_idx <- mechanism[['paths.closed']][[i]]
		
		# Get path length
		path_length <- length(joint_idx)

		# Get joint types
		joint_types <- mechanism[['joint.types']][joint_idx]

		#'solved-1'=mechanism[['status']][['solved']][,1],
		#'transformed-1'=mechanism[['status']][['transformed']][,1],
		
		# Get whether halves are solved
		if(print.progress) print(mechanism[['paths.closed.set']][[i]])

		# Get whether joints are jointed
		joint_jointed <- rep('D', path_length)
		joint_jointed[mechanism[['status']][['jointed']][joint_idx]] <- 'J'
		
		if(print.progress) print(joint_jointed)

		break
	}
	

	# Create joint path string

	mechanism
}