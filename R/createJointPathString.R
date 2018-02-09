createJointPathString <- function(mechanism, joints, path, print.progress = FALSE){

	# Get path length
	path_length <- length(joints)

	# Get joint types
	joint_types <- mechanism[['joint.types']][joints]

	# Get whether joints are jointed
	joint_jointed <- rep('D', path_length)
	joint_jointed[mechanism[['status']][['jointed']][joints]] <- 'J'

	# Get whether halves are solved
	joint_sets <- mechanism[['paths.closed.set']][[path]]

	# Get whether joint is solved/fixed
	joint_solved_set1 <- joint_solved_set2 <- rep('N', path_length)
	for(j in 1:length(joints)){
		if(mechanism[['status']][['solved']][joints[j], joint_sets[j,1]] > 1) joint_solved_set1[j] <- 'S'
		if(mechanism[['status']][['solved']][joints[j], joint_sets[j,2]] > 1) joint_solved_set2[j] <- 'S'
	}

	# Create string
	test_str <- paste0(joint_types, '(', joint_jointed, joint_solved_set1, joint_solved_set2 ,')', collapse='-')
	
	if(print.progress){
		print_str <- paste0(mechanism[['joint.names']][joints], '(', joint_jointed, joint_solved_set1, joint_solved_set2 ,')', collapse='-')
	}else{
		print_str <- NULL
	}
	
	list(
		'test'=test_str,
		'print'=print_str
	)
}
