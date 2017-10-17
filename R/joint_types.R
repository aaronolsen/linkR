joint_types <- function(){

	joint_type <- setNames(c('R', 'U', 'O', 'S', 'L', 'P', 'T', 'ST', 'RL', 'RP', 'UL', 'RT', 'SL', 'UP', 'SP', 'UT'), 
		c('1R-0T', '2R-0T', '3R-0T', '3R-0T', '0R-1T', '0R-2T', '0R-3T', '3R-3T', '1R-1T', '1R-2T', '2R-1T', '1R-3T', '3R-1T', '2R-2T', '3R-2T', '2R-3T'))

	# Set DoFs for each joint type
	dof_num <- rbind(c(1,0), c(0,1), c(0,2), c(2,0), c(3,0), c(3,0), c(0,3)) 
	rownames(dof_num) <- c('R', 'L', 'P', 'U', 'O', 'S', 'T')

	dof_mat <- matrix(0, nrow=length(joint_type), ncol=2)
	
	for(i in 1:nrow(dof_mat)){

		type_split <- strsplit(joint_type[i], '')[[1]]

		# Identify rotational and position degrees of freedom from joint type
		if(type_split[1] %in% rownames(dof_num)) dof_mat[i, ] <- dof_mat[i, ] + dof_num[type_split[1], ]
		if(length(type_split) == 2 && type_split[2] %in% rownames(dof_num)) dof_mat[i, ] <- dof_mat[i, ] + dof_num[type_split[2], ]
	}

	colnames(dof_mat) <- c('R', 'T')
	rownames(dof_mat) <- joint_type

	list(
		names=joint_type,
		dof=dof_mat
	)
}