fit_joint_model <- function(coor, rot.wt = 'angle'){

	# Get joint model names
	joint_types_r <- joint_types()
	joint_model_names <- joint_types_r$names
	dof <- joint_types_r$dof
	
	# Get number of iterations
	n_iter <- dim(coor)[3]

	# Align coordinates by centroid
	coor_align <- alignByCentroid(coor)

	# Fit joint constraint
	fitJointConstraint(coor, type='R')

return(1)

	# Estimate CoR and AoR
	estim_rotate <- estimRotate(coor, wt=rot.wt, coor.align=coor_align)

	#
#	print(estim_rotate)


	for(i in 1:length(joint_model_names)){
	
		# 
		#print(joint_model_names[i])
		
		break
	}

	# Array of coordinates
#	print(coor)
}