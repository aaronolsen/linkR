solveUJointParamsQuat <- function(q, f.axis, r.axis, mat=diag(3)){

	# Given quaternion describing rotation, a fixed axis and a rotating axis, this 
	# solves for the angles about both axes and the error

	# Find rotational component about fixed AoR
	rot_comp1 <- rotCompQuat(q, f.axis, mat=mat)
	
	# Apply rotation to rotating AoR
	r.axis <- r.axis %*% tMatrixEP(f.axis, -rot_comp1$angles[1])

	# Find rotational components about fixed and rotated AoR
	rot_comp2 <- rotCompQuat(q, f.axis, r.axis, mat=mat)

	return(list(
		'f.angle'=-rot_comp2$angles[1],
		'r.axis.r'=r.axis,
		'r.angle'=-rot_comp2$angles[2],
		'error'=rot_comp2$error
	))
}